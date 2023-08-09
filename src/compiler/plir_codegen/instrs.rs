use std::collections::HashMap;
use std::fmt::Debug;

use crate::compiler::plir::{ProcStmt, self, Located};
use crate::err::{CursorRange, GonErr};

use super::walkers::{ExprBlockIter, ExprBlockIterMut};
use super::{PLIRErr, PLIRResult};

/// The types of blocks.
/// 
/// This determines how exits from this block are handled.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub(super) enum BlockBehavior {
    /// This block is a function body.
    Function,

    /// This block is the body for a `while` or `for` loop.
    Loop,

    /// This block is on its own.
    Bare,

    /// This block is the program.
    Top
}

impl BlockBehavior {
    pub(super) fn propagates(&self, term_stmt: &ProcStmt) -> Result<bool, PLIRErr> {
        assert!(term_stmt.is_simple_terminal(), "expected statement to be terminal");
        match (self, term_stmt) {
            (_, ProcStmt::Decl(_)) => unreachable!("not terminal statement"),
            (_, ProcStmt::Expr(_)) => unreachable!("not terminal statement"),

            (BlockBehavior::Function, ProcStmt::Return(_)) => Ok(false),
            (BlockBehavior::Function, ProcStmt::Break)     => Err(PLIRErr::CannotBreak),
            (BlockBehavior::Function, ProcStmt::Continue)  => Err(PLIRErr::CannotContinue),
            (BlockBehavior::Function, ProcStmt::Throw(_))  => Ok(false),
            (BlockBehavior::Function, ProcStmt::Exit(_))   => Ok(false),

            (BlockBehavior::Loop, ProcStmt::Return(_)) => Ok(true),
            (BlockBehavior::Loop, ProcStmt::Break)     => Ok(false),
            (BlockBehavior::Loop, ProcStmt::Continue)  => Ok(false),
            (BlockBehavior::Loop, ProcStmt::Throw(_))  => Ok(true),
            (BlockBehavior::Loop, ProcStmt::Exit(_))   => Ok(false),

            (BlockBehavior::Bare, ProcStmt::Return(_)) => Ok(true),
            (BlockBehavior::Bare, ProcStmt::Break)     => Ok(true),
            (BlockBehavior::Bare, ProcStmt::Continue)  => Ok(true),
            (BlockBehavior::Bare, ProcStmt::Throw(_))  => Ok(true),
            (BlockBehavior::Bare, ProcStmt::Exit(_))   => Ok(false),

            (BlockBehavior::Top, ProcStmt::Return(_)) => Err(PLIRErr::CannotBreak),
            (BlockBehavior::Top, ProcStmt::Break)     => Err(PLIRErr::CannotContinue),
            (BlockBehavior::Top, ProcStmt::Continue)  => Err(PLIRErr::CannotReturn),
            (BlockBehavior::Top, ProcStmt::Throw(_))  => Ok(true),
            (BlockBehavior::Top, ProcStmt::Exit(_))   => Err(PLIRErr::CannotReturn),
        }
    }
}

/// This struct designates a path to some instruction in a tree.
/// It is assumed that the tree is a block (vec of instructions).
/// 
/// This is typically of the format 14.3.43.0.14.9 etc.,
/// or more generally `[instruction index](.[block index].[instruction index])*`.
/// 
/// InstrAddr must have at least one element at all times.
/// 
/// Note that blocks have instructions, and instructions contain expressions which can contain blocks!
#[derive(Clone, Hash, PartialEq, Eq)]
pub(super) struct InstrAddr(Vec<usize>);

// Implementation note:
// Since appending from the end of a stack is easier than the front,
// indexes nearer to the end are actually indexing nearer to root.
impl InstrAddr {
    pub(super) fn new(instr_idx: usize) -> Self {
        Self(vec![instr_idx])
    }

    /// Instruction addresses are built from depths to root, 
    /// so this function allows appending a parent.
    pub(super) fn add_parent(&mut self, instr_idx: usize, block_idx: usize) {
        self.0.extend([block_idx, instr_idx])
    }

    pub(super) fn path_from_root(&self) -> impl Iterator<Item = usize> + '_ {
        self.0.iter().rev().cloned()
    }

    // pub(super) fn with_parent_removed(&self) -> Option<InstrAddr> {
    //     (self.0.len() >= 3).then(|| {
    //         InstrAddr(self.0[2..].to_vec())
    //     })
    // }
}

impl Debug for InstrAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut segs = self.path_from_root();
        let top = segs.next()
            .expect("InstrAddr should be instantiated with at least one element");
        
        write!(f, "{top}")?;
        for seg in segs {
            write!(f, ".{seg}")?;
        }
        Ok(())
    }
}


#[derive(Debug, Default)]
pub(super) struct InstrBlock {
    instructions: Vec<Located<ProcStmt>>,

    /// Each element represents an address to a terminating instruction
    /// in a [`InstrBlock`].
    terminals: HashMap<InstrAddr, CursorRange>,

    /// If closed, any statements added to the block should be
    /// considered unreachable, as the block is known to exit
    /// out of one of its branches.
    closed: bool,
    /// Indicates whether the next instruction should close the instruction block.
    closed_next_instr: bool,

    /// In a given set of instructions, there is an instruction number and a block number.
    /// When a block's terminal data is appended, this is used to find the block number.
    next_block_no: usize
}
impl InstrBlock {
    pub(super) fn new() -> InstrBlock {
        Self {
            instructions: vec![],
            terminals: HashMap::new(),
            closed: false,
            closed_next_instr: false,
            next_block_no: 0
        }
    }

    /// Determine whether another statement can be pushed into the insert block.
    pub(super) fn is_open(&self) -> bool {
        !self.closed
    }

    /// Determine whether the instruction block currently holds any instructions.
    pub(super) fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }
    /// The number of instructions currently in the instruction block.
    pub(super) fn len(&self) -> usize {
        self.instructions.len()
    }

    /// Push a singular statement into this instruction block (if not open).
    /// 
    /// This also registers the statement into the terminal list if it is
    /// a simple terminal statement.
    pub(super) fn push(&mut self, stmt: Located<impl Into<ProcStmt>>) {
        if self.is_open() {
            let stmt = stmt.map(Into::into);
            if stmt.is_simple_terminal() {
                let instr_no = self.len();
                
                self.terminals.insert(InstrAddr::new(instr_no), stmt.range());
                self.closed = true;
            }
            self.next_block_no = 0;
            self.closed |= std::mem::take(&mut self.closed_next_instr);

            self.instructions.push(stmt);
        }
    }

    /// Pop off the last instruction.
    /// 
    /// This does not update terminals, and therefore should not destroy 
    /// or modify any terminal addresses.
    pub(super) fn pop(&mut self) -> Option<Located<ProcStmt>> {
        self.instructions.pop()
        // let linstr = self.instructions.pop();
        
        // if linstr.is_some() {
        //     let mfrag = self.terminals.remove_terminals_w_instr(self.next_instr_no());
        //     Some((linstr, mfrag))
        // } else {
        //     linstr.map(|instr| (instr, None))
        // }
    }

    /// Index instructions, and return a ref to the statement at the index if present.
    pub(super) fn get(&self, addr: &InstrAddr) -> Option<&ProcStmt> {
        let mut addr_scanner = addr.path_from_root();
        
        let top = addr_scanner.next().unwrap();
        let mut stmt = &**self.instructions.get(top)?;

        while let Some((block_idx, stmt_idx)) = Option::zip(addr_scanner.next(), addr_scanner.next()) {
            let block = match stmt {
                | ProcStmt::Decl(plir::Decl { val: e, .. })
                | ProcStmt::Return(Some(e))
                | ProcStmt::Exit(Some(e))
                | ProcStmt::Expr(e)
                => ExprBlockIter::new(e).nth(block_idx),

                | ProcStmt::Return(None)
                | ProcStmt::Break
                | ProcStmt::Continue
                | ProcStmt::Throw(_)
                | ProcStmt::Exit(None)
                => None
            }?;

            stmt = block.1.get(stmt_idx)?;
        }

        Some(stmt)
    }

    fn raw_get_mut<'a>(instrs: &'a mut [Located<ProcStmt>], addr: &InstrAddr) -> Option<&'a mut ProcStmt> {
        let mut addr_scanner = addr.path_from_root();
        
        let top = addr_scanner.next().unwrap();
        let mut stmt = &mut **instrs.get_mut(top)?;

        while let Some((block_idx, stmt_idx)) = Option::zip(addr_scanner.next(), addr_scanner.next()) {
            let block = match stmt {
                | ProcStmt::Decl(plir::Decl { val: e, .. })
                | ProcStmt::Return(Some(e))
                | ProcStmt::Exit(Some(e))
                | ProcStmt::Expr(e)
                => ExprBlockIterMut::new(e).nth(block_idx),

                | ProcStmt::Return(None)
                | ProcStmt::Break
                | ProcStmt::Continue
                | ProcStmt::Throw(_)
                | ProcStmt::Exit(None)
                => None
            }?;

            stmt = block.1.get_mut(stmt_idx)?;
        }

        Some(stmt)
    }

    /// Index instructions, and return a mut ref to the statement at the index if present.
    /// 
    /// A lot of uses for this will be broken. 
    /// 
    /// Non-terminal statements replacing non-terminal statements,
    /// and terminal statements replacing terminal statements will be valid.
    /// 
    /// However, other than that, the block status will not be updated properly.
    pub(super) fn get_mut(&mut self, addr: &InstrAddr) -> Option<&mut ProcStmt> {
        Self::raw_get_mut(&mut self.instructions, addr)
    }

    /// Removes any terminal statements that would be propagated out of this block 
    /// (which is given the provided BlockBehavior).
    pub(super) fn split_off_unpropagated_terminals(&mut self, btype: BlockBehavior) -> PLIRResult<Vec<Located<InstrAddr>>> {
        let len = self.terminals.len();

        let mut propagated = vec![];
        let mut not_propagated = vec![];

        for (addr, loc) in self.terminals.drain() {
            let Some(term) = Self::raw_get_mut(&mut self.instructions, &addr) else {
                panic!("address {addr:?} has no statement");
            };

            let is_propagating = btype.propagates(term).map_err(|e| e.at_range(loc.clone()))?;
            if is_propagating {
                propagated.push((addr, loc));
            } else {
                not_propagated.push(Located::new(addr, loc));
            }
        }

        let plen = propagated.len();
        self.terminals.extend(propagated);

        if btype == BlockBehavior::Loop {
            // assume loops are always open, because we don't know if the code executes
            self.closed = false;
        } else {
            // if any branches were removed, that means they exit into the enclosing block,
            // so statements can appear after this
            self.closed &= len <= plen;
        }

        Ok(not_propagated)
    }

    /// Pulls out the instructions and terminal data from this InstrBlock.
    pub(super) fn unravel(self) -> (Vec<Located<ProcStmt>>, TerminalFrag) {
        let InstrBlock { instructions, terminals, closed, closed_next_instr: _, next_block_no: _ } = self;
        (instructions, TerminalFrag { terminals, closed })
    }

    fn prepare_fragment(&mut self, frag: &mut TerminalFrag) {
        frag.terminals = frag.terminals.drain()
            .map(|(mut addr, loc)| {
                addr.add_parent(self.len(), self.next_block_no);
                (addr, loc)
            })
            .collect();

        self.next_block_no += 1;
    }
    
    /// Adds terminal data to the instruction block.
    /// 
    /// The fragment addresses and closed status are relative to 
    /// the next pushed instruction in the block.
    pub(super) fn add_fragment(&mut self, mut frag: TerminalFrag) {
        if self.is_open() {
            self.prepare_fragment(&mut frag);
            
            self.terminals.extend(frag.terminals);
            self.closed_next_instr = frag.closed;
        }
    }
    /// Adds a set of conditional fragments to the instruction block.
    /// 
    /// This assumes that the program will have to pass through one of the blocks
    /// associated with the fragments.
    /// 
    /// The fragment addresses and closed statuses are relative to 
    /// the next pushed instruction in the block.
    pub(super) fn add_conditional_fragments(&mut self, mut frags: Vec<TerminalFrag>) {
        if self.is_open() {
            for frag in frags.iter_mut() {
                self.prepare_fragment(frag);
            }
            
            // next instruction is closed if all branches are closed
            let mut closed = true; 

            for frag in frags {
                self.terminals.extend(frag.terminals);
                closed &= frag.closed;
            }
            self.closed_next_instr = closed;
        }
    }
}
impl std::ops::Deref for InstrBlock {
    type Target = [Located<ProcStmt>];

    fn deref(&self) -> &Self::Target {
        &self.instructions
    }
}

/// Struct which handles all of the exits from a block.
#[derive(Debug)]
pub(super) struct TerminalFrag {
    /// Each element represents a pointer to a terminating instruction
    /// in a [`InstrBlock`].
    pub(super) terminals: HashMap<InstrAddr, CursorRange>,

    /// If closed, any statements added to the block should be
    /// considered unreachable, as the block is known to exit
    /// out of one of its branches.
    pub(super) closed: bool,
}
impl TerminalFrag {
    pub(super) fn new() -> Self {
        Self {
            terminals: HashMap::new(),
            closed: false
        }
    }
}