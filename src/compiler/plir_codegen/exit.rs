use std::collections::HashMap;
use std::fmt::Debug;

use crate::compiler::plir::ProcStmt;
use crate::err::CursorRange;

use super::PLIRErr;

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
}

impl Debug for InstrAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (root_instr_idx, rest) = self.0.split_last()
            .unwrap_or_else(|| panic!("InstrAddr should be instantiated with at least one element"));
        
        write!(f, "{root_instr_idx}")?;
        for seg in rest.iter().rev() {
            write!(f, ".{seg}")?;
        }
        Ok(())
    }
}

/// Struct which handles all of the exits from a block.
#[derive(Debug)]
pub(super) struct BlockTerminals {
    /// Each element represents a pointer to a terminating instruction
    /// in a [`InstrBlock`].
    pub(super) branches: HashMap<InstrAddr, CursorRange>,

    /// If closed, any statements added to the block should be
    /// considered unreachable, as the block is known to exit
    /// out of one of its branches.
    pub(super) closed: bool,
}

impl BlockTerminals {
    pub(super) fn new() -> Self {
        Self {
            branches: HashMap::new(),
            closed: false
        }
    }

    pub(super) fn is_closed(&self) -> bool {
        self.closed
    }

    // /// Exits the block and determines which exits will affect the enclosing block.
    // /// 
    // /// The block behavior indicates which type of block this BlockTerminals is from.
    // pub(super) fn propagate(&mut self, bb: BlockBehavior) -> PLIRResult<Vec<Located<Type>>> {
    //     let len = self.branches.len();

    //     let mut propagated = vec![];
    //     let mut halted = vec![];

    //     for Located(exit, exit_range) in std::mem::take(&mut self.branches) {
    //         match bb.handle_exit(exit).map_err(|e| e.at_range(exit_range.clone()))? {
    //             ControlFlow::Continue(ex) => propagated.push(Located::new(ex, exit_range)),
    //             ControlFlow::Break(t)     => halted.push(Located::new(t, exit_range)),
    //         }
    //     }

    //     if bb == BlockBehavior::Loop {
    //         // assume loops are always open, because we don't know if the code executes
    //         self.closed = false;
    //     } else {
    //         // if any branches were removed, that means they exit into the enclosing block,
    //         // so statements can appear after this
    //         self.closed &= len == propagated.len();
    //     }
    //     self.branches = propagated;

    //     Ok(halted)
    // }

    pub(super) fn add_exit(&mut self, addr: InstrAddr, loc: CursorRange, is_unconditional: bool) {
        if !self.closed {
            self.branches.insert(addr, loc);
            self.closed = is_unconditional;
        }
    }
    pub(super) fn add_propagated(&mut self, b: BlockTerminals) {
        if !self.closed {
            self.branches.extend(b.branches);
            self.closed = b.closed;
        }
    }
    pub(super) fn add_conditional(&mut self, blocks: Vec<BlockTerminals>) {
        if !self.closed {
            let mut closed = true;
            for block in blocks {
                self.branches.extend(block.branches);
                closed &= block.closed;
            }
            self.closed = closed;
        }
    }
    pub(super) fn into_fragmented(self) -> TerminalFrag {
        TerminalFrag(self)
    }
}

/// Struct which handles all of the exits from a block.
#[derive(Debug)]
pub(super) struct TerminalFrag(pub(super) BlockTerminals);