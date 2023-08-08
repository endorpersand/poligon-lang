use std::collections::{HashSet, HashMap};
use std::ops::ControlFlow;

use crate::compiler::plir::{self, Type, ty, Located};
use crate::err::{GonErr, CursorRange};

use super::{PLIRErr, PLIRResult};

/// The types of methods in which a block can be exited.
#[derive(Debug, Clone)]
pub(super) enum BlockExit {
    /// This block exited by returning a value.
    Return(Type),

    /// This block exited by `break`.
    Break,

    /// This block exited by `continue`.
    Continue,

    /// This block exited normally.
    Exit(Type),

    /// An exception was thrown
    Throw,
}

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

type BlockExitHandle = ControlFlow<Type, BlockExit>;

impl BlockBehavior {
    fn handle_exit(&self, exit: BlockExit) -> Result<BlockExitHandle, PLIRErr> {
        match self {
            BlockBehavior::Function => match exit {
                BlockExit::Return(t) => Ok(BlockExitHandle::Break(t)),
                BlockExit::Break     => Err(PLIRErr::CannotBreak),
                BlockExit::Continue  => Err(PLIRErr::CannotContinue),
                BlockExit::Exit(t)   => Ok(BlockExitHandle::Break(t)),
                BlockExit::Throw     => Ok(BlockExitHandle::Continue(exit)),
            },
            BlockBehavior::Loop => match exit {
                BlockExit::Return(_) => Ok(BlockExitHandle::Continue(exit)),
                BlockExit::Break     => Ok(BlockExitHandle::Break(ty![Type::S_VOID])),
                BlockExit::Continue  => Ok(BlockExitHandle::Break(ty![Type::S_VOID])),
                BlockExit::Exit(t)   => Ok(BlockExitHandle::Break(t)),
                BlockExit::Throw     => Ok(BlockExitHandle::Continue(exit)),
            },
            BlockBehavior::Bare => match exit {
                BlockExit::Return(_) => Ok(BlockExitHandle::Continue(exit)),
                BlockExit::Break     => Ok(BlockExitHandle::Continue(exit)),
                BlockExit::Continue  => Ok(BlockExitHandle::Continue(exit)),
                BlockExit::Exit(t)   => Ok(BlockExitHandle::Break(t)),
                BlockExit::Throw     => Ok(BlockExitHandle::Continue(exit)),
            },
            BlockBehavior::Top => match exit {
                BlockExit::Return(_) => Err(PLIRErr::CannotBreak),
                BlockExit::Break     => Err(PLIRErr::CannotContinue),
                BlockExit::Continue  => Err(PLIRErr::CannotReturn),
                BlockExit::Exit(_)   => Err(PLIRErr::CannotReturn),
                BlockExit::Throw     => Ok(BlockExitHandle::Continue(exit)),
            },
        }
    }

    pub(super) fn propagates(&self, term_stmt: &plir::ProcStmt) -> Result<bool, PLIRErr> {
        assert!(term_stmt.is_simple_terminal(), "expected statement to be terminal");
        match (self, term_stmt) {
            (_, plir::ProcStmt::Decl(_)) => unreachable!("not terminal statement"),
            (_, plir::ProcStmt::Expr(_)) => unreachable!("not terminal statement"),

            (BlockBehavior::Function, plir::ProcStmt::Return(_)) => Ok(false),
            (BlockBehavior::Function, plir::ProcStmt::Break)     => Err(PLIRErr::CannotBreak),
            (BlockBehavior::Function, plir::ProcStmt::Continue)  => Err(PLIRErr::CannotContinue),
            (BlockBehavior::Function, plir::ProcStmt::Throw(_))  => Ok(false),
            (BlockBehavior::Function, plir::ProcStmt::Exit(_))   => Ok(false),

            (BlockBehavior::Loop, plir::ProcStmt::Return(_)) => Ok(true),
            (BlockBehavior::Loop, plir::ProcStmt::Break)     => Ok(false),
            (BlockBehavior::Loop, plir::ProcStmt::Continue)  => Ok(false),
            (BlockBehavior::Loop, plir::ProcStmt::Throw(_))  => Ok(true),
            (BlockBehavior::Loop, plir::ProcStmt::Exit(_))   => Ok(false),

            (BlockBehavior::Bare, plir::ProcStmt::Return(_)) => Ok(true),
            (BlockBehavior::Bare, plir::ProcStmt::Break)     => Ok(true),
            (BlockBehavior::Bare, plir::ProcStmt::Continue)  => Ok(true),
            (BlockBehavior::Bare, plir::ProcStmt::Throw(_))  => Ok(true),
            (BlockBehavior::Bare, plir::ProcStmt::Exit(_))   => Ok(false),

            (BlockBehavior::Top, plir::ProcStmt::Return(_)) => Err(PLIRErr::CannotBreak),
            (BlockBehavior::Top, plir::ProcStmt::Break)     => Err(PLIRErr::CannotContinue),
            (BlockBehavior::Top, plir::ProcStmt::Continue)  => Err(PLIRErr::CannotReturn),
            (BlockBehavior::Top, plir::ProcStmt::Throw(_))  => Ok(true),
            (BlockBehavior::Top, plir::ProcStmt::Exit(_))   => Err(PLIRErr::CannotReturn),
        }
    }
}

/// Struct which handles all of the exits from a block.
#[derive(Debug)]
pub(super) struct BlockTerminals {
    /// Each element represents a pointer to a terminating instruction
    /// in a [`InstrBlock`].
    /// 
    /// This index is reversed. The rightmost element of the index refers to
    /// the outermost statements of a program.
    pub(super) branches: HashMap<Vec<usize>, CursorRange>,

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

    pub(super) fn addresses(&self) -> impl Iterator<Item=&Vec<usize>> {
        self.branches.keys()
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

    pub(super) fn add_exit(&mut self, addr: Vec<usize>, loc: CursorRange, is_unconditional: bool) {
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