use std::ops::ControlFlow;

use crate::compiler::plir::{Type, ty, Located};
use crate::err::GonErr;

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
#[derive(PartialEq, Eq, Debug)]
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

pub(super) type BlockExitHandle = ControlFlow<Type, BlockExit>;
// /// Types of ways exits could be handled from a block
// enum BlockExitHandle {
//     /// Continue running in the upper loop.
//     /// 
//     /// If a block exits here, it has a known value type.
//     Continue(Type),
    
//     /// This is either an `break` or `continue`.
//     /// 
//     /// If a block exits here, there is no type.
//     LoopExit,

//     /// Propagate the exit to the upper loop.
//     Propagate(BlockExit),
// }

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
}

/// Struct which handles all of the exits from a block.
#[derive(Debug)]
pub(super) struct BlockTerminals {
    /// All exits which will cause an exit out of this block
    branches: Vec<Located<BlockExit>>,

    /// If closed, any statements added to the block should be
    /// considered unreachable, as the block is known to exit
    /// out of one of its branches.
    closed: bool
}

impl BlockTerminals {
    pub(super) fn new() -> Self {
        Self {
            branches: vec![],
            closed: false
        }
    }

    pub(super) fn is_closed(&self) -> bool {
        self.closed
    }

    /// Exits the block and determines which exits will affect the enclosing block.
    /// 
    /// The block behavior indicates which type of block this BlockTerminals is from.
    pub(super) fn propagate(&mut self, bb: BlockBehavior) -> PLIRResult<Vec<Located<Type>>> {
        let len = self.branches.len();

        let mut propagated = vec![];
        let mut halted = vec![];

        for Located(exit, exit_range) in std::mem::take(&mut self.branches) {
            match bb.handle_exit(exit).map_err(|e| e.at_range(exit_range.clone()))? {
                ControlFlow::Continue(ex) => propagated.push(Located::new(ex, exit_range)),
                ControlFlow::Break(t)     => halted.push(Located::new(t, exit_range)),
            }
        }

        if bb == BlockBehavior::Loop {
            // assume loops are always open, because we don't know if the code executes
            self.closed = false;
        } else {
            // if any branches were removed, that means they exit into the enclosing block,
            // so statements can appear after this
            self.closed &= len == propagated.len();
        }
        self.branches = propagated;

        Ok(halted)
    }

    pub(super) fn add_exit(&mut self, exit: Located<BlockExit>, is_unconditional: bool) {
        if !self.closed {
            self.branches.push(exit);
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
}