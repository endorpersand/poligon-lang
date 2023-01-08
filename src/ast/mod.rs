use std::rc::Rc;

pub mod op;
mod display;

#[derive(Debug, PartialEq)]
pub struct Program(pub Block);

#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<Stmt>);

#[derive(Debug, PartialEq)]
pub enum Stmt {
    /// A variable declaration with a specified right-hand side
    Decl(Decl),
    
    /// A return statement that tells the program to exit the function body
    /// 
    /// This return statement can return nothing (void), 
    /// or return a value from a given expression
    Return(Option<Expr>),
    
    /// `break`
    Break,

    /// `continue`
    Continue,
    
    /// A function declaration with a specified body
    FunDecl(FunDecl),

    /// A function declaration without a specified body
    /// 
    /// In the compiler, this is used to call functions from libc
    ExternFunDecl(FunSignature),

    /// An expression
    Expr(Expr)
}

#[derive(Debug, PartialEq)]
pub struct Decl {
    pub rt: ReasgType,
    pub pat: DeclPat,
    pub ty: Option<Type>,
    pub val: Expr
}
#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    pub rt: ReasgType,
    pub mt: MutType,
    pub ident: String,
    pub ty: Option<Type>
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ReasgType { Let, Const }
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MutType { Mut, Immut }

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Type(pub String, pub Vec<Type>);

#[derive(Debug, PartialEq, Eq)]
pub struct FunSignature {
    pub ident: String,
    pub params: Vec<Param>,
    pub ret: Option<Type>,
}
#[derive(Debug, PartialEq)]
pub struct FunDecl {
    pub sig: FunSignature,
    pub block: Rc<Block>
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    /// Variable access.
    Ident(String),

    /// A block of statements.
    Block(Block),

    /// An int, float, char, or string literal.
    Literal(Literal),

    /// A list literal (e.g. `[1, 2, 3, 4]`).
    ListLiteral(Vec<Expr>),

    /// A set literal (e.g. `set {1, 2, 3, 4}`).
    SetLiteral(Vec<Expr>),

    /// A dict literal (e.g. `dict {1: "a", 2: "b", 3: "c", 4: "d"}`).
    DictLiteral(Vec<(Expr, Expr)>),
    
    /// An assignment operation.
    Assign(AsgPat, Box<Expr>),

    /// A path (e.g. `obj.prop.prop.prop`).
    Path(Path),

    /// A chain of unary operations (e.g. `+-+-~!+e`).
    UnaryOps {
        /// The operators applied. These are in display order 
        /// (i.e. they are applied to the expression from right to left).
        ops: Vec<op::Unary>,
        /// Expression to apply the unary operations to.
        expr: Box<Expr>
    },

    /// A binary operation (e.g. `a + b`).
    BinaryOp {
        /// Operator to apply.
        op: op::Binary,
        /// The left expression.
        left: Box<Expr>,
        /// The right expression.
        right: Box<Expr>
    },

    /// A comparison operation (e.g. `a < b < c < d`).
    /// 
    /// Compound comparison operations are broken down by `&&`.
    /// For example, `a < b < c < d` breaks down into `a < b && b < c && c < d`.
    Comparison {
        /// The left expression
        left: Box<Expr>,
        /// A list of comparison operators and a right expressions to apply.
        rights: Vec<(op::Cmp, Expr)>
    },

    /// A range (e.g. `1..10` or `1..10 step 1`).
    Range {
        /// The left expression
        left: Box<Expr>,
        /// The right expression
        right: Box<Expr>,
        /// The expression for the step if it exists
        step: Option<Box<Expr>>
    },

    /// An if expression or if-else expression. (e.g. `if cond {}`, `if cond {} else {}`, `if cond1 {} else if cond2 {} else {}`).
    If {
        /// The condition and block connected to each `if` of the chain
        conditionals: Vec<(Expr, Block)>,
        /// The final bare `else` block (if it exists)
        last: Option<Block>
    },

    /// A `while` loop.
    While {
        /// The condition to check before each iteration.
        condition: Box<Expr>,
        /// The block to run in each iteration.
        block: Block
    },

    /// A `for` loop.
    For {
        /// Variable to bind elements of the iterator to.
        ident: String,
        /// The iterator.
        iterator: Box<Expr>,
        /// The block to run in each iteration.
        block: Block
    },

    /// A function call.
    Call {
        /// The function to call.
        funct: Box<Expr>,
        /// The parameters to the function call.
        params: Vec<Expr>
    },

    /// An index operation (e.g. `lst[0]`).
    Index(Index),

    /// A spread operation (e.g. `..`, `..lst`).
    Spread(Option<Box<Expr>>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(isize),
    Float(f64),
    Char(char),
    Str(String),
    Bool(bool)
}

impl Literal {
    pub fn from_numeric(s: &str) -> Option<Self> {
        s.parse::<isize>()
            .map(Literal::Int)
            .ok()
            .or_else(|| s.parse::<f64>()
                .map(Literal::Float)
                .ok()
            )
        
    }
}

#[derive(Debug, PartialEq)]
pub struct Path {
    pub obj: Box<Expr>,

    // the attribute & whether or not it's static
    // a.b.c.d vs a::b::c::d
    pub attrs: Vec<(String, bool)>
}

#[derive(Debug, PartialEq)]
pub struct Index {
    pub expr: Box<Expr>,
    pub index: Box<Expr>
}

impl Stmt {
    pub fn ends_with_block(&self) -> bool {
        matches!(self, 
            | Stmt::FunDecl(_)
            | Stmt::Expr(Expr::Block(_))
            | Stmt::Expr(Expr::If { .. })
            | Stmt::Expr(Expr::While { .. })
            | Stmt::Expr(Expr::For { .. })
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum AsgUnit {
    Ident(String),
    Path(Path),
    Index(Index),
}
#[derive(Debug, PartialEq, Eq)]
pub struct DeclUnit(pub String, pub MutType);

#[derive(Debug, PartialEq)]
pub enum Pat<T> {
    Unit(T),
    Spread(Option<Box<Self>>),
    List(Vec<Self>)
}
pub type AsgPat = Pat<AsgUnit>;
pub type DeclPat = Pat<DeclUnit>;

#[derive(Debug, PartialEq, Eq)]
pub enum PatErr {
    InvalidAssignTarget,
    CannotSpreadMultiple,
}

impl TryFrom<Expr> for AsgUnit {
    type Error = PatErr;
    
    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Ident(ident) => Ok(AsgUnit::Ident(ident)),
            Expr::Path(attrs)  => Ok(AsgUnit::Path(attrs)),
            Expr::Index(idx)   => Ok(AsgUnit::Index(idx)),
            _ => Err(PatErr::InvalidAssignTarget)
        }
    }
}

impl<T: TryFrom<Expr, Error = PatErr>> TryFrom<Expr> for Pat<T> {
    type Error = PatErr;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Spread(me) => match me {
                Some(e) => {
                    let inner = Some(Box::new(Self::try_from(*e)?));
                    
                    Ok(Self::Spread(inner))
                },
                None => Ok(Self::Spread(None)),
            }
            Expr::ListLiteral(lst) => {
                let vec: Vec<_> = lst.into_iter()
                    .map(TryInto::try_into)
                    .collect::<Result<_, _>>()?;

                // check spread count is <2
                let mut it = vec.iter()
                    .filter(|pat| matches!(pat, Self::Spread(_)));
                
                it.next(); // skip 
                if it.next().is_some() {
                    Err(PatErr::CannotSpreadMultiple)
                } else {
                    Ok(Self::List(vec))
                }
            }
            e => {
                let unit = T::try_from(e)?;
                
                Ok(Self::Unit(unit))
            }
        }
    }
}
