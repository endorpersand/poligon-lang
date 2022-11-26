use std::rc::Rc;

pub mod op;
pub mod display;

#[derive(Debug, PartialEq)]
pub struct Program(pub Block);

#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<Stmt>);

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Decl(Decl),           // declaration
    Return(Option<Expr>), // return x;
    Break,                // break;
    Continue,             // continue;
    FunDecl(FunDecl),     // function declaration
    Expr(Expr)            // any expression
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

#[derive(Debug, PartialEq)]
pub struct FunDecl {
    pub ident: String,
    pub params: Vec<Param>,
    pub ret: Option<Type>,
    pub block: Rc<Block>
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Ident(String), // a variable
    Block(Block), // a block
    Literal(Literal), // int, float, char, str literal
    ListLiteral(Vec<Expr>), // [1, 2, 3, 4]
    SetLiteral(Vec<Expr>), // set {1, 2, 3, 4}
    DictLiteral(Vec<(Expr, Expr)>), // dict {1: 1, 2: 2, 3: 3, 4: 4}
    
    Assign(AsgPat, Box<Expr>),
    Path(Path),
    UnaryOps {
        ops: Vec<op::Unary>,
        expr: Box<Expr>
    },
    BinaryOp {
        op: op::Binary,
        left: Box<Expr>,
        right: Box<Expr>
    },
    Comparison {
        left: Box<Expr>,
        rights: Vec<(op::Cmp, Expr)>
    },
    Range {
        left: Box<Expr>,
        right: Box<Expr>,
        step: Option<Box<Expr>>
    },
    If {
        conditionals: Vec<(Expr, Block)>,
        last: Option<Block>
    },
    While {
        condition: Box<Expr>,
        block: Block
    },
    For {
        ident: String,
        iterator: Box<Expr>,
        block: Block
    },
    Call {
        funct: Box<Expr>,
        params: Vec<Expr>
    },
    Index(Index),
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
#[derive(Debug, PartialEq)]
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
