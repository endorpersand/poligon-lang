use std::rc::Rc;

pub mod op;

#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<Stmt>);

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
    pub mt: MutType,
    pub ident: String,
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
    pub block: Rc<Program>
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Ident(String), // a variable
    Block(Program), // a block
    Literal(Literal), // int, float, char, str literal
    ListLiteral(Vec<Expr>), // [1, 2, 3, 4]
    SetLiteral(Vec<Expr>), // set {1, 2, 3, 4}
    DictLiteral(Vec<(Expr, Expr)>), // dict {1: 1, 2: 2, 3: 3, 4: 4}
    
    Assign(AsgPat, Box<Expr>),
    Path(Path),
    UnaryOps(UnaryOps),
    BinaryOp(BinaryOp),
    Comparison {
        left: Box<Expr>,
        rights: Vec<(op::Cmp, Expr)>
    },
    Range {
        left: Box<Expr>,
        right: Box<Expr>,
        step: Option<Box<Expr>>
    },
    If(If),
    While {
        condition: Box<Expr>,
        block: Program
    },
    For {
        ident: String,
        iterator: Box<Expr>,
        block: Program
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
pub struct UnaryOps {
    pub ops: Vec<op::Unary>,
    pub expr: Box<Expr>
}

#[derive(Debug, PartialEq)]
pub struct BinaryOp {
    pub op: op::Binary,
    pub left: Box<Expr>,
    pub right: Box<Expr>
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub conditionals: Vec<(Expr, Program)>,
    pub last: Option<Program>
}

#[derive(Debug, PartialEq)]
pub struct Index {
    pub expr: Box<Expr>,
    pub index: Box<Expr>
}

#[derive(Debug, PartialEq)]
pub enum AsgPat {
    Ident(String),
    Path(Path),
    Index(Index),
    Spread(Option<Box<AsgPat>>),
    List(Vec<AsgPat>)
}

#[derive(Debug, PartialEq, Eq)]
pub enum AsgPatErr {
    InvalidAssignTarget,
    CannotSpreadMultiple,
}

impl TryFrom<Expr> for AsgPat {
    type Error = AsgPatErr;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Ident(ident) => Ok(AsgPat::Ident(ident)),
            Expr::Path(attrs)  => Ok(AsgPat::Path(attrs)),
            Expr::Index(idx)   => Ok(AsgPat::Index(idx)),
            Expr::Spread(me) => match me {
                Some(e) => {
                    let inner = Some(Box::new(AsgPat::try_from(*e)?));
                    
                    Ok(AsgPat::Spread(inner))
                },
                None => Ok(AsgPat::Spread(None)),
            }
            Expr::ListLiteral(lst) => {
                let vec: Vec<_> = lst.into_iter()
                    .map(TryInto::try_into)
                    .collect::<Result<_, _>>()?;

                // check spread count is <2
                let mut it = vec.iter()
                    .filter(|pat| matches!(pat, AsgPat::Spread(_)));
                
                it.next(); // skip 
                if it.next().is_some() {
                    Err(AsgPatErr::CannotSpreadMultiple)
                } else {
                    Ok(AsgPat::List(vec))
                }
            }
            _ => Err(AsgPatErr::InvalidAssignTarget),
        }
    }
}