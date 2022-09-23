use crate::lexer::token::Token;

pub type Program = Vec<Stmt>;

pub enum Node {
    Program(Program),
    Stmt(Stmt)
}

pub enum Stmt {
    Decl(Decl),       // declaration
    Return(Expr),     // return x;
    Break,            // break;
    Continue,         // continue;
    FunDecl(FunDecl), // function declaration
    Expr(Expr)        // any expression
}

pub struct Decl {
    pub rt: ReasnType,
    pub mt: MutType,
    pub var: String,
    pub ty: Option<Type>,
    pub val: Expr
}

pub enum ReasnType { Let, Const }
pub enum MutType { Mut, Immut }

pub struct Type;

pub struct FunDecl {
    params: Vec<Decl>,
    ret: Option<Type>
}

pub enum Expr {
    Ident(String), // a variable
    Block(Program), // a block
    Literal(Literal), // int, float, char, str literal
    ListLiteral(Vec<Expr>), // [1, 2, 3, 4]
    SetLiteral(Vec<Expr>), // set {1, 2, 3, 4}
    DictLiteral(Vec<(Expr, Expr)>), // dict {1: 1, 2: 2, 3: 3, 4: 4}
    
    Assignment(String, Box<Expr>),
    Attr(Attr), // a.b.c.d
    StaticAttr(Attr), // a::b::c::d
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    Comparison {
        left: Box<Expr>,
        right: (Token, Box<Expr>),
        extra: Option<(Token, Box<Expr>)>
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
    }
}

pub enum Literal {
    Int(isize),
    Float(f64),
    Char(char),
    Str(String)
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

pub struct Attr {
    pub obj: Box<Expr>,
    pub attr: String
}

pub struct UnaryOp {
    pub op: Token,
    pub expr: Box<Expr>
}

pub struct BinaryOp {
    pub op: Token,
    pub left: Box<Expr>,
    pub right: Box<Expr>
}

pub struct If {
    pub condition: Box<Expr>,
    pub if_true: Program,
    pub if_false: Option<Box<Else>>
}
pub enum Else {
    If(If),
    Block(Program)
}