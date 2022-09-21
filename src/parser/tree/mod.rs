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
    
    Accessor(Accessor), // a.b.c.d
    StaticAccessor(Accessor), // a::b::c::d
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp)
}

pub enum Literal {
    Int(isize),
    Float(f64),
    Char(char),
    Str(String)
}

pub struct Accessor {
    obj: Box<Expr>,
    attr: String
}

pub struct UnaryOp {
    op: Token,
    expr: Box<Expr>
}

pub struct BinaryOp {
    op: Token,
    left: Box<Expr>,
    right: Box<Expr>
}