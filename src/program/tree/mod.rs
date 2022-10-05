use super::TraverseRt;

pub type Program = Vec<Stmt>;
pub mod op;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Decl(Decl),       // declaration
    Return(Expr),     // return x;
    Break,            // break;
    Continue,         // continue;
    FunDecl(FunDecl), // function declaration
    Expr(Expr)        // any expression
}

#[derive(Debug, PartialEq)]
pub struct Decl {
    pub rt: ReasgType,
    pub mt: MutType,
    pub ident: String,
    pub ty: Option<Type>,
    pub val: Expr
}
#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct Type(pub String, pub Vec<Type>);

#[derive(Debug, PartialEq)]
pub struct FunDecl {
    pub ident: String,
    pub params: Vec<Param>,
    pub ret: Option<Type>
}

#[derive(Debug, PartialEq)]
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
    UnaryOps(UnaryOps),
    BinaryOp(BinaryOp),
    Comparison {
        left: Box<Expr>,
        right: (op::Cmp, Box<Expr>),
        extra: Vec<(op::Cmp, Box<Expr>)>
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
    Index {
        expr: Box<Expr>,
        index: Box<Expr>
    }
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
pub struct Attr {
    pub obj: Box<Expr>,
    pub attr: String
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
    pub condition: Box<Expr>,
    pub if_true: Program,
    pub if_false: Option<Box<Else>>
}

#[derive(Debug, PartialEq)]
pub enum Else {
    If(If),
    Block(Program)
}

impl Expr {
    pub fn apply_unary(&self, o: &op::Unary, ctx: &mut super::BlockContext) -> super::RtResult<super::Value> {
        self.traverse_rt(ctx).and_then(|v| v.apply_unary(o))
    }

    pub fn apply_binary(&self, o: &op::Binary, right: &Self, ctx: &mut super::BlockContext) -> super::RtResult<super::Value> {
        match o {
            // &&, || have special short circuiting that needs to be dealt with
            op::Binary::LogAnd => {
                let left = self.traverse_rt(ctx)?;
                Ok(if left.truth() { right.traverse_rt(ctx)? } else { left })
            },
            op::Binary::LogOr => {
                let left = self.traverse_rt(ctx)?;
                Ok(if left.truth() { left } else { right.traverse_rt(ctx)? })
            },
    
            // fallback to eager value binary
            _ => self.traverse_rt(ctx).and_then(|v| v.apply_binary(o, &right.traverse_rt(ctx)?))
        }
    }
}