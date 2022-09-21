use std::collections::VecDeque;

use crate::lexer::token::{Token, token};

pub mod tree;

pub fn parse(tokens: impl IntoIterator<Item=Token>) -> Result<tree::Program, ParseErr> {
    Parser::new(tokens.into_iter().collect()).parse()
}

struct Parser {
    tokens: VecDeque<Token>
}

#[derive(Debug)]
pub enum ParseErr {
    ExpectedTokens(Vec<Token>),
    ExpectedIdent
}

impl Parser {
    fn new(tokens: VecDeque<Token>) -> Self {
        Self { tokens }
    }

    fn expect(&mut self, one_of: &[Token]) -> Result<Token, ParseErr> {
        if let Some(t) = self.tokens.pop_front() {
            if one_of.contains(&t) {
                return Ok(t)
            }
        }
    
        Err(ParseErr::ExpectedTokens(one_of.into()))
    }

    fn expect1(&mut self, u: Token) -> Result<(), ParseErr> {
        if let Some(t) = self.tokens.pop_front() {
            if t == u {
                return Ok(())
            }
        }
    
        Err(ParseErr::ExpectedTokens(vec![u]))
    }

    fn match_n(&mut self, one_of: &[Token]) -> Option<Token> {
        match self.tokens.get(0) {
            Some(t) if one_of.contains(t) => self.tokens.pop_front(),
            _ => None,
        }
    }

    fn match1(&mut self, u: Token) -> bool {
        match self.tokens.get(0) {
            Some(t) if t == &u => self.tokens.pop_front(),
            _ => None,
        }.is_some()
    }

    fn parse(mut self) -> Result<tree::Program, ParseErr> {
        let mut program = vec![];

        // this consumes the entire token list
        while !self.tokens.is_empty() {
            // if statement exists, add it to program
            // if statement is nothing, ignore it
            let mst = self.match_stmt()?;
            if let Some(st) = mst { program.push(st); }

            self.expect1(Token::LineSep)?;
        }
        
        Ok(program)
    }

    fn match_stmt(&mut self) -> Result<Option<tree::Stmt>, ParseErr> {
        let st = match self.tokens.get(0) {
            Some(token![let]) | Some(token![const]) => Some(self.expect_decl()?).map(tree::Stmt::Decl),
            Some(token![return])   => Some(self.expect_return()?),
            Some(token![break])    => Some(self.expect_break()?),
            Some(token![continue]) => Some(self.expect_cont()?),
            Some(token![fun])      => Some(self.expect_fun()?).map(tree::Stmt::FunDecl),
            Some(_)                => Some(self.expect_expr()?).map(tree::Stmt::Expr),
            None                   => None
        };

        Ok(st)
    }

    fn expect_decl(&mut self) -> Result<tree::Decl, ParseErr> {
        let rt = match self.tokens.pop_front() {
            Some(token![let])   => tree::ReasnType::Let,
            Some(token![const]) => tree::ReasnType::Const,
            _ => unreachable!()
        };
        
        let mt = if self.match1(token![mut]) {
            tree::MutType::Mut
        } else {
            tree::MutType::Immut
        };

        let ident = self.expect_ident()?;

        let ty = if self.match1(token![:]) {
            Some(self.expect_type()?)
        } else {
            None
        };

        self.expect1(token![=])?;
        let expr = self.expect_expr()?;

        Ok(tree::Decl {
            rt,
            mt,
            var: ident,
            ty,
            val: expr
        })
    }
    fn expect_return(&mut self) -> Result<tree::Stmt, ParseErr> {
        self.expect1(token![return])?;
        let e = self.expect_expr()?;

        Ok(tree::Stmt::Return(e))
    }
    fn expect_break(&mut self) -> Result<tree::Stmt, ParseErr> {
        self.expect1(token![break])?;

        Ok(tree::Stmt::Break)
    }
    fn expect_cont(&mut self) -> Result<tree::Stmt, ParseErr> {
        self.expect1(token![continue])?;

        Ok(tree::Stmt::Continue)
    }
    fn expect_fun(&mut self) -> Result<tree::FunDecl, ParseErr> {
        self.expect1(token![fun])?;

        let ident = self.expect_ident();

        self.expect1(token!["("])?;
        self.expect1(token![")"])?;

        let ret = if self.match1(token![->]) {
            Some(self.expect_type()?)
        } else {
            None
        };

        todo!()
    }

    fn expect_ident(&mut self) -> Result<String, ParseErr> {
        if let Some(Token::Ident(s)) = self.tokens.pop_front() {
            Ok(s)
        } else {
            Err(ParseErr::ExpectedIdent)
        }
    }
    fn expect_type(&mut self) -> Result<tree::Type, ParseErr> {
        todo!()
    }

    fn expect_expr(&mut self) -> Result<tree::Expr, ParseErr> {
        todo!()
    }
}