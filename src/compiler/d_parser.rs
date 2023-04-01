//! Converts sequences of tokens to an AST.
//! 
//! The parser is implemented as a recursive descent parser.
//! This parser has grammatical rules, which break down into smaller grammatical rules.
//! The string is assigned the top-most rule (`program`) and the individual units of
//! this rule are computed by recursive statements.
//! 
//! This module provides:
//! - [`parse`]: A function to parse [a list of lexed tokens][`crate::lexer`] into an AST.
//! - [`Parser`]: The struct that does all the parsing.

use ast::Located;

use std::collections::VecDeque;
use std::ops::RangeInclusive;

use crate::GonErr;
use crate::err::{FullGonErr, CursorRange};
use crate::lexer::token::{Token, token, FullToken};
use crate::ast;

/// A struct that does the conversion of tokens to a parseable program tree.
/// 
/// This struct uses some terminology in its function declarations:
/// - For functions which "expect X," the next set of tokens should represent X, 
/// otherwise the function errors.
/// - For functions which "match X," if the next tokens represent X, those tokens are consumed,
/// otherwise nothing occurs.
/// 
/// Additionally, functions which match an expression don't only match one type of expression, 
/// but rather can match that one type of expression and expressions which take precedence
/// (for example, [`match_addsub`][Parser::match_addsub] matches (`+`) and (`-`) expressions
/// but also (`*`), (`/`), etc. expressions.)
/// 
/// # Example
/// ```
/// use poligon_lang::lexer::tokenize;
/// # use poligon_lang::parser::Parser;
/// use poligon_lang::ast::*;
/// 
/// let tokens = tokenize("hello;").unwrap();
/// let program = Parser::new(tokens, false).parse().unwrap();
/// assert_eq!(program, Program(vec![
///     Located::new(Stmt::Expr(
///         Located::new(Expr::Ident(String::from("hello")), (0, 0) ..= (0, 4))),
///         (0, 0) ..= (0, 5)
///     )
/// ]));
/// ```
pub struct DParser {
    tokens: VecDeque<FullToken>,
    eof: (usize, usize),
    tree_locs: Vec<RangeBlock>
}

#[derive(Clone, Debug)]
struct RangeBlock(&'static str, Option<CursorRange>);

/// An error that occurs in the parsing process.
#[derive(Debug, PartialEq, Eq)]
pub enum DParseErr {
    /// The parser expected one of the tokens.
    ExpectedTokens(Vec<Token>),

    /// The parser expected an identifier.
    ExpectedIdent,

    /// The parser expected a type expression (e.g. `list<str>`).
    ExpectedType,

    UnexpectedToken
}
impl GonErr for DParseErr {
    fn err_name(&self) -> &'static str {
        "syntax error"
    }
}

impl std::fmt::Display for DParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DParseErr::ExpectedTokens(tokens) => match tokens.len() {
                0 => write!(f, "expected eof"),
                1 => write!(f, "expected '{}'", tokens[0]),
                _ => {
                    let (first, rest) = tokens.split_first().unwrap();
                    write!(f, "expected one of '{first}'")?;

                    for t in rest {
                        write!(f, ", '{t}'")?;
                    }

                    Ok(())
                }
            },
            DParseErr::ExpectedIdent   => write!(f, "expected identifier"),
            DParseErr::ExpectedType    => write!(f, "expected type expression"),
            DParseErr::UnexpectedToken => write!(f, "unexpected token"),
        }
    }
}
/// A [`Result`] type for operations in the parsing process.
pub type ParseResult<T> = Result<T, FullParseErr>;
type FullParseErr = FullGonErr<DParseErr>;

macro_rules! expected_tokens {
    ($($t:tt),*) => {
        DParseErr::ExpectedTokens(vec![$(token![$t]),*])
    }
}

/// Combine two ranges, such that the new range at least spans over the two provided ranges.
/// `l` should be left of `r`.
fn merge_ranges<T>(l: RangeInclusive<T>, r: RangeInclusive<T>) -> RangeInclusive<T> {
    let (start, _) = l.into_inner();
    let (_, end) = r.into_inner();
    
    start ..= end
}

/// Combine two ranges, such that `l` spans over the two provided ranges.
fn merge_ranges_in_place<T: Clone>(mr1: &mut Option<RangeInclusive<T>>, r2: RangeInclusive<T>) {
    let new_range = match mr1.as_ref() {
        Some(r1) => merge_ranges(r1.clone(), r2),
        None => r2
    };

    mr1.replace(new_range);
}

impl Iterator for DParser {
    type Item = FullToken;

    fn next(&mut self) -> Option<Self::Item> {
        let ft = self.tokens.pop_front()?;
        self.append_range(ft.loc.clone());
        Some(ft)
    }
}

impl DParser {
    pub fn new(tokens: impl IntoIterator<Item=FullToken>) -> Self {
        let mut tokens: VecDeque<_> = tokens.into_iter()
            .filter(|FullToken { tt, ..} | !matches!(tt, Token::Comment(_, _)))
            .collect();
        
        let eof = if let Some(FullToken { loc, ..}) = tokens.make_contiguous().last() {
            let &(lno, cno) = loc.end();
            (lno, cno + 1)
        } else {
            (0, 0)
        };

        Self { tokens, eof, tree_locs: vec![] }
    }

    pub fn unwrap_d_program(mut self) -> ParseResult<ast::Program> {
        let mut program = vec![];
        loop {
            self.push_loc_block("expect_d_program");
            let stmt = match self.peek_token() {
                Some(token![class]) => self.expect_class_decl().map(ast::Stmt::ClassDecl)?,
                Some(token![extern]) => self.expect_extern_decl()?,
                Some(_) => Err(DParseErr::UnexpectedToken.at_range(self.peek_loc()))?,
                None => break
            };

            program.push(Located::new(stmt, self.pop_loc_block("expect_d_program").unwrap()));
        }

        Ok(ast::Program(program))
    }

    /// Expect that the next token in the input is in the specified token, 
    /// erroring if the next token does not match.
    /// 
    /// # Example
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// use poligon_lang::lexer::token::token;
    /// # use poligon_lang::parser::Parser;
    /// 
    /// let mut tokens = tokenize("return true && false").unwrap();
    /// let mut parser = Parser::new(tokens, false);
    /// assert!(parser.expect1(token![return]).is_ok());
    /// assert!(parser.expect1(token![true]).is_ok());
    /// assert!(parser.expect1(token![||]).is_err());
    /// ```
    pub fn expect1(&mut self, u: Token) -> ParseResult<()> {
        if let Some(FullToken {tt: t, loc}) = self.next() {
            if t == u {
                Ok(())
            } else {
                Err(DParseErr::ExpectedTokens(vec![u]).at_range(loc))
            }
        } else {
            Err(DParseErr::ExpectedTokens(vec![u]).at(self.eof))
        }
    }

    /// Expect that the next token in the input is in the specified list of tokens.
    /// If it is, the token is returned, otherwise an error occurs.
    /// 
    /// # Example
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// use poligon_lang::lexer::token::token;
    /// # use poligon_lang::parser::Parser;
    /// 
    /// let mut tokens = tokenize("return true + false").unwrap();
    /// let mut parser = Parser::new(tokens, false);
    /// assert!(parser.expect1(token![return]).is_ok());
    /// assert_eq!(parser.expect_n(&[token![true], token![false]]).unwrap(), token![true]);
    /// assert!(parser.expect_n(&[token![||], token![&&]]).is_err());
    /// ```
    pub fn expect_n(&mut self, one_of: &[Token]) -> ParseResult<FullToken> {
        if let Some(ft) = self.next() {
            if one_of.contains(&ft.tt) {
                Ok(ft)
            } else {
                Err(DParseErr::ExpectedTokens(vec![ft.tt]).at_range(ft.loc))
            }
        } else {
            Err(DParseErr::ExpectedTokens(one_of.into()).at(self.eof))
        }
    }

    /// Test whether the next token in the input matches the specified token,
    /// and consume the token if it does.
    /// 
    /// # Example
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// use poligon_lang::lexer::token::token;
    /// # use poligon_lang::parser::Parser;
    /// 
    /// let mut tokens = tokenize("return true").unwrap();
    /// let mut parser = Parser::new(tokens, false);
    /// assert!(!parser.match1(token![break]));
    /// assert!(!parser.match1(token![continue]));
    /// assert!(parser.match1(token![return]));
    /// assert!(parser.match1(token![true]));
    /// ```
    pub fn match1(&mut self, u: Token) -> bool {
        match self.peek_token() {
            Some(t) if t == &u => self.next().is_some(),
            _ => false
        }
    }

    /// Check whether the next token in the input is in the specified list of tokens, 
    /// consuming and returning the token if it is.
    /// 
    /// # Example
    /// ```
    /// use poligon_lang::lexer::tokenize;
    /// use poligon_lang::lexer::token::token;
    /// # use poligon_lang::parser::Parser;
    /// 
    /// let mut tokens = tokenize("return true + false").unwrap();
    /// let mut parser = Parser::new(tokens, false);
    /// assert_eq!(
    ///     parser.match_n(&[token![break], token![continue], token![return]]).unwrap(), 
    ///     token![return]
    /// );
    /// assert!(parser.match1(token![true]));
    /// assert_eq!(parser.match_n(&[token![&&], token![||]]), None);
    /// assert_eq!(parser.match_n(&[token![+], token![-]]).unwrap(), token![+]);
    /// ```
    pub fn match_n(&mut self, one_of: &[Token]) -> Option<FullToken> {
        match self.peek_token() {
            Some(t) if one_of.contains(t) => self.next(),
            _ => None,
        }
    }

    /// Read the next token in the input if present.
    pub fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(0).map(|FullToken {tt, ..}| tt)
    }

    /// Read the nth following token in the input if present.
    pub fn peek_nth_token(&self, i: usize) -> Option<&Token> {
        self.tokens.get(i).map(|FullToken {tt, ..}| tt)
    }

    /// Consumes the next token in input.
    /// 
    /// If you want a FullToken, see [`Parser::next`].
    pub fn next_token(&mut self) -> Option<Token> {
        self.next().map(|FullToken {tt, ..}| tt)
    }

    /// Look at the range of the next token in the input (or return EOF).
    pub fn peek_loc(&self) -> CursorRange {
        self.tokens.get(0)
            .map_or(
                self.eof..=self.eof,
                |FullToken {loc, ..}| loc.clone()
            )
    }

    /// Add a cursor-tracking block to the parser.
    /// 
    /// While this block is on top, any statements used to remove tokens
    /// are added to this block's cursor range.
    /// 
    /// To remove the block and obtain the cursor range, use [`Parser::pop_loc_block`].
    pub fn push_loc_block(&mut self, name: &'static str) {
        self.tree_locs.push(RangeBlock(name, None));
    }

    /**
     * Looks at the top cursor-tracking block without removing it.
     */
    pub fn peek_loc_block(&mut self) -> Option<CursorRange> {
        self.tree_locs.last()
            .and_then(|RangeBlock(_, r)| r.clone())
    }

    /// Remove a cursor-tracking block from the parser.
    /// 
    /// This function expects that a given loc block will have at least 1 token,
    /// otherwise it will return None.
    /// When this function is called, the top block's range is added to the range of 
    /// the block under it.
    pub fn pop_loc_block(&mut self, name: &'static str) -> Option<CursorRange> {
        let RangeBlock(pushed, mr2) = self.tree_locs.pop()
            .expect("pop_loc_block called without a push");
        assert_eq!(pushed, name, "requested {name}, popped {pushed}");

        let r2 = mr2?;
        self.append_range(r2.clone());

        Some(r2)
    }

    /// Expect that the next tokens in input represent values of type `T` separated by commas 
    /// (and optionally a terminating comma). If successful, this function returns the
    /// matched values and a bool value indicating whether the tuple had a terminating comma.
    /// 
    /// This function requires a `Parser::match_x` function that can match values of type `T`.
    pub fn expect_tuple_of<T, F>(&mut self, mut f: F) -> ParseResult<(Vec<T>, bool /* ended in comma? */)> 
        where F: FnMut(&mut Self) -> ParseResult<Option<T>>
    {
        let mut exprs = vec![];
        let mut comma_end = true;

        // terminate when there's no more expression or when there's no more ,
        while let Some(e) = f(self)? {
            exprs.push(e);

            if !self.match1(token![,]) {
                comma_end = false;
                break;
            }
        }

        Ok((exprs, comma_end))
    }

    /// Expect that the next tokens in input represent values of type `T` separated by commas 
    /// (optionally a terminating comma), and ending with the closer token.
    /// 
    /// For example, if the closer was `}`, then `t, t, t, t }` would be valid here.
    /// The `{` would need to be [matched][Parser::match1] before this function is called.
    /// 
    /// This function requires a `Parser::match_x` function that can match values of type `T`,
    /// and an error to raise if a match was not found.
    pub fn expect_closing_tuple_of<T, F>(
        &mut self, f: F, closer: Token, or_else: DParseErr
    ) -> ParseResult<Vec<T>> 
        where F: FnMut(&mut Self) -> ParseResult<Option<T>>
    {
        let (exprs, comma_end) = self.expect_tuple_of(f)?;

        // if the next token is not the close token,
        // then raise an error, because the tuple did not close properly
        if self.match1(closer) {
            Ok(exprs)
        } else {
            let e = if comma_end {
                or_else
            } else {
                expected_tokens![,]
            };

            Err(e.at_range(self.peek_loc()))
        }
    }

    /// Match a left angle bracket in type expressions (`<`). 
    /// 
    /// This function differs from [`parser::match1(token![<])`][Parser::match1] 
    /// because the `<<` is also matched and is treated by this function as two `<`s.
    pub fn match_langle(&mut self) -> bool {
        // if the next token matches <, then done
        // also have to check for <<
        self.match1(token![<]) || {
            let is_double = matches!(self.peek_token(), Some(token![<<]));
            
            if is_double {
                let FullToken { loc, .. } = &self.tokens[0];

                let &(slno, scno) = loc.start();
                let &end = loc.end();

                self.append_range((slno, scno) ..= (slno, scno));
                self.tokens[0] = FullToken::new(token![<], (slno, scno + 1)..=end);
            }

            is_double
        }
    }

    /// Match a right angle bracket in type expressions (`>`). 
    ///
    /// This function differs from [`parser::match1(token![>])`][Parser::match1] 
    /// because the `>>` is also matched and is treated by this function as two `>`s.
    pub fn match_rangle(&mut self) -> bool {
        // if they match >, then done
        // also have to check for >>
        self.match1(token![>]) || {
            let is_double = matches!(self.peek_token(), Some(token![>>]));
            
            if is_double {
                let FullToken { loc, .. } = &self.tokens[0];

                let &(slno, scno) = loc.start();
                let &end = loc.end();

                self.append_range((slno, scno) ..= (slno, scno));
                self.tokens[0] = FullToken::new(token![>], (slno, scno + 1)..=end);
            }

            is_double
        }
    }

    /// Extends the range of the top cursor-tracking block to reach the bounds of new_range.
    fn append_range(&mut self, new_range: CursorRange) {
        if let Some(rb) = self.tree_locs.last_mut() {
            merge_ranges_in_place(&mut rb.1, new_range);
        }
    }

    /// Match the next token to a reassignment type if it represents one.
    pub fn match_reasg_type(&mut self) -> Option<ast::ReasgType> {
        if self.match1(token![let]) {
            Some(ast::ReasgType::Let)
        } else if self.match1(token![const]) {
            Some(ast::ReasgType::Const)
        } else {
            None
        }
    }
    /// Match the next tokens in the input if they represent a function parameter.
    pub fn match_param(&mut self) -> ParseResult<Option<ast::Param>> {
        let mrt = self.match_reasg_type();
        let (mut empty, rt) = match mrt {
            Some(t) => (false, t),
            None => (true, Default::default()),
        };
        
        let mt = if self.match1(token![mut]) {
            empty = false;
            ast::MutType::Mut
        } else {
            ast::MutType::Immut
        };

        // the param checked so far is fully empty and probably not an actual param:
        if empty && self.has_ident().is_none() {
            return Ok(None);
        }

        let ident = self.expect_ident()?.0;
        let ty = if self.match1(token![:]) {
            Some(self.expect_type()?)
        } else {
            None
        };

        Ok(Some(ast::Param {
            rt,
            mt,
            ident,
            ty
        }))
    }

    /// Check if the next input is an identifier.
    /// 
    /// If the next input is an identifier, this function returns the number of tokens
    /// held by this identifier.
    pub fn has_ident(&self) -> Option<usize> {
        match (self.peek_token(), self.peek_nth_token(1)) {
            (Some(Token::Ident(_) | Token::Str(_)), _) => Some(1),
            (Some(token![#]), Some(Token::Ident(_))) => Some(2),
            _ => None
        }
    }

    /// Match the next token in input if it is an identifier token,
    /// returning the identifier's string if successfully matched.
    pub fn match_ident(&mut self) -> ParseResult<Option<Located<String>>> {
        match self.has_ident() {
            None => Ok(None),
            Some(1) => {
                match self.next() {
                    Some(FullToken { tt: Token::Ident(s) | Token::Str(s), loc }) => {
                        Ok(Some(Located::new(s, loc)))
                    }
                    _ => unreachable!()
                }
            },
            Some(2) => {
                self.push_loc_block("match_ident");

                self.expect1(token![#])?;
                let Some(Token::Ident(s)) = self.next_token() else { unreachable!() };

                let ident_loc = self.pop_loc_block("match_ident").unwrap();

                Ok(Some(Located::new(format!("#{s}"), ident_loc)))
            },
            s => unreachable!("has_ident should not return {s:?}")
        }
    }

    /// Expect that the next token in the input is an identifier token,
    /// returning the identifier's string if successfully matched.
    pub fn expect_ident(&mut self) -> ParseResult<Located<String>> {
        self.match_ident()?
            .ok_or_else(|| DParseErr::ExpectedIdent.at_range(self.peek_loc()))
    }

    /// Match the next tokens in the input if they represent a type expression.
    /// 
    /// This is used to enable [`parser::expect_tuple_of(Parser::match_type)`][`Parser::expect_tuple_of`].
    /// The function that *should* be used for type expression parsing purposes is [`Parser::expect_type`].
    fn match_type(&mut self) -> ParseResult<Option<Located<ast::Type>>> {
        self.push_loc_block("match_type");
        if self.has_ident().is_some() {
            let ident = self.expect_ident()?.0;

            let params = if self.match_langle() {
                let token_pos = self.peek_loc();

                let (tpl, comma_end) = self.expect_tuple_of(DParser::match_type)?;
                if !self.match_rangle() {
                    Err(if comma_end {
                        DParseErr::ExpectedType
                    } else {
                        expected_tokens![,]
                    }.at_range(self.peek_loc()))?
                }

                if !tpl.is_empty() {
                    tpl
                } else {
                    // list<>
                    //      ^
                    Err(DParseErr::ExpectedType.at_range(token_pos))?
                }
            } else {
                vec![]
            };

            let result = Located::new(ast::Type(ident, params), self.pop_loc_block("match_type").unwrap());
            Ok(Some(result))
        } else {
            self.pop_loc_block("match_type");
            Ok(None)
        }
    }

    /// Expect that the next tokens in the input represent a type expression.
    /// 
    /// This should be verified with [`PLIRCodegen::verify_type`].
    pub fn expect_type(&mut self) -> ParseResult<Located<ast::Type>> {
        self.match_type()?
            .ok_or_else(|| DParseErr::ExpectedType.at_range(self.peek_loc()))
    }

    fn expect_d_generic_ident(&mut self) -> ParseResult<ast::GenericIdent> {
        let ident = self.expect_ident()?.0;
        
        let params = if self.match_langle() {
           let (params, _) = self.expect_tuple_of(DParser::match_ident)?;
           
           let loc = self.peek_loc();
           if !self.match_rangle() {
              Err(expected_tokens![>].at_range(loc))?;
           }

           params
        } else {
            vec![]
        };

        Ok(ast::GenericIdent {
            ident,
            params: params.into_iter().map(|t| t.0).collect(),
        })
    }

    /// Expect the next tokens represent a PLIR class.
    /// 
    /// This class should be registered by [`PLIRCodegen::register_cls`].
    fn expect_class_decl(&mut self) -> ParseResult<ast::Class> {
        self.expect1(token![class])?;
        let ident = self.expect_d_generic_ident()?;

        self.expect1(token!["{"])?;
        let (fields, _) = self.expect_tuple_of(DParser::match_type)?;
        let fields = fields.into_iter()
            .enumerate()
            .map(|(i, ty)| ast::FieldDecl {
                rt: Default::default(),
                mt: Default::default(),
                ident: format!("#{i}"),
                ty,
            })
            .collect();
        self.expect1(token!["}"])?;
        self.match1(token![;]);

        Ok(ast::Class { ident, fields, methods: vec![] })
    }

    /// Expect the next tokens represent an external function declaration.
    /// 
    /// This should be registered with [`PLIRCodegen::register_fun_sig`].
    fn expect_extern_decl(&mut self) -> ParseResult<ast::Stmt> {
        self.expect1(token![extern])?;
        self.expect1(token![fun])?;

        let ident = self.expect_ident()?.0;

        self.expect1(token!["("])?;
        let (params, end_comma) = self.expect_tuple_of(DParser::match_param)?;
        let varargs = end_comma && self.match1(token![..]);
        if varargs {
            self.match1(token![,]);
        }
        self.expect1(token![")"])?;

        let ret = if self.match1(token![->]) {
            Some(self.expect_type()?)
        } else {
            None
        };
        self.expect1(token![;])?;

        Ok(ast::Stmt::ExternFunDecl(ast::FunSignature { ident, params, varargs, ret }))
    }
}