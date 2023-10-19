use crate::lexer::token::{FullToken, Token, TokenTree};

use super::ParseErr;

/// A token pattern.
/// 
/// This reads a singular token (not a group) and sees if it matches the given pattern.
/// 
/// Here are the currently implemented pattern:
/// - [`Token`] == verify if that the stream token is equal to the pattern token
/// - [`[Token]`] == verify if that the stream token is one of the tokens of the pattern
pub trait TokenPattern {
    /// Checks if the pattern matches this token.
    fn is_match(&self, token: &FullToken) -> bool;

    /// Provides the error if this pattern does not match.
    fn fail_err(&self) -> ParseErr;
}

impl TokenPattern for Token {
    fn is_match(&self, token: &FullToken) -> bool {
        self == token
    }

    fn fail_err(&self) -> ParseErr {
        ParseErr::ExpectedTokens(vec![self.clone()])
    }
}
impl TokenPattern for [Token] {
    fn is_match(&self, token: &FullToken) -> bool {
        self.contains(&token.kind)
    }

    fn fail_err(&self) -> ParseErr {
        ParseErr::ExpectedTokens(self.to_vec())
    }
}
impl<const N: usize> TokenPattern for [Token; N] {
    fn is_match(&self, token: &FullToken) -> bool {
        self.as_slice().is_match(token)
    }

    fn fail_err(&self) -> ParseErr {
        self.as_slice().fail_err()
    }
}
impl<'p, P: TokenPattern + ?Sized> TokenPattern for &'p P {
    fn is_match(&self, token: &FullToken) -> bool {
        (*self).is_match(token)
    }

    fn fail_err(&self) -> ParseErr {
        (*self).fail_err()
    }
}

pub struct MatchFn<F>(F, fn() -> ParseErr);
impl<F> MatchFn<F> 
    where F: Fn(&FullToken) -> bool,
{
    pub fn new(f: F) -> Self {
        fn match_fn_default() -> ParseErr {
            ParseErr::UnexpectedToken
        }

        MatchFn(f, match_fn_default)
    }

    pub fn new_with_err(f: F, e: fn() -> ParseErr) -> Self {
        Self(f, e)
    }
}
impl<F> TokenPattern for MatchFn<F> 
    where F: Fn(&FullToken) -> bool
{    
    fn is_match(&self, token: &FullToken) -> bool {
        (self.0)(token)
    }

    fn fail_err(&self) -> ParseErr {
        (self.1)()
    }
}