//! A utility module for handling [`TokenPattern`]s,
//! which can match a singular token tree into some useful value.
//! 
//! Token patterns are used in [`Parser::match_`] and [`Parser::expect`].
//! 
//! [`Parser::match_`]: [`super::Parser::match_`]
//! [`Parser::expect`]: [`super::Parser::expect`]

use crate::lexer::token::{FullToken, Token, TokenTree, Group, Delimiter};
use crate::span::Spanned;

use super::ParseErr;

/// A token pattern.
/// 
/// This reads a singular token tree and sees if it matches the given pattern.
/// 
/// Here are the currently implemented pattern:
/// - [`Token`] == verify if that the stream token is equal to the pattern token
/// - [`[Token]`] == verify if that the stream token is one of the tokens of the pattern
pub trait TokenPattern<'tt> {
    /// The type that this pattern munches the tree into.
    type Munched: Spanned + 'tt;

    /// Attempts to match the token tree and convert it into the munched type.
    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched>;

    /// Provides the error if this pattern does not match.
    fn fail_err(&self) -> ParseErr;
}

impl<'tt, P: TokenPattern<'tt> + ?Sized> TokenPattern<'tt> for &'tt P {
    type Munched = P::Munched;

    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched> {
        (*self).try_munch(tt)
    }

    fn fail_err(&self) -> ParseErr {
        (*self).fail_err()
    }
}
impl<'tt> TokenPattern<'tt> for Token {
    type Munched = FullToken;

    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched> {
        match tt {
            TokenTree::Token(token) => (self == token).then_some(token).cloned(),
            TokenTree::Group(_) => None,
        }
    }

    fn fail_err(&self) -> ParseErr {
        ParseErr::ExpectedTokens(vec![self.clone()])
    }
}
impl<'tt> TokenPattern<'tt> for [Token] {
    type Munched = FullToken;

    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched> {
        match tt {
            TokenTree::Token(token) => self.contains(&token.kind).then_some(token).cloned(),
            TokenTree::Group(_) => None,
        }
    }

    fn fail_err(&self) -> ParseErr {
        ParseErr::ExpectedTokens(self.to_vec())
    }
}
impl<'tt, const N: usize> TokenPattern<'tt> for [Token; N] {
    type Munched = FullToken;

    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched> {
        self.as_slice().try_munch(tt)
    }

    fn fail_err(&self) -> ParseErr {
        self.as_slice().fail_err()
    }
}

/// Matches a group enclosed by the provided delimiter.
impl<'tt> TokenPattern<'tt> for Delimiter {
    type Munched = &'tt Group;

    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched> {
        match tt {
            TokenTree::Token(_) => None,
            TokenTree::Group(g) => (&g.delimiter == self).then_some(g),
        }
    }

    fn fail_err(&self) -> ParseErr {
        ParseErr::ExpectedTokens(vec![Token::Delimiter(*self, false)])
    }
}

/// Matches an arbitrary function.
pub struct MatchFn<F>(F, fn() -> ParseErr);
impl<F, T: Spanned> MatchFn<F> 
    where F: Fn(&TokenTree) -> Option<T>,
{
    /// Creates a new match function with a default error.
    pub fn new(f: F) -> Self {
        fn match_fn_default() -> ParseErr {
            ParseErr::UnexpectedToken
        }

        MatchFn(f, match_fn_default)
    }

    /// Creates a new match function with a defined error.
    pub fn new_with_err(f: F, e: fn() -> ParseErr) -> Self {
        Self(f, e)
    }
}
impl<'tt, F, T: Spanned + 'tt> TokenPattern<'tt> for MatchFn<F> 
    where F: Fn(&TokenTree) -> Option<T>
{    
    type Munched = T;

    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched> {
        (self.0)(tt)
    }

    fn fail_err(&self) -> ParseErr {
        (self.1)()
    }
}