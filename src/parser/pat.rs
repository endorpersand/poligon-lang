//! A utility module for handling [`TokenPattern`]s,
//! which can match a singular token tree into some useful value.
//! 
//! Token patterns are used in [`Parser::match_`] and [`Parser::expect`].
//! 
//! [`Parser::match_`]: [`super::Parser::match_`]
//! [`Parser::expect`]: [`super::Parser::expect`]

use crate::err::GonErr;
use crate::token::{FullToken, Token, TokenTree, Group, Delimiter};
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
    /// The type of the error if this pattern does not match.
    type Err: GonErr;

    /// Attempts to match the token tree and convert it into the munched type.
    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched>;

    /// Provides the error if this pattern does not match.
    fn fail_err(&self) -> Self::Err;
}

impl<'tt, P: TokenPattern<'tt> + ?Sized> TokenPattern<'tt> for &'tt P {
    type Munched = P::Munched;
    type Err = P::Err;

    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched> {
        (*self).try_munch(tt)
    }

    fn fail_err(&self) -> Self::Err {
        (*self).fail_err()
    }
}
impl<'tt> TokenPattern<'tt> for Token {
    type Munched = FullToken;
    type Err = ParseErr;

    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched> {
        match tt {
            TokenTree::Token(token) => (self == token).then_some(token).cloned(),
            TokenTree::Group(_) => None,
        }
    }

    fn fail_err(&self) -> Self::Err {
        ParseErr::ExpectedTokens(Box::new([self.clone()]))
    }
}
impl<'tt> TokenPattern<'tt> for [Token] {
    type Munched = FullToken;
    type Err = ParseErr;

    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched> {
        match tt {
            TokenTree::Token(token) => self.contains(&token.kind).then_some(token).cloned(),
            TokenTree::Group(_) => None,
        }
    }

    fn fail_err(&self) -> Self::Err {
        ParseErr::ExpectedTokens(self.to_vec().into_boxed_slice())
    }
}
impl<'tt, const N: usize> TokenPattern<'tt> for [Token; N] {
    type Munched = FullToken;
    type Err = ParseErr;

    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched> {
        self.as_slice().try_munch(tt)
    }

    fn fail_err(&self) -> Self::Err {
        self.as_slice().fail_err()
    }
}

/// Matches a group enclosed by the provided delimiter.
impl<'tt> TokenPattern<'tt> for Delimiter {
    type Munched = &'tt Group;
    type Err = ParseErr;

    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched> {
        match tt {
            TokenTree::Token(_) => None,
            TokenTree::Group(g) => (&g.delimiter == self).then_some(g),
        }
    }

    fn fail_err(&self) -> Self::Err {
        ParseErr::ExpectedTokens(Box::new([self.left()]))
    }
}

/// Matches an arbitrary function.
pub struct MatchFn<F, E>(F, fn() -> E);
impl<F, T> MatchFn<F, ParseErr> 
    where F: Fn(&TokenTree) -> Option<T>,
          T: Spanned
{
    /// Creates a new match function with a default error.
    pub const fn new(f: F) -> Self {
        fn match_fn_default() -> ParseErr {
            ParseErr::UnexpectedToken
        }

        MatchFn(f, match_fn_default)
    }
}

impl<F, T, E> MatchFn<F, E> 
    where F: Fn(&TokenTree) -> Option<T>,
          T: Spanned,
          E: GonErr
{
    /// Creates a new match function with a defined error.
    pub const fn new_with_err(f: F, e: fn() -> E) -> Self {
        Self(f, e)
    }
}
impl<'tt, F, T, E> TokenPattern<'tt> for MatchFn<F, E> 
    where F: Fn(&TokenTree) -> Option<T>,
          T: Spanned + 'tt,
          E: GonErr
{    
    type Munched = T;
    type Err = E;

    fn try_munch(&self, tt: &'tt TokenTree) -> Option<Self::Munched> {
        (self.0)(tt)
    }

    fn fail_err(&self) -> Self::Err {
        (self.1)()
    }
}
impl<F: Clone, E> Clone for MatchFn<F, E> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1)
    }
}
impl<F: Copy, E> Copy for MatchFn<F, E> {}