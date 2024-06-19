use std::process::{exit, Output};

use crate::lexer::{Punctuation, Token, TokenKind};

type Out = Option<Fun>;

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct Fun(fn(&mut Stacks, TokenKind) -> bool);

pub trait Rule: Copy {
    type Output;

    fn push(stack: &mut Stacks, token: TokenKind) -> bool;

    fn and<R: Rule>(self, rule: R) -> And<Self, R> {
        And {
            left: self,
            right: rule,
        }
    }

    fn or<R: Rule>(self, rule: R) -> Or<Self, R> {
        Or {
            left: self,
            right: rule,
        }
    }

    fn list(self) -> List<Self> {
        List { item: self }
    }
}

pub trait NamedRule: Copy {
    type Inner: Rule;
}

impl<R: NamedRule> Rule for R {
    type Output = <R::Inner as Rule>::Output;

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        R::Inner::push(stack, token)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StringRule;

impl Rule for StringRule {
    type Output = String;

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        match token {
            TokenKind::String => true,
            t => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LBraceRule;

impl Rule for LBraceRule {
    type Output = ();

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        match token {
            TokenKind::Punctuation(Punctuation::LBrace) => true,
            t => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RBraceRule;

impl Rule for RBraceRule {
    type Output = ();

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        match token {
            TokenKind::Punctuation(Punctuation::RBrace) => true,
            t => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LBracketRule;

impl Rule for LBracketRule {
    type Output = ();

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        match token {
            TokenKind::Punctuation(Punctuation::LBracket) => true,
            t => false,
        }
    }
}
#[derive(Debug, Clone, Copy)]
pub struct RBracketRule;

impl Rule for RBracketRule {
    type Output = ();

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        match token {
            TokenKind::Punctuation(Punctuation::RBracket) => true,
            t => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CommaRule;

impl Rule for CommaRule {
    type Output = ();

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        match token {
            TokenKind::Punctuation(Punctuation::Comma) => true,
            t => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ColonRule;

impl Rule for ColonRule {
    type Output = ();

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        match token {
            TokenKind::Punctuation(Punctuation::Colon) => true,
            t => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NumberRule;

impl Rule for NumberRule {
    type Output = u32;

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        match token {
            TokenKind::Int => true,
            t => false,
        }
    }
}

// #[derive(Debug, Clone, Copy)]
// struct PunctRule(Punctuation);

// fn punct_get(stack: &mut Stacks, token: TokenKind) -> Out {
//     match token {
//         TokenKind::Punctuation(p, s) => {
//             if p ==  { stack.top_get::<Punctuation>() } {
//                  { stack.pop::<Punctuation>() };
//                 let fun = if stack.is_empty() {
//                     Ok(None)
//                 } else {
//                     Ok(Some( { stack.pop() }))
//                 };
//                 stack.push(());
//                 fun
//             } else {
//                 Err(TokenKind::Punctuation(p, s))
//             }
//         }
//         t => Err(t),
//     }
// }

//  impl Rule for PunctRule {
//     type Output = ();

//     fn get(self, stack: &mut Stacks) -> Fun {
//         stack.push(self.0);
//         Fun(punct_get)
//     }
// }

#[derive(Debug, Clone, Copy)]
pub struct And<L, R> {
    left: L,
    right: R,
}

impl<L: Rule, R: Rule> Rule for And<L, R> {
    type Output = (L::Output, R::Output);

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        stack.push(Fun(R::push));
        L::push(stack, token)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Or<L, R> {
    left: L,
    right: R,
}

impl<L: Rule, R: Rule> Rule for Or<L, R> {
    type Output = (L::Output, R::Output);

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        stack.fork_into(Fun(R::push));
        L::push(stack, token)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct List<R> {
    item: R,
}

impl<R: Rule> Rule for List<R> {
    type Output = ();

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        stack.fork();
        stack.push(Fun(Self::push));
        R::push(stack, token)
    }
}

pub struct Stacks {
    pub inner: Vec<Vec<Fun>>,
    current_stack: usize,
}

impl Stacks {
    pub fn push(&mut self, fun: Fun) {
        self.inner[self.current_stack].push(fun);
    }

    pub fn pop(&mut self) -> Option<Fun> {
        self.inner[self.current_stack].pop()
    }

    pub fn fork_into(&mut self, fun: Fun) {
        let mut new_stack = self.inner[self.current_stack].clone();
        new_stack.push(fun);
        self.inner.push(new_stack);
    }

    pub fn fork(&mut self) {
        let mut new_stack = self.inner[self.current_stack].clone();
        self.inner.push(new_stack);
    }
}

pub struct Ctx<R> {
    stacks: Stacks,
    rule: R,
}

impl<R: Rule> Ctx<R> {
    pub fn new(rule: R) -> Self {
        Self {
            rule,
            stacks: Stacks {
                current_stack: 0,
                inner: vec![vec![Fun(R::push)]],
            },
        }
    }

    pub fn feed(&mut self, token: Token) {
        self.stacks.current_stack = 0;

        let mut okays = vec![];
        while self.stacks.current_stack < self.stacks.inner.len() {
            let ok = if let Some(Fun(fun)) = self.stacks.pop() {
                fun(&mut self.stacks, token.kind())
            } else {
                false
            };
            okays.push(ok);
            self.stacks.current_stack += 1;
        }

        let mut okay_idx = 0;
        self.stacks.inner.retain(|_| {
            let keep = okays[okay_idx];
            okay_idx += 1;
            keep
        });
    }

    pub fn done(&self) -> bool {
        self.stacks
            .inner
            .iter()
            .filter(|stack| stack.is_empty())
            .count()
            > 0
    }
}
