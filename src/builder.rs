use std::{
    hint::black_box,
    process::{exit, Output},
    rc::Rc,
    sync::atomic::fence,
    time::{Duration, Instant},
};

use crate::{
    lexer::{Lexer, Punctuation, Token, TokenKind},
    StringSource, Value, DURATION, DURATION_2,
};

type Out = Option<Fun>;

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct Fun(fn(&mut Stacks, TokenKind) -> bool);

pub trait Rule: Copy {
    type Output;

    fn push(stack: &mut Stacks, token: TokenKind) -> bool;

    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output;

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
    type Output;

    fn map(raw: <Self::Inner as Rule>::Output) -> Self::Output;
}

impl<R: NamedRule> Rule for R {
    type Output = <Self as NamedRule>::Output;

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        R::Inner::push(stack, token)
    }

    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output {
        Self::map(R::Inner::get(decisions, tokens))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StringRule;

impl Rule for StringRule {
    type Output = String;

    fn push(_: &mut Stacks, token: TokenKind) -> bool {
        token == TokenKind::String
    }

    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output {
        match tokens.next().unwrap() {
            Token::String(s) => s.value,
            _ => unreachable!(),
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

    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output {
        tokens.next();
        ()
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

    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output {
        tokens.next();
        ()
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

    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output {
        tokens.next();
        ()
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
    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output {
        tokens.next();
        ()
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

    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output {
        tokens.next();
        ()
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

    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output {
        tokens.next();
        ()
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

    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output {
        match tokens.next().unwrap() {
            Token::Int(i) => i.value,
            _ => unreachable!(),
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

    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output {
        let l = L::get(decisions, tokens);
        let r = R::get(decisions, tokens);
        (l, r)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Or<L, R> {
    left: L,
    right: R,
}

impl<T, L: Rule<Output = T>, R: Rule<Output = T>> Rule for Or<L, R> {
    type Output = T;

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        stack.fork_into(Fun(R::push));
        L::push(stack, token)
    }

    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output {
        let decision = decisions.next().unwrap();
        if decision {
            R::get(decisions, tokens)
        } else {
            L::get(decisions, tokens)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct List<R> {
    item: R,
}

impl<R: Rule> Rule for List<R> {
    type Output = Vec<R::Output>;

    fn push(stack: &mut Stacks, token: TokenKind) -> bool {
        stack.fork();
        stack.push(Fun(Self::push));
        R::push(stack, token)
    }

    fn get(
        decisions: &mut impl Iterator<Item = bool>,
        tokens: &mut impl Iterator<Item = Token>,
    ) -> Self::Output {
        let mut out = vec![];
        while !decisions.next().unwrap() {
            out.push(R::get(decisions, tokens));
        }
        out
    }
}

#[derive(Debug, Clone)]
pub struct Stack {
    node: Option<Rc<Node>>,
    decisions: Vec<bool>,
}

#[derive(Debug)]
struct Node {
    value: Fun,
    prev: Option<Rc<Node>>,
}

pub struct Stacks {
    pub inner: Vec<Stack>,
    pub main_decisions: Vec<bool>,
    current_stack: usize,
}

impl Stack {
    pub fn push(&mut self, fun: Fun) {
        let prev = std::mem::replace(&mut self.node, None);
        self.node = Some(Rc::new(Node { prev, value: fun }));
    }

    pub fn pop(&mut self) -> Option<Fun> {
        let node = std::mem::replace(&mut self.node, None);
        if let Some(node) = node {
            self.node = node.prev.clone();
            Some(node.value)
        } else {
            None
        }
    }

    pub fn is_empty(&self) -> bool {
        self.node.is_none()
    }
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
        self.inner[self.current_stack].decisions.push(false);
        new_stack.decisions.push(true);
        new_stack.push(fun);
        self.inner.push(new_stack);
    }

    pub fn fork(&mut self) {
        let mut new_stack = self.inner[self.current_stack].clone();
        self.inner[self.current_stack].decisions.push(false);
        new_stack.decisions.push(true);
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
                inner: vec![Stack {
                    node: Some(Rc::new(Node {
                        value: Fun(R::push),
                        prev: None,
                    })),
                    decisions: vec![],
                }],
                main_decisions: vec![],
            },
        }
    }

    #[inline(never)]
    pub fn feed(&mut self, token: TokenKind) {
        self.stacks.current_stack = 0;

        let mut okays = vec![];
        while self.stacks.current_stack < self.stacks.inner.len() {
            let ok = if let Some(Fun(fun)) = self.stacks.pop() {
                fun(&mut self.stacks, token)
            } else {
                false
            };

            okays.push(ok);
            self.stacks.current_stack += 1;
        }

        let mut okay_idx = 0;
        self.stacks.inner.retain_mut(|_| {
            let keep = okays[okay_idx];
            okay_idx += 1;
            keep
        });

        if self.stacks.inner.len() == 1 {
            self.stacks
                .main_decisions
                .extend(self.stacks.inner[0].decisions.drain(..));
        }
    }

    pub fn done(self) -> Option<Stack> {
        let mut ok_stacks: Vec<_> = self
            .stacks
            .inner
            .into_iter()
            .filter(|stack| stack.is_empty())
            .collect();

        if ok_stacks.len() > 1 {
            panic!("ambiguous grammar");
        }

        let mut stack = ok_stacks.pop()?;

        stack.decisions = self.stacks.main_decisions;
        Some(stack)
    }
}

pub fn parse_magic_name<R: Rule<Output = Value>>(
    tokens: Vec<Token>,
    rule: R,
) -> Option<(R::Output, Duration)> {
    let start = Instant::now();

    let mut ctx = Ctx::new(rule);

    for token in &tokens {
        ctx.feed(token.kind());
    }

    unsafe {
        DURATION += start.elapsed();
    }

    fence(std::sync::atomic::Ordering::SeqCst);

    let stack = ctx.done()?;

    let start = Instant::now();

    let mut decisions = stack.decisions.into_iter();
    let mut tokens = tokens.into_iter();

    let out = R::get(&mut decisions, &mut tokens);

    // match out {
    //     Value::Object(o) => println!("{:?}", o.len()),
    //     _ => (),
    // }

    unsafe {
        DURATION_2 += start.elapsed();
    }

    // Some((Value::Number(0), Duration::ZERO))
    Some((out, Duration::ZERO))
}
