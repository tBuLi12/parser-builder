use core::fmt;
use std::{collections::HashSet, fmt::Debug, hash::Hash, iter};

// 3.57 * 10^5
// pl - 3,13 * 10^5

// 3,57 * 10^5 - 3,13 * 10^5 = (3,57 - 3,13) * 10^5

pub trait Source {
    type Token: Token;
    fn get(&mut self) -> Self::Token;
    fn report_error(
        &mut self,
        token: &Self::Token,
        expected: HashSet<<Self::Token as Token>::Kind>,
    );
}

struct Src<S: Source> {
    inner: S,
    buffer: Vec<S::Token>,
}

impl<S: Source> Src<S>
where
    S::Token: Token,
{
    fn get(&mut self) -> S::Token {
        let token = self.buffer.remove(0);
        if self.buffer.is_empty() {
            self.buffer.push(self.inner.get());
        }
        token
    }

    fn peek(&self) -> &S::Token {
        &self.buffer[0]
    }

    fn lookahead(&mut self, len: usize) -> &[S::Token] {
        while self.buffer.len() < len {
            self.buffer.push(self.inner.get());
        }

        &self.buffer
    }

    fn apply(&mut self, edits: Edits<S::Token>) {
        let edited = edits.apply_to(std::mem::take(&mut self.buffer).into_iter());
        self.buffer.extend(edited);
    }
}

pub trait Token: Clone + Debug {
    type Kind: Eq + Copy + Debug + Hash;

    fn kind(&self) -> Self::Kind;
    fn inserted(kind: Self::Kind) -> Self;
}

#[derive(Clone, Copy, Debug)]
pub enum Edit<T> {
    Insert(T),
    Delete,
    Keep,
}

#[derive(Clone, Debug)]
pub struct Edits<T>(Vec<Edit<T>>);

fn min<T: Token>(edits1: Edits<T>, edits2: Edits<T>) -> Edits<T> {
    let cost1 = edits1.cost();
    let cost2 = edits2.cost();

    if cost1 > cost2 {
        edits2
    } else {
        edits1
    }
}

struct EditsIter<T, EI: Iterator<Item = Edit<T>>, TI: Iterator<Item = T>> {
    tokens: TI,
    edits: EI,
}

impl<T, EI: Iterator<Item = Edit<T>>, TI: Iterator<Item = T>> Iterator for EditsIter<T, EI, TI> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.edits.next().unwrap_or(Edit::Keep) {
            Edit::Insert(token) => Some(token),
            Edit::Delete => {
                self.tokens.next();
                self.next()
            }
            Edit::Keep => self.tokens.next(),
        }
    }
}

impl<T: Token> Edits<T> {
    fn apply_to(self, iter: impl Iterator<Item = T>) -> impl Iterator<Item = T> {
        EditsIter {
            tokens: iter,
            edits: self.0.into_iter(),
        }
    }

    fn cost(&self) -> usize {
        self.0
            .iter()
            .map(|edit| match edit {
                Edit::Insert(_) => 1,
                Edit::Delete => 2,
                Edit::Keep => 0,
            })
            .sum()
    }
}

pub trait Rule: Copy + std::fmt::Debug {
    type Token: Token;
    type Output;
    fn parse(
        self,
        mut input: impl Source<Token = Self::Token>,
        follow: impl Rule<Token = Self::Token>,
    ) -> Option<Self::Output> {
        let mut src = Src {
            buffer: vec![input.get()],
            inner: input,
        };
        self.parse_src(&mut src, follow)
    }
    fn parse_src(
        self,
        input: &mut Src<impl Source<Token = Self::Token>>,
        follow: impl Rule<Token = Self::Token>,
    ) -> Option<Self::Output>;
    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
        inserts_remaining: usize,
    ) -> Edits<Self::Token>;
    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token>;

    fn first(self) -> HashSet<<Self::Token as Token>::Kind>;

    fn nullable(self) -> bool;

    fn and<Other: Rule<Token = Self::Token>>(
        self,
        other: Other,
    ) -> impl Rule<Token = Self::Token, Output = (Self::Output, Other::Output)> {
        And(self, other)
    }

    fn or<Other: Rule<Token = Self::Token, Output = Self::Output>>(
        self,
        other: Other,
    ) -> impl Rule<Token = Self::Token, Output = Self::Output> {
        Or(self, other)
    }

    fn list(self) -> impl Rule<Token = Self::Token, Output = Vec<Self::Output>> {
        List(self)
    }

    fn map<O, F: FnOnce(Self::Output) -> O + Copy>(
        self,
        fun: F,
    ) -> impl Rule<Token = Self::Token, Output = O> {
        Map { rule: self, fun }
    }
}

pub trait NamedRule {
    type Token: Token;
    type Output;

    fn get(self) -> impl Rule<Token = Self::Token, Output = Self::Output>;
}

impl<T: NamedRule + Copy + Debug> Rule for T {
    type Token = <T as NamedRule>::Token;
    type Output = <T as NamedRule>::Output;

    fn parse_src(
        self,
        input: &mut Src<impl Source<Token = Self::Token>>,
        follow: impl Rule<Token = Self::Token>,
    ) -> Option<Self::Output> {
        self.get().parse_src(input, follow)
    }

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        self.get().get_edits(tokens, follow, inserts_remaining)
    }

    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        self.get().get_edits_no_follow(tokens, inserts_remaining)
    }

    fn first(self) -> HashSet<<Self::Token as Token>::Kind> {
        self.get().first()
    }

    fn nullable(self) -> bool {
        self.get().nullable()
    }
}

pub fn single<T: Token>(kind: T::Kind) -> impl Rule<Token = T, Output = T> {
    Single(kind)
}

struct Single<T: Token>(T::Kind);
impl<T: Token> Copy for Single<T> {}
impl<T: Token> Clone for Single<T> {
    fn clone(&self) -> Self {
        Single(self.0.clone())
    }
}

impl<T: Token> std::fmt::Debug for Single<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Single({:?})", self.0)
    }
}

#[derive(Clone, Copy, Debug)]
struct And<L, R>(L, R);

#[derive(Clone, Copy, Debug)]
struct Or<L, R>(L, R);

#[derive(Clone, Copy)]
struct Map<R, F> {
    rule: R,
    fun: F,
}

impl<R: fmt::Debug, F> fmt::Debug for Map<R, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Map({:?})", self.rule)
    }
}

#[derive(Clone, Copy, Debug)]
struct List<R>(R);

impl<T: Token> Rule for Single<T> {
    type Token = T;
    type Output = T;

    fn parse_src(
        self,
        input: &mut Src<impl Source<Token = Self::Token>>,
        _: impl Rule<Token = Self::Token>,
    ) -> Option<Self::Output> {
        if input.peek().kind() == self.0 {
            Some(input.get())
        } else {
            None
        }
    }

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} -> {:?} : {:?}", self, follow, tokens);

        if tokens.len() == 0 {
            return Edits(vec![]);
        }

        if tokens[0].kind() == self.0 {
            let mut keep: Edits<T> = Edits(vec![Edit::Keep]);
            keep.0.extend(
                follow
                    .get_edits_no_follow(&tokens[1..], inserts_remaining)
                    .0
                    .into_iter(),
            );
            return keep;
        }

        let del_count = tokens
            .iter()
            .position(|t| t.kind() == self.0)
            .unwrap_or(tokens.len());

        let mut dels: Edits<T> = Edits(vec![Edit::Delete; del_count]);
        let mut insert = Edits(vec![Edit::Insert(T::inserted(self.0))]);

        let deleted: Vec<T> = dels
            .clone()
            .apply_to(tokens.iter().cloned())
            .skip(1)
            .collect();
        let inserted: Vec<T> = insert
            .clone()
            .apply_to(tokens.iter().cloned())
            .skip(1)
            .collect();
        let del_edits = follow.get_edits_no_follow(&deleted, inserts_remaining);
        dels.0.extend(del_edits.0.into_iter());

        if inserts_remaining > 0 {
            let insert_edits = follow.get_edits_no_follow(&inserted, inserts_remaining - 1);
            insert.0.extend(insert_edits.0.into_iter());
            //     eprintln!("-{:?}", self);
            return min(dels, insert);
        }

        // eprintln!("-{:?}", self);
        return dels;
    }

    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        if tokens.len() == 0 {
            return Edits(vec![]);
        }

        if tokens[0].kind() == self.0 {
            let keep: Edits<T> = Edits(vec![Edit::Keep]);
            return keep;
        }

        let del_count = tokens
            .iter()
            .position(|t| t.kind() == self.0)
            .unwrap_or(tokens.len());

        let dels: Edits<T> = Edits(vec![Edit::Delete; del_count]);

        if inserts_remaining > 0 {
            let insert = Edits(vec![Edit::Insert(T::inserted(self.0))]);
            return min(dels, insert);
        }

        // eprintln!("-{:?}", self);
        return dels;
    }

    fn first(self) -> HashSet<<Self::Token as Token>::Kind> {
        iter::once(self.0).collect()
    }

    fn nullable(self) -> bool {
        false
    }
}

impl<T: Token, L, R> Rule for And<L, R>
where
    L: Rule<Token = T>,
    R: Rule<Token = T>,
{
    type Token = T;
    type Output = (L::Output, R::Output);

    fn parse_src(
        self,
        input: &mut Src<impl Source<Token = Self::Token>>,
        follow: impl Rule<Token = Self::Token>,
    ) -> Option<Self::Output> {
        let l = self.0.parse_src(input, self.1.and(follow))?;
        if let Some(r) = self.1.parse_src(input, follow) {
            return Some((l, r));
        }

        input.inner.report_error(&input.buffer[0], self.0.first());

        let lookahead = input.lookahead(9);
        let edits = self.1.get_edits(lookahead, follow, 9);
        eprintln!("expected {:?} -> {:?}", self.1, follow);
        eprintln!("lookahead - {:?}", lookahead);
        eprintln!("edits - {:?}", edits);
        input.apply(edits);
        let r = self.1.parse_src(input, follow).unwrap();

        Some((l, r))
    }

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} -> {:?} : {:?}", self, follow, tokens);
        let e = self
            .0
            .get_edits(tokens, self.1.and(follow), inserts_remaining);
        // eprintln!("-{:?}", self);
        e
    }

    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} : {:?}", self, tokens);
        let e = self.0.get_edits(tokens, self.1, inserts_remaining);
        // eprintln!("-{:?}", self);
        e
    }

    fn first(self) -> HashSet<<Self::Token as Token>::Kind> {
        if self.0.nullable() {
            self.0.first().union(&self.1.first()).copied().collect()
        } else {
            self.0.first()
        }
    }

    fn nullable(self) -> bool {
        self.0.nullable() && self.1.nullable()
    }
}

impl<T: Token, O, L, R> Rule for Or<L, R>
where
    L: Rule<Token = T, Output = O>,
    R: Rule<Token = T, Output = O>,
{
    type Token = T;
    type Output = O;

    fn parse_src(
        self,
        input: &mut Src<impl Source<Token = Self::Token>>,
        follow: impl Rule<Token = Self::Token>,
    ) -> Option<Self::Output> {
        self.0
            .parse_src(input, follow)
            .or_else(|| self.1.parse_src(input, follow))
    }

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} -> {:?} : {:?}", self, follow, tokens);
        let l_edits = self.0.get_edits(tokens, follow, inserts_remaining);
        let r_edits = self.1.get_edits(tokens, follow, inserts_remaining);
        // eprintln!("-{:?}", self);
        min(l_edits, r_edits)
    }

    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} : {:?}", self, tokens);
        let l_edits = self.0.get_edits_no_follow(tokens, inserts_remaining);
        let r_edits = self.1.get_edits_no_follow(tokens, inserts_remaining);
        // eprintln!("-{:?}", self);
        min(l_edits, r_edits)
    }

    fn first(self) -> HashSet<<Self::Token as Token>::Kind> {
        self.0.first().union(&self.1.first()).copied().collect()
    }

    fn nullable(self) -> bool {
        self.0.nullable() || self.1.nullable()
    }
}

impl<T: Token, O, F, R> Rule for Map<R, F>
where
    R: Rule<Token = T>,
    F: FnOnce(R::Output) -> O + Copy,
{
    type Token = T;
    type Output = O;
    fn parse_src(
        self,
        input: &mut Src<impl Source<Token = Self::Token>>,
        follow: impl Rule<Token = Self::Token>,
    ) -> Option<Self::Output> {
        self.rule.parse_src(input, follow).map(self.fun)
    }

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} -> {:?} : {:?}", self, follow, tokens);
        let r = self.rule.get_edits(tokens, follow, inserts_remaining);
        // eprintln!("-{:?}", self);
        r
    }

    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} : {:?}", self, tokens);
        let r = self.rule.get_edits_no_follow(tokens, inserts_remaining);
        // eprintln!("-{:?}", self);
        r
    }

    fn first(self) -> HashSet<<Self::Token as Token>::Kind> {
        self.rule.first()
    }

    fn nullable(self) -> bool {
        self.rule.nullable()
    }
}

impl<T: Token, R> Rule for List<R>
where
    R: Rule<Token = T>,
{
    type Token = T;
    type Output = Vec<R::Output>;

    fn parse_src(
        self,
        input: &mut Src<impl Source<Token = Self::Token>>,
        follow: impl Rule<Token = Self::Token>,
    ) -> Option<Self::Output> {
        let mut out = vec![];
        while let Some(item) = self.0.parse_src(input, self.and(follow)) {
            eprintln!("adding next item");
            out.push(item)
        }
        Some(out)
    }

    fn get_edits(
        self,
        tokens: &[Self::Token],
        follow: impl Rule<Token = Self::Token>,
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} -> {:?} : {:?}", self, follow, tokens);
        if tokens.is_empty() {
            //     eprintln!("-{:?}", self);
            return Edits(vec![]);
        }

        let one = self
            .0
            .and(self)
            .and(follow)
            .get_edits_no_follow(tokens, inserts_remaining);

        let two = follow.get_edits_no_follow(tokens, inserts_remaining);

        // eprintln!("-{:?}", self);
        min(one, two)
    }

    fn get_edits_no_follow(
        self,
        tokens: &[Self::Token],
        inserts_remaining: usize,
    ) -> Edits<Self::Token> {
        // eprintln!("+{:?} : {:?}", self, tokens);
        if tokens.is_empty() {
            //     eprintln!("-{:?}", self);
            return Edits(vec![]);
        }

        let delete_all = Edits(vec![Edit::Delete; tokens.len()]);
        let other = self
            .0
            .and(self)
            .get_edits_no_follow(tokens, inserts_remaining);
        // eprintln!("-{:?}", self);
        min(delete_all, other)
    }

    fn first(self) -> HashSet<<Self::Token as Token>::Kind> {
        self.0.first()
    }

    fn nullable(self) -> bool {
        true
    }
}

#[macro_export]
macro_rules! grammar {
    ($token:ty, $kind:ty, {
        $(
            $name:ident: $out:ty => $rule:expr
        ),*
    }) => {
        $(
            struct $name;

            impl $crate::NamedRule for $name {
                type Token = $token;
                type Output = $out;

                fn get(self) -> impl $crate::Rule<Token = Self::Token, Output = Self::Output> {
                    $rule
                }
            }
        )*
    };
}
