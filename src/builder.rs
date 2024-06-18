use std::process::{exit, Output};

use crate::lexer::{Punctuation, Token};

struct Stack;

impl Stack {
    fn push<T>(&mut self, value: T) {}

    unsafe fn pop<T>(&mut self) -> T {
        panic!("")
    }

    unsafe fn top_mut<T>(&mut self) -> &mut T {
        panic!("")
    }

    unsafe fn top_get<T: Copy>(&self) -> T {
        panic!("")
    }

    fn is_empty(&self) -> bool {}
}

type Out = Option<Fun>;

#[repr(transparent)]
struct Fun(fn(&mut Stack, Token) -> Option<Self>);

unsafe trait Rule: Copy {
    type Output;

    fn push(stack: &mut Stack, token: Token) -> Result<Option<Fun>, Token>;
}

#[derive(Debug, Clone, Copy)]
struct StringRule;

unsafe impl Rule for StringRule {
    type Output = String;

    fn push(stack: &mut Stack, token: Token) -> Result<Option<Fun>, Token> {
        match token {
            Token::String(s) => {
                stack.push(s.value);
                Ok(None)
            }
            t => Err(t),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct NumberRule;

unsafe impl Rule for NumberRule {
    type Output = u32;

    fn push(stack: &mut Stack, token: Token) -> Result<Option<Fun>, Token> {
        match token {
            Token::Int(i) => {
                stack.push(i.value);
                Ok(None)
            }
            t => Err(t),
        }
    }
}

// #[derive(Debug, Clone, Copy)]
// struct PunctRule(Punctuation);

// fn punct_get(stack: &mut Stack, token: Token) -> Out {
//     match token {
//         Token::Punctuation(p, s) => {
//             if p == unsafe { stack.top_get::<Punctuation>() } {
//                 unsafe { stack.pop::<Punctuation>() };
//                 let fun = if stack.is_empty() {
//                     Ok(None)
//                 } else {
//                     Ok(Some(unsafe { stack.pop() }))
//                 };
//                 stack.push(());
//                 fun
//             } else {
//                 Err(Token::Punctuation(p, s))
//             }
//         }
//         t => Err(t),
//     }
// }

// unsafe impl Rule for PunctRule {
//     type Output = ();

//     fn get(self, stack: &mut Stack) -> Fun {
//         stack.push(self.0);
//         Fun(punct_get)
//     }
// }

#[derive(Debug, Clone, Copy)]
struct And<L, R> {
    left: L,
    right: R,
}

impl<L: Rule, R: Rule> And<L, R> {
    fn middle(stack: &mut Stack, token: Token) -> Out {
        let left_out = unsafe { stack.pop::<L::Output>() };
        let right: R = unsafe { stack.pop() };
        stack.push(left_out);
        // stack.push(Self::after);
        let fun = (right.get(stack).0)(stack, token);
    }

    fn after(stack: &mut Stack, token: Token) -> Out {
        let right_out = unsafe { stack.pop::<R::Output>() };
        let left_out = unsafe { stack.pop::<L::Output>() };
        if stack.is_empty() {
            stack.push((left_out, right_out));
            Ok(None)
        } else {
            let fun: Fun = unsafe { stack.pop() };
            (fun.0)(stack, token)
        }
    }
}

unsafe impl<L: Rule, R: Rule> Rule for And<L, R> {
    type Output = (L::Output, R::Output);

    fn push(stack: &mut Stack, token: Token) -> Result<Option<Fun>, Token> {
        L::push(stack, token).map(|fun| match fun {})
    }
}
