use std::{
    env, fs,
    mem::ManuallyDrop,
    process::exit,
    str::Chars,
    time::{Duration, Instant},
};

use builder::{
    And, ColonRule, CommaRule, LBraceRule, LBracketRule, List, NamedRule, NumberRule, Or,
    RBraceRule, RBracketRule, StringRule,
};
use lexer::{Int, Keyword, Lexer, Punctuation, Source, Span, StringLit, Token, TokenKind};

mod builder;
mod lexer;

struct StringSource<'s> {
    source: Chars<'s>,
}

impl<'s> Source for StringSource<'s> {
    fn next(&mut self) -> Option<char> {
        self.source.next()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Value {
    String(String),
    Array(Vec<Value>),
    Object(Vec<(String, Value)>),
    Number(u32),
}

union Ctx {
    marker: Marker,
    array: ManuallyDrop<Vec<Value>>,
    object: ManuallyDrop<Vec<(String, Value)>>,
    name: ManuallyDrop<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Marker {
    Object,
    Array,
}

struct PushParser {
    state: fn(Self, Token) -> Result<Value, Self>,
    stack: Vec<Ctx>,
}

impl PushParser {
    fn array_value(mut self, token: Token) -> Result<Value, Self> {
        match token {
            Token::String(s) => {
                unsafe {
                    self.stack
                        .last_mut()
                        .unwrap()
                        .array
                        .push(Value::String(s.value));
                }
                self.state = Self::array_comma;
                Err(self)
            }
            Token::Int(i) => {
                unsafe {
                    self.stack
                        .last_mut()
                        .unwrap()
                        .array
                        .push(Value::Number(i.value));
                }
                self.state = Self::array_comma;
                Err(self)
            }
            Token::Punctuation(Punctuation::LBracket, _) => {
                self.stack.push(Ctx {
                    marker: Marker::Array,
                });
                self.stack.push(Ctx {
                    array: ManuallyDrop::new(vec![]),
                });
                self.state = Self::array_value;
                Err(self)
            }
            Token::Punctuation(Punctuation::LBrace, _) => {
                self.stack.push(Ctx {
                    marker: Marker::Array,
                });
                self.stack.push(Ctx {
                    object: ManuallyDrop::new(vec![]),
                });
                self.state = Self::name;
                Err(self)
            }
            Token::Punctuation(Punctuation::RBracket, _) => {
                let value = Value::Array(unsafe {
                    ManuallyDrop::into_inner(self.stack.pop().unwrap().array)
                });
                Self::_reduce(self.stack, value)
            }
            _ => exit(1),
        }
    }

    fn object_value(mut self, token: Token) -> Result<Value, Self> {
        match token {
            Token::String(s) => {
                let name = unsafe { ManuallyDrop::into_inner(self.stack.pop().unwrap().name) };
                unsafe {
                    self.stack
                        .last_mut()
                        .unwrap()
                        .object
                        .push((name, Value::String(s.value)));
                }
                self.state = Self::object_comma;
                Err(self)
            }
            Token::Int(i) => {
                let name = unsafe { ManuallyDrop::into_inner(self.stack.pop().unwrap().name) };
                unsafe {
                    self.stack
                        .last_mut()
                        .unwrap()
                        .object
                        .push((name, Value::Number(i.value)));
                }
                self.state = Self::object_comma;
                Err(self)
            }
            Token::Punctuation(Punctuation::LBrace, _) => {
                self.stack.push(Ctx {
                    marker: Marker::Object,
                });
                self.stack.push(Ctx {
                    object: ManuallyDrop::new(vec![]),
                });
                self.state = Self::name;
                Err(self)
            }
            Token::Punctuation(Punctuation::LBracket, _) => {
                self.stack.push(Ctx {
                    marker: Marker::Object,
                });
                self.stack.push(Ctx {
                    array: ManuallyDrop::new(vec![]),
                });
                self.state = Self::array_value;
                Err(self)
            }
            _ => exit(1),
        }
    }

    fn value(mut self, token: Token) -> Result<Value, Self> {
        match token {
            Token::String(s) => Ok(Value::String(s.value)),
            Token::Int(i) => Ok(Value::Number(i.value)),
            Token::Punctuation(Punctuation::LBracket, _) => {
                self.stack.push(Ctx {
                    array: ManuallyDrop::new(vec![]),
                });
                self.state = Self::array_value;
                Err(self)
            }
            Token::Punctuation(Punctuation::LBrace, _) => {
                self.stack.push(Ctx {
                    object: ManuallyDrop::new(vec![]),
                });
                self.state = Self::name;
                Err(self)
            }
            _ => exit(1),
        }
    }

    fn array_comma(mut self, token: Token) -> Result<Value, Self> {
        match token {
            Token::Punctuation(Punctuation::Comma, _) => {
                self.state = Self::array_value;
                Err(self)
            }
            Token::Punctuation(Punctuation::RBracket, _) => {
                let value = Value::Array(unsafe {
                    ManuallyDrop::into_inner(self.stack.pop().unwrap().array)
                });
                Self::_reduce(self.stack, value)
            }
            _ => exit(1),
        }
    }

    fn object_comma(mut self, token: Token) -> Result<Value, Self> {
        match token {
            Token::Punctuation(Punctuation::Comma, _) => {
                self.state = Self::name;
                Err(self)
            }
            Token::Punctuation(Punctuation::RBrace, _) => {
                let value = Value::Object(unsafe {
                    ManuallyDrop::into_inner(self.stack.pop().unwrap().object)
                });
                Self::_reduce(self.stack, value)
            }
            _ => exit(1),
        }
    }

    fn name(mut self, token: Token) -> Result<Value, Self> {
        match token {
            Token::String(s) => {
                self.stack.push(Ctx {
                    name: ManuallyDrop::new(s.value),
                });
                self.state = Self::colon;
                Err(self)
            }
            Token::Punctuation(Punctuation::RBrace, _) => {
                let value = Value::Object(unsafe {
                    ManuallyDrop::into_inner(self.stack.pop().unwrap().object)
                });
                Self::_reduce(self.stack, value)
            }
            _ => exit(1),
        }
    }

    fn colon(mut self, token: Token) -> Result<Value, Self> {
        match token {
            Token::Punctuation(Punctuation::Colon, _) => {
                self.state = Self::object_value;
                Err(self)
            }
            _ => exit(1),
        }
    }

    fn push(self, token: Token) -> Result<Value, Self> {
        (self.state)(self, token)
    }

    fn _reduce(mut stack: Vec<Ctx>, value: Value) -> Result<Value, Self> {
        match stack.pop() {
            Some(marker) => match unsafe { marker.marker } {
                Marker::Object => unsafe {
                    let name = ManuallyDrop::into_inner(stack.pop().unwrap().name);
                    stack.last_mut().unwrap().object.push((name, value));
                    Err(Self {
                        state: Self::object_comma,
                        stack,
                    })
                },
                Marker::Array => unsafe {
                    stack.last_mut().unwrap().array.push(value);
                    Err(Self {
                        state: Self::array_comma,
                        stack,
                    })
                },
            },

            None => Ok(value),
        }
    }
}

fn main() {
    let path = env::args().nth(1).unwrap();

    let text = fs::read_to_string(&path).unwrap();

    fn parse(lexer: &mut Lexer<StringSource>) -> bool {
        #[derive(Clone, Copy, Debug)]
        struct ValueRule;
        impl NamedRule for ValueRule {
            type Inner = Or<StringRule, Or<NumberRule, Or<ObjectRule, ArrayRule>>>;
        }

        #[derive(Clone, Copy, Debug)]
        struct PropRule;
        impl NamedRule for PropRule {
            type Inner = And<StringRule, And<ColonRule, ValueRule>>;
        }

        #[derive(Clone, Copy, Debug)]
        struct ObjectRule;
        impl NamedRule for ObjectRule {
            type Inner =
                And<LBraceRule, And<PropRule, And<List<And<CommaRule, PropRule>>, RBraceRule>>>;
        }

        #[derive(Clone, Copy, Debug)]
        struct ArrayRule;
        impl NamedRule for ArrayRule {
            type Inner = And<
                LBracketRule,
                And<ValueRule, And<List<And<CommaRule, ValueRule>>, RBracketRule>>,
            >;
        }

        let mut ctx = builder::Ctx::new(ValueRule);
        let ok = loop {
            let token = lexer.next();
            if token.kind() == TokenKind::Eof {
                break ctx.done();
            }
            ctx.feed(token);
        };

        ok
    }

    let mut lexer = Lexer::new(StringSource {
        source: text.chars(),
    });
    let value = parse(&mut lexer);

    if !value {
        panic!("invalid syntax")
    }

    let mut duration = Duration::ZERO;
    let mut lex_duration = Duration::ZERO;
    for _ in 0..100 {
        let start = Instant::now();
        let mut lexer = Lexer::new(StringSource {
            source: text.chars(),
        });
        lex_duration += start.elapsed();
        let start = Instant::now();
        parse(&mut lexer);
        duration += start.elapsed();
    }
    println!("{:?}", lex_duration);
    println!("{:?}", duration);
}
