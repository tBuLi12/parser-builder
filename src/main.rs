use std::{
    env, fs,
    mem::ManuallyDrop,
    process::exit,
    str::Chars,
    time::{Duration, Instant},
};

use lexer::{Int, Keyword, Lexer, Punctuation, Source, Span, StringLit, Token, TokenKind};

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

enum State {
    Value,
    ObjectValue, // (Vec<(String, Value)>, String),
    ArrayValue,  // (Vec<Value>),
    ArrayComma,  // (Vec<Value>),
    ObjectComma, // (Vec<(String, Value)>),
    Name,        //(Vec<(String, Value)>),
    Colon,       //(Vec<(String, Value)>, String),
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
    state: State,
    stack: Vec<Ctx>,
}

impl PushParser {
    fn push(mut self, token: Token) -> Result<Value, Self> {
        match self.state {
            State::ArrayValue => match token {
                Token::String(s) => {
                    unsafe {
                        self.stack
                            .last_mut()
                            .unwrap()
                            .array
                            .push(Value::String(s.value));
                    }
                    self.state = State::ArrayComma;
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
                    self.state = State::ArrayComma;
                    Err(self)
                }
                Token::Punctuation(Punctuation::LBracket, _) => {
                    self.stack.push(Ctx {
                        marker: Marker::Array,
                    });
                    self.stack.push(Ctx {
                        array: ManuallyDrop::new(vec![]),
                    });
                    self.state = State::ArrayValue;
                    Err(self)
                }
                Token::Punctuation(Punctuation::LBrace, _) => {
                    self.stack.push(Ctx {
                        marker: Marker::Array,
                    });
                    self.stack.push(Ctx {
                        object: ManuallyDrop::new(vec![]),
                    });
                    self.state = State::Name;
                    Err(self)
                }
                Token::Punctuation(Punctuation::RBracket, _) => {
                    let value = Value::Array(unsafe {
                        ManuallyDrop::into_inner(self.stack.pop().unwrap().array)
                    });
                    Self::_reduce(self.stack, value)
                }
                _ => exit(1),
            },
            State::ObjectValue => match token {
                Token::String(s) => {
                    let name = unsafe { ManuallyDrop::into_inner(self.stack.pop().unwrap().name) };
                    unsafe {
                        self.stack
                            .last_mut()
                            .unwrap()
                            .object
                            .push((name, Value::String(s.value)));
                    }
                    self.state = State::ObjectComma;
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
                    self.state = State::ObjectComma;
                    Err(self)
                }
                Token::Punctuation(Punctuation::LBrace, _) => {
                    self.stack.push(Ctx {
                        marker: Marker::Object,
                    });
                    self.stack.push(Ctx {
                        object: ManuallyDrop::new(vec![]),
                    });
                    self.state = State::Name;
                    Err(self)
                }
                Token::Punctuation(Punctuation::LBracket, _) => {
                    self.stack.push(Ctx {
                        marker: Marker::Object,
                    });
                    self.stack.push(Ctx {
                        array: ManuallyDrop::new(vec![]),
                    });
                    self.state = State::ArrayValue;
                    Err(self)
                }
                _ => exit(1),
            },
            State::Value => match token {
                Token::String(s) => Ok(Value::String(s.value)),
                Token::Int(i) => Ok(Value::Number(i.value)),
                Token::Punctuation(Punctuation::LBracket, _) => {
                    self.stack.push(Ctx {
                        array: ManuallyDrop::new(vec![]),
                    });
                    self.state = State::ArrayValue;
                    Err(self)
                }
                Token::Punctuation(Punctuation::LBrace, _) => {
                    self.stack.push(Ctx {
                        object: ManuallyDrop::new(vec![]),
                    });
                    self.state = State::Name;
                    Err(self)
                }
                _ => exit(1),
            },
            State::ArrayComma => match token {
                Token::Punctuation(Punctuation::Comma, _) => {
                    self.state = State::ArrayValue;
                    Err(self)
                }
                Token::Punctuation(Punctuation::RBracket, _) => {
                    let value = Value::Array(unsafe {
                        ManuallyDrop::into_inner(self.stack.pop().unwrap().array)
                    });
                    Self::_reduce(self.stack, value)
                }
                _ => exit(1),
            },
            State::ObjectComma => match token {
                Token::Punctuation(Punctuation::Comma, _) => {
                    self.state = State::Name;
                    Err(self)
                }
                Token::Punctuation(Punctuation::RBrace, _) => {
                    let value = Value::Object(unsafe {
                        ManuallyDrop::into_inner(self.stack.pop().unwrap().object)
                    });
                    Self::_reduce(self.stack, value)
                }
                _ => exit(1),
            },
            State::Name => match token {
                Token::String(s) => {
                    self.stack.push(Ctx {
                        name: ManuallyDrop::new(s.value),
                    });
                    self.state = State::Colon;
                    Err(self)
                }
                Token::Punctuation(Punctuation::RBrace, _) => {
                    let value = Value::Object(unsafe {
                        ManuallyDrop::into_inner(self.stack.pop().unwrap().object)
                    });
                    Self::_reduce(self.stack, value)
                }
                _ => exit(1),
            },
            State::Colon => match token {
                Token::Punctuation(Punctuation::Colon, _) => {
                    self.state = State::ObjectValue;
                    Err(self)
                }
                _ => exit(1),
            },
        }
    }

    fn _reduce(mut stack: Vec<Ctx>, value: Value) -> Result<Value, Self> {
        match stack.pop() {
            Some(marker) => match unsafe { marker.marker } {
                Marker::Object => unsafe {
                    let name = ManuallyDrop::into_inner(stack.pop().unwrap().name);
                    stack.last_mut().unwrap().object.push((name, value));
                    Err(Self {
                        state: State::ObjectComma,
                        stack,
                    })
                },
                Marker::Array => unsafe {
                    stack.last_mut().unwrap().array.push(value);
                    Err(Self {
                        state: State::ArrayComma,
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

    fn parse(lexer: &mut Lexer<StringSource>) -> Value {
        let mut value_parser = PushParser {
            state: State::Value,
            stack: vec![],
        };
        let value = loop {
            match value_parser.push(lexer.next()) {
                Ok(value) => {
                    break value;
                }
                Err(inner) => {
                    value_parser = inner;
                }
            }
        };

        value
    }

    let mut lexer = Lexer::new(StringSource {
        source: text.chars(),
    });
    let value = parse(&mut lexer);

    println!("{:?}", value);

    let mut duration = Duration::ZERO;
    for _ in 0..100 {
        // let start = Instant::now();
        let mut lexer = Lexer::new(StringSource {
            source: text.chars(),
        });
        let start = Instant::now();
        parse(&mut lexer);
        duration += start.elapsed();
    }
    println!("{:?}", duration);
}
