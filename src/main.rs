use std::{
    env, fs,
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
    ObjectValue(Vec<(String, Value)>, String),
    ArrayValue(Vec<Value>),
    ArrayComma(Vec<Value>),
    ObjectComma(Vec<(String, Value)>),
    Name(Vec<(String, Value)>),
    Colon(Vec<(String, Value)>, String),
}

enum Ctx {
    Array(Vec<Value>),
    Object(Vec<(String, Value)>, String),
}

struct PushParser {
    state: State,
    stack: Vec<Ctx>,
}

impl PushParser {
    fn push(mut self, token: Token) -> Result<Value, Self> {
        match self.state {
            State::ArrayValue(mut values) => match token {
                Token::String(s) => {
                    values.push(Value::String(s.value));
                    self.state = State::ArrayComma(values);
                    Err(self)
                }
                Token::Int(i) => {
                    values.push(Value::Number(i.value));
                    self.state = State::ArrayComma(values);
                    Err(self)
                }
                Token::Punctuation(Punctuation::LBracket, _) => {
                    self.stack.push(Ctx::Array(values));
                    self.state = State::ArrayValue(vec![]);
                    Err(self)
                }
                Token::Punctuation(Punctuation::LBrace, _) => {
                    self.stack.push(Ctx::Array(values));
                    self.state = State::Name(vec![]);
                    Err(self)
                }
                Token::Punctuation(Punctuation::RBracket, _) => {
                    Self::_reduce(self.stack, Value::Array(values))
                }
                _ => exit(1),
            },
            State::ObjectValue(mut fields, name) => match token {
                Token::String(s) => {
                    fields.push((name, Value::String(s.value)));
                    self.state = State::ObjectComma(fields);
                    Err(self)
                }
                Token::Int(i) => {
                    fields.push((name, Value::Number(i.value)));
                    self.state = State::ObjectComma(fields);
                    Err(self)
                }
                Token::Punctuation(Punctuation::LBrace, _) => {
                    self.stack.push(Ctx::Object(fields, name));
                    self.state = State::Name(vec![]);
                    Err(self)
                }
                Token::Punctuation(Punctuation::LBracket, _) => {
                    self.stack.push(Ctx::Object(fields, name));
                    self.state = State::ArrayValue(vec![]);
                    Err(self)
                }
                _ => exit(1),
            },
            State::Value => match token {
                Token::String(s) => Ok(Value::String(s.value)),
                Token::Int(i) => Ok(Value::Number(i.value)),
                Token::Punctuation(Punctuation::LBracket, _) => {
                    self.state = State::ArrayValue(vec![]);
                    Err(self)
                }
                Token::Punctuation(Punctuation::LBrace, _) => {
                    self.state = State::Name(vec![]);
                    Err(self)
                }
                _ => exit(1),
            },
            State::ArrayComma(values) => match token {
                Token::Punctuation(Punctuation::Comma, _) => {
                    self.state = State::ArrayValue(values);
                    Err(self)
                }
                Token::Punctuation(Punctuation::RBracket, _) => {
                    Self::_reduce(self.stack, Value::Array(values))
                }
                _ => exit(1),
            },
            State::ObjectComma(fields) => match token {
                Token::Punctuation(Punctuation::Comma, _) => {
                    self.state = State::Name(fields);
                    Err(self)
                }
                Token::Punctuation(Punctuation::RBrace, _) => {
                    Self::_reduce(self.stack, Value::Object(fields))
                }
                _ => exit(1),
            },
            State::Name(fields) => match token {
                Token::String(s) => {
                    self.state = State::Colon(fields, s.value);
                    Err(self)
                }
                Token::Punctuation(Punctuation::RBrace, _) => {
                    Self::_reduce(self.stack, Value::Object(fields))
                }
                _ => exit(1),
            },
            State::Colon(fields, name) => match token {
                Token::Punctuation(Punctuation::Colon, _) => {
                    self.state = State::ObjectValue(fields, name);
                    Err(self)
                }
                _ => exit(1),
            },
        }
    }

    fn _reduce(mut stack: Vec<Ctx>, value: Value) -> Result<Value, Self> {
        match stack.pop() {
            Some(Ctx::Array(mut values)) => {
                values.push(value);
                Err(Self {
                    state: State::ArrayComma(values),
                    stack,
                })
            }
            Some(Ctx::Object(mut fields, name)) => {
                fields.push((name, value));
                Err(Self {
                    state: State::ObjectComma(fields),
                    stack,
                })
            }
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
