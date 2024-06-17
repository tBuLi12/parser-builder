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
    offset: usize,
}

impl<'s> Source for StringSource<'s> {
    fn next(&mut self) -> Option<char> {
        self.source.next()
    }
}

struct Parser<'s> {
    lexer: lexer::Lexer<StringSource<'s>>,
    recovery: Vec<lexer::TokenKind>,
    current: lexer::Token,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Value {
    String(String),
    Array(Vec<Value>),
    Object(Vec<(String, Value)>),
    Number(u32),
}

impl<'s> Parser<'s> {
    fn new(source: &'s str) -> Self {
        let mut lexer = lexer::Lexer::new(StringSource {
            source: source.chars(),
            offset: 0,
        });

        Self {
            recovery: vec![],
            current: lexer.next(),
            lexer,
        }
    }

    fn next(&mut self) -> lexer::Token {
        let token = std::mem::replace(&mut self.current, self.lexer.next());

        token
    }

    fn string(&mut self) -> Result<StringLit, ()> {
        if self.current.kind() != TokenKind::String {
            return Err(());
        }

        match self.next() {
            Token::String(s) => Ok(s),
            _ => unreachable!(),
        }
    }

    fn keyword(&mut self, kw: Keyword) -> Result<Span, ()> {
        if self.current.kind() != TokenKind::Keyword(kw) {
            return Err(());
        }

        match self.next() {
            Token::Keyword(_, s) => Ok(s),
            _ => unreachable!(),
        }
    }

    fn punct(&mut self, p: Punctuation) -> Result<Span, ()> {
        if self.current.kind() != TokenKind::Punctuation(p) {
            return Err(());
        }

        match self.next() {
            Token::Punctuation(_, s) => Ok(s),
            _ => unreachable!(),
        }
    }

    fn number(&mut self) -> Result<Int, ()> {
        if self.current.kind() != TokenKind::Int {
            return Err(());
        }

        match self.next() {
            Token::Int(i) => Ok(i),
            _ => unreachable!(),
        }
    }

    fn array(&mut self) -> Result<Vec<Value>, ()> {
        let l_bracket = self.punct(Punctuation::LBracket)?;

        let mut values = vec![];

        loop {
            let Ok(value) = self.value() else {
                if let Ok(r_bracket) = self.punct(Punctuation::RBracket) {
                    return Ok(values);
                }
                if let Ok(comma) = self.punct(Punctuation::Comma) {
                    continue;
                }
                if self.recovery.contains(&self.current.kind()) {
                    return Ok(values);
                }
                self.next();
                continue;
            };

            values.push(value);

            let Ok(comma) = self.punct(Punctuation::Comma) else {
                if let Ok(r_bracket) = self.punct(Punctuation::RBracket) {
                    return Ok(values);
                }
                if let Ok(value) = self.value() {
                    values.push(value);
                    continue;
                }
                if self.recovery.contains(&self.current.kind()) {
                    return Ok(values);
                }
                self.next();
                continue;
            };
        }
    }

    fn field(&mut self) -> Result<(String, Value), ()> {
        let name = self.string()?;
        let colon = self.punct(Punctuation::Colon).unwrap();
        let value = self.value().unwrap();

        Ok((name.value, value))
    }

    fn object(&mut self) -> Result<Vec<(String, Value)>, ()> {
        let l_brace = self.punct(Punctuation::LBrace)?;

        let mut fields = vec![];

        loop {
            let Ok(field) = self.field() else {
                break;
            };
            fields.push(field);

            let Ok(comma) = self.punct(Punctuation::Comma) else {
                break;
            };
        }

        let r_brace = self.punct(Punctuation::RBrace).unwrap();

        Ok(fields)
    }

    fn value(&mut self) -> Result<Value, ()> {
        self.string()
            .map(|s| Value::String(s.value))
            .or_else(|_| self.array().map(Value::Array))
            .or_else(|_| self.object().map(Value::Object))
            .or_else(|_| self.number().map(|n| Value::Number(n.value)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ParseArray {
    state: ParseArrayState,
    values: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParseArrayState {
    Comma,
    Value(ParseValue),
}

impl ParseArray {
    fn push(mut self, token: Token) -> Result<Vec<Value>, Self> {
        if self.state == ParseArrayState::Value(ParseValue::None)
            && token.kind() == TokenKind::Punctuation(Punctuation::RBracket)
        {
            return Ok(self.values);
        }

        match self.state {
            ParseArrayState::Comma => {
                if token.kind() != TokenKind::Punctuation(Punctuation::Comma) {
                    exit(1);
                }
                self.state = ParseArrayState::Value(ParseValue::None);
                Err(self)
            }
            ParseArrayState::Value(inner) => {
                match inner.push(token) {
                    Ok(value) => {
                        self.values.push(value);
                        self.state = ParseArrayState::Comma;
                    }
                    Err(inner) => {
                        self.state = ParseArrayState::Value(inner);
                    }
                }
                Err(self)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ParseObject {
    state: ParseObjectState,
    fields: Vec<(String, Value)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParseObjectState {
    Comma,
    Field(ParseField),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParseField {
    None,
    Colon(String),
    Value(String, ParseValue),
}

impl ParseField {
    fn push(self, token: Token) -> Result<(String, Value), Self> {
        match self {
            ParseField::None => match token {
                Token::String(s) => Err(Self::Colon(s.value)),
                _ => exit(1),
            },
            ParseField::Colon(name) => {
                if token.kind() != TokenKind::Punctuation(Punctuation::Colon) {
                    exit(1);
                }
                Err(ParseField::Value(name, ParseValue::None))
            }
            ParseField::Value(name, inner) => match inner.push(token) {
                Ok(value) => Ok((name, value)),
                Err(inner) => Err(ParseField::Value(name, inner)),
            },
        }
    }
}

impl ParseObject {
    fn push(mut self, token: Token) -> Result<Vec<(String, Value)>, Self> {
        if self.state == ParseObjectState::Field(ParseField::None)
            && token.kind() == TokenKind::Punctuation(Punctuation::RBracket)
        {
            return Ok(self.fields);
        }

        match self.state {
            ParseObjectState::Comma => {
                if token.kind() != TokenKind::Punctuation(Punctuation::Comma) {
                    exit(1);
                }
                self.state = ParseObjectState::Field(ParseField::None);
                Err(self)
            }
            ParseObjectState::Field(inner) => {
                match inner.push(token) {
                    Ok(value) => {
                        self.fields.push(value);
                        self.state = ParseObjectState::Comma;
                    }
                    Err(inner) => {
                        self.state = ParseObjectState::Field(inner);
                    }
                }
                Err(self)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParseValue {
    None,
    Array(Box<ParseArray>),
    Object(Box<ParseObject>),
}

impl ParseValue {
    fn push(mut self, token: Token) -> Result<Value, Self> {
        match self {
            ParseValue::None => match token {
                Token::String(s) => Ok(Value::String(s.value)),
                Token::Int(i) => Ok(Value::Number(i.value)),
                Token::Punctuation(Punctuation::LBracket, _) => {
                    Err(Self::Array(Box::new(ParseArray {
                        state: ParseArrayState::Value(ParseValue::None),
                        values: vec![],
                    })))
                }
                Token::Punctuation(Punctuation::LBrace, _) => {
                    Err(Self::Object(Box::new(ParseObject {
                        state: ParseObjectState::Field(ParseField::None),
                        fields: vec![],
                    })))
                }
                _ => exit(1),
            },
            ParseValue::Array(mut inner) => match inner.push(token) {
                Ok(value) => Ok(Value::Array(value)),
                Err(inner) => Err(Self::Array(Box::new(inner))),
            },
            ParseValue::Object(mut inner) => match inner.push(token) {
                Ok(value) => Ok(Value::Object(value)),
                Err(inner) => Err(Self::Object(Box::new(inner))),
            },
        }
    }
}

fn main() {
    let path = env::args().nth(1).unwrap();

    let text = fs::read_to_string(&path).unwrap();

    fn parse(text: &str) -> Value {
        let mut lexer = Lexer::new(StringSource {
            source: text.chars(),
            offset: 0,
        });

        let mut value_parser = ParseValue::None;
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

    let value = parse(&text);

    println!("{:?}", value);

    let mut duration = Duration::ZERO;
    for _ in 0..100 {
        let start = Instant::now();
        parse(&text);
        duration += start.elapsed();
    }
    println!("{:?}", duration);
}
