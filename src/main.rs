use std::{env, str::Chars};

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

#[derive(Debug)]
enum Value {
    String(String),
    Array(Vec<Value>),
    Object(Vec<(StringLit, Value)>),
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
                break;
            };
            values.push(value);

            let Ok(comma) = self.punct(Punctuation::Comma) else {
                break;
            };
        }

        let r_bracket = match self.punct(Punctuation::RBracket) {
            Ok(span) => span,
            Err(_) => {
                panic!("unexpected token");
                // if self.recovery.contains(&self.current.kind()) {
                //     self.lexer.current_position().extend_back(1)
                // } else {
                //     match self.current.kind() {
                //         TokenKind::Eof => self.lexer.current_position().extend_back(1),
                //         TokenKind::Keyword(_) => {}
                //         TokenKind::Punctuation(_) => {}
                //         _ => {}
                //     }
                // }
            }
        };

        Ok(values)
    }

    fn field(&mut self) -> Result<(StringLit, Value), ()> {
        let name = self.string()?;
        let colon = self.punct(Punctuation::Colon).unwrap();
        let value = self.value().unwrap();

        Ok((name, value))
    }

    fn object(&mut self) -> Result<Vec<(StringLit, Value)>, ()> {
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

fn main() {
    let text = env::args().nth(1).unwrap();

    println!("{:?}", text);

    let mut parser = Parser::new(&text);

    println!("{:?}", parser.value().unwrap());
}
