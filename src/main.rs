use lexer::{Keyword, Punctuation, Source, Span, StringLit, TokenKind};

mod lexer;

struct StringSource {
    source: String,
    offset: usize,
}

impl Source for StringSource {
    fn next(&mut self) -> Option<char> {
        if self.offset >= self.source.len() {
            return None;
        }
        let c = self.source.chars().nth(self.offset)?;
        self.offset += 1;
        Some(c)
    }
}

struct Parser {
    lexer: lexer::Lexer<StringSource>,
    recovery: Vec<lexer::TokenKind>,
    current: lexer::Token,
}

enum Value {
    String(String),
    Array(Vec<Value>),
}

impl Parser {
    fn new(source: String) -> Self {
        let mut lexer = lexer::Lexer::new(StringSource { source, offset: 0 });

        Self {
            recovery: vec![],
            current: lexer.next(),
            lexer,
        }
    }

    fn next(&mut self) -> Result<lexer::Token, ()> {
        let token = self.current;
        self.current = self.lexer.next();

        Ok(token)
    }

    fn string(&mut self) -> Result<StringLit, ()> {}

    fn keyword(&mut self, kw: Keyword) -> Result<Span, ()> {}
    fn punct(&mut self, kw: Punctuation) -> Result<Span, ()> {}

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
                if self.recovery.contains(&self.current.kind()) {
                    self.lexer.current_position().extend_back(1)
                } else {
                    match self.current.kind() {
                        TokenKind::Eof => self.lexer.current_position().extend_back(1),
                        TokenKind::Keyword(_) => {}
                        TokenKind::Punctuation(_) => {}
                        _ => {}
                    }
                }
            }
        };

        Ok(values)
    }

    fn value(&mut self) -> Result<Value, ()> {}
}

fn main() {
    println!("Hello, world!");
}
