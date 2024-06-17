use std::vec::IntoIter;

pub struct Lexer<S> {
    messages: Vec<String>,
    source: S,
    current: Option<char>,
    tokens: IntoIter<Token>,
    offset: u32,
    column: u32,
    line: u32,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Keyword {
    Type,
    Data,
    Trait,
    Def,
    Fun,
    As,
    If,
    Else,
    While,
    Let,
    Var,
    Const,
    Is,
    True,
    False,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Punctuation {
    Plus,
    Asterisk,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Period,
    Semicolon,
    ThinArrow,
    Minus,
    Eq,
    DoubleEq,
    NotEq,
    Less,
    Bang,
    LessEq,
    Greater,
    GreaterEq,
    Pipe,
    Et,
}

#[derive(PartialEq, Eq, Clone, Debug, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Ident {
    pub span: Span,
    pub value: Option<String>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Int {
    pub span: Span,
    pub value: u32,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct StringLit {
    pub span: Span,
    pub value: String,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Token {
    Ident(Ident),
    Int(Int),
    String(StringLit),
    Keyword(Keyword, Span),
    Punctuation(Punctuation, Span),
    Eof(Span),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TokenKind {
    Ident,
    Int,
    String,
    Keyword(Keyword),
    Punctuation(Punctuation),
    Eof,
}

impl Token {
    pub fn span(&self) -> Span {
        match self {
            Self::Ident(v) => v.span,
            Self::Int(v) => v.span,
            Self::String(v) => v.span,
            Self::Keyword(_, s) => *s,
            Self::Punctuation(_, s) => *s,
            Self::Eof(s) => *s,
        }
    }
}

macro_rules! err {
    ($span:expr, $msg:expr) => {
        $msg
    };
}

pub trait Source: Sized {
    fn next(&mut self) -> Option<char>;
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        match self {
            Self::Ident(_) => TokenKind::Ident,
            Self::Int(_) => TokenKind::Int,
            Self::String(_) => TokenKind::String,
            Self::Keyword(kw, _) => TokenKind::Keyword(*kw),
            Self::Punctuation(p, _) => TokenKind::Punctuation(*p),
            Self::Eof(_) => TokenKind::Eof,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Position {
    line: u32,
    column: u32,
}

impl Position {
    pub fn extend_back(&self, len: u32) -> Span {
        Span {
            start: *self,
            end: Position {
                line: self.line,
                column: self.column + len,
            },
        }
    }
}

impl<S: Source> Lexer<S> {
    pub fn new(mut source: S) -> Self {
        let first = source.next();
        let mut lexer = Lexer {
            messages: vec![],
            tokens: vec![].into_iter(),
            source,
            current: first,
            offset: 0,
            column: 0,
            line: 0,
        };

        let mut tokens = vec![];

        loop {
            let token = lexer._next();
            if token.kind() == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        lexer.tokens = tokens.into_iter();
        lexer
    }

    pub fn next(&mut self) -> Token {
        self.tokens
            .next()
            .unwrap_or_else(|| Token::Eof(self.current_position().extend_back(1)))
    }

    fn _next(&mut self) -> Token {
        loop {
            match self.current {
                Some(c) if c.is_whitespace() => {
                    self.read_char();
                }
                _ => break,
            }
        }

        if self.current.is_none() {
            return Token::Eof(self.current_position().extend_back(1));
        }

        let token = self
            .read_ident_or_keyword()
            .or_else(|| self.read_string_literal())
            .or_else(|| self.read_punctuation())
            .or_else(|| self.read_numeric_literal())
            .unwrap_or_else(|| {
                self.messages.push(err!(
                    &self.current_position().extend_back(1),
                    "unexpected character".to_string()
                ));
                self.read_char();
                self.next()
            });

        token
    }

    pub fn current_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }

    fn read_ident_or_keyword(&mut self) -> Option<Token> {
        if !self.current?.is_alphabetic() {
            return None;
        }

        let mut ident = String::from(self.read_char()?);

        while let Some(char) = self.current {
            if !char.is_alphanumeric() {
                break;
            }
            ident.push(char);
            self.read_char();
        }

        let span = self.current_position().extend_back(ident.len() as u32);

        Some(match &ident[..] {
            "data" => Token::Keyword(Keyword::Data, span),
            "type" => Token::Keyword(Keyword::Type, span),
            "trait" => Token::Keyword(Keyword::Trait, span),
            "def" => Token::Keyword(Keyword::Def, span),
            "as" => Token::Keyword(Keyword::As, span),
            "fun" => Token::Keyword(Keyword::Fun, span),
            "if" => Token::Keyword(Keyword::If, span),
            "else" => Token::Keyword(Keyword::Else, span),
            "while" => Token::Keyword(Keyword::While, span),
            "let" => Token::Keyword(Keyword::Let, span),
            "var" => Token::Keyword(Keyword::Var, span),
            "const" => Token::Keyword(Keyword::Const, span),
            "is" => Token::Keyword(Keyword::Is, span),
            "true" => Token::Keyword(Keyword::True, span),
            "false" => Token::Keyword(Keyword::False, span),
            _ => Token::Ident(Ident {
                span,
                value: Some(ident),
            }),
        })
    }

    fn read_punctuation(&mut self) -> Option<Token> {
        if !self.current?.is_ascii_punctuation() {
            return None;
        }

        macro_rules! punct {
            ($name:ident, $len:expr) => {
                Some(Token::Punctuation(
                    Punctuation::$name,
                    self.current_position().extend_back($len),
                ))
            };
        }

        macro_rules! punct_impl {
            ([$char:literal => $name:ident , $($rest:tt)*] [$($out:tt)*] $none:expr, $depth:expr) => {
                punct_impl!([$($rest)*] [$($out)*
                    Some($char) => {
                        self.read_char();
                        punct!($name, $depth)
                    },
                ] $none, $depth)
            };

            ([$char:literal => $name:ident { $($nested:tt)* } , $($rest:tt)*] [$($out:tt)*] $none:expr, $depth:expr) => {
                punct_impl!([$($rest)*] [$($out)*
                    Some($char) => {
                        self.read_char();
                        punct_impl!([$($nested)*] [] Some(Token::Punctuation(
                            Punctuation::$name,
                            self.current_position().extend_back($depth),
                        )), $depth + 1)
                    },
                ] $none, $depth)
            };

            ([] [$($out:tt)*] $none:expr, $depth:expr) => {
                match self.current {
                    $($out)*
                    _ => $none,
                }
            };
        }

        macro_rules! puncts {
            ($($rest:tt)*) => {
                punct_impl!([$($rest)*] [] None, 1)
            };
        }

        let punct = puncts! {
            '+' => Plus,
            '*' => Asterisk,
            '(' => LParen,
            ')' => RParen,
            '{' => LBrace,
            '}' => RBrace,
            '[' => LBracket,
            ']' => RBracket,
            ':' => Colon,
            ';' => Semicolon,
            ',' => Comma,
            '.' => Period,
            '|' => Pipe,
            '&' => Et,
            '-' => Minus {
                '>' => ThinArrow,
            },
            '=' => Eq {
                '=' => DoubleEq,
            },
            '!' => Bang {
                '=' => NotEq,
            },
            '<' => Less {
                '=' => LessEq,
            },
            '>' => Greater {
                '=' => GreaterEq,
            },
        };
        if punct.is_none() {
            self.read_char();
            self.messages.push(err!(
                &self.current_position().extend_back(1),
                "unknown punctuation".to_string()
            ));
            return Some(self.next());
        }
        punct
    }

    fn read_numeric_literal(&mut self) -> Option<Token> {
        if !self.current?.is_digit(10) {
            return None;
        }

        let mut value = 0;
        let mut len = 0;
        while let Some(Some(digit)) = self.current.map(|c| c.to_digit(10)) {
            self.read_char();
            len += 1;
            value *= 10;
            value += digit;
        }

        Some(Token::Int(Int {
            span: self.current_position().extend_back(len),
            value,
        }))
    }

    fn read_string_literal(&mut self) -> Option<Token> {
        if self.current != Some('"') {
            return None;
        }

        self.read_char();
        let mut value = String::new();

        loop {
            match self.read_char() {
                Some('"') | None => {
                    return Some(Token::String(StringLit {
                        span: self.current_position().extend_back(value.len() as u32),
                        value,
                    }));
                }
                Some(character) => value.push(character),
            }
        }
    }

    fn read_char(&mut self) -> Option<char> {
        let old = self.current;

        if let Some(char) = old {
            self.offset += 1;
            if char == '\n' {
                self.column = 0;
                self.line += 1;
            } else {
                self.column += 1;
            }
        }

        self.current = self.source.next();

        old
    }
}
