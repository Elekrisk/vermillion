use std::{
    collections::HashMap,
    iter::{FromIterator, Peekable},
    rc::Rc,
};

use num_bigint::BigInt;
use num_traits::Num;

use crate::{
    generator_adapter::GeneratorAdapter,
    lexer::token::{EscapeError, TokenError},
};

use self::token::{Location, Position, Token, TokenType};

pub mod token;

pub struct Lexer<I: Iterator<Item = char>> {
    code: Peekable<I>,
    start_pos: Position,
    next_pos: Position,
    file_name: Rc<String>,
    is_first_token_on_line: bool,
    buffer: String,
    next_char: Option<char>,

    single_char_tokens: HashMap<char, TokenType>,
    ambichar_tokens: HashMap<char, (TokenType, char, TokenType)>,
    keywords: HashMap<&'static str, TokenType>,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(file_name: Rc<String>, code: impl IntoIterator<IntoIter = I>) -> Self {
        Self {
            code: code.into_iter().peekable(),
            start_pos: Position::default(),
            next_pos: Position::default(),
            file_name,
            is_first_token_on_line: true,
            buffer: String::new(),
            next_char: None,
            single_char_tokens: HashMap::from_iter([
                ('(', TokenType::LParen),
                (')', TokenType::RParen),
                ('[', TokenType::LBracket),
                (']', TokenType::RBracket),
                ('{', TokenType::LBrace),
                ('}', TokenType::RBrace),
                ('+', TokenType::Plus),
                ('*', TokenType::Star),
                ('=', TokenType::Eq),
                ('.', TokenType::Period),
                (',', TokenType::Comma),
                (':', TokenType::Colon),
                (';', TokenType::Semicolon),
                ('!', TokenType::ExclamationMark),
                ('%', TokenType::Percent),
            ]),
            ambichar_tokens: HashMap::from_iter([
                ('-', (TokenType::Minus, '>', TokenType::RightArrow)),
                ('/', (TokenType::Slash, '/', TokenType::DoubleSlash)),
                (
                    '!',
                    (
                        TokenType::Error(TokenError::Unknown('!')),
                        '=',
                        TokenType::Neq,
                    ),
                ),
                ('>', (TokenType::Gt, '=', TokenType::Gteq)),
            ]),
            keywords: HashMap::from_iter([
                ("_", TokenType::Underscore),
                ("and", TokenType::And),
                ("else", TokenType::Else),
                ("false", TokenType::Bool(false)),
                ("fn", TokenType::Fn),
                ("for", TokenType::For),
                ("if", TokenType::If),
                ("in", TokenType::In),
                ("let", TokenType::Let),
                ("loop", TokenType::Loop),
                ("nil", TokenType::Nil),
                ("or", TokenType::Or),
                ("return", TokenType::Return),
                ("true", TokenType::Bool(true)),
                ("var", TokenType::Var),
                ("while", TokenType::While),
                ("break", TokenType::Break),
                ("continue", TokenType::Continue),
            ]),
        }
    }

    pub fn step(&mut self) -> Token {
        self.start_pos = self.next_pos;
        self.buffer.clear();

        let c = match self.next() {
            Some(c) => c,
            None => return self.eof(),
        };

        fn handle_escaped<I: Iterator<Item = char>>(
            s: &mut Lexer<I>,
            is_string: bool,
        ) -> Result<char, TokenError> {
            let unclosed = if is_string {
                TokenError::UnclosedString
            } else {
                TokenError::UnclosedChar
            };
            fn err(ee: EscapeError) -> Result<char, TokenError> {
                Err(TokenError::EscapeError(ee))
            }
            Ok(match s.next() {
                Some(c) => match c {
                    '\\' => '\\',
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\'' => '\'',
                    '"' => '"',
                    '0' => '\0',
                    'x' => {
                        let d1 = match s.next() {
                            Some(d) if d.is_ascii_hexdigit() => d.to_digit(16).unwrap(),
                            Some(_) => return err(EscapeError::EscapedHexDigit(0)),
                            None => return Err(unclosed),
                        };
                        let d2 = match s.next() {
                            Some(d) if d.is_ascii_hexdigit() => d.to_digit(16).unwrap(),
                            Some(_) => return err(EscapeError::EscapedHexDigit(1)),
                            None => return Err(unclosed),
                        };
                        char::from_u32(d1 * 16 + d2).unwrap()
                    }
                    'u' => {
                        let mut c = 0;
                        if let Some('{') = s.next() {
                            let mut has_rbrace = false;
                            for _ in 0..6 {
                                let d = match s.next() {
                                    Some('}') => {
                                        has_rbrace = true;
                                        break;
                                    }
                                    Some(d) if d.is_ascii_hexdigit() => d.to_digit(16).unwrap(),
                                    Some(_) => break,
                                    None => return Err(unclosed),
                                };
                                c = c * 16 + d;
                            }
                            if !has_rbrace {
                                match s.next() {
                                    Some('}') => (),
                                    Some(_) => return err(EscapeError::EscapedUniUnclosed),
                                    None => return Err(unclosed),
                                }
                            }
                        } else {
                            return err(EscapeError::EscapedUniBrace);
                        }
                        char::from_u32(c).unwrap()
                    }
                    _ => return err(EscapeError::UnknownEscapeSequence),
                },
                None => {
                    return Err(if is_string {
                        TokenError::UnclosedString
                    } else {
                        TokenError::UnclosedChar
                    })
                }
            })
        }

        match c {
            '#' => loop {
                match self.peek() {
                    None | Some('\n') => break self.step(),
                    _ => {
                        self.next();
                    }
                }
            }
            '<' => {
                match self.peek() {
                    Some('-') => {
                        self.next();
                        self.create_token(TokenType::LeftArrow)
                    },
                    Some('=') => {
                        self.next();
                        self.create_token(TokenType::Lteq)
                    }
                    _ => self.create_token(TokenType::Lt)
                }
            }
            '-' if self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) => {
                self.buffer.push('-');

                while self.peek().map(|c| c.is_digit(10)).unwrap_or_default() {
                    let c = self.next().unwrap();
                    self.buffer.push(c);
                }

                // This might be the dot in a float or a period for method calling
                if self.peek() == Some('.') {
                    // Remember current position if the dot isn't part of a float
                    let pos = self.next_pos;
                    self.next();
                    if self.peek().map(|c| c.is_ascii_digit()).unwrap_or_default() {
                        // This is now a float
                        self.buffer.push('.');
                        let c = self.next().unwrap();
                        self.buffer.push(c);

                        while self.peek().map(|c| c.is_digit(10)).unwrap_or_default() {
                            let c = self.next().unwrap();
                            self.buffer.push(c);
                        }

                        // Same thing with floats and e-syntax
                        if self.peek() == Some('e') {
                            let pos = self.next_pos;
                            self.next();
                            if self.peek().map(|c| c.is_ascii_digit() || c == '+' || c == '-').unwrap_or_default() {
                                self.buffer.push('e');
                                let c = self.next().unwrap();
                                self.buffer.push(c);

                                while self.peek().map(|c| c.is_digit(10)).unwrap_or_default() {
                                    let c = self.next().unwrap();
                                    self.buffer.push(c);
                                }

                                self.create_token(TokenType::Float(self.buffer.parse().unwrap()))
                            } else {
                                self.next_char = Some('e');
                                Token::new(Location::new(self.file_name.clone(), self.start_pos, pos), TokenType::Float(self.buffer.parse().unwrap()))
                            }
                        } else {
                            self.create_token(TokenType::Float(self.buffer.parse().unwrap()))
                        }
                    } else {
                        // This is an int
                        self.next_char = Some('.');
                        Token::new(Location::new(self.file_name.clone(), self.start_pos, pos), TokenType::Integer(self.buffer.parse().unwrap()))
                    }
                } else {
                    self.create_token(TokenType::Integer(self.buffer.parse().unwrap()))
                }
            }
            '\n' => self.create_token(TokenType::Eol),
            o if o.is_whitespace() => {
                while self.peek().map(|c| c.is_whitespace()).unwrap_or_default() {
                    self.next();
                }
                self.step()
            }
            o if let Some(token_type) = self.single_char_tokens.get(&o) => {
                let token_type = token_type.clone();
                self.create_token(token_type)
            }
            o if let Some((t1, ch, t2)) = self.ambichar_tokens.get(&o) => {
                let (t1, t2) = (t1.clone(), t2.clone());
                let ch = *ch;
                if self.peek().map(|c| c == ch).unwrap_or_default() {
                    self.next();
                    self.create_token(t2)
                } else {
                    self.create_token(t1)
                }
            }
            '"' => {
                loop {
                    let pos = self.next_pos;
                    match self.next() {
                        Some('\\') => {
                            match handle_escaped(self, true) {
                                Ok(c) => self.buffer.push(c),
                                Err(e) => return Token::new(Location::new(self.file_name.clone(), pos, self.next_pos), TokenType::Error(e)),
                            }
                        },
                        Some('"') => return self.create_token(TokenType::String(self.buffer.clone())),
                        Some(o) => self.buffer.push(o),
                        None => return self.create_token(TokenType::Error(TokenError::UnclosedString)),
                    };
                }
            }
            '\'' => {
                let pos = self.next_pos;
                let c = match self.next() {
                    Some('\\') => {
                        match handle_escaped(self, false) {
                            Ok(c) => c,
                            Err(e) => return Token::new(Location::new(self.file_name.clone(), pos, self.next_pos), TokenType::Error(e)),
                        }
                    },
                    Some('\'') => return self.create_token(TokenType::Error(TokenError::EmptyChar)),
                    Some(o) => o,
                    None => return self.create_token(TokenType::Error(TokenError::UnclosedChar)),
                };
                if let Some('\'') = self.next() {
                    self.create_token(TokenType::Char(c))
                } else {
                    self.create_token(TokenType::Error(TokenError::UnclosedChar))
                }
            }
            '0' if self.peek() == Some('x') => {
                self.next();

                while self.peek().map(|c| c.is_digit(16)).unwrap_or_default() {
                    let c = self.next().unwrap();
                    self.buffer.push(c);
                }

                if self.buffer.is_empty() {
                    self.create_token(TokenType::Error(TokenError::HexWithoutDigits))
                } else {
                    self.create_token(TokenType::Integer(BigInt::from_str_radix(&self.buffer, 16).unwrap()))
                }
            }
            '0' if self.peek() == Some('b') => {
                self.next();

                while self.peek().map(|c| c.is_digit(2)).unwrap_or_default() {
                    let c = self.next().unwrap();
                    self.buffer.push(c);
                }

                if self.buffer.is_empty() {
                    self.create_token(TokenType::Error(TokenError::BinWithoutDigits))
                } else {
                    self.create_token(TokenType::Integer(BigInt::from_str_radix(&self.buffer, 2).unwrap()))
                }
            }
            o if o.is_ascii_digit() => {
                self.buffer.push(o);

                while self.peek().map(|c| c.is_digit(10)).unwrap_or_default() {
                    let c = self.next().unwrap();
                    self.buffer.push(c);
                }

                // This might be the dot in a float or a period for method calling
                if self.peek() == Some('.') {
                    // Remember current position if the dot isn't part of a float
                    let pos = self.next_pos;
                    self.next();
                    if self.peek().map(|c| c.is_ascii_digit()).unwrap_or_default() {
                        // This is now a float
                        self.buffer.push('.');
                        let c = self.next().unwrap();
                        self.buffer.push(c);

                        while self.peek().map(|c| c.is_digit(10)).unwrap_or_default() {
                            let c = self.next().unwrap();
                            self.buffer.push(c);
                        }

                        // Same thing with floats and e-syntax
                        if self.peek() == Some('e') {
                            let pos = self.next_pos;
                            self.next();
                            if self.peek().map(|c| c.is_ascii_digit() || c == '+' || c == '-').unwrap_or_default() {
                                self.buffer.push('e');
                                let c = self.next().unwrap();
                                self.buffer.push(c);

                                while self.peek().map(|c| c.is_digit(10)).unwrap_or_default() {
                                    let c = self.next().unwrap();
                                    self.buffer.push(c);
                                }

                                self.create_token(TokenType::Float(self.buffer.parse().unwrap()))
                            } else {
                                self.next_char = Some('e');
                                Token::new(Location::new(self.file_name.clone(), self.start_pos, pos), TokenType::Float(self.buffer.parse().unwrap()))
                            }
                        } else {
                            self.create_token(TokenType::Float(self.buffer.parse().unwrap()))
                        }
                    } else {
                        // This is an int
                        self.next_char = Some('.');
                        Token::new(Location::new(self.file_name.clone(), self.start_pos, pos), TokenType::Integer(self.buffer.parse().unwrap()))
                    }
                } else {
                    self.create_token(TokenType::Integer(self.buffer.parse().unwrap()))
                }
            }
            o if o.is_alphabetic() || o == '_' => {
                self.buffer.push(o);
                while self.peek().map(|c| c.is_alphanumeric() || c == '_').unwrap_or_default() {
                    let c = self.next().unwrap();
                    self.buffer.push(c);
                }

                if let Some(tt) = self.keywords.get(self.buffer.as_str()) {
                    let tt = tt.clone();
                    self.create_token(tt)
                } else {
                    self.create_token(TokenType::Identifier(self.buffer.clone()))
                }
            }
            o => self.create_token(TokenType::Error(TokenError::Unknown(o)))
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.next_char.or_else(|| self.code.peek().copied())
    }

    fn next(&mut self) -> Option<char> {
        self.next_char.take().or_else(|| match self.code.next() {
            Some(c) => {
                if c == '\n' {
                    self.is_first_token_on_line = true;
                }
                self.next_pos.step(c);
                Some(c)
            }
            None => None,
        })
    }

    fn create_token(&mut self, token_type: TokenType) -> Token {
        self.is_first_token_on_line = false;
        Token::new(
            Location::new(self.file_name.clone(), self.start_pos, self.next_pos),
            token_type,
        )
    }

    fn eof(&mut self) -> Token {
        self.create_token(TokenType::Eof)
    }

    pub fn tokens(&mut self) -> impl Iterator<Item = Token> + '_ {
        GeneratorAdapter::new(|| loop {
            let t = self.step();
            if let TokenType::Eof = t.token_type() {
                yield t;
                break;
            } else {
                yield t;
            }
        })
    }
}
