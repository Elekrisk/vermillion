use std::rc::Rc;

use num_bigint::BigInt;


/// Represents a single position in a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    /// Byte index.
    index: usize,
    /// 1-indexed.c
    col: usize,
    /// 1-indexed.c
    row: usize,
}

impl Position {
    /// Get the position's row.
    pub fn row(&self) -> usize {
        self.row
    }

    /// Get the position's col.
    pub fn col(&self) -> usize {
        self.col
    }

    /// Get the position's index.
    pub fn index(&self) -> usize {
        self.index
    }

    pub(super) fn step(&mut self, c: char) {
        self.index += 1;
        match c {
            '\n' => {
                self.col = 1;
                self.row += 1;
            }
            _ => self.col += 1,
        }
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            index: 0,
            col: 1,
            row: 1,
        }
    }
}

/// Represents a span of source code.
/// Cannot span multiple files.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    file: Rc<String>,
    start: Position,
    end: Position,
}

impl Location {
    pub fn new(file: Rc<String>, start: Position, end: Position) -> Self {
        Self { file, start, end }
    }

    /// Creates a Location spanning from `start` to `end`, including both and the space between.
    pub fn span(start: Location, end: &Location) -> Self {
        assert_eq!(start.file, end.file);
        Self {
            end: end.end,
            ..start
        }
    }

    pub fn file(&self) -> &str {
        self.file.as_str()
    }

    pub fn start(&self) -> Position {
        self.start
    }

    pub fn end(&self) -> Position {
        self.end
    }
}

/// Represents a single token.
#[derive(Debug, Clone)]
pub struct Token {
    loc: Location,
    pub token_type: TokenType,
}

impl Token {
    pub(super) fn new(loc: Location, token_type: TokenType) -> Self {
        Self { loc, token_type }
    }

    /// Get a reference to the token's loc.
    pub fn loc(&self) -> &Location {
        &self.loc
    }

    /// Get a reference to the token's token type.
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }
}

/// The type of the token.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    Plus,
    Minus,
    Star,
    Slash,
    DoubleSlash,
    Eq,
    Neq,
    Gt,
    Gteq,
    Lt,
    Lteq,
    RightArrow,
    Period,
    Comma,
    Colon,
    Semicolon,
    LeftArrow,
    ExclamationMark,
    Percent,

    Underscore,

    And,
    Else,
    Fn,
    For,
    If,
    In,
    Let,
    Loop,
    Nil,
    Or,
    Var,
    While,
    Break,
    Continue,
    Return,
    Yield,

    Identifier(String),
    String(String),
    Integer(BigInt),
    Float(f64),
    Char(char),
    Bool(bool),

    Eol,
    Eof,

    Error(TokenError),
}

impl TokenType {
    pub fn is_error(&self) -> bool {
        matches!(self, TokenType::Error(_))
    }

    pub const fn name(&self) -> &'static str {
        match self {
            TokenType::LParen => "left parenthesis",
            TokenType::RParen => "right parenthesis",
            TokenType::LBracket => "left bracket",
            TokenType::RBracket => "right bracket",
            TokenType::LBrace => "left brace",
            TokenType::RBrace => "right brace",
            TokenType::Plus => "plus",
            TokenType::Minus => "minus",
            TokenType::Star => "star",
            TokenType::Slash => "slash",
            TokenType::DoubleSlash => "double slash",
            TokenType::Eq => "eq",
            TokenType::Neq => "neq",
            TokenType::Gt => "gt",
            TokenType::Gteq => "gteq",
            TokenType::Lt => "lt",
            TokenType::Lteq => "lteq",
            TokenType::RightArrow => "right arrow",
            TokenType::Period => "period",
            TokenType::Comma => "comma",
            TokenType::Colon => "colon",
            TokenType::Semicolon => "semicolon",
            TokenType::LeftArrow => "left arrow",
            TokenType::Underscore => "underscore",
            TokenType::And => "and",
            TokenType::Else => "else",
            TokenType::Fn => "fn",
            TokenType::For => "for",
            TokenType::If => "if",
            TokenType::In => "in",
            TokenType::Let => "let",
            TokenType::Loop => "loop",
            TokenType::Nil => "nil",
            TokenType::Or => "or",
            TokenType::Var => "var",
            TokenType::While => "while",
            TokenType::Break => "break",
            TokenType::Continue => "continue",
            TokenType::Identifier(_) => "identifier",
            TokenType::String(_) => "string",
            TokenType::Integer(_) => "integer",
            TokenType::Float(_) => "float",
            TokenType::Char(_) => "char",
            TokenType::Bool(_) => "bool",
            TokenType::Eol => "eol",
            TokenType::Eof => "eof",
            TokenType::Error(_) => "error",
            TokenType::Return => "return",
            TokenType::ExclamationMark => "exclamation mark",
            TokenType::Yield => "yield",
            TokenType::Percent => "percent",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenError {
    Unknown(char),
    HexWithoutDigits,
    BinWithoutDigits,
    UnclosedString,
    UnclosedChar,
    EmptyChar,
    EscapeError(EscapeError),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum EscapeError {
    EscapedHexDigit(usize),
    EscapedUniBrace,
    EscapedUniUnclosed,
    UnknownEscapeSequence
}
