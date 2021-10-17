use std::convert::TryFrom;

use num_bigint::BigInt;

use crate::lexer::token::{Token, TokenType};

pub struct Program {
    pub functions: Vec<Function>,
}

pub struct Function {
    pub fn_tok: Token,
    pub ident: Identifier,
    pub params: Vec<Identifier>,
    pub lbrace: Token,
    pub stmnts: Vec<Statement>,
    pub rbrace: Token,
}

pub enum Statement {
    Let {
        let_tok: Token,
        pattern: Pattern,
        eq: Token,
        expr: Expression,
        semicolon: Option<Token>,
    },
    Var {
        var: Token,
        pattern: Pattern,
        assign: Option<(Token, Expression)>,
        semicolon: Option<Token>,
    },
    Assign {
        pattern: Pattern,
        arrow: Token,
        expr: Expression,
        semicolon: Option<Token>,
    },
    Expression {
        expr: Expression,
        semicolon: Option<Token>,
    },
}

pub enum Expression {
    Lambda {
        params: Vec<Pattern>,
        arrow: Token,
        body: Box<Expression>,
    },
    Tuple(SeparatedList<Expression>),
    Binary {
        left: Box<Expression>,
        right: Box<Expression>,
        op: BinaryOperator,
    },
    FunctionCall {
        func: Box<Expression>,
        args: Vec<Expression>,
    },
    MethodCall {
        object: Box<Expression>,
        method_name: Identifier,
        args: Vec<Expression>,
    },
    Block {
        lbrace: Token,
        statements: Vec<Statement>,
        rbrace: Token,
    },
    Identifier(Identifier),
    String {
        contents: String,
        token: Token,
    },
    Integer {
        value: BigInt,
        token: Token,
    },
    Float {
        value: f64,
        token: Token,
    },
    Char {
        value: char,
        token: Token,
    },
    Nil(Token),
}

pub enum Pattern {
    Tuple(SeparatedList<Pattern>),
    Identifier(Identifier),
    String {
        contents: String,
        token: Token,
    },
    Integer {
        value: BigInt,
        token: Token,
    },
    Float {
        value: f64,
        token: Token,
    },
    Char {
        value: char,
        token: Token,
    },
    Nil(Token),
    Discard(Token),
}

pub struct Identifier {
    pub name: String,
    pub token: Token,
}

impl TryFrom<Token> for Identifier {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        if let TokenType::Identifier(s) = &value.token_type() {
            Ok(Self {
                name: s.clone(),
                token: value,
            })
        } else {
            Err(())
        }
    }
}

pub struct BinaryOperator {
    pub token: Token,
    pub op: BinaryOperatorType,
}

pub enum BinaryOperatorType {
    And,
    Or,
    Eq,
    Neq,
    Gt,
    Gteq,
    Lt,
    Lteq,
    Plus,
    Minus,
    Multiply,
    Divide,
    IntegerDivide,
}

impl TryFrom<Token> for BinaryOperator {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        use BinaryOperatorType::*;
        let op = match value.token_type() {
            TokenType::Plus => Plus,
            TokenType::Minus => Minus,
            TokenType::Star => Multiply,
            TokenType::Slash => Divide,
            TokenType::DoubleSlash => IntegerDivide,
            TokenType::Eq => Eq,
            TokenType::Neq => Neq,
            TokenType::Gt => Gt,
            TokenType::Gteq => Gteq,
            TokenType::Lt => Lt,
            TokenType::Lteq => Lteq,
            TokenType::And => And,
            TokenType::Or => Or,
            _ => return Err(())
        };
        Ok(Self { token: value, op })
    }
}

pub struct SeparatedList<T> {
    pub first: Box<T>,
    pub rest: Vec<(Token, T)>,
}

impl<T> SeparatedList<T> {
}
