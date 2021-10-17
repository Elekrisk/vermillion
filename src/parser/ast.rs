use std::convert::TryFrom;

use num_bigint::BigInt;

use crate::lexer::token::{Location, Token, TokenType};

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub fn_tok: Token,
    pub ident: Identifier,
    pub params: Vec<Identifier>,
    pub lbrace: Token,
    pub stmnts: Vec<Statement>,
    pub rbrace: Token,
}

#[derive(Debug)]
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
    Break {
        break_tok: Token,
        expr: Option<Expression>,
        semicolon: Option<Token>,
    },
    Continue {
        continue_tok: Token,
        semicolon: Option<Token>,
    },
    Return {
        return_tok: Token,
        expr: Option<Expression>,
        semicolon: Option<Token>,
    },
    Yield {
        yield_tok: Token,
        expr: Option<Expression>,
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

#[derive(Debug)]
pub enum Expression {
    If {
        cond: Box<Expression>,
        body: Box<Expression>,
        elifs: Vec<(Expression, Expression)>,
        else_body: Option<Box<Expression>>,
    },
    Loop(Box<Expression>),
    For {
        pattern: Pattern,
        expr: Box<Expression>,
        body: Box<Expression>,
    },
    While {
        cond: Box<Expression>,
        body: Box<Expression>,
    },
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
        excl_mark: Token,
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
    Bool {
        value: bool,
        token: Token,
    },
    Nil(Token),
}

impl Expression {
    pub fn loc(&self) -> Location {
        match self {
            Expression::If {
                cond,
                body,
                elifs,
                else_body,
            } => todo!(),
            Expression::Loop(_) => todo!(),
            Expression::For {
                pattern,
                expr,
                body,
            } => todo!(),
            Expression::While { cond, body } => todo!(),
            Expression::Lambda {
                params,
                arrow,
                body,
            } => todo!(),
            Expression::Tuple(v) => todo!(),
            Expression::Binary { left, right, op } => Location::span(left.loc(), &right.loc()),
            Expression::FunctionCall {
                func,
                excl_mark,
                args,
            } => Location::span(
                func.loc(),
                args.last()
                    .map(|e| e.loc())
                    .as_ref()
                    .unwrap_or_else(|| excl_mark.loc()),
            ),
            Expression::MethodCall {
                object,
                method_name,
                args,
            } => Location::span(
                object.loc(),
                args.last()
                    .map(|e| e.loc())
                    .as_ref()
                    .unwrap_or_else(|| method_name.token.loc()),
            ),
            Expression::Block {
                lbrace,
                statements,
                rbrace,
            } => Location::span(lbrace.loc().clone(), rbrace.loc()),
            Expression::Identifier(i) => i.token.loc().clone(),
            Expression::String { contents, token } => token.loc().clone(),
            Expression::Integer { value, token } => token.loc().clone(),
            Expression::Float { value, token } => token.loc().clone(),
            Expression::Char { value, token } => token.loc().clone(),
            Expression::Bool { value, token } => token.loc().clone(),
            Expression::Nil(tok) => tok.loc().clone(),
        }
    }
}

impl TryFrom<Token> for Expression {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        Ok(match token.token_type() {
            TokenType::Nil => Expression::Nil(token),
            TokenType::Identifier(_v) => Expression::Identifier(token.try_into().unwrap()),
            TokenType::String(v) => Expression::String {
                contents: v.clone(),
                token,
            },
            TokenType::Integer(v) => Expression::Integer {
                value: v.clone(),
                token,
            },
            TokenType::Float(v) => Expression::Float { value: *v, token },
            TokenType::Char(v) => Expression::Char { value: *v, token },
            TokenType::Bool(v) => Expression::Bool { value: *v, token },
            _ => return Err(()),
        })
    }
}

#[derive(Debug)]
pub enum Pattern {
    Tuple(SeparatedList<Pattern>),
    Identifier(Identifier),
    String { contents: String, token: Token },
    Integer { value: BigInt, token: Token },
    Float { value: f64, token: Token },
    Char { value: char, token: Token },
    Bool { value: bool, token: Token },
    Nil(Token),
    Discard(Token),
}

impl TryFrom<Token> for Pattern {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        Ok(match token.token_type() {
            TokenType::Identifier(_v) => Pattern::Identifier(token.try_into().unwrap()),
            TokenType::String(v) => Pattern::String {
                contents: v.clone(),
                token,
            },
            TokenType::Integer(v) => Pattern::Integer {
                value: v.clone(),
                token,
            },
            TokenType::Float(v) => Pattern::Float { value: *v, token },
            TokenType::Char(v) => Pattern::Char { value: *v, token },
            TokenType::Bool(v) => Pattern::Bool { value: *v, token },
            TokenType::Nil => Pattern::Nil(token),
            TokenType::Underscore => Pattern::Discard(token),
            _ => return Err(()),
        })
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct BinaryOperator {
    pub token: Token,
    pub op: BinaryOperatorType,
}

#[derive(Debug)]
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
    Remainder
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
            TokenType::Percent => Remainder,
            TokenType::Eq => Eq,
            TokenType::Neq => Neq,
            TokenType::Gt => Gt,
            TokenType::Gteq => Gteq,
            TokenType::Lt => Lt,
            TokenType::Lteq => Lteq,
            TokenType::And => And,
            TokenType::Or => Or,
            _ => return Err(()),
        };
        Ok(Self { token: value, op })
    }
}

#[derive(Debug)]
pub struct SeparatedList<T> {
    pub first: Box<T>,
    pub rest: Vec<(Token, T)>,
}

impl<T> SeparatedList<T> {
    pub fn values(&self) -> impl Iterator<Item = &T> {
        std::iter::once(self.first.as_ref()).chain(self.rest.iter().map(|(_, v)| v))
    }
}
