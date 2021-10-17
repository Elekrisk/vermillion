use std::{convert::TryInto, iter::{self, FromIterator}, ops::{ControlFlow, FromResidual, Try}};

use num_bigint::BigInt;
use num_traits::Zero;

use self::ast::{Expression, Function, Identifier, Pattern, Program, SeparatedList, Statement};
use crate::generator_adapter::IntoIterator2;
use crate::lexer::token::{Token, TokenType};

mod ast;

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    index_stack: Vec<usize>,
}

pub type ParseResult<T> = ParseResultFull<T, ParseError, ParseError>;

pub enum ParseResultFull<T, E, F> {
    Ok(T),
    Err(E),
    Fatal(F),
}

impl<T, E, F> ParseResultFull<T, E, F> {
    fn map<U, Func: FnOnce(T) -> U>(self, f: Func) -> ParseResultFull<U, E, F> {
        match self {
            ParseResultFull::Ok(v) => ParseResultFull::Ok(f(v)),
            ParseResultFull::Err(e) => ParseResultFull::Err(e),
            ParseResultFull::Fatal(f) => ParseResultFull::Fatal(f),
        }
    }

    fn transmute<U>(self) -> ParseResultFull<U, E, F> {
        match self {
            ParseResultFull::Ok(_) => unreachable!(),
            ParseResultFull::Err(e) => ParseResultFull::Err(e),
            ParseResultFull::Fatal(f) => ParseResultFull::Fatal(f),
        }
    }
}

impl<T, E> ParseResultFull<T, E, E> {
    fn make_fatal(self) -> ParseResultFull<T, E, E> {
        match self {
            ParseResultFull::Err(e) => ParseResultFull::Fatal(e),
            ParseResultFull::Ok(v) => ParseResultFull::Ok(v),
            ParseResultFull::Fatal(f) => ParseResultFull::Fatal(f),
        }
    }

    fn is_ok(&self) -> bool {
        matches!(self, ParseResultFull::Ok(_))
    }
}

impl<T, E, F> FromIterator<ParseResultFull<T, E, F>> for ParseResultFull<Vec<T>, E, F> {
    fn from_iter<I: IntoIterator<Item = ParseResultFull<T, E, F>>>(iter: I) -> Self {
        let mut ret = vec![];
        for item in iter {
            match item {
                ParseResultFull::Ok(v) => ret.push(v),
                ParseResultFull::Err(_) => break,
                ParseResultFull::Fatal(f) => return Self::Fatal(f),
            }
        }
        Self::Ok(ret)
    }
}

impl<T, E, F> Try for ParseResultFull<T, E, F> {
    type Output = T;

    type Residual = ParseResultFull<!, E, F>;

    fn from_output(output: Self::Output) -> Self {
        Self::Ok(output)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            ParseResultFull::Ok(v) => ControlFlow::Continue(v),
            other => ControlFlow::Break(other.transmute()),
        }
    }
}

impl<T, E, F> FromResidual for ParseResultFull<T, E, F> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        match residual {
            ParseResultFull::Err(e) => ParseResultFull::Err(e),
            ParseResultFull::Fatal(e) => ParseResultFull::Fatal(e),
            ParseResultFull::Ok(_) => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    TokenExpected(TokenType),
    StatementExpected,
    PatternExpected,
    ExpressionExpected,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            index_stack: vec![],
        }
    }

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut fns = vec![];
        while !self.eof() {
            self.skip_eols();
            match self.parse_function() {
                ParseResult::Ok(f) => fns.push(f),
                ParseResult::Err(_) => break,
                ParseResult::Fatal(f) => return ParseResult::Fatal(f),
            }
        }
        self.skip_eols();
        self.match_token(TokenType::Eof).make_fatal()?;
        ParseResult::Ok(Program { functions: fns })
    }

    pub fn parse_function(&mut self) -> ParseResult<Function> {
        let fn_tok = self.match_token(TokenType::Fn)?;
        let ident = self.match_identifier().make_fatal()?.try_into().unwrap();

        let params = iter::repeat_with(|| {
            self.skip_eols();
            self.match_identifier().map(|v| v.try_into().unwrap())
        })
        .collect::<ParseResult<Vec<Identifier>>>()?;

        let lbrace = self.match_token(TokenType::LBrace).make_fatal()?;

        let stmnts = iter::repeat_with(|| {
            self.skip_eols();
            self.parse_statement()
        })
        .collect::<ParseResult<_>>()?;
        self.skip_eols();

        let rbrace = self.match_token(TokenType::RBrace).make_fatal()?;

        ParseResult::Ok(Function {
            fn_tok,
            ident,
            params,
            lbrace,
            stmnts,
            rbrace,
        })
    }

    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        self.push();

        let f1 = |s: &mut Self| match s.match_token(TokenType::Let) {
            ParseResult::Ok(let_tok) => {
                let pattern = s.parse_pattern().make_fatal()?;
                let eq = s.match_token(TokenType::Eq).make_fatal()?;
                let expr = s.parse_expression().make_fatal()?;
                let semicolon = s.match_eol().make_fatal()?;

                ParseResult::Ok(Statement::Let {
                    let_tok,
                    pattern,
                    eq,
                    expr,
                    semicolon,
                })
            }
            other => other.transmute::<Statement>(),
        };

        let f2 = |s: &mut Self| match s.match_token(TokenType::Var) {
            ParseResult::Ok(var) => {
                let pattern = s.parse_pattern().make_fatal()?;
                let assign = {
                    match s.match_token(TokenType::LeftArrow) {
                        ParseResultFull::Ok(tok) => {
                            let expr = s.parse_expression().make_fatal()?;
                            Some((tok, expr))
                        }
                        ParseResultFull::Err(_) => None,
                        ParseResultFull::Fatal(f) => return ParseResult::Fatal(f),
                    }
                };
                let semicolon = s.match_eol().make_fatal()?;

                ParseResultFull::Ok(Statement::Var {
                    var,
                    pattern,
                    assign,
                    semicolon,
                })
            }
            other => other.transmute(),
        };

        let f3 = |s: &mut Self| {
            s.push();
            match s.parse_pattern() {
                ParseResult::Ok(pattern) => match s.match_token(TokenType::LeftArrow) {
                    ParseResultFull::Ok(arrow) => {
                        let expr = s.parse_expression().make_fatal()?;
                        let semicolon = s.match_eol().make_fatal()?;

                        s.pop();
                        ParseResultFull::Ok(Statement::Assign {
                            pattern,
                            arrow,
                            expr,
                            semicolon,
                        })
                    }
                    ParseResultFull::Err(e) => {
                        s.backtrack();
                        ParseResultFull::Err(e)
                    }
                    ParseResultFull::Fatal(f) => ParseResult::Fatal(f),
                },
                other => other.transmute(),
            }
        };

        let f4 = |s: &mut Self| match s.parse_expression() {
            ParseResult::Ok(expr) => {
                let semicolon = s.match_eol().make_fatal()?;

                ParseResult::Ok(Statement::Expression { expr, semicolon })
            }
            other => other.transmute(),
        };

        let b1: Box<dyn FnOnce(&mut Self) -> ParseResult<Statement>> = Box::new(f1);
        let b2: Box<dyn FnOnce(&mut Self) -> ParseResult<Statement>> = Box::new(f2);
        let b3: Box<dyn FnOnce(&mut Self) -> ParseResult<Statement>> = Box::new(f3);
        let b4: Box<dyn FnOnce(&mut Self) -> ParseResult<Statement>> = Box::new(f4);

        let ret = self.ordered_choice([b1, b2, b3, b4], ParseError::StatementExpected);
        self.pop();
        ret
    }

    pub fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        self.parse_pattern_0()
    }

    fn parse_pattern_0(&mut self) -> ParseResult<Pattern> {
        self.parse_token_separated_list(Self::parse_pattern_1, TokenType::Comma, Pattern::Tuple)
    }

    fn parse_pattern_1(&mut self) -> ParseResult<Pattern> {
        self.parse_pattern_atom()
    }

    fn parse_pattern_atom(&mut self) -> ParseResult<Pattern> {
        let f1 = |s: &mut Self| {
            ParseResult::Ok(Pattern::Identifier(
                s.match_identifier()?.try_into().unwrap(),
            ))
        };
        let f2 = |s: &mut Self| {
            let token = s.match_string()?;
            let contents = match token.token_type() {
                TokenType::String(s) => s.clone(),
                _ => unreachable!(),
            };

            ParseResult::Ok(Pattern::String { contents, token })
        };
        let f3 = |s: &mut Self| {
            let token = s.match_integer()?;
            let value = match token.token_type() {
                TokenType::Integer(v) => v.clone(),
                _ => unreachable!(),
            };

            ParseResult::Ok(Pattern::Integer { value, token })
        };
        let f4 = |s: &mut Self| {
            let token = s.match_float()?;
            let value = match token.token_type() {
                TokenType::Float(v) => *v,
                _ => unreachable!(),
            };

            ParseResult::Ok(Pattern::Float { value, token })
        };
        let f5 = |s: &mut Self| {
            let token = s.match_char()?;
            let value = match token.token_type() {
                TokenType::Char(v) => *v,
                _ => unreachable!(),
            };

            ParseResult::Ok(Pattern::Char { value, token })
        };
        let f6 = |s: &mut Self| ParseResult::Ok(Pattern::Nil(s.match_token(TokenType::Nil)?));
        let f7 =
            |s: &mut Self| ParseResult::Ok(Pattern::Discard(s.match_token(TokenType::Underscore)?));

        let b1: Box<dyn FnOnce(&mut Self) -> ParseResult<Pattern>> = Box::new(f1);
        let b2: Box<dyn FnOnce(&mut Self) -> ParseResult<Pattern>> = Box::new(f2);
        let b3: Box<dyn FnOnce(&mut Self) -> ParseResult<Pattern>> = Box::new(f3);
        let b4: Box<dyn FnOnce(&mut Self) -> ParseResult<Pattern>> = Box::new(f4);
        let b5: Box<dyn FnOnce(&mut Self) -> ParseResult<Pattern>> = Box::new(f5);
        let b6: Box<dyn FnOnce(&mut Self) -> ParseResult<Pattern>> = Box::new(f6);
        let b7: Box<dyn FnOnce(&mut Self) -> ParseResult<Pattern>> = Box::new(f7);

        let funcs = [b1, b2, b3, b4, b5, b6, b7];

        self.ordered_choice(funcs, ParseError::PatternExpected)
    }

    pub fn parse_expression(&mut self) -> ParseResult<Expression> {
        self.parse_expression_0()
    }

    pub fn parse_expression_0(&mut self) -> ParseResult<Expression> {
        self.push();

        match iter::repeat_with(|| self.parse_pattern()).collect::<ParseResult<_>>() {
            ParseResultFull::Ok(params) => match self.match_token(TokenType::RightArrow) {
                ParseResultFull::Ok(arrow) => {
                    let body = Box::new(self.parse_expression().make_fatal()?);

                    self.pop();
                    return ParseResult::Ok(Expression::Lambda {
                        params,
                        arrow,
                        body,
                    });
                }
                ParseResultFull::Err(_) => {
                    self.backtrack();
                }
                other => return other.transmute(),
            },
            ParseResultFull::Err(_) => {
                self.backtrack();
            }
            other => return other.transmute(),
        }

        self.pop();
        self.parse_expression_1()
    }
    pub fn parse_expression_1(&mut self) -> ParseResult<Expression> {
        self.parse_token_separated_list(
            Self::parse_expression_2,
            TokenType::Comma,
            Expression::Tuple,
        )
    }
    pub fn parse_expression_2(&mut self) -> ParseResult<Expression> {
        self.parse_token_separated_left_tree(
            Self::parse_expression_3,
            vec![TokenType::Or],
            |l, r, o| Expression::Binary {
                left: Box::new(l),
                right: Box::new(r),
                op: o.try_into().unwrap(),
            },
        )
    }
    pub fn parse_expression_3(&mut self) -> ParseResult<Expression> {
        self.parse_token_separated_left_tree(
            Self::parse_expression_4,
            vec![TokenType::And],
            |l, r, o| Expression::Binary {
                left: Box::new(l),
                right: Box::new(r),
                op: o.try_into().unwrap(),
            },
        )
    }
    pub fn parse_expression_4(&mut self) -> ParseResult<Expression> {
        let mut left = self.parse_expression_5()?;
        let token = self.peek();
        if vec![
            TokenType::Eq,
            TokenType::Neq,
            TokenType::Gt,
            TokenType::Gteq,
            TokenType::Lt,
            TokenType::Lteq,
        ]
        .contains(token.token_type())
        {
            let token = self.next().clone();
            let right = self.parse_expression_5().make_fatal()?;
            left = Expression::Binary {
                left: Box::new(left),
                right: Box::new(right),
                op: token.try_into().unwrap(),
            };
        }
        ParseResult::Ok(left)
    }
    pub fn parse_expression_5(&mut self) -> ParseResult<Expression> {
        self.parse_token_separated_left_tree(
            Self::parse_expression_6,
            vec![TokenType::Plus, TokenType::Minus],
            |l, r, o| Expression::Binary {
                left: Box::new(l),
                right: Box::new(r),
                op: o.try_into().unwrap(),
            },
        )
    }
    pub fn parse_expression_6(&mut self) -> ParseResult<Expression> {
        self.parse_token_separated_left_tree(
            Self::parse_expression_7,
            vec![TokenType::Star, TokenType::Slash, TokenType::DoubleSlash],
            |l, r, o| Expression::Binary {
                left: Box::new(l),
                right: Box::new(r),
                op: o.try_into().unwrap(),
            },
        )
    }
    pub fn parse_expression_7(&mut self) -> ParseResult<Expression> {
        let mut exprs =
            iter::repeat_with(|| self.parse_expression_8()).collect::<ParseResult<_>>()?;
        match exprs.len() {
            0 => ParseResult::Err(ParseError::ExpressionExpected),
            1 => ParseResult::Ok(exprs.pop().unwrap()),
            _ => ParseResult::Ok(Expression::FunctionCall {
                func: Box::new(exprs.remove(0)),
                args: exprs,
            }),
        }
    }
    pub fn parse_expression_8(&mut self) -> ParseResult<Expression> {
        let expr = self.parse_expression_9()?;
        match self.match_token(TokenType::Period) {
            ParseResultFull::Ok(_tok) => {
                let name = self.match_identifier().make_fatal()?;
                let args =
                    iter::repeat_with(|| self.parse_expression_9()).collect::<ParseResult<_>>()?;
                ParseResult::Ok(Expression::MethodCall {
                    object: Box::new(expr),
                    method_name: name.try_into().unwrap(),
                    args,
                })
            }
            ParseResultFull::Err(_) => ParseResult::Ok(expr),
            other => other.transmute(),
        }
    }
    pub fn parse_expression_9(&mut self) -> ParseResult<Expression> {
        self.parse_expression_atom()
    }
    pub fn parse_expression_atom(&mut self) -> ParseResult<Expression> {
        let funcs: Vec<Box<dyn FnOnce(&mut Self) -> ParseResult<Expression>>> = vec![
            Box::new(|s: &mut Self| {
                s.match_token(TokenType::LParen)?;
                let expr = s.parse_expression().make_fatal()?;
                s.match_token(TokenType::RParen).make_fatal()?;
                ParseResult::Ok(expr)
            }),
            Box::new(|s: &mut Self| {
                let lbrace = s.match_token(TokenType::LBrace)?;
                let statements = iter::repeat_with(|| {
                    s.skip_eols();
                    s.parse_statement()
                })
                .collect::<ParseResult<_>>()?;
                s.skip_eols();
                let rbrace = s.match_token(TokenType::RBrace).make_fatal()?;
                ParseResult::Ok(Expression::Block {
                    lbrace,
                    statements,
                    rbrace,
                })
            }),
            Box::new(|s: &mut Self| {
                ParseResult::Ok(Expression::Identifier(
                    s.match_identifier()?.try_into().unwrap(),
                ))
            }),
            Box::new(|s: &mut Self| {
                let token = s.match_string()?;
                let contents = match token.token_type() {
                    TokenType::String(s) => s.clone(),
                    _ => unreachable!(),
                };

                ParseResult::Ok(Expression::String { contents, token })
            }),
            Box::new(|s: &mut Self| {
                let token = s.match_integer()?;
                let value = match token.token_type() {
                    TokenType::Integer(v) => v.clone(),
                    _ => unreachable!(),
                };

                ParseResult::Ok(Expression::Integer { value, token })
            }),
            Box::new(|s: &mut Self| {
                let token = s.match_float()?;
                let value = match token.token_type() {
                    TokenType::Float(v) => *v,
                    _ => unreachable!(),
                };

                ParseResult::Ok(Expression::Float { value, token })
            }),
            Box::new(|s: &mut Self| {
                let token = s.match_char()?;
                let value = match token.token_type() {
                    TokenType::Char(v) => *v,
                    _ => unreachable!(),
                };

                ParseResult::Ok(Expression::Char { value, token })
            }),
            Box::new(|s: &mut Self| {
                ParseResult::Ok(Expression::Nil(s.match_token(TokenType::Nil)?))
            }),
        ];

        self.ordered_choice(funcs, ParseError::ExpressionExpected)
    }

    fn parse_token_separated_list<
        F: FnMut(&mut Self) -> ParseResult<R>,
        B: FnOnce(SeparatedList<R>) -> R,
        R,
    >(
        &mut self,
        mut part: F,
        token: TokenType,
        builder: B,
    ) -> ParseResult<R> {
        let first = part(self)?;
        let rest = (|| loop {
            let tok = match self.match_token(token.clone()) {
                ParseResultFull::Ok(v) => v,
                ParseResultFull::Err(_) => return,
                ParseResultFull::Fatal(f) => {
                    yield ParseResult::Fatal(f);
                    return;
                }
            };
            let part = match part(self) {
                ParseResultFull::Ok(v) => v,
                ParseResultFull::Err(_) => return,
                ParseResultFull::Fatal(f) => {
                    yield ParseResult::Fatal(f);
                    return;
                }
            };
            yield ParseResult::Ok((tok, part));
        })
        .into_iter()
        .collect::<ParseResult<_>>()?;

        if !rest.is_empty() {
            ParseResult::Ok(builder(SeparatedList {
                first: Box::new(first),
                rest,
            }))
        } else {
            ParseResult::Ok(first)
        }
    }

    fn parse_token_separated_left_tree<
        F: FnMut(&mut Self) -> ParseResult<R>,
        B: FnMut(R, R, Token) -> R,
        R,
    >(
        &mut self,
        mut part: F,
        tokens: Vec<TokenType>,
        mut builder: B,
    ) -> ParseResult<R> {
        let mut l = part(self)?;
        loop {
            let token = self.peek();
            if tokens.contains(token.token_type()) {
                let token = self.next().clone();
                let r = part(self).make_fatal()?;
                l = builder(l, r, token)
            } else {
                break;
            }
        }
        ParseResult::Ok(l)
    }

    fn match_token(&mut self, tt: TokenType) -> ParseResult<Token> {
        let tok = self.peek();
        if tok.token_type() == &tt {
            ParseResult::Ok(self.next().clone())
        } else {
            ParseResult::Err(ParseError::TokenExpected(tt))
        }
    }

    fn match_identifier(&mut self) -> ParseResult<Token> {
        let tok = self.peek();
        if matches!(tok.token_type(), TokenType::Identifier(_)) {
            ParseResult::Ok(self.next().clone())
        } else {
            ParseResult::Err(ParseError::TokenExpected(TokenType::Identifier(
                String::new(),
            )))
        }
    }

    fn match_string(&mut self) -> ParseResult<Token> {
        let tok = self.peek();
        if matches!(tok.token_type(), TokenType::String(_)) {
            ParseResult::Ok(self.next().clone())
        } else {
            ParseResult::Err(ParseError::TokenExpected(TokenType::String(String::new())))
        }
    }

    fn match_integer(&mut self) -> ParseResult<Token> {
        let tok = self.peek();
        if matches!(tok.token_type(), TokenType::Integer(_)) {
            ParseResult::Ok(self.next().clone())
        } else {
            ParseResult::Err(ParseError::TokenExpected(
                TokenType::Integer(BigInt::zero()),
            ))
        }
    }

    fn match_float(&mut self) -> ParseResult<Token> {
        let tok = self.peek();
        if matches!(tok.token_type(), TokenType::Float(_)) {
            ParseResult::Ok(self.next().clone())
        } else {
            ParseResult::Err(ParseError::TokenExpected(TokenType::Float(0.0)))
        }
    }

    fn match_char(&mut self) -> ParseResult<Token> {
        let tok = self.peek();
        if matches!(tok.token_type(), TokenType::Char(_)) {
            ParseResult::Ok(self.next().clone())
        } else {
            ParseResult::Err(ParseError::TokenExpected(TokenType::Char('\0')))
        }
    }

    fn match_eol(&mut self) -> ParseResult<Option<Token>> {
        let tok = self.peek();
        match tok.token_type() {
            TokenType::Semicolon => ParseResult::Ok(Some(self.next().clone())),
            TokenType::Eol => ParseResult::Ok(None),
            _ => ParseResult::Err(ParseError::TokenExpected(TokenType::Eol)),
        }
    }

    fn ordered_choice<I: IntoIterator<Item = Box<dyn FnOnce(&mut Self) -> ParseResult<R>>>, R>(
        &mut self,
        funcs: I,
        error: ParseError,
    ) -> ParseResult<R> {
        for func in funcs {
            match func(self) {
                ParseResultFull::Ok(v) => return ParseResult::Ok(v),
                ParseResultFull::Err(_) => {}
                ParseResultFull::Fatal(f) => return ParseResult::Fatal(f),
            }
        }

        ParseResult::Err(error)
    }

    fn skip_eols(&mut self) {
        while self.match_token(TokenType::Eol).is_ok() {}
    }

    fn push(&mut self) {
        self.index_stack.push(self.index);
    }

    fn backtrack(&mut self) {
        self.index = *self.index_stack.last().unwrap();
    }

    fn pop(&mut self) {
        self.index_stack.pop();
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn next(&mut self) -> &Token {
        let ret = &self.tokens[self.index];
        self.index += 1;
        ret
    }

    fn eof(&self) -> bool {
        self.peek().token_type() == &TokenType::Eof
    }
}
