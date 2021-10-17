pub mod ast;

use crate::lexer::token::{Token, TokenType};
use ast::*;

use TokenType::*;

peg::parser! {
    pub grammar parser() for [Token] {
        pub rule program() -> Program
            = eol()* f:function() ** (eol()*) eol()* tok(Eof)
            { Program { functions: f } }

        rule function() -> Function
            = f:tok(Fn) i:ident() pp:ident()* lb:tok(LBrace)
              eol()* b:stmnt() ** (eol()*) eol()* rb:tok(RBrace)
              { Function { fn_tok: f, ident: i.try_into().unwrap(), lbrace: lb,
                           params: pp.into_iter().map(|t| t.try_into().unwrap()).collect(), rbrace: rb, stmnts: b } }

        rule stmnt() -> Statement
            = l:tok(Let) p:ptrn() e:tok(Eq) x:expr() s:eol()
              { Statement::Let { eq: e, expr: x, let_tok: l, pattern: p, semicolon: s } }
            / v:tok(Var) p:ptrn() a:(a:tok(LeftArrow) e:expr() { (a, e) })? s:eol()
              { Statement::Var { var: v, pattern: p, assign: a, semicolon: s } }
            / b:tok(Break) e:expr()? s:eol()
              { Statement::Break { break_tok: b, expr: e, semicolon: s } }
            / c:tok(Continue) s:eol()
                { Statement::Continue { continue_tok: c, semicolon: s } }
            / r:tok(Return) e:expr()? s:eol()
                { Statement::Return { return_tok: r, expr: e, semicolon: s } }
            / y:tok(Yield) e:expr()? s:eol()
                { Statement::Yield { yield_tok: y, expr: e, semicolon: s } }
            / p:ptrn() a:tok(LeftArrow) e:expr() s:eol()
              { Statement::Assign { pattern: p, arrow: a, expr: e, semicolon: s } }
            / e:expr() s:eol()
              { Statement::Expression { expr: e, semicolon: s } }


        rule ptrn() -> Pattern = ptrn0()
        rule ptrn0() -> Pattern
            = f:ptrn1() r:(c:tok(Comma) p:ptrn1() { (c, p) })*
              { if r.is_empty() { f } else { Pattern::Tuple(SeparatedList { first: Box::new(f), rest: r }) } }
        rule ptrn1() -> Pattern = ptrn_atom()
        rule ptrn_atom() -> Pattern
            = tok(LParen) p:ptrn() tok(RParen) { p }
            / t:ident() { Pattern::Identifier(t.try_into().unwrap()) }
            / t:string() { t.try_into().unwrap() }
            / t:int() { t.try_into().unwrap() }
            / t:float() { t.try_into().unwrap() }
            / t:char() { t.try_into().unwrap() }
            / t:bool() { t.try_into().unwrap() }
            / t:tok(Nil) { t.try_into().unwrap() }
            / t:tok(Underscore) { t.try_into().unwrap() }


        rule expr() -> Expression = expr0()
        rule expr0() -> Expression
            = p:ptrn()* a:tok(RightArrow) e:expr()
              { Expression::Lambda { arrow: a, params: p, body: Box::new(e) } }
            / expr1()
        rule expr1() -> Expression
            = l:expr2() r:(t:tok(Comma) r:expr2() { (t, r) })* e:tok(Comma)?
            {
                if r.is_empty() && e.is_none() {
                    l
                } else {
                    Expression::Tuple(SeparatedList { first: Box::new(l), rest: r })
                }
            }
        rule expr2() -> Expression
            = l:expr3() r:(t:tok(Or) r:expr3() { (t, r) })*
            {
                let mut left = l;
                for (tok, right) in r {
                    left = Expression::Binary { left: Box::new(left), right: Box::new(right), op: tok.try_into().unwrap() };
                }
                left
            }
        rule expr3() -> Expression
            = l:expr4() r:(t:tok(And) r:expr4() { (t, r) })*
            {
                let mut left = l;
                for (tok, right) in r {
                    left = Expression::Binary { left: Box::new(left), right: Box::new(right), op: tok.try_into().unwrap() };
                }
                left
            }
        rule expr4() -> Expression
            = l:expr5() r:(t:(tok(Eq)/tok(Neq)/tok(Gt)/tok(Gteq)/tok(Lt)/tok(Lteq)) r:expr5() { (t, r) })?
            {
                if let Some((t, r)) = r {
                    Expression::Binary { left: Box::new(l), right: Box::new(r), op: t.try_into().unwrap() }
                } else {
                    l
                }
            }

        rule expr5() -> Expression
            = l:expr6() r:(t:(tok(Plus)/tok(Minus)) r:expr6() { (t, r) })*
            {
                let mut left = l;
                for (tok, right) in r {
                    left = Expression::Binary { left: Box::new(left), right: Box::new(right), op: tok.try_into().unwrap() };
                }
                left
            }
        rule expr6() -> Expression
            = l:expr7() r:(t:(tok(Star)/tok(Slash)/tok(DoubleSlash)/tok(Percent)) r:expr7() { (t, r) })*
            {
                let mut left = l;
                for (tok, right) in r {
                    left = Expression::Binary { left: Box::new(left), right: Box::new(right), op: tok.try_into().unwrap() };
                }
                left
            }
        rule expr7() -> Expression
            = e:expr8() r:(t:tok(ExclamationMark) a:expr8()* { (t, a) })?
            {
                if let Some((t, a)) = r {
                    Expression::FunctionCall {
                        func: Box::new(e),
                        excl_mark: t,
                        args: a
                    }
                } else {
                    e
                }
            }
        rule expr8() -> Expression
            = o:expr9() r:(p:tok(Period) i:ident() tok(ExclamationMark) e:expr9()* { (p, i, e) })?
            {
                if let Some((p, i, e)) = r {
                    Expression::MethodCall {
                        object: Box::new(o),
                        method_name: i.try_into().unwrap(),
                        args: e
                    }
                } else {
                    o
                }
            }
        rule expr9() -> Expression = expr_atom()
        rule expr_atom() -> Expression
            = tok(If) c:expr() b:expr() ei:(tok(Else) tok(If) c:expr() b:expr() { (c, b) })*
              eb:(tok(Else) b:expr() { b })?
              {
                  Expression::If {
                      cond: Box::new(c),
                      body: Box::new(b),
                      elifs: ei,
                      else_body: eb.map(Box::new)
                  }
              }
            / tok(For) p:ptrn() tok(In) e:expr() b:expr()
              { Expression::For { pattern: p, expr: Box::new(e), body: Box::new(b) } }
            / tok(Loop) b:expr() { Expression::Loop(Box::new(b)) }
            / tok(While) c:expr() b:expr() { Expression::While { cond: Box::new(c), body: Box::new(b) } }
            / tok(LParen) e:expr() tok(RParen) { e }
            / lb:tok(LBrace) eol()* s:stmnt() ** (eol()*) eol()* rb:tok(RBrace)
              { Expression::Block { lbrace: lb, rbrace: rb, statements: s } }
            / t:ident() { Expression::Identifier(t.try_into().unwrap()) }
            / t:string() { t.try_into().unwrap() }
            / t:int() { t.try_into().unwrap() }
            / t:float() { t.try_into().unwrap() }
            / t:char() { t.try_into().unwrap() }
            / t:bool() { t.try_into().unwrap() }
            / t:tok(Nil) { t.try_into().unwrap() }
            / t:tok(Underscore) { t.try_into().unwrap() }

        rule body() -> Vec<Statement>
            = tok(LBrace) eol()* b:stmnt() ** (eol()*) eol()* tok(RBrace) { b }

        rule eol() -> Option<Token> = tok(Eol) { None } / t:tok(Semicolon) { Some(t) }


        rule ident() -> Token
            = [t @ Token { token_type: Identifier(_), .. }] { t }
        rule string() -> Token
            = [t @ Token { token_type: String(_), .. }] { t }
        rule int() -> Token
            = [t @ Token { token_type: Integer(_), .. }] { t }
        rule float() -> Token
            = [t @ Token { token_type: Float(_), .. }] { t }
        rule char() -> Token
            = [t @ Token { token_type: Char(_), .. }] { t }
        rule bool() -> Token
            = [t @ Token { token_type: Bool(_), .. }] { t }

        rule tok(tt: TokenType) -> Token
            = t:$([_]) {? let t = t[0].clone(); if t.token_type() == &tt { Ok(t) } else { Err(tt.name()) } }
    }
}
