#![feature(if_let_guard)]
#![feature(never_type)]
#![feature(generator_trait)]
#![feature(generators)]
#![feature(drain_filter)]
#![feature(try_trait_v2)]
#![feature(inline_const)]
#![feature(maybe_uninit_uninit_array)]
#![feature(maybe_uninit_array_assume_init)]
#![feature(format_args_capture)]

use std::rc::Rc;



use crate::lexer::token::TokenType;

mod arena;
mod lexer;
mod parser;
mod parser_old;
mod generator_adapter;
mod compiler;

fn main() -> Result<(), String> {
    let file = "./playground.verm";
    let code = std::fs::read_to_string(file).unwrap();
    let mut lexer = lexer::Lexer::new(Rc::new(file.to_string()), code.chars());
    let tokens: Vec<_> = lexer.tokens().collect();
    let mut has_errors = false;
    for token in &tokens {
        // print!("{:?} ", token.token_type());
        if token.token_type() == &TokenType::Eol {
            // println!();
        }
        if token.token_type().is_error() {
            has_errors = true;
            println!("{:?}", token);
        }
    }
    // println!();

    if has_errors {
        return Err("".to_string());
    }

    match parser::parser::program(&tokens) {
        Ok(program) => {
            compiler::compile(program)
        },
        Err(e) => {
            let i = e.location;
            let loc = tokens.get(i).map(|t| format!("{:?}", t.loc())).unwrap_or_else(|| format!("?{}", i));
            Err(format!("Error: {:?}: {:?}", loc, e.expected))
        }
    }
}
