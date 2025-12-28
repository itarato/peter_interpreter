use clap::Parser;
use log::{error, info};
use std::fs;

use crate::{
    common::{EXIT_CODE_LEXICAL_ERROR, EXIT_CODE_SUCCESS},
    scanner::Scanner,
};

mod ast;
mod common;
mod parser;
mod scanner;
mod str_reader;
mod token;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct ProgramArgs {
    command: String,
    filename: String,
}

fn main() {
    // unsafe { std::env::set_var("RUST_LOG", "debug") };
    pretty_env_logger::init();

    info!("Peter Interpreter Start");

    let program_args = ProgramArgs::parse();

    let mut exit_code = EXIT_CODE_SUCCESS;

    match program_args.command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(&program_args.filename).unwrap_or_else(|_| {
                error!("Failed to read file {}", program_args.filename);
                String::new()
            });

            match Scanner::new(&file_contents).scan() {
                Ok(tokens) => {
                    tokens.iter().for_each(|token| token.dump_short());
                    if tokens.iter().any(|t| t.is_error()) {
                        exit_code = EXIT_CODE_LEXICAL_ERROR;
                    }
                }
                Err(err) => error!("Error while scanning source: {:?}", err),
            }
        }
        other => {
            error!("Unknown command: {}", other);
        }
    }

    std::process::exit(exit_code)
}
