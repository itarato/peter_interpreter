use clap::Parser;
use std::fs;

mod common;
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
    let program_args = ProgramArgs::parse();

    match program_args.command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            eprintln!("Logs from your program will appear here!");

            let file_contents = fs::read_to_string(&program_args.filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", program_args.filename);
                String::new()
            });

            if !file_contents.is_empty() {
                panic!("Scanner not implemented");
            } else {
                println!("EOF  null");
            }
        }
        other => {
            eprintln!("Unknown command: {}", other);
        }
    }
}
