use std::{path::PathBuf, process::ExitCode};

use karta::KartaContext;

pub fn main() -> ExitCode {
    let Some(path) = std::env::args().nth(1) else {
        eprintln!("usage: karta <file>.k");
        return ExitCode::from(2);
    };

    let mut kctx = KartaContext::new();

    let path = PathBuf::from(path);

    match kctx.run_file(&path) {
        Ok(_) => ExitCode::SUCCESS,
        Err(msg) => {
            eprintln!("{msg:?}");
            ExitCode::FAILURE
        }
    }
}
