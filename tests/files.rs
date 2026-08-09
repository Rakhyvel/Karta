use std::{fs, path::PathBuf};

use karta::KartaContext;

#[derive(Clone, Copy)]
enum Expect {
    Value,
    Error,
}

#[test]
fn integration() {
    run_dir("tests/integration", Expect::Value);
}

#[test]
fn negative() {
    run_dir("tests/negative", Expect::Error);
}

/// Run all the tests in the dir, with the given expectation (pass or not)
fn run_dir(dir: &str, kind: Expect) {
    let mut failures = Vec::new();
    let files = karta_files(dir);
    assert!(!files.is_empty(), "no karta files?");

    // Go through each file and run it, and make sure it does what its supposed to
    for path in files {
        let src = fs::read_to_string(&path).unwrap();
        let prefix = match kind {
            Expect::Value => "; expect:",
            Expect::Error => "; expect-error:",
        };
        let name = path.display();

        // get the expected string
        let Some(expected) = directive(&src, prefix) else {
            failures.push(format!("{name}: missing `{prefix}` directive"));
            continue;
        };

        // Run the file and check its outputs are what we expect
        match (kind, kctx_run(&path)) {
            // Expect that it should evaluate to a value
            (Expect::Value, Ok(got)) if got.trim() == expected => {}
            (Expect::Value, Ok(got)) => failures.push(format!(
                "{name}: expected `{expected}`, got `{}`",
                got.trim()
            )),
            (Expect::Value, Err(e)) => failures.push(format!("{name}: unexpected error: {e}")),

            // Expect that there should be an error
            (Expect::Error, Err(e)) if e.trim().replace("\\", "/") == expected => {}
            (Expect::Error, Err(e)) => failures.push(format!(
                "{name}: expected error `{expected}`, got `{}`",
                e.trim().replace("\\", "/")
            )),
            (Expect::Error, Ok(got)) => failures.push(format!(
                "{name}: expected error `{expected}`, got `{}`",
                got.trim()
            )),
        }
    }

    assert!(failures.is_empty(), "\n{}", failures.join("\n"));
}

/// Create a fresh karta context and run a file
fn kctx_run(path: &PathBuf) -> Result<String, String> {
    KartaContext::new().run_file(path)
}

fn karta_files(dir: &str) -> Vec<PathBuf> {
    let mut out: Vec<_> = fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("{dir}: {e}"))
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().is_some_and(|x| x == "k"))
        .collect();
    out.sort();
    out
}

/// try to find the line that contains the prefix, and return the text after it
fn directive(src: &str, prefix: &str) -> Option<String> {
    src.lines()
        .find_map(|l| l.trim_start().strip_prefix(prefix))
        .map(|s| s.trim().to_string())
}
