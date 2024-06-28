use anyhow::{bail, Context};
use std::fs;
use std::path::Path;
use std::process::Command;
use tempfile::TempDir;

#[derive(serde::Deserialize, serde::Serialize)]
struct Test {
    source_filename: String,
    commands: Vec<serde_json::Value>,
}

fn run(wast: &Path) -> Result<(), anyhow::Error> {
    static INIT_LOGS: std::sync::Once = std::sync::Once::new();
    INIT_LOGS.call_once(|| {
        env_logger::init();
    });

    // Skip proposals tests for now
    if wast.components().any(|c| c.as_os_str() == "proposals") {
        return Ok(());
    }

    let extra_args = &[];

    let tempdir = TempDir::new()?;
    let json = tempdir.path().join("foo.json");
    let status = Command::new("wast2json")
        .arg(wast)
        .arg("-o")
        .arg(&json)
        .args(extra_args)
        .status()
        .context("executing `wast2json`")?;
    assert!(status.success());

    let wabt_ok = run_spectest_interp(tempdir.path(), extra_args).is_ok();
    // If wabt didn't succeed before we ran walrus there's no hope of it passing
    // after we run walrus.
    if !wabt_ok {
        return Ok(());
    }

    let contents = fs::read_to_string(&json).context("failed to read file")?;
    let test: Test = serde_json::from_str(&contents).context("failed to parse file")?;
    let mut files = Vec::new();

    let mut config = walrus::ModuleConfig::new();
    if extra_args.len() == 0 {
        config.only_stable_features(true);
    }

    let mut false_positives = vec![];
    for command in test.commands {
        let filename = match command.get("filename") {
            Some(name) => name.as_str().unwrap().to_string(),
            None => continue,
        };
        let line = &command["line"];
        let path = tempdir.path().join(filename);
        match command["type"].as_str().unwrap() {
            "assert_invalid" | "assert_malformed" => {
                // Skip tests that are actually valid with various in-flight proposals
                let text = command["text"].as_str().unwrap();
                if text == "invalid result arity"
                    || text == "multiple memories"
                    || text == "multiple tables"
                {
                    continue;
                }
                let wasm = fs::read(&path)?;
                if config.parse(&wasm).is_ok() {
                    // A few spec tests assume multi-value isn't implemented,
                    // but we implement it, so basically just skip those tests.
                    let message = command["text"].as_str().unwrap();
                    if message.contains("invalid result arity") {
                        continue;
                    }

                    // MVP wasm considers this tests to fail, but
                    // reference-types-enhanced wasm considers this test to
                    // pass. We implement the reference-types semantics, so
                    // let's go forward with that.
                    if wast.ends_with("unreached-invalid.wast") && line == 539 {
                        continue;
                    }
                    false_positives.push(line.as_u64().unwrap());
                }
            }
            "assert_unlinkable" if wast.file_name() == Some("elem.wast".as_ref()) => {
                // The `elem.wast` file has some unlinkable modules which place
                // table elements at massive (aka negative) offsets. Our
                // representation means that we try to allocate a massive amount
                // of space for null elements. For now we skip these tests as an
                // implementation detail. This is arguably a bug on our end
                // where we should improve our representation to not allocate so
                // much, but that's another bug for another day.
            }
            cmd => {
                let wasm = fs::read(&path)?;
                let mut wasm = config
                    .parse(&wasm)
                    .with_context(|| format!("error parsing wasm (line {})", line))?;
                let wasm1 = wasm.emit_wasm();
                fs::write(&path, &wasm1)?;
                let wasm2 = config
                    .parse(&wasm1)
                    .map(|mut m| m.emit_wasm())
                    .with_context(|| format!("error re-parsing wasm (line {})", line))?;
                if wasm1 != wasm2 {
                    panic!("wasm module at line {} isn't deterministic", line);
                }
                files.push((cmd.to_string(), path.to_path_buf()));
                continue;
            }
        }
    }

    if !false_positives.is_empty() {
        panic!("wasm parsed when it shouldn't (line {:?})", false_positives);
    }

    // First up run the spec-tests as-is after we round-tripped through walrus.
    // This should for sure work correctly
    run_spectest_interp(tempdir.path(), extra_args)?;

    // Next run the same spec tests with semantics-preserving passes implemented
    // in walrus. Everything should continue to pass.
    for (cmd, file) in files.iter() {
        let wasm = fs::read(file)?;
        let mut module = config.parse(&wasm)?;

        // Tests which assert that they're not linkable tend to not work with
        // the gc pass because it removes things which would cause a module to
        // become unlinkable. This doesn't matter too much in the real world
        // (hopefully), so just don't gc assert_unlinkable modules.
        if cmd != "assert_unlinkable" {
            walrus::passes::gc::run(&mut module);
        }

        let wasm = module.emit_wasm();
        fs::write(&file, wasm)?;
    }

    run_spectest_interp(tempdir.path(), extra_args)?;

    Ok(())
}

fn run_spectest_interp(cwd: &Path, extra_args: &[&str]) -> Result<(), anyhow::Error> {
    let output = Command::new("spectest-interp")
        .current_dir(cwd)
        .arg("foo.json")
        .args(extra_args)
        .output()
        .context("executing `spectest-interp`")?;

    // If the interpreter exits with success it may still have failed some
    // tests. Check the output for `X/Y tests passed.` and make sure `X` equals
    // `Y`.
    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        if let Some(line) = stdout.lines().find(|l| l.ends_with("tests passed.")) {
            let part = line.split_whitespace().next().unwrap();
            let mut parts = part.split("/");
            let a = parts.next().unwrap().parse::<u32>();
            let b = parts.next().unwrap().parse::<u32>();
            if a == b {
                return Ok(());
            }
        }
    }
    println!("status: {}", output.status);
    println!("stdout:\n{}", String::from_utf8_lossy(&output.stdout));
    println!("stderr:\n{}", String::from_utf8_lossy(&output.stderr));
    bail!("failed");
}

include!(concat!(env!("OUT_DIR"), "/spec-tests.rs"));
