use failure::ResultExt;
use std::fs;
use std::path::Path;
use std::process::Command;
use tempfile::TempDir;

#[derive(serde::Deserialize, serde::Serialize)]
struct Test {
    source_filename: String,
    commands: Vec<serde_json::Value>,
}

fn run(wast: &Path) -> Result<(), failure::Error> {
    let proposal = wast
        .iter()
        .skip_while(|part| *part != "proposals")
        .skip(1)
        .next()
        .map(|s| s.to_str().unwrap());
    let extra_args: &[&str] = match proposal {
        None => &[],
        Some("mutable-global") => &[],
        Some("sign-extension-ops") => &["--enable-sign-extension"],

        // TODO: requires support in parity-wasm it looks like
        // Some("nontrapping-float-to-int-conversions") => &["--enable-saturating-float-to-int"],
        Some("nontrapping-float-to-int-conversions") => return Ok(()),

        // TODO: we should actually implement this proposal!
        Some("multi-value") => return Ok(()),

        // TODO: should get threads working
        // Some("threads") => &["--enable-threads"],
        Some("threads") => return Ok(()),

        Some(other) => panic!("unknown wasm proposal: {}", other),
    };

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

    let contents = fs::read_to_string(&json).context("failed to read file")?;
    let test: Test = serde_json::from_str(&contents).context("failed to parse file")?;
    let prev_len = test.commands.len();
    let mut new_commands = Vec::new();

    for command in test.commands {
        let filename = match command.get("filename") {
            Some(name) => name.as_str().unwrap().to_string(),
            None => {
                new_commands.push(command);
                continue;
            }
        };
        let line = &command["line"];
        let path = tempdir.path().join(filename);
        match command["type"].as_str().unwrap() {
            "assert_invalid" | "assert_malformed" => {
                if walrus::module::Module::from_file(&path).is_ok() {
                    panic!("wasm parsed when it shouldn't (line {})", line);
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
                let mut wasm = walrus::module::Module::from_file(&path)
                    .context(format!("error parsing wasm (line {})", line))?;

                // If a module is supposed to be unlinkable we'll often gc out
                // items which would have otherwise made it unlinkable, so make
                // sure that everything is exported.
                if cmd == "assert_unlinkable" {
                    for (i, memory) in wasm.memories.iter().enumerate() {
                        let name = format!("__memory_{}", i);
                        wasm.exports.add(&name, memory.id());
                    }
                    for (i, table) in wasm.tables.iter().enumerate() {
                        let name = format!("__table_{}", i);
                        wasm.exports.add(&name, table.id());
                    }
                    for (i, func) in wasm.funcs.iter().enumerate() {
                        let name = format!("__function_{}", i);
                        wasm.exports.add(&name, func.id());
                    }
                    for (i, global) in wasm.globals.iter().enumerate() {
                        let name = format!("__global_{}", i);
                        wasm.exports.add(&name, global.id());
                    }
                }

                let wasm1 = wasm
                    .emit_wasm()
                    .context(format!("error emitting wasm (line {})", line))?;
                fs::write(&path, &wasm1)?;
                let wasm2 = walrus::module::Module::from_buffer(&wasm1)
                    .and_then(|m| m.emit_wasm())
                    .context(format!("error re-parsing wasm (line {})", line))?;
                if wasm1 != wasm2 {
                    panic!("wasm module at line {} isn't deterministic", line);
                }
                new_commands.push(command);
                continue;
            }
        }
    }

    // The JSON parser in `spectest-interp` seems like it doesn't really parse
    // JSON as it bails on valid json that serde emits. Work around this by just
    // not rewriting the json file if we didn't actually filter out any
    // commands, which is needed to get a spec test working.
    if prev_len != new_commands.len() {
        let contents = serde_json::to_string_pretty(&Test {
            source_filename: test.source_filename,
            commands: new_commands,
        })
        .context("error creating json")?;
        fs::write(json, contents).context("error writing json")?;
    }

    let output = Command::new("spectest-interp")
        .current_dir(tempdir.path())
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
    panic!("failed");
}

include!(concat!(env!("OUT_DIR"), "/spec-tests.rs"));
