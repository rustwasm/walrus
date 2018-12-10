use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::{Once, ONCE_INIT};

fn require_wat2wasm() {
    let status = Command::new("wat2wasm")
        .arg("--help")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect(
            "Could not spawn wat2wasm; do you have https://github.com/WebAssembly/wabt installed?",
        );
    assert!(
        status.success(),
        "wat2wasm did not run OK; do you have https://github.com/WebAssembly/wabt installed?"
    )
}

/// Compile the `.wat` file at the given path into a `.wasm`.
pub fn wat2wasm(path: &Path) -> PathBuf {
    static CHECK: Once = ONCE_INIT;
    CHECK.call_once(require_wat2wasm);

    let mut wasm = PathBuf::from(path);
    wasm.set_extension("wasm");

    let mut cmd = Command::new("wat2wasm");
    cmd.arg(path).arg("-o").arg(&wasm).arg("-v");
    println!("running: {:?}", cmd);
    let status = cmd.status().expect("should spawn wat2wasm OK");
    assert!(status.success(), "should run wat2wasm OK");

    wasm
}

fn require_wasm2wat() {
    let status = Command::new("wasm2wat")
        .arg("--help")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect(
            "Could not spawn wasm2wat; do you have https://github.com/WebAssembly/wabt installed?",
        );
    assert!(
        status.success(),
        "wasm2wat did not run OK; do you have https://github.com/WebAssembly/wabt installed?"
    )
}

/// Disassemble the `.wasm` file at the given path into a `.wat`.
pub fn wasm2wat(path: &Path) -> PathBuf {
    static CHECK: Once = ONCE_INIT;
    CHECK.call_once(require_wasm2wat);

    let mut wasm = PathBuf::from(path);
    wasm.set_extension("wasm");

    let mut cmd = Command::new("wasm2wat");
    cmd.arg(path).arg("-o").arg(&wasm).arg("-v");
    println!("running: {:?}", cmd);
    let status = cmd.status().expect("should spawn wasm2wat OK");
    assert!(status.success(), "should run wasm2wat OK");

    wasm
}
