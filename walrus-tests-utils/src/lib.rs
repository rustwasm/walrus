use std::fs;
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
pub fn wat2wasm(path: &Path) -> Vec<u8> {
    static CHECK: Once = ONCE_INIT;
    CHECK.call_once(require_wat2wasm);

    let file = tempfile::NamedTempFile::new().unwrap();

    let mut wasm = PathBuf::from(path);
    wasm.set_extension("wasm");

    let mut cmd = Command::new("wat2wasm");
    cmd.arg(path).arg("-o").arg(file.path());
    println!("running: {:?}", cmd);
    let status = cmd.status().expect("should spawn wat2wasm OK");
    assert!(status.success(), "should run wat2wasm OK");

    fs::read(file.path()).unwrap()
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
pub fn wasm2wat(path: &Path) -> String {
    static CHECK: Once = ONCE_INIT;
    CHECK.call_once(require_wasm2wat);

    let mut cmd = Command::new("wasm2wat");
    cmd.arg(path);
    println!("running: {:?}", cmd);
    let output = cmd.output().expect("should spawn wasm2wat OK");
    assert!(output.status.success(), "should run wasm2wat OK");
    String::from_utf8_lossy(&output.stdout).into_owned()
}

pub fn handle<T: TestResult>(result: T) {
    result.handle();
}

pub trait TestResult {
    fn handle(self);
}

impl TestResult for () {
    fn handle(self) {}
}

impl TestResult for Result<(), failure::Error> {
    fn handle(self) {
        let err = match self {
            Ok(()) => return,
            Err(e) => e,
        };
        eprintln!("got an error:");
        for c in err.iter_chain() {
            eprintln!("  {}", c);
        }
        eprintln!("{}", err.backtrace());
        panic!("test failed");
    }
}
