use failure::ResultExt;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Once;

pub type Result<T> = std::result::Result<T, failure::Error>;

pub const FEATURES: &[&str] = &[
    "--enable-threads",
    "--enable-bulk-memory",
    "--enable-reference-types",
    "--enable-simd",
    // TODO
    // "--enable-saturating-float-to-int",
    // "--enable-sign-extension",
    // "--enable-tail-call",
    // "--enable-annotations",
];

fn require_tool(tool: &str, repo: &str) {
    let diagnostic = format!("Could not spawn {}; do you have {} installed?", tool, repo);
    let status = Command::new("wat2wasm")
        .arg("--help")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect(&diagnostic);
    assert!(status.success(), "{}", diagnostic)
}

fn require_wat2wasm() {
    require_tool("wat2wasm", "https://github.com/WebAssembly/wabt");
}

/// Compile the `.wat` file at the given path into a `.wasm`.
pub fn wat2wasm(path: &Path) -> Result<Vec<u8>> {
    static CHECK: Once = Once::new();
    CHECK.call_once(require_wat2wasm);

    let file = tempfile::NamedTempFile::new().context("could not create named temp file")?;

    let mut wasm = PathBuf::from(path);
    wasm.set_extension("wasm");

    let mut cmd = Command::new("wat2wasm");
    cmd.arg(path)
        .args(FEATURES)
        .arg("--debug-names")
        .arg("-o")
        .arg(file.path());
    println!("running: {:?}", cmd);
    let output = cmd.output().context("could not spawn wat2wasm")?;

    if !output.status.success() {
        failure::bail!(
            "wat2wasm exited with status {:?}\n\nstderr = '''\n{}\n'''",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let buf = fs::read(file.path())?;
    Ok(buf)
}

fn require_wasm2wat() {
    require_tool("wasm2wat", "https://github.com/WebAssembly/wabt");
}

/// Disassemble the `.wasm` file at the given path into a `.wat`.
pub fn wasm2wat(path: &Path) -> Result<String> {
    static CHECK: Once = Once::new();
    CHECK.call_once(require_wasm2wat);

    let mut cmd = Command::new("wasm2wat");
    cmd.arg(path);
    cmd.args(FEATURES);
    println!("running: {:?}", cmd);
    let output = cmd.output().context("could not run wasm2wat")?;
    if !output.status.success() {
        failure::bail!(
            "wasm2wat exited with status {:?}\n\nstderr = '''\n{}\n'''",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn require_wasm_interp() {
    require_tool("wasm-insterp", "https://github.com/WebAssembly/wabt");
}

/// Run `wasm-interp` on the given wat file.
pub fn wasm_interp(path: &Path) -> Result<String> {
    static CHECK: Once = Once::new();
    CHECK.call_once(require_wasm_interp);

    let mut cmd = Command::new("wasm-interp");
    cmd.arg(path);
    cmd.arg("--run-all-exports");
    // This requires a build of WABT at least as new as `41adcbfb` to get
    // `wasm-interp`'s `--dummy-import-func`.
    cmd.arg("--dummy-import-func");
    cmd.args(FEATURES);
    println!("running: {:?}", cmd);
    let output = cmd.output().context("could notrun wasm-interp")?;
    if !output.status.success() {
        failure::bail!(
            "wasm-interp exited with status {:?}\n\nstderr = '''\n{}\n'''",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn require_wasm_opt() {
    require_tool("wasm-opt", "https://github.com/WebAssembly/binaryen");
}

/// Run `wasm-opt` on the given input file with optional extra arguments, and
/// return the resulting wasm binary as an in-memory buffer.
pub fn wasm_opt<A, S>(input: &Path, args: A) -> Result<Vec<u8>>
where
    A: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    static CHECK: Once = Once::new();
    CHECK.call_once(require_wasm_opt);

    let tmp = tempfile::NamedTempFile::new().unwrap();

    let mut cmd = Command::new("wasm-opt");
    cmd.arg(input);
    cmd.arg("-o");
    cmd.arg(tmp.path());
    cmd.args(&[
        "--enable-threads",
        "--enable-bulk-memory",
        // "--enable-reference-types",
        "--enable-simd",
    ]);
    cmd.args(args);
    println!("running: {:?}", cmd);
    let output = cmd.output().context("could not run wasm-opt")?;
    if !output.status.success() {
        failure::bail!(
            "wasm-opt exited with status {:?}\n\nstderr = '''\n{}\n'''",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let buf = fs::read(tmp.path())?;
    Ok(buf)
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

impl TestResult for std::result::Result<(), failure::Error> {
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
