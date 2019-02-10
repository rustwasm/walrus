//! Fuzz testing for `walrus`.

#![deny(missing_docs)]

use rand::{Rng, SeedableRng};
use std::cmp;
use std::fmt;
use std::fs;
use std::path::Path;
use std::time;
use walrus_tests_utils::{wasm_interp, wat2wasm};

#[derive(Copy, Clone, Debug)]
enum ValType {
    I32,
}

/// Configuration for fuzzing.
pub struct Config {
    fuel: usize,
    timeout: u64,
    scratch: tempfile::NamedTempFile,
}

impl Config {
    /// The default fuel level.
    pub const DEFAULT_FUEL: usize = 64;

    /// The default timeout (in seconds).
    pub const DEFAULT_TIMEOUT_SECS: u64 = 5;

    /// Construct a new fuzzing configuration.
    pub fn new() -> Config {
        let fuel = Self::DEFAULT_FUEL;
        let timeout = Self::DEFAULT_TIMEOUT_SECS;

        let dir = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("target")
            .join("walrus-fuzz");
        fs::create_dir_all(&dir).unwrap();
        let scratch = tempfile::NamedTempFile::new_in(dir).unwrap();

        Config {
            fuel,
            timeout,
            scratch,
        }
    }

    /// Set the fuel level.
    ///
    /// `fuel` must be greater than zero.
    pub fn set_fuel(mut self, fuel: usize) -> Config {
        assert!(fuel > 0);
        self.fuel = fuel;
        self
    }

    fn gen_wat(&self, seed: u64) -> String {
        WatGen::generate(rand::rngs::SmallRng::seed_from_u64(seed), self.fuel)
    }

    fn wat2wasm(&self, wat: &str) -> Vec<u8> {
        fs::write(self.scratch.path(), wat).unwrap();
        wat2wasm(self.scratch.path())
    }

    fn interp(&self, wasm: &[u8]) -> String {
        fs::write(self.scratch.path(), &wasm).unwrap();
        wasm_interp(self.scratch.path())
    }

    fn round_trip_through_walrus(&self, wasm: &[u8]) -> Vec<u8> {
        walrus::Module::from_buffer(&wasm)
            .unwrap()
            .emit_wasm()
            .unwrap()
    }

    fn run_one(&self, wat: &str) -> Option<FailingTestCase> {
        let wasm = self.wat2wasm(&wat);
        let expected = self.interp(&wasm);

        let walrus_wasm = self.round_trip_through_walrus(&wasm);
        let actual = self.interp(&walrus_wasm);

        if expected == actual {
            return None;
        }

        Some(FailingTestCase {
            wat: wat.to_string(),
            expected,
            actual,
        })
    }

    /// Generate a wasm file and then compare its output in the reference
    /// interpreter before and after round tripping it through `walrus`.
    ///
    /// Returns the reduced failing test case, if any.
    pub fn run(&mut self) -> Option<FailingTestCase> {
        let start = time::Instant::now();
        let timeout = time::Duration::from_secs(self.timeout);
        let mut seed = rand::thread_rng().gen();
        let mut failing = None;
        loop {
            let wat = self.gen_wat(seed);
            match self.run_one(&wat) {
                // We reduced fuel as far as we could, so return the last
                // failing test case.
                None if failing.is_some() => return failing,
                // Used all of our time, and didn't find any failing test cases.
                None if time::Instant::now().duration_since(start) > timeout => return None,
                // This RNG seed did not produce a failing test case, so choose
                // a new one.
                None => {
                    seed = rand::thread_rng().gen();
                }
                Some(f) => {
                    failing = Some(f);

                    // If we can try and reduce this test case with another
                    // iteration but with smaller fuel, do that. Otherwise
                    // return the failing test case.
                    if self.fuel > 1 {
                        self.fuel /= 2;
                    } else {
                        return failing;
                    }
                }
            }
        }
    }
}

/// A failing wasm test case where round tripping the wasm through walrus
/// produces an observably different execution in the reference interpreter.
#[derive(Clone, Debug)]
pub struct FailingTestCase {
    /// The WAT disassembly of the wasm test case.
    pub wat: String,

    /// The reference interpeter's output while interpreting the wasm *before* it
    /// has been round tripped through `walrus`.
    pub expected: String,

    /// The reference interpeter's output while interpreting the wasm *after* it
    /// has been round tripped through `walrus`.
    pub actual: String,
}

impl fmt::Display for FailingTestCase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "\
Found a failing test case!

{wat}

BEFORE round tripping through walrus:

{before}

AFTER round tripping through walrus:

{after}

Here is a standalone test case:

----------------8<----------------8<----------------8<----------------
#[test]
fn test_name() {{
    walrus_fuzz::assert_round_trip_execution_is_same(\"\\
{wat}\");
}}
----------------8<----------------8<----------------8<----------------
",
            wat = self.wat,
            before = self.expected,
            after = self.actual
        )
    }
}

/// Assert that the given WAT has the same execution trace before and after
/// round tripping it through walrus.
pub fn assert_round_trip_execution_is_same(wat: &str) {
    let config = Config::new();
    let failing_test_case = config.run_one(wat);
    assert!(failing_test_case.is_none());
}

struct WatGen<R> {
    rng: R,
    wat: String,
}

impl<R> WatGen<R>
where
    R: Rng,
{
    fn generate(rng: R, fuel: usize) -> String {
        let wat = String::new();
        let mut g = WatGen { rng, wat };
        g.prefix();
        g.gen_instructions(fuel);
        g.suffix();
        g.wat
    }

    fn prefix(&mut self) {
        self.wat.push_str(
            "\
(module
  (import \"host\" \"print\" (func (param i32) (result i32)))
  (func (export \"$f\")
",
        );
    }

    fn suffix(&mut self) {
        self.wat.push_str("  ))");
    }

    fn gen_instructions(&mut self, fuel: usize) {
        assert!(fuel > 0);

        let mut stack = vec![];

        for _ in 0..fuel {
            self.op(&mut stack);
            if !stack.is_empty() {
                self.instr("call 0");
            }
        }

        for _ in stack {
            self.instr("call 0");
            self.instr("drop");
        }
    }

    fn instr_imm<S, I>(&mut self, operator: impl ToString, immediates: I)
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        self.wat.push_str("    ");
        self.wat.push_str(&operator.to_string());

        for op in immediates.into_iter() {
            self.wat.push_str(" ");
            self.wat.push_str(op.as_ref());
        }

        self.wat.push('\n');
    }

    fn instr(&mut self, operator: impl ToString) {
        self.instr_imm(operator, None::<String>);
    }

    fn op(&mut self, stack: &mut Vec<ValType>) {
        let arity = self.rng.gen_range(0, cmp::min(3, stack.len() + 1));
        match arity {
            0 => self.op_0(stack),
            1 => self.op_1(stack.pop().unwrap(), stack),
            2 => self.op_2(stack.pop().unwrap(), stack.pop().unwrap(), stack),
            _ => unreachable!(),
        }
    }

    fn op_0(&mut self, stack: &mut Vec<ValType>) {
        match self.rng.gen_range(0, 2) {
            0 => {
                let value = self.rng.gen::<i32>().to_string();
                self.instr_imm("i32.const", Some(value));
                stack.push(ValType::I32);
            }
            1 => {
                self.instr("nop");
            }
            _ => unreachable!(),
        }
    }

    fn op_1(&mut self, _operand: ValType, stack: &mut Vec<ValType>) {
        match self.rng.gen_range(0, 2) {
            0 => {
                self.instr("drop");
            }
            1 => {
                self.instr("i32.popcnt");
                stack.push(ValType::I32);
            }
            _ => unreachable!(),
        }
    }

    fn op_2(&mut self, _a: ValType, _b: ValType, stack: &mut Vec<ValType>) {
        match self.rng.gen_range(0, 2) {
            0 => {
                self.instr("i32.add");
                stack.push(ValType::I32);
            }
            1 => {
                self.instr("i32.mul");
                stack.push(ValType::I32);
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn fuzz() {
        let mut config = super::Config::new();
        if let Some(failing_test_case) = config.run() {
            println!("{}", failing_test_case);
            panic!("Found a failing test case");
        }
    }
}
