use std::env;
use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use walkdir::WalkDir;

fn is_known_failing(name: &str) -> bool {
    match name {
        // TODO issues to investigate: wasm parsed when it shouldn't
        "tests_spec_tests_proposals_bulk_memory_operations_binary_wast"
        | "tests_spec_tests_proposals_reference_types_binary_wast" => true,

        // SIMD isn't fully implemented yet.
        "tests_spec_tests_proposals_simd_simd_boolean_wast"
        | "tests_spec_tests_proposals_simd_simd_conversions_wast"
        | "tests_spec_tests_proposals_simd_simd_f32x4_rounding_wast"
        | "tests_spec_tests_proposals_simd_simd_f64x2_rounding_wast"
        | "tests_spec_tests_proposals_simd_simd_i16x8_extadd_pairwise_i8x16_wast"
        | "tests_spec_tests_proposals_simd_simd_i16x8_extmul_i8x16_wast"
        | "tests_spec_tests_proposals_simd_simd_i16x8_q15mulr_sat_s_wast"
        | "tests_spec_tests_proposals_simd_simd_i32x4_extadd_pairwise_i16x8_wast"
        | "tests_spec_tests_proposals_simd_simd_i32x4_extmul_i16x8_wast"
        | "tests_spec_tests_proposals_simd_simd_i32x4_trunc_sat_f64x2_wast"
        | "tests_spec_tests_proposals_simd_simd_i64x2_arith2_wast"
        | "tests_spec_tests_proposals_simd_simd_i64x2_cmp_wast"
        | "tests_spec_tests_proposals_simd_simd_i64x2_extmul_i32x4_wast"
        | "tests_spec_tests_proposals_simd_simd_i8x16_arith2_wast"
        | "tests_spec_tests_proposals_simd_simd_int_to_int_extend_wast"
        | "tests_spec_tests_proposals_simd_simd_load16_lane_wast"
        | "tests_spec_tests_proposals_simd_simd_load32_lane_wast"
        | "tests_spec_tests_proposals_simd_simd_load64_lane_wast"
        | "tests_spec_tests_proposals_simd_simd_load8_lane_wast"
        | "tests_spec_tests_proposals_simd_simd_load_zero_wast"
        | "tests_spec_tests_proposals_simd_simd_store16_lane_wast"
        | "tests_spec_tests_proposals_simd_simd_store32_lane_wast"
        | "tests_spec_tests_proposals_simd_simd_store64_lane_wast"
        | "tests_spec_tests_proposals_simd_simd_store8_lane_wast" => true,

        _ => false,
    }
}

fn for_each_wat_file<P, F>(dir: P, mut f: F)
where
    P: AsRef<Path>,
    F: FnMut(&Path),
{
    println!("cargo:rerun-if-changed={}", dir.as_ref().display());
    for entry in WalkDir::new(dir) {
        let entry = entry.unwrap();
        if entry.path().extension() == Some(OsStr::new("wat"))
            || entry.path().extension() == Some(OsStr::new("wast"))
        {
            println!("cargo:rerun-if-changed={}", entry.path().display());
            f(entry.path());
        }
    }
}

fn path_to_ident(p: &Path) -> String {
    p.display()
        .to_string()
        .chars()
        .map(|c| match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => c,
            _ => '_',
        })
        .collect()
}

fn generate_tests(name: &str) {
    let mut tests = String::new();

    for_each_wat_file(Path::new("tests").join(name), |path| {
        let test_name = path_to_ident(path);
        let ignore_test = if is_known_failing(&test_name) {
            "#[ignore]"
        } else {
            ""
        };
        tests.push_str(&format!(
            "#[test] {} fn {}() {{ walrus_tests_utils::handle(run(\"{}\".as_ref())); }}\n",
            ignore_test,
            test_name,
            path.display(),
        ));
    });

    let out_dir = env::var("OUT_DIR").unwrap();
    fs::write(Path::new(&out_dir).join(name).with_extension("rs"), &tests)
        .expect("should write generated valid.rs file OK");
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=WALRUS_TESTS_DOT");

    generate_tests("valid");
    generate_tests("round_trip");
    generate_tests("spec-tests");
    generate_tests("function_imports");
    generate_tests("invalid");
}
