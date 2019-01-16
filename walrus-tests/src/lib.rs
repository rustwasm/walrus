use std::fs;
use std::path::Path;

pub struct FileCheck {
    patterns: Vec<Vec<String>>,
}

impl FileCheck {
    pub fn from_file(path: &Path) -> FileCheck {
        let contents = fs::read_to_string(path).expect("should read file to string OK");
        FileCheck::from_lines(contents.lines())
    }

    pub fn from_lines<'a, I: IntoIterator<Item = &'a str>>(iter: I) -> FileCheck {
        let mut patterns = vec![];
        for line in iter {
            let line = line.trim();
            if line.starts_with(";; CHECK:") {
                let p = line[";; CHECK:".len()..].to_string();
                patterns.push(vec![p]);
            }
            if line.starts_with(";; NEXT:") {
                let p = patterns
                    .last_mut()
                    .expect("NEXT should never come before CHECK");
                p.push(line[";; NEXT:".len()..].to_string());
            }
        }
        FileCheck { patterns }
    }

    pub fn check(&self, output: &str) {
        let output_lines = output.lines().collect::<Vec<_>>();

        'outer: for pattern in &self.patterns {
            let first_line = &pattern[0];

            let mut start = 0;

            'inner: while let Some(pos) = output_lines[start..]
                .iter()
                .position(|l| matches(*l, first_line))
            {
                start = pos + 1;
                if output_lines[pos..].len() + 1 < pattern.len() {
                    break;
                }
                for (out_line, pat_line) in output_lines[pos + 1..].iter().zip(&pattern[1..]) {
                    if !matches(out_line, pat_line) {
                        continue 'inner;
                    }
                }

                continue 'outer;
            }
            self.missing_pattern(pattern, output);
        }
    }

    fn missing_pattern(&self, pattern: &[String], output: &str) -> ! {
        let pattern = pattern
            .iter()
            .enumerate()
            .map(|(i, l)| format!("    {}: {}", if i == 0 { "CHECK" } else { "NEXT" }, l,))
            .collect::<Vec<_>>()
            .join("\n");

        let output = output
            .lines()
            .map(|l| format!("    {}", l.trim_end()))
            .collect::<Vec<_>>()
            .join("\n");

        panic!(
            "\
             CHECK failed!\n\n\
             Did not find pattern\n\n\
             {}\n\n\
             in output\n\n\
             {}\n\n",
            pattern, output
        );
    }
}

fn matches(mut actual: &str, expected: &str) -> bool {
    actual = actual.trim();
    // skip a leading comment
    if actual.starts_with("(;") {
        actual = actual[actual.find(";)").unwrap() + 2..].trim();
    }
    actual.starts_with(expected.trim())
}
