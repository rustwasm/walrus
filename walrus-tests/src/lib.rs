use std::fs;
use std::path::Path;

pub struct FileCheck {
    patterns: Vec<String>,
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
                let mut p = line[";; CHECK:".len()..].trim().to_string();
                p.push('\n');
                patterns.push(p);
            }
            if line.starts_with(";; NEXT:") {
                let p = patterns
                    .last_mut()
                    .expect("NEXT should never come before CHECK");
                p.push_str(&line[";; NEXT:".len()..].trim());
                p.push('\n');
            }
        }
        FileCheck { patterns }
    }

    pub fn check(&self, output: &str) {
        for pattern in &self.patterns {
            let mut pattern_lines = pattern.lines();
            let first_line = pattern_lines
                .next()
                .expect("should be at least one line in a pattern");

            let output_lines = output.lines().collect::<Vec<_>>();
            let pos = match output_lines.iter().position(|l| *l == first_line) {
                None => self.missing_pattern(pattern, output),
                Some(pos) => pos,
            };
            for (out_line, pat_line) in output_lines[pos + 1..].iter().zip(pattern_lines) {
                if out_line.trim() != pat_line.trim() {
                    self.missing_pattern(pattern, output);
                }
            }
        }
    }

    fn missing_pattern(&self, pattern: &str, output: &str) -> ! {
        let pattern = pattern
            .lines()
            .enumerate()
            .map(|(i, l)| {
                format!(
                    "    {}: {}",
                    if i == 0 { "CHECK" } else { "NEXT" },
                    l.trim()
                )
            })
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
