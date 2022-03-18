use ariadne::{Label, Report, ReportKind, Source};
use chumsky::Parser;
use execution::execute;
use imperative_parser::{case_parser, main_parser};

mod execution;

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let (case_src, end) = case_parser().parse(src.clone()).unwrap();

    let case_src = format!(
        "{}{}",
        case_src
            .iter()
            .map(|(i, s)| {
                format!(
                    "{}\"{}\"",
                    i.iter().collect::<String>().to_ascii_lowercase(),
                    s.iter().collect::<String>()
                )
            })
            .collect::<Vec<String>>()
            .join(""),
        end.iter().collect::<String>().to_ascii_lowercase()
    );

    match main_parser().parse(case_src) {
        Ok(ast) => execute(ast),
        Err(parse_errs) => {
            Report::build(ReportKind::Error, (), parse_errs[0].span().start)
                .with_label(Label::new(parse_errs[0].span()))
                .finish()
                .print(Source::from(src))
                .unwrap();
        }
    }
}
