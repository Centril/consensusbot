use std::collections::BTreeSet;

use nom::{space1, space0, non_empty, IResult};

#[derive(Debug, PartialEq, Eq)]
pub struct Poll<S> {
    teams: BTreeSet<S>,
    question: S,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Command<S> {
    Merge,
    Close,
    Postpone,
    Cancel,
    Reviewed,
    Concern(S),
    Resolve(S),
    Hold,
    Poll(Poll<S>),
    FeedbackRequest(S),
}

type PCommand<'a> = Command<&'a str>;


macro_rules! oneof_nc {
    ($i: expr, $($tag: ident)|+) => {
        alt_complete!($i, $(tag_no_case!(stringify!($tag)))|*)
    };
}

macro_rules! command {
    ($cmd: ident, $val: expr, $( $tag: ident)|+) => {
        named!(pub $cmd<&str, PCommand<'_>>, value!($val, oneof_nc!($($tag)|+)));
    };
    ($cmd: ident, $($parser: tt)+) => {
        named!(pub $cmd<&str, PCommand<'_>>, $($parser)+);
    };
}

command!(merge, Command::Merge, merging | merged | merges | merge);
command!(close, Command::Close, closing | closed | closes | close);
command!(postpone, Command::Postpone, postponing | postponed | postpones | postpone);
command!(cancel, Command::Cancel, canceling | canceled | cancels | cancel);
command!(reviewed, Command::Reviewed, reviewing | reviewed | reviews | review);
command!(hold, Command::Hold, holding | holds | hold | held);

named!(not_inline<&str, &str>, alt_complete!(is_not!("}}") | non_empty));

named!(line_remainder<&str, &str>, do_parse!(
    space1 >>
    line: not_inline >>
    (line.trim())
));

command!(concern, do_parse!(
    oneof_nc!(concerning | concerned | concerns | concern) >>
    line: line_remainder >>
    (Command::Concern(line))
));

command!(resolve, do_parse!(
    oneof_nc!(resolving | resolved | resolves | resolve) >>
    line: line_remainder >>
    (Command::Resolve(line))
));

named!(poll_token<&str, &str>, oneof_nc!(
    asking | asked | asks | ask |
    polling | polled | polls | poll |
    querying | queried | queries | query |
    inquiring | inquired | inquires | inquire |
    quizzing | quizzed | quizzes | quiz |
    surveying | surveyed | surveys | survey
));

named!(poll_teams<&str, BTreeSet<&str>>, map!(
    ws!(terminated!(
        separated_list_complete!(
            is_a!(", \t\n\r"),
            verify!(is_not!(">, \t\n\r"), |s: &str| !s.ends_with("}}"))
        ),
        opt!(is_a!(", \t\n\r"))
    )),
    |teams| teams.into_iter().collect()
));

command!(poll, do_parse!(
    poll_token >>
    space1 >>
    teams: poll_teams >>
    tag!(">") >>
    question: preceded!(space0, not_inline) >>
    (Command::Poll(Poll { teams, question: question.trim() }))
));

command!(feedback_req, do_parse!(
    tag_no_case!("f?") >>
    space1 >>
    tag!("@") >>
    user: not_inline >>
    (Command::FeedbackRequest(user.trim()))
));

command!(subcommand, alt_complete!(
    hold | merge | close | postpone | cancel | reviewed |
    feedback_req | concern | resolve | poll
));

command!(invocation, do_parse!(
    ws!(preceded!(tag!("@rfcbot"), opt!(tag!(":")))) >>
    opt!(terminated!(oneof_nc!(fcp | pr), space1)) >>
    cmd: subcommand >>
    (cmd)
));

command!(inline_invocation, do_parse!(
    take_until_and_consume!("{{") >>
    cmd: invocation >>
    space0 >> tag!("}}") >>
    (cmd)
));

command!(line_parser, alt_complete!(invocation | inline_invocation));

pub fn parse(text: &str) -> impl Iterator<Item = IResult<&str, PCommand<'_>>> {
    text.lines().map(line_parser)
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse_vec_ok(text: &str) -> Vec<PCommand<'_>> {
        parse(text).filter_map(Result::ok).map(|(_, cmd)| cmd).collect::<Vec<_>>()
    }

    #[test]
    fn multiple_commands() {
let text = r#"
someothertext
@rfcbot: resolved CONCERN_NAME
somemoretext
somemoretext
@rfcbot: fcp cancel
foobar
@rfcbot concern foobar
"#;

        assert_eq!(parse_vec_ok(text), vec![
            Command::Resolve("CONCERN_NAME"),
            Command::Cancel,
            Command::Concern("foobar"),
        ]);
    }

    #[test]
    fn accept_leading_whitespace() {
let text = r#"
someothertext
       @rfcbot: resolved CONCERN_NAME
somemoretext
somemoretext
   @rfcbot: fcp cancel
foobar
 @rfcbot concern foobar
"#;

        assert_eq!(parse_vec_ok(text), vec![
            Command::Resolve("CONCERN_NAME"),
            Command::Cancel,
            Command::Concern("foobar"),
        ]);
    }

    #[test]
    fn fix_issue_225() {
let text = r#"
someothertext
    @rfcbot : resolved CONCERN_NAME
somemoretext
somemoretext
@rfcbot : fcp cancel
foobar
@rfcbot : concern foobar
"#;

        assert_eq!(parse_vec_ok(text), vec![
            Command::Resolve("CONCERN_NAME"),
            Command::Cancel,
            Command::Concern("foobar"),
        ]);
    }

    fn ensure_take_singleton<I: IntoIterator>(iter: I) -> I::Item {
        let mut iter = iter.into_iter();
        let singleton = iter.next().unwrap();
        assert!(iter.next().is_none());
        singleton
    }

    macro_rules! test_from_str {
        ($test: ident, $expected: expr, $message: expr, [$($cmd: expr),+]) => {
            test_from_str!($test, $expected, [$(concat!($cmd, $message)),+]);
        };

        ($test: ident, $expected: expr, [$($cmd: expr),+]) => {
            #[test]
            fn $test() {
                let expected = $expected;

                $({
                    let tests = [
                        concat!(concat!("@rfcbot : ", $cmd), "\n\nfoobar"),
                        concat!(concat!("@rfcbot: ", $cmd), "\n\nfoobar"),
                        concat!(concat!("@rfcbot ", $cmd), "\n\nfoobar"),
                        concat!(concat!("@rfcbot : fcp ", $cmd), "\n\nfoobar"),
                        concat!(concat!("@rfcbot: fcp ", $cmd), "\n\nfoobar"),
                        concat!(concat!("@rfcbot fcp ", $cmd), "\n\nfoobar"),
                        concat!(concat!("@rfcbot : pr ", $cmd), "\n\nfoobar"),
                        concat!(concat!("@rfcbot: pr ", $cmd), "\n\nfoobar"),
                        concat!(concat!("@rfcbot pr ", $cmd), "\n\nfoobar"),
                        concat!(concat!("{{@rfcbot : ", $cmd), "}}\tabcd"),
                        concat!(concat!("bla bla {{@rfcbot: ", $cmd), "}}  \tabcd"),
                        concat!(concat!("bla bla {{@rfcbot ", $cmd), "}} abcd"),
                        concat!(concat!("bla bla{{@rfcbot : fcp ", $cmd), "}}\tabcd"),
                        concat!(concat!("bla bla{{  @rfcbot: fcp ", $cmd), "}}\tabcd"),
                        concat!(concat!("bla bla {{  @rfcbot fcp ", $cmd), "  }}\tabcd"),
                        concat!(concat!("bla bla{{@rfcbot : pr ", $cmd), " }}\tabcd"),
                        concat!(concat!("bla bla{{ @rfcbot: pr ", $cmd), " }}\tabcd"),
                        concat!(concat!("bla bla {{@rfcbot pr ", $cmd), "}}\tabcd"),
                    ];
                    for test in &tests {
                        println!("{}", test);
                        assert_eq!(
                            expected,
                            ensure_take_singleton(parse_vec_ok(test))
                        );
                    }
                })+
            }
        };
    }

    test_from_str!(success_merge, Command::Merge,
        ["merge", "merged", "merging", "merges"]);

    test_from_str!(success_reviewed, Command::Reviewed,
        ["reviewed", "review", "reviewing", "reviews"]);

    test_from_str!(success_close, Command::Close,
        ["close", "closed", "closing", "closes"]);

    test_from_str!(success_postpone, Command::Postpone,
        ["postpone", "postponed", "postponing", "postpones"]);

    test_from_str!(success_cancel, Command::Cancel,
        ["cancel", "canceled", "canceling", "cancels"]);

    test_from_str!(success_hold, Command::Hold,
        ["hold", "holds", "held", "holding"]);

    test_from_str!(success_concern, Command::Concern("CONCERN_NAME"),
        " CONCERN_NAME",
        ["concern", "concerned", "concerning", "concerns"]);

    test_from_str!(success_resolve, Command::Resolve("CONCERN_NAME"),
        " CONCERN_NAME",
        ["resolve", "resolved", "resolving", "resolves"]);

    test_from_str!(success_feedback, Command::FeedbackRequest("bob"),
        " @bob", ["f?"]);

    test_from_str!(success_ask_question,
        Command::Poll(Poll {
            teams: btreeset! {
                "avengers",
                "T-justice-league",
                "@rust-lang/rust",
            },
            question: "TO BE OR NOT TO BE?",
        }),
        " avengers T-justice-league, @rust-lang/rust  , > TO BE OR NOT TO BE?",
        ["ask", "asked", "asking", "asks",
         "poll", "polled", "polling", "polls",
         "query", "queried", "querying", "queries",
         "inquire", "inquired", "inquiring", "inquires",
         "quiz", "quizzed", "quizzing", "quizzes",
         "survey", "surveyed", "surveying", "surveys"]);

    #[test]
    fn success_resolve_mid_body() {
        let body = "someothertext
@rfcbot: resolved CONCERN_NAME
somemoretext
somemoretext";
        let body_no_colon = "someothertext
somemoretext
@rfcbot resolved CONCERN_NAME
somemoretext";

        let with_colon = ensure_take_singleton(parse_vec_ok(body));
        let without_colon = ensure_take_singleton(parse_vec_ok(body_no_colon));

        assert_eq!(with_colon, without_colon);
        assert_eq!(with_colon, Command::Resolve("CONCERN_NAME"));
    }
}
