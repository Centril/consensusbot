//! Defines the concrete syntax for @rfcbot commands and parses it to an AST.

use std::fmt;
use std::iter;

use command::ast::{SCommand, Command, Poll, TeamSet};

use combine::{
    Parser, ParseError as CError, stream::FullRangeStream,
    easy::{Errors, Info, Error},
    stream::state::{State, SourcePosition},
    any, token, tokens2, value, optional, eof, try, one_of, none_of,
    skip_many, skip_many1, sep_end_by1, many1, char::{spaces, space},
    parser::{range::recognize, repeat::skip_until},
};

/// Parses an `@rfcbot ...` command from an input string and gives back
/// either a list of `Command`s each in the form of an AST or returns
/// a `ParseError` if any command could not be successfully parsed.
pub fn parse(input: &str) -> Result<Vec<SCommand<'_>>, ParseError<'_>> {
    let mut commands = vec![];
    let mut errors = vec![];

    // Parse line by line, adding parsed commands and errors
    // to separate lists.
    for (line_no, input) in input.lines().enumerate() {
        // combine will see each line as starting from `1` in error reporting,
        // so we need to offset the starting point line by the line number.
        let pos = SourcePosition { column: 1, line: 1 + line_no as i32 };
        let state = State::with_positioner(input, pos);

        // Parse the line:
        match line_parser().easy_parse(state) {
            Ok((cmd, _)) => commands.extend(cmd),
            // We look for an actual error -- combine will think that
            // lines we don't actually consider to be attempts at command
            // invocations to be parse errors; We need to make sure that
            // those are not considered to be errors.
            Err(mut err) => if has_actual_error(&mut err) {
                errors.push(err)
            }
        }
    }

    // If we had any errors, we don't get any commands;
    // but we want both successful commands and errors to
    // be recorded in error messages.
    if errors.is_empty() {
        Ok(commands)
    } else {
        Err(ParseError {
            commands,
            errors
        })
    }
}

/// Parse errors detected during parsing;
/// If there are any errors, we don't produce any commands
/// to preserve a degree of atomicity in commands.
#[derive(Debug)]
pub struct ParseError<'s> {
    /// Successfully parsed commands.
    commands: Vec<SCommand<'s>>,
    /// Any parse errors that occurred.
    errors: Vec<Errors<char, &'s str, SourcePosition>>,
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        // Header:
        writeln!(fmt, "Failed to parse @rfcbot commands.\n")?;

        // Errors:
        writeln!(fmt, "The following errors were found:\n")?;

        for (idx, error) in self.errors.iter().enumerate() {
            writeln!(fmt, "({}) {}", idx + 1, error)?;
        }

        if self.commands.is_empty() {
            return Ok(());
        }

        // Commands:
        writeln!(fmt, "The following commands were successfully parsed:\n")?;
        for (idx, cmd) in self.commands.iter().enumerate() {
            writeln!(fmt, "({}) `{}`", idx + 1, cmd.linearize())?;
        }

        // Done:
        Ok(())
    }
}

//==============================================================================
// Error detection
//==============================================================================

/// A message we add when `@rfcbot` has been at least parsed so we can
/// distinguish between non-commands and commands that are ill-formed.
const ACTUAL_ERROR_MARK: &str = "__RFCBOT_PARSE_ERROR__";

/// Determine if the parse error was an actual parse error or
/// if it was due to a non-command not conforming.
///
/// Note that this is not a pure function;
/// calling this again on the same `err` will potentially modify its state.
/// In particular, if calling the function once returns true it won't again.
fn has_actual_error<I, R, P>(err: &mut Errors<I, R, P>) -> bool {
    let mut found = false;
    err.errors.retain(|err|
        // 
        if let Error::Message(Info::Borrowed(ACTUAL_ERROR_MARK)) = err {
            found = true;
            false
        } else {
            true
        }
    );
    found
}

//==============================================================================
// Parser helper macros
//==============================================================================

/// Constructs a named parser over the input type user for command parsing.
macro_rules! named {
    ($name:ident($($arg:ident: $argt:ty),*) -> $out:ty, $body:expr) => {
        fn $name<'s, I>($($arg: $argt),*)
            -> impl Parser<Input = I, Output = $out>
        where
            I: FullRangeStream<Item = char, Range = &'s str>,
            I::Error: CError<I::Item, I::Range, I::Position>,
        {
            $body
        }
    };
}

/// Constructs a parser which recognizes any of the given tags.
/// Tags marked `report` will be reported as expected on error.
macro_rules! oneof_nc {
    (__inner report $tag:expr) => {
        tag_nc($tag).expected($tag)
    };
    (__inner $tag:expr) => {
        tag_nc($tag)
    };
    ( $( [ $($tag:tt)+ ] )+ ) => {
        choice!(
            $(try(oneof_nc!(__inner $($tag)+))),+
        )
    };
}

//==============================================================================
// Command parsers
//==============================================================================

/// Are we parsing in an inline (`{...}`) context?
#[derive(Copy, Clone)]
enum IsInline { Yes, No }

/// Constructs a named command parser.
macro_rules! command {
    // Just delegate to oneof_nc!(..) for simple parsers.
    ($cmd:ident($($arg:ident: $argt: ty),*), $val:expr, $($tag:tt)+) => {
        named!($cmd($($arg: $argt),*) -> SCommand<'s>,
            oneof_nc!($($tag)+).with(value($val)));
    };
    // Need more complex logic.
    ($cmd:ident($($arg:ident: $argt:ty),*), $($parser:tt)+) => {
        named!($cmd($($arg: $argt),*) -> SCommand<'s>, $($parser)+);
    };
}

/// Parser recognizing rfcbot commands in a single line;
/// This parser expects that any text has been split up into lines before.
named!(line_parser() -> Vec<SCommand<'s>>, spaces0().with(choice!(
    invocation(IsInline::No).map(|cmd| vec![cmd]),
    many1(inline_invocation())
)));

/// Parser recognizing an inline rfcbot invocation in the form of:
/// `some text {@rfcbot command...}`.
///
/// Example: `I think we should totally {@rfcbot merge} this`.
command!(inline_invocation(),
    (skip_until(token(INLINE_START)), token(INLINE_START), spaces0())
        .silent()
        .with(invocation(IsInline::Yes))
        .skip((spaces0(), token(INLINE_END)))
);

/// Parser which recognizes a full rfcbot invocation including
/// `@rfcbot` optional colon, optional "fcp" / "pr", and the subcommand.
///
/// This parser is parameterized on whether we are in an inline context or not.
command!(invocation(inline: IsInline), try(tag_nc("@rfcbot")).with(
    choice!(
        // If there is a colon then you don't need a leading space.
        try(spaces().skip(token(':')).skip(spaces())),
        // If there's no colon then we need at least one space.
        spaces1()
    )
    // Optional `fcp` or `pr` prefix:
    .skip(optional((oneof_nc!(["fcp"] ["pr"]), spaces1())))
    // Finally the subcommand:
    .with({
        // Don't ask why we are doing the same thing in both branches;
        // error messages get weird if we merge the branches.
        let sub = subcommand(inline);
        if let IsInline::Yes = inline {
            sub.message(ERR_MSG_COMMAND).left()
        } else {
            // TODO!
            sub.skip(choice!(eof(), spaces1().skip(skip_many(any()))))
               .message(ERR_MSG_COMMAND).right()
        }
    })
    // This marks that the error was an actual parse error.
    // When "@rfcbot" hasn't been parsed yet, we don't know that it is an error.
    // Once it is parsed, we expect subsequent bits to follow our grammar.
    .message(ACTUAL_ERROR_MARK)
));

/// Parser recognizing all the subcommands without
/// the leading `@rfcbot` invocation.
command!(subcommand(inline: IsInline), choice!(
    merge(inline), close(inline), postpone(inline),
    concern(inline), resolve(inline),
    cancel(), reviewed(), hold(),
    poll(inline), feedback_req(inline),
    add_team(inline), remove_team(inline)
));

//==============================================================================
// Command parsers: Subcommands
//==============================================================================

/// Parser recognizing a `cancel` command.
/// Example: `@rfcbot cancel`.
command!(cancel(), Command::Cancel,
    ["canceling"] ["canceled"] ["cancels"] [report "cancel"]);

/// Parser recognizing a `reviewed` command.
/// Example: `@rfcbot reviewed`.
command!(reviewed(), Command::Reviewed,
    [report "reviewed"] ["reviewing"] ["reviews"] ["review"]);

/// Parser recognizing a `hold` command.
/// Example: `@rfcbot hold`.
command!(hold(), Command::Hold,
    ["holding"] ["holds"] ["held"] [report "hold"]);

/// Parser recognizing a `merge` command.
/// Example: `@rfcbot merge`.
/// Example: `@rfcbot merge lang, compiler`.
command!(merge(inline: IsInline),
    oneof_nc!(["merging"] ["merged"] ["merges"] [report "merge"])
        .with(team_set_opt(inline))
        .map(Command::Merge)
);

/// Parser recognizing a `close` command.
/// Example: `@rfcbot close`.
/// Example: `@rfcbot close lang, compiler`.
command!(close(inline: IsInline),
    oneof_nc!(["closing"] ["closed"] ["closes"] [report "close"])
        .with(team_set_opt(inline))
        .map(Command::Close)
);

/// Parser recognizing a `postpone` command.
/// Example: `@rfcbot postpone`.
/// Example: `@rfcbot postpone lang, compiler`.
command!(postpone(inline: IsInline),
    oneof_nc!(["postponing"] ["postponed"] ["postpones"] [report "postpone"])
        .with(team_set_opt(inline))
        .map(Command::Postpone)
);

/// Parser recognizing an `add-team` command.
/// Example: `@rfcbot add-team lang, compiler`.
command!(add_team(inline: IsInline),
    oneof_nc!(
        ["add-teams"] [report "add-team"] ["add_teams"] ["add_team"]
        ["add teams"] ["add team"] ["adding teams"] ["adding team"]
        ["adds teams"] ["adds team"] ["added teams"] ["added team"]
    )
    .with(team_set(inline).message(ERR_ADD_TEAM))
    .map(Command::AddTeam)
);

/// Parser recognizing a `remove-team` command.
/// Example: `@rfcbot remove-team lang, compiler`.
command!(remove_team(inline: IsInline),
    oneof_nc!(
        ["remove-teams"] [report "remove-team"] ["remove_teams"] ["remove_team"]
        ["remove teams"] ["remove team"] ["removing teams"] ["removing team"]
        ["removes teams"] ["removes team"] ["removed teams"] ["removed team"]
    )
    .with(team_set(inline).message(ERR_REMOVE_TEAM))
    .map(Command::RemoveTeam)
);

/// Parser recognizing a concern message.
named!(concern_msg(inline: IsInline) -> &'s str,
    spaces1().with(line_remainder(inline).expected(ERR_NO_CONCERN_MSG)));

/// Parser recognizing a `concern` command.
/// Example: `@rfcbot concern this seems wrong`.
command!(concern(inline: IsInline),
    oneof_nc!(["concerning"] ["concerned"] ["concerns"] [report "concern"])
        .with(concern_msg(inline).message(ERR_CONCERN))
        .map(Command::Concern));

/// Parser recognizing a `resolve` command.
/// Example: `@rfcbot resolve this seems wrong`.
command!(resolve(inline: IsInline),
    oneof_nc!(["resolving"] ["resolved"] ["resolves"] [report "resolve"])
        .with(concern_msg(inline).message(ERR_RESOLVE))
        .map(Command::Resolve));

/// Parser recognizing a feedback request command.
/// Example: `@rfcbot f? @user`.
command!(feedback_req(inline: IsInline),
    tag_nc("f?").with(
        spaces1().with(
            token('@').with(
                line_remainder(inline).expected(ERR_NO_USER)
            ).expected(ERR_NO_USER)
        ).message(ERR_FEEDBACK_REQUEST)
    ).map(Command::FeedbackRequest)
);

/// Parser recognizing a poll command.
/// Example: `@rfcbot poll lang, compiler > Isn't this nice?`
command!(poll(inline: IsInline),
    struct_parser! { Poll {
        _: oneof_nc!(
            ["polling"] ["polled"] ["polls"] [report "poll"]
            ["asking"] ["asked"] ["asks"] ["ask"]
            ["querying"] ["queried"] ["queries"] ["query"]
            ["inquiring"] ["inquired"] ["inquires"] ["inquire"]
            ["quizzing"] ["quizzed"] ["quizzes"] ["quiz"]
            ["surveying"] ["surveyed"] ["surveys"] ["survey"]
        ),
        teams: team_set_opt(inline),
        _: token('>').message(ERR_TEAMS_QSEP),
        _: spaces0(),
        question: line_remainder(inline)
            .expected(ERR_NO_QUESTION)
            .message(ERR_POLL),
    }}
    .map(Command::Poll)
);

//==============================================================================
// Parser helpers
//==============================================================================

/// Parser recognizing a set of teams.
named!(team_set(inline: IsInline) -> TeamSet<&'s str>, {
    let team = {
        // The idea here is that we construct the complement of a lexically
        // valid team and then we find as long a string as possible which
        // doesn't match that.
        let neg = ">, \t".chars();
        let tt = if let IsInline::Yes = inline {
            none_of(neg.chain(iter::once(INLINE_END))).right()
        } else {
            none_of(neg).left()
        };
        recognize(skip_many1(tt)).expected(ERR_NO_TEAM)
    };
    let sep = skip_many1(one_of(", \t".chars()));
    spaces1().with(sep_end_by1(team, sep).message(ERR_NO_TEAMS))
});

/// Parser recognizing an optional set of teams.
named!(team_set_opt(inline: IsInline) -> TeamSet<&'s str>, choice!(
    try(team_set(inline)),
    spaces().map(|_| btreeset![])
));

/// Parser which recognizes the remainder of a line or if `inline == true`
/// then it matches until `inline_end()`.
named!(line_remainder(inline: IsInline) -> &'s str,
    recognize(if let IsInline::Yes = inline {
        (
            none_of(" \t".chars().chain(iter::once(INLINE_END))),
            skip_until(token(INLINE_END))
        ).left()
    } else {
        (
            none_of(" \t".chars()),
            skip_until(eof())
        ).right()
    }).map(|s: &str| s.trim())
);

/// Parser which recognizes a case insensitive match on the given tag.
named!(tag_nc(tag: &'s str) -> &'s str, recognize(tokens2(
    |l, r: char| l.to_lowercase().eq(r.to_lowercase()),
    tag.chars(),
)));

/// Parser which eats \s+.
named!(spaces1() -> (), space().with(spaces0()));

/// Parser which eats \s*.
named!(spaces0() -> (), spaces().silent());

/// Token starting an inline rfcbot invocation.
const INLINE_START: char = '{';

/// Token ending an inline rfcbot invocation.
const INLINE_END: char = '}';

//==============================================================================
// Parser error messages
//==============================================================================

// These should be fairly self explanatory:

const ERR_NO_CONCERN_MSG: &str = "a concern message";
const ERR_CONCERN: &str = "while parsing a `concern` command";
const ERR_RESOLVE: &str = "while parsing a `resolve` command";
const ERR_NO_USER: &str = "a GitHub user";
const ERR_FEEDBACK_REQUEST: &str = "while parsing a feedback request";
const ERR_NO_QUESTION: &str = "a question to ask";
const ERR_TEAMS_QSEP: &str = "the teams and the question must be separated by `>`;";
const ERR_NO_TEAM: &str = "a team";
const ERR_NO_TEAMS: &str = "while parsing a list of teams,";
const ERR_ADD_TEAM: &str = "while parsing an `add-team` command";
const ERR_REMOVE_TEAM: &str = "while parsing a `remove-team` command";
const ERR_POLL: &str = "while parsing a `poll` command";
const ERR_MSG_COMMAND: &str = "while parsing an rfcbot command";

#[cfg(test)]
mod test {
    use super::*;

    fn parse_vec_ok(text: &str) -> Vec<SCommand<'_>> {
        match parse(text) {
            Ok(commands) => commands,
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn multiple_commands() {
        let text = "
        someothertext\n\
        @rfcbot: hold\n\
        somemoretext\n\
        somemoretext\n\
        @rfcbot: fcp cancel\n\
        foobar\n\
        @rfcbot concern foobar";
        assert_eq!(parse_vec_ok(text), vec![
            Command::Hold,
            Command::Cancel,
            Command::Concern("foobar"),
        ]);
    }

    #[test]
    fn accept_leading_whitespace() {
        let text = "
        someothertext\n
            @rfcbot: hold\n
        somemoretext\n
        somemoretext\n
        @rfcbot: fcp cancel\n
        foobar\n
        @rfcbot concern foobar\n
        ";
        assert_eq!(parse_vec_ok(text), vec![
            Command::Hold,
            Command::Cancel,
            Command::Concern("foobar"),
        ]);
    }

    #[test]
    fn fix_issue_225() {
        let text = "
        someothertext\n\
            @rfcbot : hold\n\
        somemoretext\n\
        somemoretext\n\
        @rfcbot : fcp cancel\n\
        foobar\n\
        @rfcbot :concern foobar\n\
        barfoo\n\
        @rfcbot:f? @foo\n\
        ";
        assert_eq!(parse_vec_ok(text), vec![
            Command::Hold,
            Command::Cancel,
            Command::Concern("foobar"),
            Command::FeedbackRequest("foo"),
        ]);
    }

    #[test]
    fn success_resolve_mid_body() {
        let body = "
            someothertext\n\
            @rfcbot: resolved CONCERN_NAME\n\
            somemoretext\n\
            somemoretext";
        let body_no_colon = "
            someothertext\n\
            somemoretext\n\
            @rfcbot resolved CONCERN_NAME\n\
            somemoretext";

        let with_colon = ensure_take_singleton(parse_vec_ok(body));
        let without_colon = ensure_take_singleton(parse_vec_ok(body_no_colon));

        assert_eq!(with_colon, without_colon);
        assert_eq!(with_colon, Command::Resolve("CONCERN_NAME"));
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
                        concat!(concat!("{@rfcbot : ", $cmd), "}\tabcd"),
                        concat!(concat!("bla bla {@rfcbot: ", $cmd), "}  \tabcd"),
                        concat!(concat!("bla bla {@rfcbot ", $cmd), "} abcd"),
                        concat!(concat!("bla bla{@rfcbot : fcp ", $cmd), "}\tabcd"),
                        concat!(concat!("bla bla{  @rfcbot: fcp ", $cmd), "}\tabcd"),
                        concat!(concat!("bla bla {  @rfcbot fcp ", $cmd), "  }\tabcd"),
                        concat!(concat!("bla bla{@rfcbot : pr ", $cmd), " }\tabcd"),
                        concat!(concat!("bla bla{ @rfcbot: pr ", $cmd), " }\tabcd"),
                        concat!(concat!("bla bla {@rfcbot pr ", $cmd), "}\tabcd"),
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

    test_from_str!(success_hold, Command::Hold,
        ["hold", "holds", "held", "holding"]);

    test_from_str!(success_reviewed, Command::Reviewed,
        ["reviewed", "review", "reviewing", "reviews"]);

    test_from_str!(success_cancel, Command::Cancel,
        ["cancel", "canceled", "canceling", "cancels"]);

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

    test_from_str!(success_merge, Command::Merge(btreeset!{}),
        ["merge", "merged", "merging", "merges"]);

    test_from_str!(success_merge_teamed,
        Command::Merge(btreeset! { "lang", "compiler" }),
        ["merge lang, compiler", "merged lang compiler,",
         "merging lang, compiler,", "merges lang compiler"]);

    test_from_str!(success_postpone, Command::Postpone(btreeset!{}),
        ["postpone", "postponed", "postponing", "postpones"]);

    test_from_str!(success_postpone_teamed,
        Command::Postpone(btreeset! { "lang", "compiler" }),
        ["postpone lang, compiler", "postponed lang compiler,",
         "postponing lang, compiler,", "postpones lang compiler"]);

    test_from_str!(success_close, Command::Close(btreeset!{}),
        ["close", "closed", "closing", "closes"]);

    test_from_str!(success_close_teamed,
        Command::Close(btreeset! { "lang", "compiler" }),
        ["close lang, compiler", "closed lang compiler,",
         "closing lang, compiler,", "closes lang compiler"]);

    test_from_str!(success_add_team_teamed,
        Command::AddTeam(btreeset! { "lang", "compiler" }),
        ["add-team lang, compiler", "add-teams lang compiler,",
         "add_teams lang, compiler,", "add_team lang compiler",
         "add team lang compiler", "add teams lang compiler",
         "adds team lang compiler", "adds teams lang compiler",
         "added team lang compiler", "added teams lang compiler",
         "adding team lang compiler", "adding teams lang compiler"]);

    test_from_str!(success_remove_team_teamed,
        Command::RemoveTeam(btreeset! { "lang", "compiler" }),
        ["remove-team lang, compiler", "remove-teams lang compiler,",
         "remove_teams lang, compiler,", "remove_team lang compiler",
         "remove team lang compiler", "remove teams lang compiler",
         "removes team lang compiler", "removes teams lang compiler",
         "removed team lang compiler", "removed teams lang compiler",
         "removing team lang compiler", "removing teams lang compiler"]);

    use proptest::prelude::*;

    fn gen_teams(min: usize) -> impl Strategy<Value = TeamSet<String>> {
        prop::collection::btree_set("[^,\\s>}}]+", min..100)
    }

    fn gen_command() -> impl Strategy<Value = Command<String>> {
        let line_rem = "[^\\s}}]+";
        prop_oneof![
            Just(Command::Cancel),
            Just(Command::Reviewed),
            Just(Command::Hold),
            gen_teams(1).prop_map(Command::AddTeam),
            gen_teams(1).prop_map(Command::RemoveTeam),
            gen_teams(0).prop_map(Command::Merge),
            gen_teams(0).prop_map(Command::Close),
            gen_teams(0).prop_map(Command::Postpone),
            (gen_teams(0), line_rem).prop_map(
                |(teams, question)| Command::Poll(Poll { teams, question })
            ),
            line_rem.prop_map(Command::Concern),
            line_rem.prop_map(Command::Resolve),
            line_rem.prop_map(Command::FeedbackRequest),
        ]
    }

    proptest! {
        #[test]
        fn parser_doesnt_crash(string in "\\PC*") {
            let _ = parse(&string);
        }

        #[test]
        fn parser_can_roundtrip(cmd1 in gen_command()) {
            let linearized = cmd1.linearize();
            let cmd2 = ensure_take_singleton(parse_vec_ok(&linearized));
            let cmd3 = cmd1.map(String::as_ref);
            assert_eq!(cmd3, cmd2);
        }

        #[test]
        fn parser_can_roundtrip_inline(
            cmd1 in gen_command(),
            mut front in INLINE_FRONT,
            back in INLINE_BACK,
        ) {
            let linearized = cmd1.linearize();
            front.push_str(&*linearized);
            front.push_str(&*back);

            let cmd2 = ensure_take_singleton(parse_vec_ok(&front));
            let cmd3 = cmd1.map(String::as_ref);
            assert_eq!(cmd3, cmd2);
        }
    }

    const INLINE_FRONT: &str = r"[^\{\n\r]*\{[ \t]*";
    const INLINE_BACK: &str = r"[ \t]*\}[^\{]+";
}
