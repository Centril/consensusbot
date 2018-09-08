//! Defines the concrete syntax for @rfcbot commands and parses it to an AST.

use std::fmt;

use command::ast::{Command, Poll, TeamSet};

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
pub fn parse(input: &str) -> Result<Vec<Command<&str>>, ParseError<'_>> {
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
    commands: Vec<Command<&'s str>>,
    /// Any parse errors that occurred.
    errors: Vec<Errors<char, &'s str, SourcePosition>>,
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        // Header:
        writeln!(fmt, "Failed to parse @rfcbot commands.\n")?;

        // Errors:
        writeln!(fmt, "The following errors were found:")?;
        writeln!(fmt, "--------------------------------")?;
        for error in &self.errors {
            fmt::Display::fmt(error, fmt)?;
            writeln!(fmt)?;
        }

        if self.commands.is_empty() {
            return Ok(());
        }

        // Commands:
        writeln!(fmt, "The following commands were successfully parsed:")?;
        writeln!(fmt, "------------------------------------------------")?;
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

/// Constructs a parser which recognizes any of the given identifiers.
/// At least two idents are needed. The first ident will be the one reported
/// as expected on error.
macro_rules! oneof_nc {
    ($tagf:expr, $($tag:expr),+) => {
        choice!(
            try(tag_nc($tagf).expected($tagf)),
            $(try(tag_nc($tag))),+
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
    ($cmd:ident($($arg:ident: $argt: ty),*), $val:expr, $($tag:expr),+) => {
        named!($cmd($($arg: $argt),*) -> Command<&'s str>,
            oneof_nc!($($tag),+).with(value($val)));
    };
    // Need more complex logic.
    ($cmd:ident($($arg:ident: $argt:ty),*), $($parser:tt)+) => {
        named!($cmd($($arg: $argt),*) -> Command<&'s str>, $($parser)+);
    };
}

/// Parser recognizing rfcbot commands in a single line;
/// This parser expects that any text has been split up into lines before.
named!(line_parser() -> Vec<Command<&'s str>>, choice!(
    invocation(IsInline::No).map(|cmd| vec![cmd]),
    many1(inline_invocation())
));

/// Parser recognizing an inline rfcbot invocation in the form of:
/// `some text {@rfcbot command...}`.
///
/// Example: `I think we should totally {@rfcbot merge} this`.
command!(inline_invocation(),
    (skip_until(token(INLINE_START)), token(INLINE_START), spaces().silent())
        .silent()
        .with(invocation(IsInline::Yes))
        .skip((spaces().silent(), token(INLINE_END)))
);

/// Parser which recognizes a full rfcbot invocation including
/// `@rfcbot` optional colon, optional "fcp" / "pr", and the subcommand.
///
/// This parser is parameterized on whether we are in an inline context or not.
command!(invocation(inline: IsInline), tag_nc("@rfcbot").with(
    choice!(
        // If there's no colon then we need at least one space.
        spaces1(),
        // If there is a colon then you don't need a leading space.
        spaces().skip(token(':')).skip(spaces())
    )
    // Optional `fcp` or `pr` prefix:
    .skip(optional((oneof_nc!("fcp", "pr"), spaces1())))
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
    hold(), cancel(), reviewed(),
    merge(inline), close(inline), postpone(inline),
    feedback_req(inline),
    concern(inline),
    resolve(inline),
    poll(inline)
));

//==============================================================================
// Command parsers: Subcommands
//==============================================================================

/// Parser recognizing a `cancel` command.
/// Example: `@rfcbot cancel`.
command!(cancel(), Command::Cancel, "cancel", "canceling", "canceled", "cancels");

/// Parser recognizing a `reviewed` command.
/// Example: `@rfcbot reviewed`.
command!(reviewed(), Command::Reviewed, "reviewed", "reviewing", "reviews", "review");

/// Parser recognizing a `hold` command.
/// Example: `@rfcbot hold`.
command!(hold(), Command::Hold, "hold", "holding", "holds", "held");

/// Parser recognizing a `merge` command.
/// Example: `@rfcbot merge`.
/// Example: `@rfcbot merge lang, compiler`.
command!(merge(inline: IsInline),
    oneof_nc!("merge", "merging", "merged", "merges")
        .with(team_set_opt(inline))
        .map(Command::Merge)
);

/// Parser recognizing a `close` command.
/// Example: `@rfcbot close`.
/// Example: `@rfcbot close lang, compiler`.
command!(close(inline: IsInline),
    oneof_nc!("close", "closing", "closed", "closes")
        .with(team_set_opt(inline))
        .map(Command::Close)
);

/// Parser recognizing a `postpone` command.
/// Example: `@rfcbot postpone`.
/// Example: `@rfcbot postpone lang, compiler`.
command!(postpone(inline: IsInline),
    oneof_nc!("postpone", "postponing", "postponed", "postpones")
        .with(team_set_opt(inline))
        .map(Command::Postpone)
);

/// Parser recognizing an `add-team` command.
/// Example: `@rfcbot add-team lang, compiler`.
command!(add_team(inline: IsInline),
    oneof_nc!(
        "add-team", "add-teams", "add_team", "add_teams", "add team",
        "add teams", "adding team", "adding teams", "adds team",
        "adds teams", "added team", "added teams"
    )
    .with(team_set(inline))
    .map(Command::AddTeam)
);

/// Parser recognizing a `remove-team` command.
/// Example: `@rfcbot remove-team lang, compiler`.
command!(remove_team(inline: IsInline),
    oneof_nc!(
        "removed-team", "remove-teams", "remove_team", "remove_teams", "remove team",
        "removed teams", "removing team", "removing teams", "removes team",
        "removes teams", "removed team", "removed teams"
    )
    .with(team_set(inline))
    .map(Command::RemoveTeam)
);

/// Parser recognizing a concern message.
named!(concern_msg(inline: IsInline) -> &'s str,
    spaces1().with(line_remainder(inline).expected(ERR_NO_CONCERN_MSG)));

/// Parser recognizing a `concern` command.
/// Example: `@rfcbot concern this seems wrong`.
command!(concern(inline: IsInline),
    oneof_nc!("concern", "concerning", "concerned", "concerns")
        .with(concern_msg(inline).message(ERR_CONCERN))
        .map(Command::Concern));

/// Parser recognizing a `resolve` command.
/// Example: `@rfcbot resolve this seems wrong`.
command!(resolve(inline: IsInline),
    oneof_nc!("resolve", "resolving", "resolved", "resolves")
        .with(concern_msg(inline).message(ERR_RESOLVE))
        .map(Command::Resolve));

/// Parser recognizing a feedback request command.
/// Example: `@rfcbot f? @user`.
command!(feedback_req(inline: IsInline),
    tag_nc("f?").with(
        spaces1().with(
            token('@').with(line_remainder(inline)).expected(ERR_NO_USER)
        ).message(ERR_FEEDBACK_REQUEST)
    ).map(Command::FeedbackRequest)
);

/// Parser recognizing the `poll` token and friends.
named!(poll_token() -> &'s str, oneof_nc!(
    "poll", "polling", "polled", "polls",
    "asking", "asked", "asks", "ask",
    "querying", "queried", "queries", "query",
    "inquiring", "inquired", "inquires", "inquire",
    "quizzing", "quizzed", "quizzes", "quiz",
    "surveying", "surveyed", "surveys", "survey"
));

/// Parser recognizing a poll command.
/// Example: `@rfcbot poll lang, compiler > Isn't this nice?`
command!(poll(inline: IsInline),
    struct_parser! { Poll {
        _: poll_token(),
        teams: team_set_opt(inline),
        _: token('>').message(ERR_TEAMS_QSEP),
        _: spaces().silent(),
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
            none_of(neg.chain(::std::iter::once(INLINE_END))).right()
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
    recognize((
        none_of(" \t".chars()),
        if let IsInline::Yes = inline {
            skip_until(token(INLINE_END)).left()
        } else {
            skip_until(eof()).right()
        },
    )).map(|s: &str| s.trim())
);

/// Parser which recognizes a case insensitive match on the given tag.
named!(tag_nc(tag: &'s str) -> &'s str, recognize(tokens2(
    |l, r: char| l.to_lowercase().eq(r.to_lowercase()),
    tag.chars(),
)));

/// Parser which eats \s+.
named!(spaces1() -> (), (space(), spaces().silent()).map(|_| ()));

/// Token starting an inline rfcbot invocation.
const INLINE_START: char = '{';

/// Token ending an inline rfcbot invocation.
const INLINE_END: char = '}';

//==============================================================================
// Parser error messages
//==============================================================================

// These should be fairly self explanatory:

const ERR_NO_CONCERN_MSG: &str = "a concern message";
const ERR_CONCERN: &str = "while parsing a concern command";
const ERR_RESOLVE: &str = "while parsing a resolve command";
const ERR_NO_USER: &str = "a GitHub user";
const ERR_FEEDBACK_REQUEST: &str = "while parsing a feedback request";
const ERR_NO_QUESTION: &str = "a question to ask";
const ERR_TEAMS_QSEP: &str = "the teams and the question must be separated by `>`;";
const ERR_NO_TEAM: &str = "a team";
const ERR_NO_TEAMS: &str = "while parsing a list of teams,";
const ERR_POLL: &str = "while parsing a poll command";
const ERR_MSG_COMMAND: &str = "while parsing an rfcbot command";
