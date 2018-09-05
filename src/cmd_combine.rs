use std::collections::BTreeSet;
use indexmap::IndexSet;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Poll<S> {
    teams: BTreeSet<S>,
    question: S,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

use combine::{
    ParseError, stream::FullRangeStream, easy::Errors,
    token, tokens2, value, optional, eof, try, one_of, none_of, skip_many1,
    sep_end_by1, many1, char::{spaces, space},
    parser::{range::recognize, repeat::skip_until},
};

//==============================================================================
// Parser helpers
//==============================================================================

/// Constructs a named parser over the input type user for command parsing.
macro_rules! named {
    ($name:ident($($arg:ident: $argt:ty),*) -> $out:ty, $body:expr) => {
        fn $name<'s, I>($($arg: $argt),*)
            -> impl Parser<Input = I, Output = $out>
        where
            I: FullRangeStream<Item = char, Range = &'s str>,
            I::Error: ParseError<I::Item, I::Range, I::Position>,
        {
            $body
        }
    };
}

/// Parser which recognizes a case insensitive match on the given tag.
named!(tag_nc(tag: &'s str) -> &'s str, recognize(tokens2(
    |l, r: char| l.to_lowercase().eq(r.to_lowercase()),
    tag.chars(),
)));

/// Parser which eats \s+.
named!(spaces1() -> (), (space(), spaces().silent()).map(|_| ()));

/// Parser which eats the start of an inline command.
named!(inline_start() -> char, token('{'));

/// Parser which eats the end of an inline command.
named!(inline_end() -> char, token('}'));

/// Parser which recognizes the remainder of a line or if `inline == true`
/// then it matches until `inline_end()`.
named!(line_remainder(inline: IsInline) -> &'s str,
    recognize((
        none_of(" \t".chars()),
        if let IsInline::Yes = inline {
            skip_until(inline_end()).left()
        } else {
            skip_until(eof()).right()
        },
    )).map(|s: &str| s.trim())
);

/// Constructs a parser which recognizes any of the given identifiers.
/// At least two idents are needed. The first ident will be the one reported
/// as expected on error.
macro_rules! oneof_nc {
    ($tagf:ident | $($tag:ident)|+) => {
        choice!(
            try(tag_nc(stringify!($tagf)).expected(stringify!($tagf))),
            $(try(tag_nc(stringify!($tag)))),+
        )
    };
}

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
// Command parsers
//==============================================================================

/// Constructs a named command parser.
macro_rules! command {
    // Just delegate to oneof_nc!(..) for simple parsers.
    ($cmd:ident($($arg:ident: $argt: ty),*), $val:expr, $($tag:ident)|+) => {
        named!($cmd($($arg: $argt),*) -> Command<&'s str>,
            oneof_nc!($($tag)|+).with(value($val)));
    };
    // Need more complex logic.
    ($cmd:ident($($arg:ident: $argt:ty),*), $($parser:tt)+) => {
        named!($cmd($($arg: $argt),*) -> Command<&'s str>, $($parser)+);
    };
}

/// Parser recognizing a `merge` command.
/// Example: `@rfcbot merge`.
command!(merge(), Command::Merge, merge | merging | merged | merges);

/// Parser recognizing a `close` command.
/// Example: `@rfcbot close`.
command!(close(), Command::Close, close | closing | closed | closes);

/// Parser recognizing a `postpone` command.
/// Example: `@rfcbot postpone`.
command!(postpone(), Command::Postpone, postpone | postponing | postponed | postpones);

/// Parser recognizing a `cancel` command.
/// Example: `@rfcbot cancel`.
command!(cancel(), Command::Cancel, cancel | canceling | canceled | cancels);

/// Parser recognizing a `reviewed` command.
/// Example: `@rfcbot reviewed`.
command!(reviewed(), Command::Reviewed, reviewed | reviewing | reviews | review);

/// Parser recognizing a `hold` command.
/// Example: `@rfcbot hold`.
command!(hold(), Command::Hold, hold | holding | holds | held);

/// Parser recognizing a concern message.
named!(concern_msg(inline: IsInline) -> &'s str,
    spaces1().with(line_remainder(inline).expected(ERR_NO_CONCERN_MSG)));

/// Parser recognizing a `concern` command.
/// Example: `@rfcbot concern this seems wrong`.
command!(concern(inline: IsInline),
    oneof_nc!(concern | concerning | concerned | concerns)
        .with(concern_msg(inline).message(ERR_CONCERN))
        .map(Command::Concern));

/// Parser recognizing a `resolve` command.
/// Example: `@rfcbot resolve this seems wrong`.
command!(resolve(inline: IsInline),
    oneof_nc!(resolve | resolving | resolved | resolves)
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
    poll | polling | polled | polls |
    asking | asked | asks | ask |
    querying | queried | queries | query |
    inquiring | inquired | inquires | inquire |
    quizzing | quizzed | quizzes | quiz |
    surveying | surveyed | surveys | survey
));

/// Parser recognizing a list of teams to poll.
named!(poll_teams(inline: IsInline) -> BTreeSet<&'s str>, sep_end_by1(
    {
        // The idea here is that we construct the complement of a lexically
        // valid team and then we find as long a string as possible which
        // doesn't match that.
        let neg = ">, \t".chars();
        let nt = one_of(neg.clone());
        let nt = if let IsInline::Yes = inline {
            inline_end().or(nt).left()
        } else {
            nt.right()
        };
        recognize((none_of(neg), skip_until(nt))).expected(ERR_NO_TEAM)
    },
    skip_many1(one_of(", \t".chars()))
));

/// Parser recognizing a poll command.
/// Example: `@rfcbot poll lang, compiler > Isn't this nice?`
command!(poll(inline: IsInline),
    struct_parser! { Poll {
        _: poll_token(),
        _: spaces1(),
        teams: poll_teams(inline).message(ERR_NO_TEAMS),
        _: token('>').message(ERR_TEAMS_QSEP),
        _: spaces().silent(),
        question: line_remainder(inline)
            .expected(ERR_NO_QUESTION)
            .message(ERR_POLL),
    }}
    .map(Command::Poll)
);

/// Parser recognizing all the subcommands without
/// the leading `@rfcbot` invocation.
command!(subcommand(inline: IsInline), choice!(
    hold(), merge(), close(), postpone(), cancel(), reviewed(),
    feedback_req(inline),
    concern(inline),
    resolve(inline),
    poll(inline)
));

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
    .skip(optional((oneof_nc!(fcp | pr), spaces1())))
    // Finally the subcommand:
    .with({
        // Don't ask why we are doing the same thing in both branches;
        // error messages get weird if we merge the branches.
        let sub = subcommand(inline);
        if let IsInline::Yes = inline {
            sub.message(ERR_MSG_COMMAND).left()
        } else {
            sub.message(ERR_MSG_COMMAND).right()
        }
    })
    // This marks that the error was an actual parse error.
    // When "@rfcbot" hasn't been parsed yet, we don't know that it is an error.
    // Once it is parsed, we expect subsequent bits to follow our grammar.
    .message(ACTUAL_ERROR_MARK)
));

/// Parser recognizing an inline rfcbot invocation in the form of:
/// `some text {@rfcbot command...}`.
///
/// Example: `I think we should totally {@rfcbot merge} this`.
command!(inline_invocation(),
    (skip_until(inline_start()), inline_start(), spaces().silent())
        .silent()
        .with(invocation(IsInline::Yes))
        .skip((spaces().silent(), inline_end()))
);

/// Parser recognizing rfcbot commands in a single line;
/// This parser expects that any text has been split up into lines before.
named!(line_parser() -> IndexSet<Command<&'s str>>, choice!(
    invocation(IsInline::No).map(|cmd| indexset![cmd]),
    many1(inline_invocation())
));

/// Are we parsing in an inline (`{...}`) context?
#[derive(Copy, Clone)]
enum IsInline { Yes, No }


/*
pub fn parse(text: &str) -> impl Iterator<Item = IResult<&str, PCommand<'_>>> {
    text.lines().map(line_parser)
}
*/

use combine::Parser;
use combine::stream::state::State;
use combine::easy::Stream;
use combine::easy::Info;
use combine::easy::Error;

pub fn main() {
    let _input = "{@rfcbot poll a, b >";
    //let _input = "@rfcbot concern";
    //let _input = "@rfcbot f? centril";

    match line_parser().parse(Stream(State::new(_input))) {
        Ok(res) => println!("{:#?}", res),
        Err(mut err) => {
            println!("actual error? {}", has_actual_error(&mut err));
            println!("{}\n", err);
            println!("{:#?}", err);
        },
    }
}
