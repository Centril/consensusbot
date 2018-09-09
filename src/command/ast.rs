//! Defines the abstract syntax of @rfcbot commands.

use std::collections::BTreeSet;
use std::borrow::Cow;

use itertools::Itertools;

/// A set of teams for which a command is relevant in some way.
pub type TeamSet<S> = BTreeSet<S>;

/// The details of a poll command.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Poll<S> {
    /// The explicit list of teams to poll.
    pub teams: TeamSet<S>,
    /// The question to ask of the teams.
    pub question: S,
}

/// The AST for an @rfcbot command.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Command<S> {
    /// An `@rfcbot merge $teams>?` command.
    Merge(TeamSet<S>),
    /// An `@rfcbot close $teams>?` command.
    Close(TeamSet<S>),
    /// An `@rfcbot postpone $teams?` command.
    Postpone(TeamSet<S>),
    /// An `@rfcbot add-team $teams` command.
    AddTeam(TeamSet<S>),
    /// An `@rfcbot remove-team $teams` command.
    RemoveTeam(TeamSet<S>),
    /// An `@rfcbot cancel` command.
    Cancel,
    /// An `@rfcbot reviewed` command.
    Reviewed,
    /// An `@rfcbot concern $concern` command.
    Concern(S),
    /// An `@rfcbot resolve $concern` command.
    Resolve(S),
    /// An `@rfcbot hold` command.
    Hold,
    /// An `@rfcbot unhold` command.
    Unhold,
    /// An `@rfcbot poll $teams? > $question` command.
    Poll(Poll<S>),
    /// An `@rfcbot f? @$github_user` command.
    FeedbackRequest(S),
}

impl<S: AsRef<str>> Poll<S> {
    /// Functor mapping on Poll.
    fn map<'s, O: Ord>(&'s self, mapper: impl Fn(&'s S) -> O) -> Poll<O> {
        Poll {
            question: mapper(&self.question),
            teams: self.teams.iter().map(mapper).collect(),
        }
    }
}

impl<S: AsRef<str>> Command<S> {
    /// Linearize an `@rfcbot ..` command.
    pub fn linearize(&self) -> Cow<'static, str> {
        fn add_teams(teams: &TeamSet<impl AsRef<str>>, mut buf: String) -> String {
            buf.extend(teams.iter().map(|s| s.as_ref()).intersperse(", "));
            buf
        }

        fn with_teams(base: &'static str, teams: &TeamSet<impl AsRef<str>>)
            -> Cow<'static, str>
        {
            if teams.is_empty() {
                base.into()
            } else {
                let mut base = String::from(base);
                base.push_str(" ");
                add_teams(&teams, base).into()
            }
        }

        use self::Command::*;
        match self {
            Poll(poll)
                => {
                let mut buf = add_teams(&poll.teams, String::from("@rfcbot poll "));
                buf.push_str(" > ");
                buf.push_str(poll.question.as_ref());
                buf.into()
            },
            Merge(teams)
                => with_teams("@rfcbot merge", teams),
            Close(teams)
                => with_teams("@rfcbot close", teams),
            Postpone(teams)
                => with_teams("@rfcbot postpone", teams),
            AddTeam(teams)
                => with_teams("@rfcbot add-team", teams),
            RemoveTeam(teams)
                => with_teams("@rfcbot remove-team", teams),
            Cancel
                => "@rfcbot cancel".into(),
            Reviewed
                => "@rfcbot reviewed".into(),
            Concern(concern)
                => format!("@rfcbot concern {}", concern.as_ref()).into(),
            Resolve(concern)
                => format!("@rfcbot resolve {}", concern.as_ref()).into(),
            Hold
                => "@rfcbot hold".into(),
            Unhold
                => "@rfcbot unhold".into(),
            FeedbackRequest(gh_user)
                => format!("@rfcbot f? @{}", gh_user.as_ref()).into()
        }
    }

    /// Functor mapping on Command.
    pub fn map<'s, O: Ord>(&'s self, mapper: impl Fn(&'s S) -> O) -> Command<O> {
        use self::Command::*;
        match self {
            Merge(teams) => Merge(teams.iter().map(mapper).collect()),
            Close(teams) => Close(teams.iter().map(mapper).collect()),
            Postpone(teams) => Postpone(teams.iter().map(mapper).collect()),
            AddTeam(teams) => AddTeam(teams.iter().map(mapper).collect()),
            RemoveTeam(teams) => RemoveTeam(teams.iter().map(mapper).collect()),
            Cancel => Cancel,
            Reviewed => Reviewed,
            Concern(concern) => Concern(mapper(concern)),
            Resolve(concern) => Resolve(mapper(concern)),
            Hold => Hold,
            Unhold => Unhold,
            Poll(poll) => Poll(poll.map(mapper)),
            FeedbackRequest(gh_user) => FeedbackRequest(mapper(gh_user)),
        }
    }
}

/// Command AST using string slices.
pub type SCommand<'s> = Command<&'s str>;
