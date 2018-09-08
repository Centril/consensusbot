//! Defines the abstract syntax of @rfcbot commands.

use std::collections::BTreeSet;

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
    Merge(TeamSet<S>),
    Close(TeamSet<S>),
    Postpone(TeamSet<S>),
    AddTeam(TeamSet<S>),
    RemoveTeam(TeamSet<S>),
    Cancel,
    Reviewed,
    Concern(S),
    Resolve(S),
    Hold,
    Poll(Poll<S>),
    FeedbackRequest(S),
}
