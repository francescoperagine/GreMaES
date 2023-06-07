:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(ordsets)).

:- dynamic fact/2.
:- dynamic fact_history/2.
:- dynamic usedfact/2.
:- dynamic rule/3.
:- leash(none).

debug(off).

% assert_fact/1
assert_fact(Fact):-
    \+ (fact(_,Fact)),
    new_index(ID),
    asserta(fact(ID,Fact)),
    add_fact_history(ID),
    !.                              
assert_fact(_).

% new_index/1
new_index(NewID) :-
    not(last_index(_)),
    all(ID, rule(ID,_,_), IDs),
    max_list(IDs, LastID),
    NewID is LastID + 1,
    assert(last_index(NewID)).
new_index(NewID) :-
    last_index(LastID),
    NewID is LastID + 1,
    retract(last_index(_)),
    assert(last_index(NewID)).

% add_fact_history/1
add_fact_history(ID) :-
    assertz(fact_history([ID])).
% add_fact_history/2
add_fact_history(Prev,Curr) :-
    fact_history(History),
    memberchk(Prev,History),
    retract(fact_history(History)),
    assertz(fact_history([Curr|History])).

% forward/0
forward :- done, list_facts_history.
forward :- 
    fact(FactID,Fact),
    not(pursuit(FactID,Fact)),
    retract(fact(FactID,Fact)),
    assertz(usedfact(FactID,Fact)),
    forward.

% done/0
done :- not(fact(_,_)).

% % list_facts_history/0
list_facts_history :-
    all(H2, (fact_history(H1), reverse(H1,H2)), Hs), maplist(writeln, Hs).

% pursuit/2
pursuit(FactID,Fact) :-
    rule(RuleID,Head,Conditions),
    rule_pursuit(FactID,Fact,RuleID,Head,Conditions),
    fail.
% rule_pursuit/5
% Searches through rules conditions, deleting entries that match the fact F
rule_pursuit(FactID,Fact,RuleID,Head,Conditions) :-
    match_conditions(Fact,Conditions),
    delete_fact(Fact,Conditions,ConditionsNew),
    new_rule(FactID,RuleID,Head,ConditionsNew).

% match_conditions/2
% Checks whether the fact is within rules' conditions
match_conditions(X,[X|L]).
match_conditions(X,[Y|L]) :- match_conditions(X,L).

% delete_fact/3
% Unifies the fact by binding the variable within the conditions and it always succedees
delete_fact(X,[],[]).
delete_fact(X,[X|L],M) :- delete_fact(X,L,M).
delete_fact(X,[Y|L],[Y|M]) :- 
    not(X=Y),
    delete_fact(X,L,M).

% new_rule/4
% When the right-hand sided of a rule is empty a new fact is made, otherwise the rule is updated.
% asserta forces the focus-of-attention on new facts, therefore last found facts will be pursued first.
new_rule(FactID,_,Head,[]) :-
    not(fact(_,Head)),
    new_index(NewID),
    asserta(fact(NewID,Head)),
    add_fact_history(FactID,NewID),
    (debug(on) -> writeln(Head) ; true).
new_rule(FactID,RuleID,Head,Conditions) :-
    not(Conditions=[]),
    asserta(rule(RuleID,Head,Conditions)),
    add_fact_history(FactID,RuleID),
    (debug(on) -> writeln(rule(RuleID,Head,Conditions)) ; true).