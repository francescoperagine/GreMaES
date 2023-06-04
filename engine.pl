:- use_module(library(lists)).
:- use_module(library(apply_macros)).

:- dynamic fact/1.
:- dynamic usedfact/1.
:- dynamic rule/3.
:- leash(none).

debug(off).

% assert_fact/1
assert_fact(Fact):-
    \+ (Fact),
    asserta(Fact),
    !.                              
assert_fact(_).

% forward/0
forward :- done, listing(history).
forward :- 
    fact(Fact),
    not(pursuit(Fact)),
    retract(fact(Fact)),
    assertz(usedfact(Fact)),
    forward.

% done/0
done :- not(fact(X)).

% pursuit/1
pursuit(Fact) :-
    rule(RuleID,Head,Conditions),
    rule_pursuit(Fact,RuleID,Head,Conditions),
    fail.
% rule_pursuit/4
% Searches through rules conditions, deleting entries that match the fact F
rule_pursuit(Fact,RuleID,Head,Conditions) :-
    match_conditions(Fact,Conditions),
    delete_fact(Fact,Conditions,ConditionsNew),
    new_rule(RuleID,Head,ConditionsNew),
    save_history(Fact,RuleID-Head).

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

% new_rule/2
% When the right-hand sided of a rule is empty a new fact is made, otherwise the rule is updated.
% asserta forces the focus-of-attention on new facts, therefore last found facts will be pursued first.
new_rule(RuleID,Head,[]) :-
    not(fact(Head)),
    asserta(fact(Head)),
    (debug(on) -> writeln(Head) ; true).
new_rule(RuleID,Head,Conditions) :-
    not(Conditions=[]),
    asserta(rule(RuleID,Head,Conditions)),
    (debug(on) -> writeln(rule(RuleID,Head,Conditions)) ; true).

% save_history/2
save_history(Item,Action) :-
    not(history(Item,Action)),
    assertz(history(Item,Action)),
    !.
save_history(_,_).