:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(ordsets)).

% engine_init/0
engine_init :-
    not(last_index(_)),
    all(ID,rule(ID,_,_),IDs),
    max_list(IDs,LastID),
    NextID is LastID + 1,
    save_index(NextID).

% next_index/1
next_index(NextID) :-
    last_index(LastID),
    NextID is LastID + 1,
    save_index(NextID).

% forward/0
forward :- done.
forward :- 
    fact(ID,Fact),
    not(pursuit(ID,Fact)),
    save_usedfact(ID,Fact),
    forward.

% done/0
done :- not(fact(_,_)).

% pursuit/2
pursuit(FactID,Fact) :-
    rule(RuleID,Head,Conditions),
    functor(Head,condition,_),
    rule_pursuit(FactID,Fact,RuleID,Head,Conditions),
    fail.
% rule_pursuit/5
% Searches through rules conditions,deleting entries that match the fact F
rule_pursuit(FactID,Fact,RuleID,Head,Conditions) :-
    match(Fact,Conditions),
    delete_fact(Fact,Conditions,ConditionsNew),
    new_rule(FactID,RuleID,Head,ConditionsNew).
% rule_pursuit(FactID,Fact,RuleID,Head,Conditions) :-
%     \+ functor(Conditions,condition,_), !.

% delete_fact/3
% Unifies the fact by binding the variable within the conditions and it always succedees
delete_fact(X,[],[]).
delete_fact(X,[X|L],M) :- delete_fact(X,L,M).
delete_fact(X,[Y|L],[Y|M]) :- 
    not(X=Y),
    delete_fact(X,L,M).

% new_rule/4
% When the right-hand sided of a rule is empty a new fact is made,otherwise the rule is updated.
new_rule(FactID,RuleID,Head,[]) :-
    save_fact(Head,FactIDNew),
    save_trail(FactID,[RuleID,FactIDNew]).
new_rule(FactID,RuleID,Head,Conditions) :-
    not(Conditions=[]),
    save_rule(RuleID,Head,Conditions),
    save_trail(FactID,RuleID).
new_rule(_,RuleID,Head,Conditions) :-
    fact(_,Head),
    Conditions=[].