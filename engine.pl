:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(ordsets)).

% full_forward/0
full_forward :- forward,doall(handle_not),forward.

% handle_not/0
handle_not :- rule(L,R),member(not(X),R),not(usedfact(X)),
  not(fact(X)),delete(not(X),R2),new_rule(L,R2).

% forward/0
forward :- done.
forward :- 
    fact(ID,Fact),
    not(pursuit(ID,Fact)),
    save_usedfact(ID,Fact),
    forward.

% doall/1
doall(P) :- not(alltried(P)).

% alltried/1
alltried(P) :- call(P),fail.

% done/0
done :- not(fact(_,_)).

% history/0
history :-
    all([P|H2],
            (fact_history(P,H1),
            usedfact(ID,manifests(P,S)),
            memberchk(ID,H1),
            reverse(H1,H2)),
        Hs),
    maplist(writeln,Hs).
% history/1
history(Hs) :-
    all([P|H2],
            (fact_history(P,H1),
            usedfact(ID,manifests(P,S)),
            memberchk(ID,H1),
            reverse(H1,H2)),
        Hs).

% pursuit/2
pursuit(FactID,Fact) :-
    rule(RuleID,Head,Conditions),
    rule_pursuit(FactID,Fact,RuleID,Head,Conditions),
    fail.
% rule_pursuit/5
% Searches through rules conditions,deleting entries that match the fact F
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