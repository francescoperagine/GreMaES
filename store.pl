:- use_module(library(lists)).

:- dynamic last_index/1.
:- dynamic fact/2.
:- dynamic usedfact/2.
:- dynamic fact_history/2.
:- dynamic rule/3.

% save_index/1
save_index(NextID) :-
    retractall(last_index(_)),
    asserta(last_index(NextID)),
    !.

% save_symptom/2
save_symptom(X,Symptom) :-
    save_fact(manifests(X,Symptom),ID),
    save_history(X,ID).

% save_symptom/3
save_fact(Fact,ID):-
    \+ fact(_,Fact),
    next_index(ID),
    assertz(fact(ID,Fact)),
    !,
    (debug(on) -> writeln(fact(ID,Fact)) ; true).
save_fact(_,_).

% save_history/2
save_history(X,ID) :-
    \+ fact_history(X,_),
    asserta(fact_history(X,[ID])),
    !,
    (debug(on) -> writeln(fact_history(X,[ID])) ; true).
save_history(X,ID) :-
    fact_history(X,History),
    retract(fact_history(X,History)),
    asserta(fact_history(X,[ID|History])),
    !,
    (debug(on) -> writeln(fact_history(X,[ID|History])) ; true).

% asserta forces the focus-of-attention on new facts,
% therefore last found facts will be pursued first.

% save_usedfact/2
save_usedfact(ID,Fact) :-
    retract(fact(ID,Fact)),
    asserta(usedfact(ID,Fact)),
    !,
    (debug(on) -> (writeln(usedfact(ID,Fact))) ; true).

% save_rule/2
save_rule(ID,Head,Conditions) :-
    \+ rule(_,Head,Conditions),
    asserta(rule(ID,Head,Conditions)),
    !,
    (debug(on) -> (writeln(rule(ID,Head,Conditions))) ; true).

% save_trail/2 
save_trail(Prev,Curr) :-
    \+ is_list(Curr),
    fact_history(X,History),
    memberchk(Prev,History),
    retract(fact_history(X,History)),
    asserta(fact_history(X,[Curr|History])),
    !,
    (debug(on) -> writeln(fact_history(X,[Curr|History])) ; true).
save_trail(Prev,[]).
save_trail(Prev,[H|T]) :-
    save_trail(Prev,H),
    save_trail(Prev,T).