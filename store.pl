:- use_module(library(lists)).

:- dynamic debug/1.
:- dynamic last_index/1.
:- dynamic fact/2.
:- dynamic usedfact/2.
:- dynamic fact_history/2.
:- dynamic rule/3.
:- dynamic actuator_status/2.
:- dynamic signs/1.

% save_debug/1
save_debug(X) :-
    \+ debug(X),
    retractall(debug(_)),
    assert(debug(X)),
    !.
save_debug(X) :-
    debug(X).

% save_index/1
save_index(NextID) :-
    retractall(last_index(_)),
    assert(last_index(NextID)),
    !.

% save_observation/2
save_observation(X,Observation) :-
    save_fact(manifests(X,Observation),ID),
    % log(manifests(X,Observation)),
    save_history(X,ID).

% save_fact/2
save_fact(Fact,ID):-
    \+ fact(_,Fact),
    next_index(ID),
    asserta(fact(ID,Fact)),
    !,
    (is_debug -> writeln(fact(ID,Fact)) ; true).
save_fact(_,_).

% save_history/2
save_history(X,ID) :-
    \+ fact_history(X,_),
    assert(fact_history(X,[ID])),
    !,
    (is_debug -> writeln(fact_history(X,[ID])) ; true).
save_history(X,ID) :-
    fact_history(X,History),
    retract(fact_history(X,History)),
    assert(fact_history(X,[ID|History])),
    !,
    (is_debug -> writeln(fact_history(X,[ID|History])) ; true).

% asserta forces the focus-of-attention on new facts,
% therefore last found facts will be pursued first.

% save_usedfact/2
save_usedfact(ID,Fact) :-
    (fact(ID,Fact) -> retract(fact(ID,Fact)) ; true),
    assert(usedfact(ID,Fact)),
    !,
    (is_debug -> (writeln(usedfact(ID,Fact))) ; true).

% save_rule/2
save_rule(ID,Head,Conditions) :-
    \+ rule(_,Head,Conditions),
    asserta(rule(ID,Head,Conditions)),
    !,
    (is_debug -> (writeln(rule(ID,Head,Conditions))) ; true).

% save_trail/2 
save_trail(Prev,Curr) :-
    \+ is_list(Curr),
    fact_history(X,History),
    memberchk(Prev,History),
    retract(fact_history(X,History)),
    assert(fact_history(X,[Curr|History])),
    !,
    (is_debug -> writeln(fact_history(X,[Curr|History])) ; true).
save_trail(Prev,[]).
save_trail(Prev,[H|T]) :-
    save_trail(Prev,H),
    save_trail(Prev,T).

% save_actuator/2
save_actuator(Actuator,Status) :-
    actuator_status(Actuator,_),
    retractall(actuator_status(Actuator,_)),
    assert(actuator_status(Actuator,Status)),
    !.
save_actuator(Actuator,Status) :-
    \+ actuator_status(Actuator,_),
    ground(Status),
    assert(actuator_status(Actuator,Status)),
    !.
    % atomic_concat(['\t',ActuatorID,' turned ',Status],Message),
    % log(Message),
    % (is_debug -> writeln(actuator_status(ID,Status)) ; true).

% save_ln/1
save_ln(X) :-
    \+ call(X),
    assert(X),
    !,
    (is_debug -> writeln(X) ; true).
save_ln(X) :-
    call(X).