:- dynamic actuator_status/2.
:- dynamic asked/2.
:- dynamic debug/1.
:- dynamic fact/2.
:- dynamic fact_history/2.
:- dynamic last_index/1.
:- dynamic rule/3.
:- dynamic signs/1.
:- dynamic usedfact/2.

% unset/1
unset(N/A) :-
    abolish(N,A).

% unset_asked/1
unset_asked(Y) :-
    (asked(Y,_) -> retract(asked(Y,_)) ; true).

% save_debug/1
save_debug(X) :-
    (debug(X) -> retract(debug(_))),
    assert(debug(X)),
    !.

% save_index/1 (-NextID)
% Returns the next available ID
save_index(NextID) :-
    \+ last_index(_),
    assert(last_index(NextID)),
    !.
save_index(NextID) :-
    last_index(_),
    retract(last_index(_)),
    assert(last_index(NextID)),
    !.

% save_observation/2
save_observation(X,Observation) :-
    save_fact(manifests(X,Observation),ID),
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
    (fact(_,Fact) -> retract(fact(_,Fact)) ; true),
    asserta(usedfact(ID,Fact)),
    !,
    (is_debug -> (writeln(usedfact(ID,Fact))) ; true).

% save_rule/3
save_rule(ID,Head,Conditions) :-
    \+ rule(_,Head,Conditions),
    asserta(rule(ID,Head,Conditions)),
    !,
    (is_debug -> (writeln(rule(ID,Head,Conditions))) ; true).

% save_trail/2 (+Prev,+Curr)
% Stores Curr in the same fact_history as Prev.
save_trail(Prev,Curr) :-
    \+ is_list(Curr),
    fact_history(X,History),
    memberchk(Prev,History),
    retract(fact_history(X,History)),
    assert(fact_history(X,[Curr|History])),
    !,
    (is_debug -> writeln(fact_history(X,[Curr|History])) ; true).
% save_trail/2 (+Prev,+List)
% Appends the elements of the list to Prev history.
save_trail(Prev,[]).
save_trail(Prev,[H|T]) :-
    save_trail(Prev,H),
    save_trail(Prev,T).

% actuator_init/1 (+Actuator)
% Initializes the status.
actuator_init(Actuator) :-
    assert(actuator_status(Actuator,off)),
    !.
% actuator_save/3 (+Actuator,+Status,-Action)
% Changes the status and returns the performed action.
actuator_save(Actuator,Status,changed) :-
    actuator_status(Actuator,OldStatus),
    OldStatus \= Status,
    retractall(actuator_status(Actuator,_)), 
    assert(actuator_status(Actuator,Status)),
    !.
% actuator_save/3
% If it's already in the right status does nothing.
actuator_save(Actuator,Status,none) :-
    actuator_status(Actuator,Status).

% saveln/1
saveln(X) :-
    \+ call(X),
    assert(X),
    !,
    (is_debug -> writeln(X) ; true).
saveln(X) :-
    call(X).

% saveln_a/1
saveln_a(X) :-
    asserta(X),
    !.