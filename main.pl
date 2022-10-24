:- prolog_flag(unknown,_,fail).

:- use_module(library(lists)).
:- use_module(library(apply_macros)).

:- dynamic asked/2.
:- dynamic current_section/1.
:- dynamic symptom/2.
:- dynamic symptom/3.
:- dynamic type/1.

:- [knowledge_base/kb, user_mode, kb_mode, monitor_mode].

% start/0
start :- 
    welcome, 
    init.

% init/0 - clears cache, sets running mode and asks to restart the program.
init :- 
    init_cleanup,
    set_fruition_mode,
    restart.

% init_cleanup/0
init_cleanup :-
    retractall(asked(_, _)),
    retractall(symptom(_,_)),
    retractall(symptom(_,_,_)).

% set_fruition_mode/0
set_fruition_mode :- 
    user_mode,
    user_start.
set_fruition_mode :-
    \+ (user_mode),
    kb_mode,
    kb_start.
set_fruition_mode :-
    \+ (user_mode),
    \+ (kb_mode),
    monitor_mode,
    monitor_start.

% restart/0
restart :- 
    askif(start_again),
    init.
restart :-
    init_cleanup,
    goodbye.

user_mode :- askif(fruition_mode(user_mode)).
kb_mode :- askif(fruition_mode(kb_mode)).
monitor_mode :- askif(fruition_mode(monitor_mode)).

% manifestations/1
manifestations(L) :- all(M, manifest_section(M, S), L).

% treatments/1
treatments(L2) :- 
    all(T, treatment(T, _), L),
    maplist(treatment_card, L, L1),
    flatten(L1, L2).

treatment_card(T, L) :- all(A, (treatment(T, D), atomic_concat([T, ': ', D], A)), L).

match(L1, L2):- forall(member(X, L1), member(X, L2)).

% problem_card/2 - The predicate holds when the first argument is a KB 'type' ground atom,
% and other arguments unify with the KB atoms. Unifies A1 with the concatenate terms
% Type T, health problem H, class C
problem_card(T, A) :-
    class(C, T),
    health_problem(H, C),
    atomic_concat([H, ' problem, ', T, ' ', C], A).

% askif/1
askif(Q) :-
    ask(Q, A),
    positive_answer(Q, A).

% ask/2
ask(Qcode, A) :- asked(Qcode, A).
ask(Qcode, A) :- \+ (asked(Qcode, A)),
    question_code(Qcode, Q),
    write(Q), write('?'), nl,
    read(A2),
    ask2(Q, Qcode, A2, A).

% positive_answer/2
positive_answer(Q, A) :- affirmative(A).
positive_answer(Qcode, A) :-
    \+ (negative(A)),
    \+ (affirmative(A)),
    message_code(yes_or_no, M),
    write(M), nl,
    read(A2),
    retract(asked(Qcode, A)),
    asserta(asked(Qcode, A2)),
    affirmative(A2).

% askifnot/1
askifnot(Q) :- not(askif(Q)).

% ask/2
ask(Qcode, A) :- asked(Qcode, A).
ask(Qcode, A) :- \+ (asked(Qcode, A)),
    question_code(Qcode, Q),
    write(Q), write('?'), nl,
    read(A2),
    ask2(Q, Qcode, A2, A).

% ask2/4
ask2(Q, Qcode, '?', A) :-
    explain(Qcode),
    ask(Qcode, A).

ask2(Q, Qcode, A, A) :-
    \+ (A = '?'),
    asserta(asked(Qcode, A)).

% explain/1
explain(X) :- 
    explanation(X, Y),
    writeln(Y).
explain(X) :-
    \+ explanation(X, Y),
    message_code(no_explanation, M),
    writeln(M).

% affirmative/1
affirmative(yes).
affirmative(y).
affirmative(ye).
affirmative(right).
affirmative(ok).
affirmative(uhhuh).

% negative/1
negative(no).
negative(n).
negative(not).
negative(never).
negative(impossible).
negative(haha).