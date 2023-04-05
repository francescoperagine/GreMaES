:- prolog_flag(unknown,_,fail).

:- use_module(library(lists)).
:- use_module(library(apply_macros)).

:- dynamic asked/2.
:- dynamic current_location/1.
:- dynamic symptom/3.

:- ensure_loaded(logger).
:- [user_mode, kb_mode, monitor_mode, knowledge_base/kb].
:- leash(none).

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
    retractall(asked(_,_)),
    retractall(symptom(_,_,_)).

% set_fruition_mode/0
set_fruition_mode :- 
    user_mode,
    ensure_loaded(user_mode),
    user_mode_start.
set_fruition_mode :-
    \+ (user_mode),
    kb_mode,
    ensure_loaded(kb_mode),
    kb_mode_start.
set_fruition_mode :-
    \+ (user_mode),
    \+ (kb_mode),
    monitor_mode,
    ensure_loaded(monitor_mode),
    monitor_mode_start.

% restart/0
restart :- 
    askif(start_again),
    init.
restart :-
    init_cleanup,
    goodbye.

% user_mode/0
user_mode :- askif(fruition_mode(user_mode)).

% kb_mode/0
kb_mode :- askif(fruition_mode(kb_mode)).

% monitor_mode/0
monitor_mode :- askif(fruition_mode(monitor_mode)).

% health_issues/1
health_issues(HealthIssues) :- 
    all(
        Issue-Problem,
        clause(health_issue(Issue), problem(Problem)),
        HealthIssues).

% conditions/1
conditions(ProblemsConditions) :-
    all(
        Problem-Condition,
        (
            clause(health_issue(Issue), problem(Problem)),
            clause(problem(Problem), condition(Condition))
        ),
        ProblemsConditions).

% locations/1
locations(Locations) :- all(Location, sign_location(_, Location), Locations).

% signs/1
signs(Signs) :- all(Sign, sign_location(Sign, _), Signs).

% colors/1
colors(Colors) :- all(Color, sign_color(_, Color), Colors).

% treatments/1
treatments(Treatments) :-
    all(Condition-Treatment, treatment(Condition, Treatment), Treatments).

% rules/1
rules(Rules) :-
    all(
        (Issue, Problem, Condition, Symptoms),
        (
            clause(health_issue(Issue), problem(Problem)),
            clause(problem(Problem), condition(Condition)),
            clause(condition(Condition), SymptomsConj),
            conj_to_list(SymptomsConj, Symptoms)
        ),
        Rules).

% problem_condition/2
problem_condition(Problem, Condition) :-
    clause(problem(Problem), condition(Condition)).

% issue_problem/2
issue_problem(Issue, Problem) :-
    clause(health_issue(Issue), problem(Problem)).

% plants/1 - Gets all plants with installed sensors
plants(SortedPlants) :-
    all(Plant, plant_sensor(Plant, _), Plants),
    sort(Plants, SortedPlants).

% plants_reading_ranges/1 
plants_reading_ranges(SortedPlants) :-
    all(
        Plant-Species-temperature_range-TemperatureMin-TemperatureMax-growth_humidity-GrowthStage-HumidityMin-HumidityMax,
        (plant(Plant, Species, GrowthStage), species(Species, TemperatureMin, TemperatureMax), growth_humidity(GrowthStage, HumidityMin, HumidityMax)),
        Plants
    ),
    sort(Plants, SortedPlants).

% timestamp/1
timestamp(T) :- 
    datime(datime(Year, Month, Day, Hour, Minute, Second)),
    T = timestamp(Year-Month-Day, Hour:Minute:Second).

% write_message/1
write_message(MessageCode) :-
    message_code(MessageCode, Message),
    write(Message).
% writeln_message/1
writeln_message(MessageCode) :-
    message_code(MessageCode, Message),
    writeln(Message).

% match/2 Checks whether every member of L1 is in L2
match(L1, L2):- forall(member(X, L1), member(X, L2)).

% conj_to_list/2
conj_to_list((H, C), [H|T]) :-
    !,
    conj_to_list(C, T).
conj_to_list(H, [H]).

% list_to_conj/2
list_to_conj([H|T], (H, C)) :-
    !,
    list_to_conj(T, C).
list_to_conj([H], H).

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

% % explain/1
% explain(X) :- 
%     explanation(X, Y),
%     writeln(Y).
% explain(X) :-
%     \+ explanation(X, Y),
%     writeln_message(no_explanation).

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