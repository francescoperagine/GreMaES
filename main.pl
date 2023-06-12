:- prolog_flag(unknown,_,fail).

:- leash(none).

:- [store,engine,utils,logger].
:- [mode_user,mode_monitor,mode_kb].
:- [kb/rules,kb/signs,kb/treatments,kb/messages,kb/devices,kb/plants,kb/species,kb/growth_stages].

:- initialization(init).

% init/0
init :- 
    engine_init,
    utils_init.

% start/0
start :- 
    cleanup_init,
    welcome,
    !,
    fruition_mode,
    restart.

% cleanup_init/0
cleanup_init :-
    retractall(asked(_,_)),
    retractall(fact_history(_,_)),
    retractall(fact(_,_)),
    retractall(usedfact(_,_)),
    retractall(actuator_status(_,_)),
    retractall(plant_status(_,_,_)).

% fruition_mode/0
fruition_mode :- 
    mode_user,
    ensure_loaded(mode_user),
    user_start.
fruition_mode :-
    \+ (mode_user),
    mode_kb,
    ensure_loaded(mode_kb),
    kb_start.
fruition_mode :-
    \+ (mode_user),
    \+ (kb_mode),
    mode_monitor,
    ensure_loaded(mode_monitor),
    monitor_init,
    monitor_start.

% mode_user/0
mode_user :- askif(fruition_mode(mode_user)).

% kb_mode/0
kb_mode :- askif(fruition_mode(kb_mode)).

% mode_monitor/0
mode_monitor :- askif(fruition_mode(mode_monitor)).

% restart/0
restart :- 
    start_again,
    start.
restart :-
    \+ start_again,
    goodbye.

% start_again/0
start_again :- askif(start_again).

% diagnosis/0
diagnosis :-
    history(HistoriesList),
    member([Plant|History],HistoriesList),
    plant_history(Plant,History,Facts),
    all(Condition,(member(ID,History), usedfact(ID,condition(Plant,Condition))),Conditions),
    explain(Conditions),
    explain_inference(Facts).

% explain/1
explain([]).
explain([Condition|T]) :-
    explain_diagnosis(Condition),
    explain_treatment(Condition),
    explain(T).

% explain_diagnosis/1
explain_diagnosis(Condition) :-
        clause(problem(Problem),condition(Condition)),
        clause(issue(Issue),problem(Problem)),
        (mode_user -> X = 'Your plant'; X = Plant),
        atomic_concat(['\n',X,' is affected by the ',Issue,' disorder - ',Condition,' ',Problem], Message),
        writeln(Message).

% explain_treatment/1
explain_treatment(Condition) :-
    problem_condition('nutrient deficiency',Condition),
    writeln_message(missing_nutrient).
explain_treatment(Condition) :-
    \+ problem_condition('nutrient deficiency',Condition),
    \+ treatment(Condition,_),
    write_message(treatment_none), writeln(Condition).
explain_treatment(Condition) :-
    \+ problem_condition('nutrient deficiency',Condition),
    all(Treatment,treatment(Condition,Treatment),Treatments),
    write('* How to treat '),write(': '),
    maplist(writeln,Treatments).

% explain_inference/1 (+Facts)
explain_inference(Facts) :-
    mode_user,
    need_explanation,
    writeln('\nReasoning performed by the forward engine (ID-Inference step):\n'),
    maplist(writeln,Facts).
explain_inference(_) :-
    mode_user,
    \+ need_explanation.
explain_inference(_) :-
    \+ mode_user.

% need_explanation/1
need_explanation :- askif(need_explanation).