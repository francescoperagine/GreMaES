:- use_module(library(system)).
:- use_module(library(random)).

:- dynamic diagnosis/3.
:- dynamic actuator_status/2.

loop_repetitions(X) :- X is 10.
loop_interval(X) :- X is 1.
reading_variability(X) :- X is 1.2.

% monitor_start/0 - Initialize the sensor mode.
monitor_start :-
    welcome_monitor, nl,
    monitor_cleanup,
    plants(L),
    maplist(writeln, L), nl,
    monitor_loop_start.

% monitor_cleanup/0
monitor_cleanup :-
    retractall(asked(_,_)),
    retractall(symptom(_,_)),
    retractall(symptom(_,_,_)),
    retractall(diagnosis(_,_,_)),
    retractall(actuator_status(_,_)).

% plants/1 
plants(L) :- all(P-S-temperature-Tmin-Tmax-humidity-Hmin-Hmax, (plant(P, S, H), species(S, Tmin, Tmax, _), stage(H, Hmin, Hmax, _)), L).

% monitor_loop_start/0
monitor_loop_start :- 
    loop_repetitions(X),
    loop_interval(Y),
    monitor_loop(X, Y).

% monitor_loop/2 - Loops X of duration Y in seconds. For each repetition, simulates a sensor reading.
monitor_loop(X, Y) :-
    X > 0,
    sampling_init,
    retractall(asked(continue_monitor_loop, _)),
    sleep(Y),
    X1 is X - 1,
    monitor_loop(X1, Y).
monitor_loop(X, Y) :-
    X = 0,
    askif(continue_monitor_loop) -> 
        monitor_loop_start
        ;
        !.

% sampling_init/0
sampling_init :- 
    all(X, sensor(X, _), L),
    random_list_element(L, D),
    sensor(D, T),
    plant_sensor(P, D),
    sampling(D, T, P),
    monitor_diagnosis.

% random_predicate_element/2
random_predicate_element(P, E) :-
    callable(P),
    call(P, L),
    random_list_element(L, E).

% random_list_element/2
random_list_element(L, E) :-
    length(L, N),
    random(0, N, R),
    nth0(R, L, E).

% sampling/3 :-
sampling(D, T, P) :-
    T \= caption,
    reading_variability(Var),
    range_value(T, Min, Max),
    MinV is Min / Var,
    MaxV is Max * Var,
    (T = humidity, MinV < 0 -> MinV1 = 0 ; MinV1 = MinV),
    (T = humidity, MaxV > 100 -> MaxV1 = 100 ; MaxV1 = MaxV),
    random(MinV1, MaxV1, R),
    V is floor(R),
    A = reading(T, V),
    store(P, T, D, A).
sampling(D, T, P) :-
    T = caption,
    random_predicate_element(manifestations, M),
    caption_forward(M, A),
    store(P, T, D, A).

% store/4
store(P, T, D, A) :-
    X = plant_reading(P, T, D, A),
    nl, writeln(X),
    assertz(X).

% range_value/3 Unifies min/max with the respective sensor_type
range_value(T, Min, Max) :-
    T = temperature,
    all(Tmin, species(_, Tmin, _, _), TminL),
    all(Tmax, species(_, _, Tmax, _), TmaxL),
    min_list(TminL, Min),
    max_list(TmaxL, Max).
range_value(T, Min, Max) :-
    T = humidity,
    all(Hmin, stage(_, Hmin, _, _), HminL),
    all(Hmax, stage(_, _, Hmax, _), HmaxL),
    min_list(HminL, Min),
    max_list(HmaxL, Max).

% caption_forward/2
caption_forward(M, A) :-
    \+ manifest_color(M, _),
    sampling_manifest_section(M, S),
    A = symptom(S, M).
caption_forward(M, A) :-
    all(X, manifest_color(M, X), L),
    random_list_element(L, C),
    sampling_manifest_section(M, S),
    A = symptom(S, M, C).

% sampling_manifest_section/2 - Unifies S with a random section related to the manifestation
sampling_manifest_section(M, S) :- 
    all(X, manifest_section(M, X), L),
    random_list_element(L, S).

% monitor_diagnosis/0
monitor_diagnosis :- 
    all((TS, P, L), (plant(P, _, _), timestamp(TS), all(A, (plant_reading(P, T, _, A), T = caption), L)), L),
    monitor_forward(L).
monitor_diagnosis :- 
    all((TS, P, [R]), (plant_reading(P, T, D, R), timestamp(TS), T \= caption), L),
    maplist(writeln, L),
    monitor_forward(L).
    
monitor_forward(L) :- 
    retract(plant_reading(_,_,_,_)),
    maplist(parse, L).

% is_caption/1
is_caption(X) :-
    functor(X, Y, Z),
    Y = symptom.

% parse/1
parse(X) :-
    X = (TS, P, L),
    maplist(is_caption, L),
    maplist(assertz, L),
    clause(type(T), B),
    conj_to_list(B, R),
    match(R, L),    % checks if the observed symptoms match any problem
    Y = diagnosis(P, T, TS),
    assertz(Y),
    maplist(retractall, L),
    problem_card(T, A),
    write('- Plant '), write(P), write(' caption diagnosis is '), write(A), write(' because of '), writeln(B),
    lognl((TS, P, caption, L, A)).
parse(X) :-
    X = (TS, P, L),
    maplist(is_caption, L),
    message_code(no_diagnosis, M),
    write('- Plant caption is '), write(L), write('. '), writeln(M),
    lognl((TS, P, caption, L, no_diagnosis)). 
parse(X) :-
    X = (TS, P, [reading(T, V)]),
    plant_range_values(P, T, Min, Max, Avg),
    range_status(V, Min, Max, S),
    D = diagnosis(P, T:S:V, TS),
    assertz(D),
    write('- Plant '), write(P), write(' reading diagnosis is '), write(T), write(' '), writeln(S),
    lognl((TS, P, T, V, S)),
    actuator_init(P, T:S:Avg). % sets the actuator to bring the value back to the avg

% timestamp/1
timestamp(T) :- 
    datime(datime(Year, Month, Day, Hour, Minute, Second)),
    T = ts(Year-Month-Day, Hour:Minute:Second).
    
% plant_range_values/5
plant_range_values(P, T, Tmin, Tmax, Tavg) :-
    T = temperature,
    species(S, Tmin, Tmax, Tavg).
plant_range_values(P, T, Hmin, Hmax, Havg) :-
    T = humidity,
    stage(H, Hmin, Hmax, Havg).

% range_status/4
range_status(N, Min, Max, S) :- 
    (N < Min, S = low)
    ;
    (N > Max, S = high)
    ;
    (S = normal).

% actuator_init/3
actuator_init(P, T:S:Avg) :- 
    plant_actuator(P, A), 
    actuator(A, T, S, K),
    actuator_forward(A, S, K),
    log((T, A, K)).
actuator_init(P, T:S:Avg) :- 
    \+ plant_actuator(P, A), 
    S \= normal,
    message_code(no_plant_actuator, M),
    atomic_concat([' * ', M, P], C),
    writeln(C),
    log(no_plant_actuator).
actuator_init(P, T:S:Avg) :- 
    \+ plant_actuator(P, A), 
    S = normal,
    atomic_concat([' * Plant ', P, ' ', T, ' is ', S], C),
    writeln(C),
    log(no_plant_actuator).
actuator_init(P, T:S:Avg) :-
    plant_actuator(P, A),
    \+ actuator(A, T, S, _),
    S \= normal,
    message_code(no_actuator, M),
    atomic_concat([' * ', M, T], C),
    writeln(C),
    log(no_actuator).
actuator_init(P, T:S:Avg) :-
    plant_actuator(P, A),
    \+ actuator(A, T, S, _),
    S = normal.

% actuator_forward/3
actuator_forward(A, S, K) :-
    S = normal,
    actuator_status(A, on),
    actuator_off(A),
    atomic_concat([' * ', K, ' ', A, ' is off.'], C),
    writeln(C),
    log(switched_off).
actuator_forward(A, S, K) :-
    S = normal,
    \+ (actuator_status(A, on)),
    atomic_concat([' * ', K, ' ', A, ' is already off.'], C),
    writeln(C),
    log(already_off).
actuator_forward(A, S, K) :-
    S \= normal,
    \+ (actuator_status(A, on)),
    actuator_on(A),
    atomic_concat([' * ', K, ' ', A, ' is on.'], C),
    writeln(C),
    log(switched_on).
actuator_forward(A, S, K) :-
    S \= normal,
    actuator_status(A, on),
    atomic_concat([' * ', K, ' ', A, ' is already on.'], C),
    writeln(C),
    log(already_on).

% actuator_on/1
actuator_on(A) :-
    retract(actuator_status(A, off)),
    assertz(actuator_status(A, on)).
actuator_on(A) :- 
    assertz(actuator_status(A, on)).

% actuator_off/1
actuator_off(A) :-
    retract(actuator_status(A, on)),
    assertz(actuator_status(A, off)).
actuator_off(A) :- 
    assertz(actuator_status(A, off)).

lognl(X) :-
    open('GreMaES.log', append, Logfile),
    nl(Logfile),
    write(Logfile, X),
    write(Logfile, ','),
    close(Logfile).
log(X) :- 
    open('GreMaES.log', append, Logfile),
    write(Logfile, X),
    close(Logfile).