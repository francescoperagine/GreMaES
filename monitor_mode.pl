:- use_module(library(system)).
:- use_module(library(random)).

:- dynamic health_status/4.
:- dynamic diagnosis/3.
:- dynamic actuator_status/2.

loop_repetitions(X) :- X is 10.
loop_interval(X) :- X is 1.
reading_variability(X) :- X is 1.2.

% monitor_start/0 - Initialize the sensor mode.
monitor_start :-
    welcome_monitor, nl,
    monitor_cleanup,
    greenhouse_init,
    actuator_init,
    plants(L),
    maplist(writeln, L), nl,
    monitor_loop_start,
    greenhouse_status.

% greenhouse_init/0
greenhouse_init :-
    health_problem(none, C),
    timestamp(TS),
    class(C, T),
    all(health_status(P, TS, C, T), plant(P,_,_), L),
    maplist(assertz, L).

% greenhouse_status/0
greenhouse_status :-
    all(health_status(P, TS, C, T), (plant(P,_,_), health_status(P, TS, C, T)), L),
    maplist(writeln, L).

% timestamp/1
timestamp(T) :- 
    datime(datime(Year, Month, Day, Hour, Minute, Second)),
    T = ts(Year-Month-Day, Hour:Minute:Second).

% actuator_init/0
actuator_init :-
    all(actuator_status(A, off), plant_actuator(P, A), L),
    maplist(assertz, L).


% monitor_cleanup/0
monitor_cleanup :-
    retractall(asked(_,_)),
    retractall(symptom(_,_)),
    retractall(symptom(_,_,_)),
    retractall(diagnosis(_,_,_)),
    retractall(actuator_status(_,_)).

% plants/1 
plants(L) :- all(P-S-temperature-Tmin-Tmax-humidity-Hmin-Hmax, (plant(P, S, H), species_temperature(S, Tmin, Tmax), stage_humidity(H, Hmin, Hmax)), L).

% monitor_loop_start/0
monitor_loop_start :- 
    loop_repetitions(X),
    loop_interval(Y),
    monitor_loop(X, Y).

% monitor_loop/2 - Loops X of duration Y in seconds. For each repetition, simulates a sensor reading.
monitor_loop(X, Y) :-
    X > 0,
    sampling_start,
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

% sampling_start/0
sampling_start :- 
    all(X, sensor(X, _), L),
    random_list_element(L, D),
    sensor(D, T),
    plant_sensor(P, D),
    sampling(D, T, P),
    parsing_start.

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
    all(Tmin, species_temperature(_, Tmin, _), TminL),
    all(Tmax, species_temperature(_, _, Tmax), TmaxL),
    min_list(TminL, Min),
    max_list(TmaxL, Max).
range_value(T, Min, Max) :-
    T = humidity,
    all(Hmin, stage_humidity(_, Hmin, _), HminL),
    all(Hmax, stage_humidity(_, _, Hmax), HmaxL),
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

% parsing_start/0
parsing_start :- 
    timestamp(TS), 
    all((TS, P, L), (plant(P, _, _), all(A, (plant_reading(P, T, _, A), T = caption), L)), L),
    parsing_readings(L).
parsing_start :- 
    timestamp(TS), 
    all((TS, P, [R]), (plant_reading(P, T, D, R), T \= caption), L),
    parsing_readings(L).
    
% parsing_readings/1
parsing_readings(L) :- 
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
    problem_card(T, H, C),
    assertz(health_status(P, TS, C, T)),
    maplist(retractall, L),
    atomic_concat(['- Plant ', P, ' caption diagnosis is ', H, ' ', C, ' ', T, ' because of '], A),
    write(A), writeln(B),
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
    abiotic_status(P, H),
    assertz(health_status(P, TS, abiotic_problem, H)),
    atomic_concat(['- Plant ', P, ' reading diagnosis is ', T, ' ', S], A),
    writeln(A),
    lognl((TS, P, T, V, S)),
    actuator_start(P, T, S).
  
% plant_range_values/5
plant_range_values(P, T, Tmin, Tmax, Tavg) :-
    T = temperature,
    species_temperature(S, Tmin, Tmax),
    Tavg is (Tmin + Tmax) /2.
plant_range_values(P, T, Hmin, Hmax, Havg) :-
    T = humidity,
    stage_humidity(H, Hmin, Hmax),
    Havg is (Hmin + Hmax) /2.

% range_status/4
range_status(N, Min, Max, S) :- 
    (N < Min, S = low)
    ;
    (N > Max, S = high)
    ;
    (S = normal).

% actuator_start/3
actuator_start(P, T, S) :-
    all(p_actuator(A, T, S, K, H), (plant_actuator(P, A), actuator(A, T, H, K)), L),
    maplist(actuator_forward, L).
actuator_start(P, T, S) :-
    \+ plant_actuator(P, A),
    writeln(no_plant_actuator),
    log(no_plant_actuator).
actuator_start(P, T, S) :- 
    plant_actuator(P, A),
    \+ actuator(A, T, H, K),
    writeln(no_type_actuator),
    log(no_type_actuator).

% actuator_forward/1
actuator_forward(X) :-
    X = p_actuator(A, T, S, K, H),
    log((A, K, H )),
    actuator_activate(A, T, S, K, H).

% actuator_activate/5
actuator_activate(A, T, S, K, H) :-
    (S = normal ; S \= H),
    actuator_status(A, on),
    actuator_off(A),
    atomic_concat([' * ', K, ' ', A, ' ', turned_off], C),
    writeln(C),
    log(-turned_off).  
actuator_activate(A, T, S, K, H) :-
    (S = normal ; S \= H),
    actuator_status(A, off),
    atomic_concat([' * ', K, ' ', A, ' ', already_off], C),
    writeln(C),
    log(-already_off).
actuator_activate(A, T, S, K, H) :-
    S \= normal,
    S = H,
    actuator_status(A, off),
    actuator_on(A),
    atomic_concat([' * ', K, ' ', A, ' ', turned_on], C),
    writeln(C),
    log(-turned_on).
actuator_activate(A, T, S, K, H) :-
    S \= normal,
    S = H,
    actuator_status(A, on),
    atomic_concat([' * ', K, ' ', A, ' ', already_on], C),
    writeln(C),
    log(-already_on).
% awry backup
actuator_activate(A, T, H, K, S) :-
    X = p_actuator(A, T, H, K, S),
    write((A, T, H, K, S)),
    writeln(something_went_awry),
    log(something_went_awry).

% actuator_on/1
actuator_on(A) :-
    retract(actuator_status(A, _)),
    assertz(actuator_status(A, on)).
actuator_on(A) :- 
    assertz(actuator_status(A, on)).

% actuator_off/1
actuator_off(A) :-
    retract(actuator_status(A, _)),
    assertz(actuator_status(A, off)).
actuator_off(A) :- 
    assertz(actuator_status(A, off)).

% lognl/1
lognl(X) :-
    open('GreMaES.log', append, Logfile),
    nl(Logfile),
    write(Logfile, X),
    write(Logfile, ','),
    close(Logfile).
% log/1
log(X) :- 
    open('GreMaES.log', append, Logfile),
    write(Logfile, X),
    close(Logfile).