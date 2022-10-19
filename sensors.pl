:- use_module(library(system)).
:- use_module(library(random)).

:- dynamic plant_symptom/3.
:- dynamic plant_symptom/4.
:- dynamic diagnosis/3.
:- dynamic actuator_status/2.
% :- dynamic log/1.

loop_repetitions(X) :- X is 5.
loop_interval(X) :- X is 1.
reading_variance(X) :- X is 1.2.

% sensor_init/0 - Initialize the sensor mode.
sensor_init :-
    sensor_cleanup,
    welcome_sensor, nl,
    plants(L),
    maplist(writeln, L), nl,
    loop_init,
    sensor_diagnosis.
    % greenhouse_status.

% sensor_cleanup/0
sensor_cleanup :-
    retractall(symptom(_,_)),
    retractall(symptom(_,_,_)),
    retractall(plant_symptom(_,_,_)),
    retractall(plant_symptom(_,_,_,_)),
    retractall(asked(continue_time_loop, _)),
    retractall(diagnosis(_)),
    retractall(actuator_status(_, _)).
    % retractall(log(_)).

% greenhouse_status :-


plants(L) :- all(P-S-temperature-Tmin-Tmax-humidity-Hmin-Hmax, (plant(P, S, H), species(S, Tmin, Tmax, _), stage(H, Hmin, Hmax, _)), L).

% loop_init/0
loop_init :- 
    loop_repetitions(X),
    loop_interval(Y),
    time_loop(X, Y).

% time_loop/2 - Loops X of duration Y in seconds. For each repetition, simulates a sensor reading.
time_loop(X, Y) :-
    X > 0,
    sampling_init,
    retractall(asked(continue_time_loop, _)),
    sleep(Y),
    X1 is X - 1,
    time_loop(X1, Y).
time_loop(X, Y) :-
    X = 0,
    askif(continue_time_loop) -> 
        loop_init
        ;
        !.

sensor_type(L) :- all(X, sensor(_, X), L).
is_color(X) :- colors(L), member(X, L).
is_sensor_type(X) :- sensor_type(L), member(X, L).
is_section(X) :- sections(L), member(X, L).
is_manifestation(X) :-  manifestations(L), member(X, L).

sampling_init :- 
    random_sensor(D),
    sensor(D, T),
    sensor_plant(D, P),
    sampling(D, T, P).

random_sensor(D) :- 
    all(X, sensor(X, _), L),
    random_list_element(L, D).

random_predicate_element(P, E) :-
    callable(P),
    call(P, L),
    random_list_element(L, E).
random_list_element(L, E) :-
    length(L, N),
    random(0, N, R),
    nth0(R, L, E).

% sampling/3 :-
sampling(D, T, P) :-
    T \= caption,
    reading_variance(Var),
    range_value(T, Min, Max),
    MinV is Min / Var,
    MaxV is Max * Var,
    (T = humidity, MinV < 0, MinV1 = 0 ; MinV1 = MinV),
    (T = humidity, MaxV > 100, MaxV1 = 100 ; MaxV1 = MaxV),
    random(MinV, MaxV1, R),
    V is floor(R),
    A = reading(T, V),
    sampling_output(P, T, D, A).
sampling(D, T, P) :-
    T = caption,
    random_predicate_element(manifestations, M),
    caption_forward(M, A),
    sampling_output(P, T, D, A).

% sampling_output/4
sampling_output(P, T, D, A) :-
    sensor_store(P, A),
    datime(datime(Year, Month, Day, Hour, Minute, Second)),
    flatten([datetime(Year/Month/Day, Hour:Minute:Second), plant(P), sensor(D, T), A], R),
    % maplist(log, R),
    writeln(R).

% sensor_store/2
sensor_store(P, A) :- 
    A = symptom(S, M, C),
    manifest_section(M, S),
    manifest_color(M, C),
    assertz(symptom(S, M, C)),
    assertz(plant_symptom(P, S, M, C)).
sensor_store(P, A) :- 
    A = symptom(S, M),
    manifest_section(M, S),
    assertz(symptom(S, M)),
    assertz(plant_symptom(P, S, M)).
sensor_store(P, A) :- 
    A = reading(T, V),
    is_sensor_type(T),
    assertz(reading(T, V)),
    assertz(plant_reading(P, T, V)).

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

% sensor_diagnosis/0
sensor_diagnosis :- 
    caption_diagnosis,
    reading_diagnosis.
sensor_diagnosis :- 
    \+ diagnosis(_,_,_),
    message_code(no_diagnosis, M),
    writeln(M).

% caption_diagnosis/0
caption_diagnosis :- 
    all(diagnosis(P, T, R), (type_body(T, R), observed_diagnosis_body(P, R)), D),
    maplist(caption_diagnosis_forward, D).
caption_diagnosis :- \+ type(X).

% reading_diagnosis/0
reading_diagnosis :- 
    all(plant_reading(P, T, V), plant_reading(P, T, V), L),
    maplist(reading_diagnosis_forward, L).
reading_diagnosis :- \+ plant_reading(_, _, _).

% caption_diagnosis_forward/1
caption_diagnosis_forward(X) :-
    X = diagnosis(P, T, R),
    assertz(X),
    \+ is_sensor_type(T),
    problem_card(T, A),
    write('- Plant '), write(P), write(' is affected by '), write(A), write(' because of '), R = [B], writeln(B).

% reading_diagnosis_forward/1
reading_diagnosis_forward(X) :-
    X = plant_reading(P, T, V),
    plant_range_values(P, T, Min, Max, Avg),
    range(V, Min, Max, S),
    write('- Plant '), write(P), write(' reading of '), write(T), write(' is '), writeln(S),
    assertz(diagnosis(P, T:S, reading(T, V, Min, Max))),
    actuator_init(P, T, S, Avg).

% observed_diagnosis_body/2 - unifies P with the plant affected by the symptoms
observed_diagnosis_body(P, B) :-
    observed_symptoms(S),
    match(B, S),
    member(X, B),
    plant_by_symptom(P, X).

plant_by_symptom(P, X) :-
    X = symptom(S, M, C),
    manifest_color(_, C),
    plant_symptom(P, S, M, C).
plant_by_symptom(P, X) :-
    X = symptom(S, M),
    plant_symptom(P, S, M).

% plant_range_values/5
plant_range_values(P, T, Tmin, Tmax, Tavg) :-
    T = temperature,
    species(S, Tmin, Tmax, Tavg).
plant_range_values(P, T, Hmin, Hmax, Havg) :-
    T = humidity,
    stage(H, Hmin, Hmax, Havg).

range(N, Min, Max, S) :- 
    (N < Min, S = low)
    ;
    (N > Max, S = high)
    ;
    (S = normal).

% actuator_init/3
actuator_init(P, T, S, Avg) :- 
    actuator(A, T, S, K),
    plant_actuator(P, A), 
    actuator_forward(P, A, S, K, Avg).
actuator_init(P, T, S, Avg). % if there isn't one, no matter.

actuator_forward(P, A, S, K, Avg) :-
    S = normal,
    actuator_off(A),
    write(' * '), write(K), write(' '), write(A), write(' is off.').
actuator_forward(P, A, S, K, Avg) :-
    S \= normal,
    actuator_on(A),
    write(' * '), write(K), write(' '), write(A), write(' is on.').

actuator_on(A) :-
    actuator_status(A, off),
    retract(actuator_status(A, off)),
    assertz(actuator_status(A, on)).
actuator_on(A) :-
    assertz(actuator_status(A, on)).

actuator_off(A) :-
    actuator_status(A, on),
    retract(actuator_status(A, on)),
    assertz(actuator_status(A, off)).
actuator_off(A) :-
    assertz(actuator_status(A, off)).
    

% log(X). % write to external log file.