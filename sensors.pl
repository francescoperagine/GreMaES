:- use_module(library(system)).
:- use_module(library(random)).

:- [knowledge_base/greenhouse].
:- dynamic plant_symptom/3.
:- dynamic plant_symptom/4.
:- dynamic diagnosis/1.
:- dynamic log/1.


loop_repetitions(X) :- X is 15.
loop_interval(X) :- X is 1.
reading_variance(X) :- X is 1.2.

% device_mode/0 Starts the sensor mode.
sensor_init :-
    sensor_cleanup,
    welcome_sensor, nl,
    plants(L),
    maplist(writeln, L), nl,
    loop_init,
    sampling_diagnosis.

sensor_cleanup :-
    retractall(symptom(_,_)),
    retractall(symptom(_,_,_)),
    retractall(plant_symptom(_,_,_)),
    retractall(plant_symptom(_,_,_,_)),
    retractall(diagnosis(_)),
    retractall(asked(continue_time_loop, _)),
    retractall(log(_)).

plants(L) :- all(P-S-temperature-Tmin-Tmax-humidity-Hmin-Hmax, (plant(P, S, H), species(S, Tmin, Tmax), stage(H, Hmin, Hmax)), L).

loop_init :- 
    loop_repetitions(X),
    loop_interval(Y),
    time_loop(X, Y).

% time_loop/1 - Loops X of duration Y in seconds. For each repetition, simulates a sensor reading.
time_loop(X, Y) :-
    X > 0,
    random_sampling,
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

types(L) :- setof(X, device(_, X, _), L).
device_plant(S, P) :- device(S, _, P).
devices_by_type(T, L) :- setof(X, device(X, T, _), L).
random_device(D) :- all(X, device(X, _, _), L), random_list_element(L, D).

random_sampling :- 
    random_device(D),
    sampling(D).

random_list_element(P, E) :-
    callable(P),
    call(P, L),
    random_list_element(L, E).
random_list_element(L, E) :-
    length(L, N),
    random(0, N, R),
    nth0(R, L, E).

% sampling(D) :-
sampling(D) :-
    device(D, T, P),
    T \= caption,
    reading_variance(V),
    range_value(T, Min, Max),
    MinV is Min / V,
    MaxV is Max * V,
    random(MinV, MaxV, N),
    N1 is floor(N),
    sampling_output(P, T, D, N1).
sampling(D) :-
    device(D, T, P),
    T = caption,
    random_list_element(manifestations, M),
    caption_forward(P, M, A),
    sampling_output(P, T, D, A).

% sampling_output/4
sampling_output(P, T, D, A) :-
    datime(datime(Year, Month, Day, Hour, Minute, Second)),
    flatten([datetime(Year/Month/Day, Hour:Minute:Second), plant(P), type(T), device(D), value(A)], R),
    assertz(log(R)),
    writeln(R).

% range_value/3 Unifies min/max with the respective types
range_value(T, Min, Max) :-
    T = temperature,
    all(Tmin, species(_, Tmin, _), TminL),
    all(Tmax, species(_, _, Tmax), TmaxL),
    min_list(TminL, Min),
    max_list(TmaxL, Max).
range_value(T, Min, Max) :-
    T = humidity,
    all(Hmin, stage(_, Hmin, _), HminL),
    all(Hmax, stage(_, Hmax, _), HmaxL),
    min_list(HminL, Min),
    max_list(HmaxL, Max).

% caption_forward/2
caption_forward(P, M, A) :-
    \+ manifest_color(M, _),
    sampling_manifest_section(M, S),
    sensor_store(P, S, M),
    A = symptom(S, M).
caption_forward(P, M, A) :-
    all(X, manifest_color(M, X), CL),
    length(CL, CLL),
    random(0, CLL, N),
    nth0(N, CL, C),
    sampling_manifest_section(M, S),
    sensor_store(P, S, M, C),
    A = symptom(S, M, C).

sensor_store(P, S, M) :- assertz(symptom(S, M)), assertz(plant_symptom(P, S, M)).
sensor_store(P, S, M, C) :- assertz(symptom(S, M, C)), assertz(plant_symptom(P, S, M, C)).
sampling_manifest_section(M, S) :- all(X, manifest_section(M, X), L), random_list_element(L, S).

plant_symptom(P, S, M) :- 
plant_symptom(P, S, M, C).

% sampling_diagnosis/0
sampling_diagnosis :- 
    observed_symptoms,
    setof(diagnosis(P, T, R), (type_body(T, B), plant_symptoms_type_body(P, B)), D),
    maplist(logs, D).
sampling_diagnosis :- 
    observed_symptoms,
    \+ diagnosis(_),
    message_code(no_diagnosis, M),
    writeln(M).

% plant_symptoms_type_body/2 unifies P with the plant affected by the symptoms
plant_symptoms_type_body(P, B) :-
    observed_symptoms(S),
    match(B, S),
    member(X, B),
    plant_by_symptom(P, X).

plant_by_symptom(P, X) :-
    X = symptom(S, M, C),
    plant_symptom(P, S, M, C),
    manifest_color(_, C).
plant_by_symptom(P, X) :-
    X = symptom(S, M),
    plant_symptom(P, S, M).

logs(D) :-
    D = diagnosis(P, T, R),
    assertz(D),
    problem_card(T, A),
    write(P), write(' is affected by '), writeln(A),
    maplist(writeln, R).

match(L1, L2):- forall(member(X, L1), member(X, L2)).


% % temperature/2
% temperature(Plant) :- 
%     device(Plant, Device, _, _),
%     temperature_sampling(Device, temperature_reading),
%     ask(temperature_reading, X).

% % humidity/2
% humidity(Plant, humidity_reading) :-
%     device(Plant, _, Device, _),
%     humidity_sampling(Device, humidity_reading),
%     ask(humidity_reading, X).

% %  caption/3 - device X image of section Y reports caption Z
% caption(Plant, /*CaptionSection,*/ caption_reading) :- 
%     device(Plant, _, _, Device),
%     caption_sampling(Device, /*CaptionSection,*/ caption_reading),
%     ask(caption_reading, X).

% nutrient_deficiency/2
% nutrient_deficiency(Plant, nutrient_deficiency) :-
%     device(Plant, _, _, Device),
%     caption(Device, /*CaptionSection,*/ caption_reading),
%     deficiency(nutrient_deficiency, /*CaptionSection,*/ caption_reading).

% % dry/1 - Soil is dry if the humidity reading of the sensor is lower than the ideal range of the plant's species.
% dry(Plant) :- 
%     plant(Plant, _, _, HumidityRef),
%     stage(HumidityRef, HumidityMin, _),
%     humidity(Plant, humidity_reading),
%     humidity_reading < HumidityMin.

% % wet/1 - Soil is wet if  the humidity reading of the sensor is higher than the ideal range of the plant's species.
% wet(Plant) :-
%     plant(Plant, _, _, HumidityRef),
%     stage(HumidityRef, _, HumidityMax),
%     humidity(Plant, humidity_reading),
%     humidity_reading > HumidityMax.

% % cold/1 - Environment temperature is too low if  the temperature reading of the sensor is lower than the ideal range
% cold(Plant) :- 
%     plant(Plant, TemperatureMin, _, _),
%     temperature(Plant, temperature_reading),
%     temperature_reading < TemperatureMin.

% % hot/1 - Environment temperature is too high if  the temperature reading of the sensor is higher than the ideal range
% hot(Plant) :- 
%     plant(Plant, _, TemperatureMax, _),
%     temperature(Plant, temperature_reading),
%     temperature_reading > TemperatureMax.

% abiotic/2
abiotic(X, dry) :- dry(X).
abiotic(X, wet) :- wet(X).
abiotic(X, cold) :- cold(X).
abiotic(X, hot) :- hot(X).
% abiotic(X, nutrient_deficiency) :- nutrient_deficiency(X, _).

% biotic/2
biotic(X, disease) :- disease(X, Y).
biotic(X, infestation) :- infestation(X, Y).

% status/2
status(X, sick) :- abiotic(X, _).
status(X, sick) :- biotic(X, _).
status(X, healthy) :- \+ status(X, sick).