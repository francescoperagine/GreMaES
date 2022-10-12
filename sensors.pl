reading_variance(X) :- X is 1.3.

% sensor_mode/0 Starts the sensor mode.
sensor_init :-
    cleanup_init,
    welcome_sensor,
    plants(L),
    maplist(wwuln, L),
    loop_init.

plants(L) :- all(P-temp-Tmin-Tmax-hum-Hrange, plant(P, Tmin, Tmax, Hrange) , L).

loop_init :- time_loop(3, 3).

% time_loop/1 - Loops X of duration Y in seconds. For each repetition, simulates a sensor reading.
time_loop(X, Y) :-
    X > 0,
    sampling_monitoring,
    sampling_diagnosis,
    retractall(asked(continue_time_loop, _)),
    sleep(Y),
    X1 is X - 1,
    time_loop(X1, Y).
time_loop(X, Y) :-
    X = 0,
    askif(continue_time_loop) -> 
        loop_init
        ;
        !, cleanup_init, abort.

sampling_monitoring :- 
    all(X, sensor_class(X), TL),
    length(TL, TLL),
    random(0, TLL, RTN), 
    nth0(RTN, TL, T),
    sensors(T, SL), 
    length(SL, SLL), 
    random(0, SLL, RSN), 
    nth0(RSN, SL, S), 
    sampling(T, S).

% sensors/2
sensors(T, L) :- T = temperature, all(D, device(_, D, _, _), L).
sensors(T, L) :- T = humidity, all(D, device(_, _, D, _), L).
sensors(T, L) :- T = caption, all(D, device(_, _, _, D), L).

% sensor_plant/2
sensor_plant(S, P) :- device(P, S, _, _).
sensor_plant(S, P) :- device(P, _, S, _).
sensor_plant(S, P) :- device(P, _, _, S).

% reading/2 - Type T, sensor S
sampling(T, S) :-
    T \= caption,
    reading_variance(V),
    range_value(T, Min, Max),
    MinV is Min / V,
    MaxV is Max * V,
    random(MinV, MaxV, N),
    sampling_output(T, S, N).
sampling(T, S) :-
    T = caption,
    manifestations(ML),
    length(ML, MLL),
    random(0, MLL, N), 
    nth0(N, ML, M), 
    caption_forward(M, A),
    sampling_output(T, S, A).

% sampling_output/3
sampling_output(T, S, A) :-
    sensor_plant(S, P),
    datime(X), X =.. [datime|L],
    atomic_list_concat(L, '-', D),
    atomic_list_concat([D, T, S, A], '_', R),
    assertz(sampling(R)),
    wwu(R).

% range_value/3 Unifies min/max with the respective types
range_value(T, Min, Max) :-
    T = temperature,
    all(Tmin, plant(_, Tmin, _, _), TminL),
    all(Tmax, plant(_, _, Tmax, _), TmaxL),
    min_list(TminL, Min),
    max_list(TmaxL, Max).
range_value(T, Min, Max) :-
    T = humidity,
    all(Hmin, humidity_range(_, Hmin, _), HminL),
    all(Hmax, humidity_range(_, Hmax, _), HmaxL),
    min_list(HminL, Min),
    max_list(HmaxL, Max).

% caption_forward/2
caption_forward(M, A) :-
    \+ manifest_color(M, _),
    caption_section(M, S),
    store(S, M),
    atomic_list_concat([S, M], '_', A).
caption_forward(M, A) :-
    all(X, manifest_color(M, X), CL),
    length(CL, CLL),
    random(0, CLL, N),
    nth0(N, CL, C),
    caption_section(M, S),
    store(S, M, C),
    atomic_list_concat([S, M, C], '_', A).

% caption_section/2
caption_section(M, S) :-
    all(X, manifest_section(M, X), SL),
    length(SL, SLL),
    random(0, SLL, N),
    nth0(N, SL, S).

% sampling_diagnosis/0
sampling_diagnosis :- 
    observed_symptoms,
    all(R, (type(X), clause(type(X), R)), C),
    maplist(problem_card, C, C1),
    maplist(wwuln, C1), nl.
sampling_diagnosis :- 
    observed_symptoms,
    message_code(no_diagnosis, M),
    wwu(' '), wwuln(M), nl.

% temperature_sampling(Device, temperature_reading) :- ask(temperature_reading, X).

% humidity_sampling(Device, humidity_reading) :- 

% caption_sampling(Device, /*CaptionSection,*/ caption_reading) :- 

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
%     humidity_range(HumidityRef, HumidityMin, _),
%     humidity(Plant, humidity_reading),
%     humidity_reading < HumidityMin.

% % wet/1 - Soil is wet if  the humidity reading of the sensor is higher than the ideal range of the plant's species.
% wet(Plant) :-
%     plant(Plant, _, _, HumidityRef),
%     humidity_range(HumidityRef, _, HumidityMax),
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