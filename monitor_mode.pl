:- use_module(library(system)).
:- use_module(library(random)).
:- use_module(library(ordsets)).

:- dynamic actuator_status/2.
:- dynamic plant_reading/5.
:- dynamic condition/1.

% How many samplings' repetitions must be made for each loop 
loop_repetitions(X) :- X is 10.
% How many senconds between each sampling
loop_interval(X) :- X is 1.
% How much the readings may differ from the standard range of values
reading_variability(X) :- X is 1.3. 
% How probable is to observe a sign during a sampling
sign_probability(X) :- X is 1.

% monitor_mode_start/0
monitor_mode_start :-
    monitor_cleanup,
    welcome_monitor,
    % greenhouse_init,
    actuator_init,
    plants(Plants),
    maplist(writeln, Plants), nl,
    monitor_mode_forward,
    health_checker.
    % greenhouse_status.

% monitor_mode_forward/0
monitor_mode_forward :-
    debug_mode,
    consult(debug).
monitor_mode_forward :-
    \+ debug_mode,
    monitor_loop_start.

% debug_mode/0
debug_mode :- askif(debug_mode).

% monitor_cleanup/0
monitor_cleanup :-
    retractall(asked(_,_)),
    retractall(symptom(_,_,_)),
    retractall(actuator_status(_,_)).

% greenhouse_init/0 Initializes the status of every plant as healthy
% greenhouse_init :-
%     timestamp(TS),
%     all(plant_overview(P, TS, C, T), plant(P,_,_), L),
%     maplist(assertz, L).

% timestamp/1
timestamp(T) :- 
    datime(datime(Year, Month, Day, Hour, Minute, Second)),
    T = timestamp(Year-Month-Day, Hour:Minute:Second).

% actuator_init/0
actuator_init :-
    all(actuator_status(Actuator, off), plant_actuator(Plant, Actuator), ActuatorsStatus),
    maplist(assertz, ActuatorsStatus).

% plants/1 
plants(Plants) :-
    all(
        Plant-Species-temperature-TemperatureMin-TemperatureMax-humidity-HumidityStage-HumidityMin-HumidityMax,
        (plant(Plant, Species, HumidityStage), species(Species, TemperatureMin, TemperatureMax), humidity_range(HumidityStage, HumidityMin, HumidityMax)),
        Plants
    ).

% monitor_loop_start/0
monitor_loop_start :- 
    retractall(asked(continue_monitor_loop, _)),
    loop_repetitions(Repetitions),
    loop_interval(Interval),
    monitor_loop(Repetitions, Interval).

% It simulates the interval between readings
monitor_loop(Repetitions, Interval) :-
    Repetitions > 0,
    sampling_init,
    sleep(Interval),
    RepetitionsNew is Repetitions - 1,
    monitor_loop(RepetitionsNew, Interval).
% at the end of the loop, asks if the user wants more samplings.
monitor_loop(Repetitions, Interval) :-
    Repetitions = 0,
    askif(continue_monitor_loop),
    monitor_loop_start.
% At the end of the sampling, it finds out the diagnoses and the starts the actuators.
monitor_loop(Repetitions, Interval) :-
    Repetitions = 0.

% sampling_init/0
% the random sensor is picked up from those already active on plants
sampling_init :- 
    all(Sensor, plant_sensor(_, Sensor), Sensors), 
    random_list_element(Sensors, RandomSensor),
    sensor(RandomSensor, SensorType),
    plant_sensor(Plant, RandomSensor),
    timestamp(Timestamp),
    sampling(Plant, Timestamp, SensorType, RandomSensor).

% random_list_element/2
random_list_element(List, Element) :-
    length(List, Length),
    random(0, Length, RandomNumber),
    nth0(RandomNumber, List, Element).

% sampling/4 Performs a random sampling from a sensor that returns an integer number
sampling(Plant, Timestamp, SensorType, Sensor) :-
    SensorType \= caption,
    reading_variability(Variability),
    range_value(SensorType, Min, Max),
    MinV is Min / Variability,
    MaxV is Max * Variability,
    (SensorType = humidity, MinV < 0 -> MinV1 = 0 ; MinV1 = MinV),
    (SensorType = humidity, MaxV > 100 -> MaxV1 = 100 ; MaxV1 = MaxV),
    random(MinV1, MaxV1, RandomValue),
    Value is floor(RandomValue),
    PlantReading = plant_reading(Plant, Timestamp, SensorType, Sensor, Value),
    store(PlantReading).
% sampling/4 Performs a random sampling from a camera device that returns a captioned symptom
sampling(Plant, Timestamp, SensorType, Sensor) :-
    SensorType = caption,
    sign_probability(SignProbability),
    random(CaptionIsSign), % Did the camera capture a sign?
    sampling_probability(Plant, Timestamp, SensorType, Sensor, SignProbability, CaptionIsSign).

% sampling/6 Simulates the probability that a plant is healthy reading SignProbability
sampling_probability(Plant, Timestamp, SensorType, Sensor, SignProbability, CaptionIsSign) :-
    CaptionIsSign =< SignProbability,
    call(signs, Signs),
    random_list_element(Signs, Sign),
    caption_forward(Sign, Value), % symptoms are the values of captions!
    PlantReading = plant_reading(Plant, Timestamp, SensorType, Sensor, Value),
    store(PlantReading).
% sampling/6 If the plant is healthy, the symptom(none, all) is stored
sampling_probability(Plant, Timestamp, SensorType, Sensor, SignProbability, CaptionIsSign) :-
    CaptionIsSign > SignProbability,
    caption_forward(none, Value),
    PlantReading = plant_reading(Plant, Timestamp, SensorType, Sensor, Value),
    store(PlantReading).

% store/5
store(PlantReading) :-
    PlantReading = plant_reading(Plant, Timestamp, SensorType, Sensor, Value),
    writeln(PlantReading),
    assertz(PlantReading).
    % log(PlantReading).
% store(Reading) :-
%     Reading = reading(SensorType, Value),
%     writeln(Reading),
%     assertz(Reading).
%     % log(Reading).
% Stores a symptom if it has not yet been stored
store(Symptom) :-  
    Symptom = symptom(Location, Sign, Color),
    % if it has not been stored yet, stores it.
    \+ symptom(Location, Sign, Color),
    assertz(Symptom).
    % log(Symptom).
% Does nothing if the symptom is already present
store(Symptom) :-  
    Symptom = symptom(Location, Sign, Color),
    symptom(Location, Sign, Color).
% store/1
% store(X) :-
%     X = actuator(ActuatorID, ActuatorStatus),
%     retractall(actuator_status(ActuatorID, _)),
%     assertz(actuator_status(ActuatorID, ActuatorStatus)).
    % log(X).

% range_value/3 Unifies min/max with the respective sensor_type between all species
% Get range values of temperature
range_value(SensorType, Min, Max) :-
    SensorType = temperature,
    all(Tmin, species(_, Tmin, _), TminL),
    all(Tmax, species(_, _, Tmax), TmaxL),
    min_list(TminL, Min),
    max_list(TmaxL, Max).
% Get range values of humidity
range_value(SensorType, Min, Max) :-
    SensorType = humidity,
    all(Hmin, humidity_range(_, Hmin, _), HminL),
    all(Hmax, humidity_range(_, _, Hmax), HmaxL),
    min_list(HminL, Min),
    max_list(HmaxL, Max).

% caption_forward/2
% If there is no associated color, sets the 3rd argument to none
caption_forward(Sign, Symptom) :-
    \+ sign_color(Sign, _),
    sampling_sign_location(Sign, RandomLocation),
    Symptom = symptom(RandomLocation, Sign, none).
caption_forward(Sign, Symptom) :-
    all(Color, sign_color(Sign, Color), Colors),
    random_list_element(Colors, RandomColor),
    sampling_sign_location(Sign, RandomLocation),
    Symptom = symptom(RandomLocation, Sign, RandomColor).

% sampling_sign_location/2 - Unifies S with a random location related to the sign
sampling_sign_location(Sign, RandomLocation) :- 
    all(Location, sign_location(Sign, Location), Locations),
    random_list_element(Locations, RandomLocation).

% health_checker/0
health_checker :-
    all(Plant, ActuatorID^plant_actuator(Plant, ActuatorID), Plants),
    sort(Plants, SortedPlants),
    writeln(SortedPlants),
    % forall(member(Plant, SortedPlants), parse_reading(Plant), retractall(plant_reading(Plant,_,_,_,_))).
    maplist(parse, SortedPlants).

% parse/1
parse(Plant) :-
    atomic_concat(['\nPlant ', Plant, ' recap: '], Message),
    log(Message),
    writeln(Message),
    parse_symptom(Plant),
    retractall(symptom(_,_,_)),
    parse_reading(Plant),
    retractall(plant_reading(Plant,_,_,_,_)).

% parse_forward/1
parse_symptom(Plant) :-
    all(Symptom, (plant_reading(Plant, _, SensorType, _, Symptom), SensorType = caption), PlantSymptoms),
    find_diagnoses(PlantSymptoms).
parse_symptom(Plant) :-
    \+ plant_reading(Plant, _, caption, _, Value),
    writeln('* shows no symptoms.').

% find_diagnoses/1
find_diagnoses(PlantSymptoms) :-
    % stores in the working memory the plant symptoms to match them with the diagnoses causes
    maplist(store, PlantSymptoms),
    all((Condition, Symptoms), (condition(Condition), clause(condition(Condition), SymptomsConj), conj_to_list(SymptomsConj, Symptoms), match(Symptoms, PlantSymptoms)), Diagnoses),
    explain_plant_diagnoses(Diagnoses).
find_diagnoses :-
    writeln('\thas no clear diagnosis.').

% explain_plant_diagnoses/1
explain_plant_diagnoses([]).
explain_plant_diagnoses([H|T]) :-
    H = (Condition, Symptoms),
    problem_condition(Problem, Condition),
    atomic_concat(['\tdiagnosis is of ', Condition, ' ', Problem, ' because of: '], Message),
    write(Message),
    maplist(writeln, Symptoms),
    explain_plant_diagnoses(T).

% abiotic_status/2
% abiotic_status(P, H) :-
%     plant(X,_,_),
%     diagnosis(X,Y,_),
%     Y = M:S:V,
%     abiotic_status_forward(M, S, H).

% abiotic_status(humidity, high, wet).
% abiotic_status(humidity, low, dry).
% abiotic_status(temperature, high, hot).
% abiotic_status(temperature, low, cold).

% abiotic_status_forward/3
% abiotic_status_forward(M, S, H) :-
%     M = humidity,
%     S = high,
%     H = wet.
% abiotic_status_forward(M, S, H) :-
%     M = humidity,
%     S = low,
%     H = dry.
% abiotic_status_forward(M, S, H) :-
%     M = temperature,
%     S = high,
%     H = hot.
% abiotic_status_forward(M, S, H) :-
%     M = temperature,
%     S = low,
%     H = cold.


% parse_reading/1
% Retrieves the SensorType associated with the Plant and starts the parsing
parse_reading(Plant) :-
    all(SensorType, (Plant^plant_actuator(Plant, ActuatorID), ActuatorID^actuator(ActuatorID, SensorType,_,_)), SensorTypes),
    write('- installed sensors '), writeln(SensorTypes),
    forall(member(SensorType, SensorTypes), parse_reading(Plant, SensorType)).

% parse_reading/2
parse_reading(Plant, SensorType) :-
    setof(V, (plant_reading(Plant, Timestamp, SensorType, SensorName, V), SensorType \= caption), Values),
    last(Values, LastValue),
    retractall(plant_reading(Plant, _, SensorType, _, _)),
    (SensorType = temperature -> Type = ' Celsius' ; Type = '%'),
    reading_status(Plant, SensorType, LastValue, Status),
    atomic_concat(['* ', SensorType, ' if of ', LastValue, Type, ', status is ', Status], Message),
    log(Message),
    writeln(Message),
    !,
    actuator_start(Plant, SensorType, Status).
parse_reading(Plant, SensorType) :-
    \+ plant_reading(Plant, _, SensorType, _, _),
    atomic_concat(['* ', SensorType, ' has_no_reading'], Message),
    log(Message),
    writeln(Message).
parse_reading(Plant, SensorType) :-
    writeln('parse_reading something went awry').

% reading_status/4
reading_status(Plant, SensorType, Value, Status) :-
    plant_range_values(Plant, SensorType, Min, Max),
    Value < Min,
    Status = low. 
reading_status(Plant, SensorType, Value, Status) :-
    plant_range_values(Plant, SensorType, Min, Max),
    Value > Max,
    Status = high.
reading_status(Plant, SensorType, Value, Status) :-
    plant_range_values(Plant, SensorType, Min, Max),
    between(Min, Max, Value),
    Status = ok.

% plant_range_values/4
plant_range_values(Plant, SensorType, Min, Max) :-
    SensorType = temperature,
    plant(Plant, Species, _),
    species(Species, Min, Max).
plant_range_values(Plant, SensorType, Min, Max) :-
    SensorType = humidity,
    plant(Plant, _, HumidityStage),
    humidity_range(HumidityStage, Min, Max).

% actuator_start/3
% There's no actuator on the plant
actuator_start(Plant, _, _) :- 
    \+ plant_actuator(Plant, _),
    Message = ', no_actuators',
    write(Message),
    log(Plant, ' ', Message).
% There's no actuator of the right type (temp/hum)
actuator_start(Plant, SensorType, _) :- 
    \+ actuator(ActuatorID, SensorType, HandledStatus, ActuatorClass),
    plant_actuator(Plant, ActuatorID),
    atomic_concat([', no_', SensorType, '_actuator'], Message),
    write(Message),
    log(Plant, ' ', Message).
% Retrieves all actuators of the same SensorType (temp/hum) that can handle the ActualStatus and activates them simultaneously
actuator_start(Plant, SensorType, ActualStatus) :-
    all((ActuatorID, ActuatorClass, ActualStatus, HandledStatus),
        (plant_actuator(Plant, ActuatorID), actuator(ActuatorID, SensorType, HandledStatus, ActuatorClass)),
        Actuators),
    maplist(actuator_forward, Actuators).

% actuator_forward/4
% it turns the actuator off if it's not required anymore
actuator_forward(X) :-
    X = (ActuatorID, ActuatorClass, ActualStatus, HandledStatus),
    ActualStatus \= HandledStatus,
    actuator(ActuatorID, on), % If it's on, it shuts it down
    retractall(actuator(ActuatorID, _)),
    assertz(actuator(ActuatorID, off)),
    atomic_concat(['\t', ActuatorID, ' ', ActuatorClass, ' was_on, now_shutted_off'], Message),
    log(Message),
    writeln(Message).
% it's off and doesn't handle the actual status, so it does nothing.
actuator_forward(X) :-
    X = (ActuatorID, ActuatorClass, ActualStatus, HandledStatus),
    ActualStatus \= HandledStatus,
    \+ actuator(ActuatorID, on),
    atomic_concat(['\t', ActuatorID, ' ', ActuatorClass, ' has_nothing_to_do'], Message),
    log(Message),
    writeln(Message).
% turns it on the actuator when needed
actuator_forward(X) :-
    X = (ActuatorID, ActuatorClass, ActualStatus, HandledStatus),
    ActualStatus = HandledStatus,
    \+ actuator(ActuatorID, on), % If it's off, turns it on.
    retractall(actuator(ActuatorID, _)),
    assertz(actuator(ActuatorID, on)),
    atomic_concat(['\t', ActuatorID, ' ', ActuatorClass, ' was_off, now_turned_on'], Message),
    log(Message),
    writeln(Message).
actuator_forward(X) :-
    X = (ActuatorID, ActuatorClass, ActualStatus, HandledStatus),
    ActualStatus = HandledStatus,
    actuator(ActuatorID, on), % If it's already on, does nothing.
    atomic_concat(['\t', ActuatorID, ' ', ActuatorClass, ' was_already_on'], Message),
    log(Message),
    writeln(Message).

% actuator_on/1
% actuator_on(ActuatorID, ActuatorClass) :-
%     \+ actuator_status(ActuatorID, on),
%     atomic_concat([' * ', ActuatorClass, ' ', ActuatorID, ' ', turned_on], Message),
%     writeln(Message),
%     retractall(actuator(ActuatorID, _)),
%     assertz(actuator(ActuatorID, on)),
%     log(ActuatorID, ActuatorClass, actuator_status(ActuatorID, on), Message).
% actuator_on(ActuatorID, ActuatorClass) :-
    % actuator_status(ActuatorID, on),

    % atomic_concat([' * ', ActuatorClass, ' ', ActuatorID, ' ', already_on], Message),
    % writeln(Message).
    % retract(actuator_status(ActuatorID, _)),
    % assertz(actuator_status(ActuatorID, on)).
% actuator_on(A) :- 
%     assertz(actuator_status(ActuatorID, on)).

% actuator_off/1
% actuator_off(ActuatorID, ActuatorClass) :-
%     \+ actuator_status(ActuatorID, off),
%     atomic_concat([' * ', ActuatorClass, ' ', ActuatorID, ' ', turned_off], Message),
%     writeln(Message),
%     retractall(actuator(ActuatorID, _)),
%     assertz(actuator(ActuatorID, off)),
%     log(ActuatorID, ActuatorClass, actuator_status(ActuatorID, off), Message).
% actuator_off(ActuatorID, ActuatorClass) :-
    % actuator_status(ActuatorID, off),

    % store(actuator(C)),
    % retract(actuator_status(ActuatorID, _)),
    % assertz(actuator_status(ActuatorID, off)).
% actuator_off(A) :- 
%     assertz(actuator_status(ActuatorID, off)).

% % actuator_activate/5
% actuator_activate(A, T, S, K, H) :-
%     (S = normal ; S \= H),
%     actuator_status(A, on),
%     actuator_off(A),
%     atomic_concat([' * ', K, ' ', A, ' ', turned_off], C),
%     store(actuator(C)).
% actuator_activate(A, T, S, K, H) :-
%     (S = normal ; S \= H),
%     actuator_status(A, off),
%     atomic_concat([' * ', K, ' ', A, ' ', already_off], C),
%     store(actuator(C)).
% actuator_activate(A, T, S, K, H) :-
%     S \= normal,
%     S = H,
%     actuator_status(A, off),
%     actuator_on(A),
%     atomic_concat([' * ', K, ' ', A, ' ', turned_on], C),
%     store(actuator(C)).
% actuator_activate(A, T, S, K, H) :-
%     S \= normal,
%     S = H,
%     actuator_status(A, on),
%     atomic_concat([' * ', K, ' ', A, ' ', already_on], C),
%     store(actuator(C)).
% awry backup
% actuator_activate(A, T, H, K, S) :-
%     X = p_actuator(A, T, H, K, S),
%     atomic_concat([A, T, H, K, S, something_went_awry], C),
%     store(actuator(C)).

% % actuator_on/1
% actuator_on(A) :-
%     retract(actuator_status(A, _)),
%     assertz(actuator_status(A, on)).
% actuator_on(A) :- 
%     assertz(actuator_status(A, on)).

% % actuator_off/1
% actuator_off(A) :-
%     retract(actuator_status(A, _)),
%     assertz(actuator_status(A, off)).
% actuator_off(A) :- 
%     assertz(actuator_status(A, off)).

% greenhouse_status/0
greenhouse_status :-
    all(plant_overview(P, TS, C, T), (plant(P,_,_), plant_overview(P, TS, C, T)), L),
    maplist(writeln, L).