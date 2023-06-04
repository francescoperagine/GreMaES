:- use_module(library(random)).
:- use_module(library(apply_macros)).
:- use_module(library(ordsets)).

:- dynamic actuator_status/2.
:- dynamic plant_reading/5.
:- dynamic condition/1.
:- dynamic problem/1.

% sampling_size/1
% How many samplings' repetitions must be made for each loop 
sampling_size(50).

% sampling_interval/1
% How many senconds between each sampling
sampling_interval(0).

% reading_variability/1
% How much the readings may differ from the standard range of values
reading_variability(1.3). 

% sign_probability/1
% How probable is to observe a sign during a sampling
sign_probability(1).

% monitor_start/0
monitor_start :-
    logger_init,
    greenhouse_init,
    !,
    monitor_forward,
    health_checker,
    greenhouse_status.

% greenhouse_init/0
greenhouse_init :-
    monitor_cleanup,
    welcome_monitor,
    plants_reading_ranges(PlantsRanges),
    maplist(writeln, PlantsRanges), nl,
    actuators_init.

% monitor_forward/0
monitor_forward :-
    debug_mode,
    consult(percepts_samples).
monitor_forward :-
    \+ debug_mode,
    monitor_loop_start.

% debug_mode/0
debug_mode :- askif(debug_mode).

% monitor_cleanup/0
monitor_cleanup :-
    retractall(asked(_,_)),
    retractall(symptom(_,_,_)),
    retractall(plant_status(_,_,_)),
    retractall(plant_reading(_,_,_,_,_)),
    retractall(actuator_status(_,_)).

% actuators_init/0
actuators_init :-
    all(actuator_status(Actuator, off), plant_actuator(Plant, Actuator), ActuatorsStatus),
    maplist(assertz, ActuatorsStatus).

% monitor_loop_start/0
monitor_loop_start :- 
    retractall(asked(continue_monitor_loop, _)),
    sampling_size(Repetitions),
    sampling_interval(Interval),
    monitor_loop(Repetitions, Interval).

% monitor_loop/2
% It simulates the interval between readings
monitor_loop(Repetitions, Interval) :-
    Interval = 0,
    Repetitions > 0,
    sampling_init,
    RepetitionsNew is Repetitions - 1,
    monitor_loop(RepetitionsNew, Interval).
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
    all(SensorID, plant_sensor(_, SensorID), Sensors), 
    random_list_element(Sensors, RandomSensorID),
    sensor(RandomSensorID, SensorType),
    plant_sensor(Plant, RandomSensorID),
    timestamp(Timestamp),
    sampling(Plant, Timestamp, SensorType, RandomSensorID).

% random_list_element/2
random_list_element(List, Element) :-
    length(List, Length),
    random(0, Length, RandomNumber),
    nth0(RandomNumber, List, Element).

% sampling/4
% Performs a random sampling from a sensor that returns an integer number
sampling(Plant, Timestamp, SensorType, SensorID) :-
    SensorType \= caption,
    reading_variability(Variability),
    plant_range_values(Plant, SensorType, Min, Max),
    MinV is Min / Variability,
    MaxV is Max * Variability,
    (SensorType = humidity, MinV < 0 -> MinV1 = 0 ; MinV1 = MinV),
    (SensorType = humidity, MaxV > 100 -> MaxV1 = 100 ; MaxV1 = MaxV),
    random(MinV1, MaxV1, RandomValue),
    Value is floor(RandomValue),
    PlantReading = plant_reading(Plant, Timestamp, SensorType, SensorID, Value),
    store(PlantReading).
% Performs a random sampling from a camera device that returns a captioned symptom
sampling(Plant, Timestamp, SensorType, SensorID) :-
    SensorType = caption,
    sign_probability(SignProbability),
    random(CaptionIsSign), % Did the camera capture a sign?
    sampling_sign_probability(Plant, Timestamp, SensorType, SensorID, SignProbability, CaptionIsSign).

% plant_range_values/4
plant_range_values(Plant, SensorType, Min, Max) :-
    SensorType = temperature,
    plant(Plant, Species, _),
    species(Species, Min, Max).
plant_range_values(Plant, SensorType, Min, Max) :-
    SensorType = humidity,
    plant(Plant, _, GrowthStage),
    growth_humidity(GrowthStage, Min, Max).

% sampling_sign_probability/6
% Simulates the probability that a plant is healthy reading SignProbability
sampling_sign_probability(Plant, Timestamp, SensorType, SensorID, SignProbability, CaptionIsSign) :-
    CaptionIsSign =< SignProbability,
    call(signs, Signs),
    random_list_element(Signs, Sign),
    caption_forward(Sign, Value), % symptoms are the values of captions!
    PlantReading = plant_reading(Plant, Timestamp, SensorType, SensorID, Value),
    store(PlantReading).
% sampling_sign_probability/6 If the plant is healthy, the symptom(none, all) is stored
sampling_sign_probability(Plant, Timestamp, SensorType, SensorID, SignProbability, CaptionIsSign) :-
    CaptionIsSign > SignProbability,
    caption_forward(none, Value),
    PlantReading = plant_reading(Plant, Timestamp, SensorType, SensorID, Value),
    store(PlantReading).

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

% store/1
% Stores a percept if it has not yet been observed. Does nothing otherwise
store(PlantReading) :-
    PlantReading = plant_reading(Plant, Timestamp, SensorType, SensorID, Value),
    assertz(PlantReading),
    logln(PlantReading).
store(PlantReading) :-
    PlantReading = plant_reading(Plant, Timestamp, SensorType, SensorID, Value),
    plant_reading(Plant, Timestamp, SensorType, SensorID, Value).
store(Symptom) :-  
    Symptom = symptom(Location, Sign, Color),
    \+ symptom(Location, Sign, Color),
    assertz(Symptom),
    log_observation(Symptom),
    logln(Symptom).
store(Symptom) :-  
    Symptom = symptom(Location, Sign, Color),
    symptom(Location, Sign, Color),
    log_observation(Symptom).
store(PlantStatus) :-
    PlantStatus = plant_status(Plant, SensorType, ReadingStatus),
    retractall(plant_status(Plant, SensorType, OldStatus)), % removes all previous stored informations of the same sensor
    assertz(PlantStatus),
    logln(PlantStatus).

% health_checker/0
health_checker :-
    all(Plant, SensorID^plant_sensor(Plant, SensorID), Plants),
    sort(Plants, SortedPlants),
    maplist(parse, SortedPlants).

% parse/1
parse(Plant) :-
    atomic_concat(['\nPlant ', Plant, ' recap: '], Message),
    logln(Message),
    parse_plant_symptoms(Plant),
    retractall(symptom(_,_,_)),
    parse_plant_readings(Plant),
    retractall(plant_reading(Plant,_,_,_,_)).

% parse_plant_symptoms/1
parse_plant_symptoms(Plant) :-
    all(Symptom, (plant_reading(Plant, _, SensorType, _, Symptom), SensorType = caption), PlantSymptoms),
    find_diagnoses(Plant, PlantSymptoms).
parse_plant_symptoms(Plant) :-
    \+ plant_reading(Plant, _, caption, _, Value),
    logln('* shows no symptoms').

% find_diagnoses/2
find_diagnoses(Plant, PlantSymptoms) :-
    % stores in the working memory the plant symptoms to match them with the diagnoses causes
    maplist(store, PlantSymptoms),
    all((Condition, Symptoms), (
        condition(Condition),
        clause(condition(Condition), SymptomsConj),
        conj_to_list(SymptomsConj, Symptoms),
        match(Symptoms, PlantSymptoms),
        store(plant_status(Plant, caption, Condition))
    ), Diagnoses),
    explain_plant_diagnoses(Diagnoses).
find_diagnoses(Plant, PlantSymptoms) :-
    logln('^ no diagnosis').

% explain_plant_diagnoses/1
explain_plant_diagnoses([]).
explain_plant_diagnoses([H|T]) :-
    H = (Condition, Symptoms),
    problem_condition(Problem, Condition),
    atomic_concat(['^ diagnosis is of ', Condition, ' ', Problem, ' because of: '], Message),
    log(Message),
    maplist(logln, Symptoms),
    explain_plant_diagnoses(T).

% parse_plant_readings/1
% Retrieves the SensorType associated with the Plant and starts the parsing
parse_plant_readings(Plant) :-
    all(SensorType, (Plant^plant_sensor(Plant, SensorID), sensor(SensorID, SensorType), SensorType \= caption), SensorTypes),
    log('- installed sensors '), logln(SensorTypes),
    forall(member(SensorType, SensorTypes), parse_plant_readings(Plant, SensorType)).
parse_plant_readings(Plant).

% parse_plant_readings/2
parse_plant_readings(Plant, SensorType) :-
    setof(V, (plant_reading(Plant, Timestamp, SensorType, SensorName, V), SensorType \= caption), Values),
    last(Values, LastValue),
    retractall(plant_reading(Plant, _, SensorType, _, _)),
    (SensorType = temperature -> Type = ' Celsius' ; Type = '%'),
    reading_status(Plant, SensorType, LastValue, ReadingStatus),
    atomic_concat(['* ', SensorType, ' if of ', LastValue, Type, ', status is ', ReadingStatus], Message),
    logln(Message),
    !,
    actuator_start(Plant, SensorType, ReadingStatus).
parse_plant_readings(Plant, SensorType) :-
    \+ plant_reading(Plant, _, SensorType, _, _),
    atomic_concat(['* ', SensorType, ' no reading'], Message),
    logln(Message).

% reading_status/4
reading_status(Plant, SensorType, Value, ReadingStatus) :-
    plant_range_values(Plant, SensorType, Min, Max),
    Value < Min,
    ReadingStatus = low,
    PlantStatus = plant_status(Plant, SensorType, ReadingStatus),
    store(PlantStatus).
reading_status(Plant, SensorType, Value, ReadingStatus) :-
    plant_range_values(Plant, SensorType, Min, Max),
    Value > Max,
    ReadingStatus = high,
    PlantStatus = plant_status(Plant, SensorType, ReadingStatus),
    store(PlantStatus).
reading_status(Plant, SensorType, Value, ReadingStatus) :-
    plant_range_values(Plant, SensorType, Min, Max),
    between(Min, Max, Value),
    ReadingStatus = normal.

% actuator_start/3
% There's no actuator on the plant
actuator_start(Plant, _, _) :- 
    \+ plant_actuator(Plant, _),
    log(Plant, ' , no actuators').
% There's no actuator of the right type (temp/hum)
actuator_start(Plant, SensorType, _) :- 
    \+ actuator(ActuatorID, SensorType, ActivationStatus, Class),
    plant_actuator(Plant, ActuatorID),
    atomic_concat([Plant, ', no ', SensorType, ' actuator'], Message),
    log(Message).
% Retrieves all actuators of the same SensorType (temp/hum) that can handle the CurrentStatus and activates them simultaneously
actuator_start(Plant, SensorType, CurrentStatus) :-
    all((ActuatorID, Class, CurrentStatus, ActivationStatus),
        (plant_actuator(Plant, ActuatorID), actuator(ActuatorID, SensorType, ActivationStatus, Class)),
        Actuators),
    maplist(actuator_forward, Actuators).

% actuator_forward/1
% it turns the actuator off if it's not required anymore
actuator_forward(X) :-
    X = (ActuatorID, Class, CurrentStatus, ActivationStatus),
    CurrentStatus \= ActivationStatus,
    actuator_status(ActuatorID, on), % If it's on, it shuts it down
    retractall(actuator_status(ActuatorID, _)),
    assertz(actuator_status(ActuatorID, off)),
    atomic_concat(['\t', ActuatorID, ' ', Class, ' has been turned off'], Message),
    logln(Message).
% it's off and doesn't handle the actual status, so it does nothing.
actuator_forward(X) :-
    X = (ActuatorID, Class, CurrentStatus, ActivationStatus),
    CurrentStatus \= ActivationStatus,
    \+ actuator_status(ActuatorID, on),
    atomic_concat(['\t', ActuatorID, ' ', Class, ' has nothing to do'], Message),
    logln(Message).
% turns it on the actuator when needed
actuator_forward(X) :-
    X = (ActuatorID, Class, CurrentStatus, ActivationStatus),
    CurrentStatus = ActivationStatus,
    \+ actuator_status(ActuatorID, on), % If it's off, turns it on.
    retractall(actuator_status(ActuatorID, _)),
    assertz(actuator_status(ActuatorID, on)),
    atomic_concat(['\t', ActuatorID, ' ', Class, ' has been turned on'], Message),
    logln(Message).
actuator_forward(X) :-
    X = (ActuatorID, Class, CurrentStatus, ActivationStatus),
    CurrentStatus = ActivationStatus,
    actuator_status(ActuatorID, on), % If it's already on, does nothing.
    atomic_concat(['\t', ActuatorID, ' ', Class, ' was already on'], Message),
    logln(Message).

% greenhouse_status/0
greenhouse_status :-
    logln('\nGreenhouse status:'),
    all(plant_health(Plant, Health), (plants(Plants), member(Plant, Plants), plant_health(Plant, Health)), PlantsStatuses),
    maplist(logln, PlantsStatuses),
    logln('\n').

% plant_health/2
plant_health(Plant, Health) :-
    plant_status(Plant, SensorType, Reading),
    clause(problem(Problem), reading(SensorType, Reading)),
    issue_problem(Issue, Problem),
    Health = Issue-Problem.
plant_health(Plant, Health) :-
    plant_status(Plant, SensorType, Reading),
    problem_condition(Problem, Reading),
    issue_problem(Issue, Problem),
    Health = Issue-Problem-Reading.
plant_health(Plant, Health) :-
    plant_status(Plant, SensorType, Reading),
    \+ clause(problem(Problem), reading(SensorType, Reading)),
    \+ problem_condition(Problem, Reading),
    Health = healthy.
plant_health(Plant, Health) :-
    \+ plant_status(Plant, SensorType, Reading),
    Health = unknown.