:- use_module(library(random)).
:- use_module(library(apply_macros)).
:- use_module(library(ordsets)).

% sampling_size/1
% How many samplings' repetitions must be made for each loop 
sampling_size(50).

% sampling_interval/1
% How many senconds between each sampling
sampling_interval(0).

% reading_variability/1
% How much a readings may differ from the ideal range of values.
reading_variability(1.3). 

% symptom_probability/1
% How probable is to observe a symptom during the sampling
symptom_probability(1).

monitor_init :-
    actuators_init.

% monitor_start/0
monitor_start :-
    % monitor_cleanup,
    welcome_monitor,
    greenhouse_init,
    !,
    % monitor_forward,
    monitor_loop_start,
    % forward,
    % health_checker,
    greenhouse_status.

% greenhouse_init/0
greenhouse_init :-
    plants_reading_ranges(PlantsRanges),
    maplist(writeln,PlantsRanges),nl.

% monitor_cleanup/0
% monitor_cleanup :-
%     retractall(asked(_,_)),
%     retractall(plant_status(_,_,_)),
%     retractall(actuator_status(_,_)).

% actuators_init/0
actuators_init :-
    all(Actuator,plant_actuator(_,Actuator),Actuators),
    member(Actuator,Actuators),
    save_actuator(Actuator,off).

% monitor_loop_start/0
monitor_loop_start :- 
    retractall(asked(continue_monitor_loop,_)),
    sampling_size(Repetitions),
    sampling_interval(Interval),
    monitor_loop(Repetitions,Interval).

% monitor_loop/2 (+Repetitions,+Interval)
% Performs a fixed number of timed samplings
monitor_loop(Repetitions,Interval) :-
    Repetitions > 0,
    sampling_init,
    (Interval > 0 -> sleep(Interval) ; true),
    RepetitionsNew is Repetitions - 1,
    monitor_loop(RepetitionsNew,Interval).
% The user is asked to continue the sampling.
monitor_loop(0,_) :-
    continue_sampling,
    monitor_loop_start.
% At the end of the sampling,it finds out the diagnoses and the starts the actuators.
monitor_loop(0,_) :-
    \+ continue_sampling.

% continue_sampling/0
continue_sampling :-
    askif(continue_monitor_loop).

% sampling_init/0
% Picks a random sensor from those already active on plants,
% and initializes the sampling which includes a timestamp
sampling_init :- 
    all(SensorID,plant_sensor(_,SensorID),Sensors),
    random_list_element(Sensors,RandomSensorID),
    sensor(RandomSensorID,SensorType),
    plant_sensor(Plant,RandomSensorID),
    timestamp(Timestamp), % evaluated here to fix the timestamp
    sampling(Plant,Timestamp,SensorType,RandomSensorID).

% random_list_element/2 (+List,-Element)
% randomly selects an element from a list
random_list_element(List,Element) :-
    length(List,Length),
    random(0,Length,RandomNumber),
    nth0(RandomNumber,List,Element).

% sampling/4 (+Plant,+Timestamp,+SensorType,+SensorID)
% Performs the sampling of an environment metric.
sampling(Plant,Timestamp,SensorType,SensorID) :-
    SensorType \= caption,
    reading_variability(Variability),
    plant_range_values(Plant,SensorType,Min,Max),
    MinV is Min / Variability,
    MaxV is Max * Variability,
    % humidity values are fixed in a [0-100] range
    (SensorType = humidity,MinV < 0 -> MinV1 = 0 ; MinV1 = MinV), 
    (SensorType = humidity,MaxV > 100 -> MaxV1 = 100 ; MaxV1 = MaxV),
    random(MinV1,MaxV1,RandomValue),
    Value is floor(RandomValue),
    reading_status(Plant,SensorType,Value,ReadingStatus),
    save_observation(Plant,reading(SensorType,ReadingStatus)),
    save_ln(plant_status(Plant,SensorType,ReadingStatus)).
    % save_ln(plant_reading(Plant,Timestamp,SensorType,SensorID,Value)).
% Performs a random sampling from a camera device
sampling(Plant,Timestamp,caption,SensorID) :-
    symptom_probability(SymptomProbability),
    random(RandomValue),
    sampling_symptom(Plant,Timestamp,SensorType,SensorID,SymptomProbability,RandomValue).

% plant_range_values/4 (+Plant,+SensorType,-Min,-Max)
% Returs environment metric's SensorType range values
plant_range_values(Plant,temperature,Min,Max) :-
    plant(Plant,Species,_),
    species(Species,Min,Max).
plant_range_values(Plant,humidity,Min,Max) :-
    plant(Plant,_,GrowthStage),
    growth_humidity(GrowthStage,Min,Max).

% sampling_symptom/6 (+Plant,+Timestamp,+SensorType,+SensorID,+SymptomProbability,+RandomValue)
% Performs the sampling of a symptom based on the probability that it happens.
% This is to simulate an actual environment and eventually to perform stress tests of the system.
% For the case study purpose, the probability of a symptom happening is 100% to test out the
% inference system when multiple symptoms are acquired for each plant.
sampling_symptom(Plant,Timestamp,SensorType,SensorID,SymptomProbability,RandomValue) :-
    RandomValue =< SymptomProbability,
    signs(Signs),
    random_list_element(Signs,Sign),
    caption_forward(Sign,Value),    % symptoms are values for captions!
    save_observation(Plant,Value).
    % save_ln(plant_reading(Plant,Timestamp,SensorType,SensorID,Value)).
% If no symptom is acquired, the null symptom is saved to keep the log consistent with the plant history.
sampling_symptom(Plant,Timestamp,SensorType,SensorID,SymptomProbability,RandomValue) :-
    RandomValue > SymptomProbability,
    save_observation(Plant,symptom(none,all)).
    % save_ln(plant_reading(Plant,Timestamp,SensorType,SensorID,symptom(none,all))).

% caption_forward/2 (+Sign,-Symptom)
% Matches Location and Color with a random values plausible for the Sign.
caption_forward(Sign,symptom(RandomLocation,Sign,RandomColor)) :-
    all(Color,sign_color(Sign,Color),Colors),
    random_list_element(Colors,RandomColor),
    random_sign_location(Sign,RandomLocation).
% For those symptoms which do not have any associated color, sets the 3rd argument to none
caption_forward(Sign,symptom(RandomLocation,Sign,none)) :-
    \+ sign_color(Sign,_),
    random_sign_location(Sign,RandomLocation).

% random_sign_location/2 (+Sign,-RandomLocation)
% Unifies S with a random location related to the sign
random_sign_location(Sign,RandomLocation) :- 
    all(Location,sign_location(Sign,Location),Locations),
    random_list_element(Locations,RandomLocation).

% % health_checker/0
% health_checker :-
%     all(Plant,SensorID^plant_sensor(Plant,SensorID),Plants),
%     sort(Plants,SortedPlants),
%     maplist(parse,SortedPlants).

% % parse/1
% parse(Plant) :-
%     atomic_concat(['\nPlant ',Plant,' recap: '],Message),
%     logln(Message),
%     parse_plant_symptoms(Plant),
%     retractall(symptom(_,_,_)),
%     parse_plant_readings(Plant),
%     retractall(plant_reading(Plant,_,_,_,_)).

% % parse_plant_symptoms/1
% parse_plant_symptoms(Plant) :-
%     all(Symptom,(plant_reading(Plant,_,SensorType,_,Symptom),SensorType = caption),PlantSymptoms),
%     find_diagnoses(Plant,PlantSymptoms).
% parse_plant_symptoms(Plant) :-
%     \+ plant_reading(Plant,_,caption,_,Value),
%     logln('* shows no symptoms').

% % find_diagnoses/2
% find_diagnoses(Plant,PlantSymptoms) :-
%     % stores in the working memory the plant symptoms to match them with the diagnoses causes
%     maplist(store,PlantSymptoms),
%     all((Condition,Symptoms),(
%         condition(Condition),
%         clause(condition(Condition),SymptomsConj),
%         conj_to_list(SymptomsConj,Symptoms),
%         match(Symptoms,PlantSymptoms),
%         store(plant_status(Plant,caption,Condition))
%     ),Diagnoses),
%     explain_plant_diagnoses(Diagnoses).
% find_diagnoses(Plant,PlantSymptoms) :-
%     logln('^ no diagnosis').

% % explain_plant_diagnoses/1
% explain_plant_diagnoses([]).
% explain_plant_diagnoses([H|T]) :-
%     H = (Condition,Symptoms),
%     problem_condition(Problem,Condition),
%     atomic_concat(['^ diagnosis is of ',Condition,' ',Problem,' because of: '],Message),
%     log(Message),
%     maplist(logln,Symptoms),
%     explain_plant_diagnoses(T).

% % parse_plant_readings/1
% % Retrieves the SensorType associated with the Plant and starts the parsing
% parse_plant_readings(Plant) :-
%     all(SensorType,(Plant^plant_sensor(Plant,SensorID),sensor(SensorID,SensorType),SensorType \= caption),SensorTypes),
%     log('- installed sensors '),logln(SensorTypes),
%     forall(member(SensorType,SensorTypes),parse_plant_readings(Plant,SensorType)).
% parse_plant_readings(Plant).

% % parse_plant_readings/2
% parse_plant_readings(Plant,SensorType) :-
%     setof(V,(plant_reading(Plant,Timestamp,SensorType,SensorName,V),SensorType \= caption),Values),
%     last(Values,LastValue),
%     retractall(plant_reading(Plant,_,SensorType,_,_)),
%     (SensorType = temperature -> Type = ' Celsius' ; Type = '%'),
%     reading_status(Plant,SensorType,LastValue,ReadingStatus),
%     atomic_concat(['* ',SensorType,' if of ',LastValue,Type,',status is ',ReadingStatus],Message),
%     logln(Message),
%     !,
%     actuator_start(Plant,SensorType,ReadingStatus).
% parse_plant_readings(Plant,SensorType) :-
%     \+ plant_reading(Plant,_,SensorType,_,_),
%     atomic_concat(['* ',SensorType,' no reading'],Message),
%     logln(Message).

% reading_status/4
reading_status(Plant,SensorType,Value,ReadingStatus) :-
    plant_range_values(Plant,SensorType,Min,Max),
    Value < Min,
    ReadingStatus = low.
reading_status(Plant,SensorType,Value,ReadingStatus) :-
    plant_range_values(Plant,SensorType,Min,Max),
    Value > Max,
    ReadingStatus = high.
reading_status(Plant,SensorType,Value,ReadingStatus) :-
    plant_range_values(Plant,SensorType,Min,Max),
    between(Min,Max,Value),
    ReadingStatus = normal.

% actuator_start/3
% There's no actuator on the plant
actuator_start(Plant,_,_) :- 
    \+ plant_actuator(Plant,_),
    log(Plant,' ,no actuators').
% There's no actuator of the right type (temp/hum)
actuator_start(Plant,SensorType,_) :- 
    \+ actuator(ActuatorID,SensorType,ActivationStatus,Class),
    plant_actuator(Plant,ActuatorID),
    atomic_concat([Plant,',no ',SensorType,' actuator'],Message),
    log(Message).
% Retrieves all actuators of the same SensorType (temp/hum) that can handle the CurrentStatus and activates them simultaneously
actuator_start(Plant,SensorType,CurrentStatus) :-
    all((ActuatorID,Class,CurrentStatus,ActivationStatus),
        (plant_actuator(Plant,ActuatorID),actuator(ActuatorID,SensorType,ActivationStatus,Class)),
        Actuators),
    maplist(actuator_forward,Actuators).

% actuator_forward/1
actuator_forward(X) :-
    X = (ActuatorID,Class,CurrentStatus,ActivationStatus),
    CurrentStatus \= ActivationStatus,
    save_actuator(ActuatorID,off).
actuator_forward(X) :-
    X = (ActuatorID,Class,CurrentStatus,ActivationStatus),
    CurrentStatus = ActivationStatus,
    save_actuator(ActuatorID,on).

% greenhouse_status/0
greenhouse_status :-
    logln('\nGreenhouse status:'),
    all(plant_health(Plant,Health),(plants(Plants),member(Plant,Plants),plant_health(Plant,Health)),PlantsStatuses),
    maplist(logln,PlantsStatuses),
    logln('\n').

% plant_health/2
plant_health(Plant,Health) :-
    plant_status(Plant,SensorType,Reading),
    clause(problem(Problem),reading(SensorType,Reading)),
    issue_problem(Issue,Problem),
    Health = Issue-Problem.
plant_health(Plant,Health) :-
    plant_status(Plant,SensorType,Reading),
    problem_condition(Problem,Reading),
    issue_problem(Issue,Problem),
    Health = Issue-Problem-Reading.
plant_health(Plant,Health) :-
    plant_status(Plant,SensorType,Reading),
    \+ clause(problem(Problem),reading(SensorType,Reading)),
    \+ problem_condition(Problem,Reading),
    Health = healthy.
plant_health(Plant,Health) :-
    \+ plant_status(Plant,SensorType,Reading),
    Health = unknown.

% plants_reading_ranges/1 
plants_reading_ranges(SortedPlants) :-
    all(
        Plant-Species-temperature_range-TemperatureMin-TemperatureMax-growth_humidity-GrowthStage-HumidityMin-HumidityMax,
        (plant(Plant,Species,GrowthStage),species(Species,TemperatureMin,TemperatureMax),growth_humidity(GrowthStage,HumidityMin,HumidityMax)),
        Plants
    ),
    sort(Plants,SortedPlants).