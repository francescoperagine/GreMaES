:- use_module(library(random)).

% sampling_size/1
% How many samplings' repetitions must be made for each loop 
sampling_size(10).

% sampling_interval/1
% How many senconds between each sampling
sampling_interval(0).

% reading_variability/1
% How much a readings may differ from the ideal range of values.
reading_variability(1.3). 

% symptom_probability/1
% How probable is to observe a symptom during the sampling
symptom_probability(1).

% monitor_init/0
% Initializes a sensors list and actuators.
monitor_init :-
    sensors_init,
    actuators_init.

% sensors_init/0
% Saves a list of all sensors to speed up the sampling
sensors_init :-
    all(SensorID,plant_sensor(_,SensorID,_),Sensors),
    saveln(sensors(Sensors)).

% actuators_init/0
% Sets all connected actuators to the off status.
actuators_init :-
    all(ActuatorID,plant_actuator(_,ActuatorID,_,_),Actuators),
    forall(member(ActuatorID,Actuators),actuator_init(ActuatorID)),
    writeln('Actuators initialized.').

% monitor_start/0
monitor_start :-
    monitor_cleanup,
    monitor_init,
    welcome_monitor,
    greenhouse_mapping,
    monitor_loop_start,
    !,
    forward,
    backward,
    environment_manager,nl.
    greenhouse_status.

% monitor_cleanup/0
monitor_cleanup :-
    % unset(plant_status),
    unset(observation/6),
    unset(sensors/1).
    unset(actuator_status/2).

% greenhouse_mapping/0
greenhouse_mapping :-
    all(Plant-Species-'temp_range'-Tmin-Tmax-'stage'-GrowthStage-Hmin-Hmax,
        (plant(Plant,Species,GrowthStage),species(Species,Tmin,Tmax),growth_humidity(GrowthStage,Hmin,Hmax)),
        Plants),
    sort(Plants,SortedPlants),
    maplist(writeln,SortedPlants),nl.

% monitor_loop_start/0
% Deletes the last confirmation about continuing the loop and initializes the loop with specified size and interval.
monitor_loop_start :- 
    unset_asked(continue_monitor_loop),
    sampling_size(Repetitions),
    sampling_interval(Interval),
    monitor_loop(Repetitions,Interval).

% monitor_loop/2 (+Repetitions,+Interval)
% Performs a fixed number of timed samplings.
monitor_loop(Repetitions,Interval) :-
    Repetitions > 0,
    sampling_init,
    (Interval > 0 -> sleep(Interval) ; true),
    RepetitionsNew is Repetitions - 1,
    monitor_loop(RepetitionsNew,Interval).
% At the end of the loop the user is asked to continue the sampling.
monitor_loop(0,_) :-
    continue_sampling,
    monitor_loop_start.
% The loop ends with a negative answer from the user.
monitor_loop(0,_) :-
    \+ continue_sampling.

% continue_sampling/0
continue_sampling :-
    askif(continue_monitor_loop).

% sampling_init/0
% Selects a random sensor from those already active on plants,
% and initializes the sampling which includes a timestamp.
sampling_init :- 
    sensors(Sensors),
    random_list_element(Sensors,RandomSensorID),
    plant_sensor(Plant,RandomSensorID,SensorType),
    timestamp_utc(Timestamp), % evaluated here to fix the timestamp
    sampling(Plant,Timestamp,SensorType,RandomSensorID).

% random_list_element/2 (+List,-Element)
% Randomly selects an element from a list
random_list_element(List,Element) :-
    length(List,Length),
    random(0,Length,RandomNumber),
    nth0(RandomNumber,List,Element).

% sampling/4 (+Plant,+Timestamp,+SensorType,+SensorID)
% Observations are stored in both working memory and logs.

% Performs a sampling of an environment metric randomly selecting values in a extended range.
sampling(Plant,Timestamp,SensorType,SensorID) :-
    SensorType \= caption,
    reading_variability(Variability),
    plant_range_values(Plant,SensorType,Min,Max),
    MinV is Min / Variability,
    MaxV is Max * Variability,
    % humidity values are fixed in a [0-100] range.
    % not proportionally scaled to exploit the reading_variability.
    (SensorType = humidity,MinV < 0 -> MinV1 = 0 ; MinV1 = MinV), 
    (SensorType = humidity,MaxV > 100 -> MaxV1 = 100 ; MaxV1 = MaxV),
    random(MinV1,MaxV1,RandomValue),
    Value is floor(RandomValue),
    reading_status(Plant,SensorType,Value,ReadingStatus),
    save_observation(Plant,reading(SensorType,ReadingStatus)),
    saveln(observation(Timestamp,Plant,SensorType,SensorID,Value,ReadingStatus)),
    atomic_concat([Timestamp,',',Plant,',',SensorType,',',SensorID,',',Value,',',ReadingStatus],Message),
    log(observation,Message).
% Performs a random sampling from a camera device that returns a symptom..
sampling(Plant,Timestamp,caption,SensorID) :-
    symptom_probability(SymptomProbability),
    random(RandomValue),
    sampling_symptom(SymptomProbability,RandomValue,Symptom),
    save_observation(Plant,Symptom),
    term_to_atom(Symptom,S),
    atomic_concat([Timestamp,',',Plant,',',caption,',',SensorID,',',S],Message),
    log(observation,Message).

% plant_range_values/4 (+Plant,+SensorType,-Min,-Max)
% Returs environment metric's SensorType range values
plant_range_values(Plant,temperature,Min,Max) :-
    plant(Plant,Species,_),
    species(Species,Min,Max).
plant_range_values(Plant,humidity,Min,Max) :-
    plant(Plant,_,GrowthStage),
    growth_humidity(GrowthStage,Min,Max).

% reading_status/4 (+Plant,+SensorType,+Value,-ReadingStatus)
% Reads the range values of the SensorType and returns the status of the current reading.
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

% sampling_symptom/3 (+SymptomProbability,+RandomValue,-Symptom)
% Performs the sampling of a symptom based on the probability that it happens.
% This is to simulate an actual environment and eventually to perform stress tests of the system.
% For the case study purpose, the probability of a symptom happening is 100% to test out the
% inference system when multiple symptoms are acquired for each plant.
sampling_symptom(SymptomProbability,RandomValue,Symptom) :-
    SymptomProbability >= RandomValue,
    signs(Signs),
    random_list_element(Signs,Sign),
    caption_forward(Sign,Symptom).
% If no symptom is acquired, the neutral symptom is returned.
sampling_symptom(SymptomProbability,RandomValue,symptom(none,all)) :-
    RandomValue > SymptomProbability.

% caption_forward/2 (+Sign,-Symptom)
% Matches Location and Color with a random Sign.
caption_forward(Sign,symptom(RandomLocation,Sign,RandomColor)) :-
    random_sign_location(Sign,RandomLocation),
    random_sign_color(Sign,RandomColor).
% For those symptoms which do not have any associated color, sets the 3rd argument to none
caption_forward(Sign,symptom(RandomLocation,Sign,none)) :-
    \+ sign_color(Sign,_),
    random_sign_location(Sign,RandomLocation).

% random_sign_location/2 (+Sign,-RandomLocation)
% Unifies Sign with a random location related to it
random_sign_location(Sign,RandomLocation) :- 
    all(Location,sign_location(Sign,Location),Locations),
    random_list_element(Locations,RandomLocation).

% random_sign_color/2 (+Sign,-RandomColor)
% Unifies Sign with a random color related to it
random_sign_color(Sign,RandomColor) :-
    all(Color,sign_color(Sign,Color),Colors),
    random_list_element(Colors,RandomColor).

% monitor_diagnosis/0
% Retrieves all plants related conditions, removes duplicates and initializes the explanation.
monitor_diagnosis :-
        findall(Plant-Condition, usedfact(_,condition(Plant,Condition)),PlantsConditionsList),
        list_to_ord_set(PlantsConditionsList,PlantsConditionsSet),
        maplist(explain_diagnosis,PlantsConditionsSet).

% environment_manager/0
% Initializes the retrieving of climate related conditions.
environment_manager :-
    plants(Plants),
    forall(member(Plant,Plants), plant_reading(Plant)).

% plant_reading/1 (+Plant)
% Retrieves all the readings of the plant and initializes the actuator handler.
plant_reading(Plant) :-
    findall(reading(Timestamp,Plant,Metric,Condition,Reading),
        (observation(Timestamp,Plant,Metric,_,_,Reading),
        rule(_,condition(Plant,Condition),[manifests(Plant,reading(Metric,_))])),
        List),
    actuator_handler(List).

% actuator_handler/1 (+List)
% Retrieves actuators able to handle the environment Metric and starts them
actuator_handler([]).
actuator_handler([reading(Timestamp,Plant,Metric,Condition,Reading)|T]) :-
    findall((Timestamp,Plant,ActuatorID,Metric,Condition,Reading,ActivationStatus),
            (plant_actuator(Plant,ActuatorID,Metric,ActivationStatus)),
            Actuators),
    (Actuators \= [] -> maplist(actuator_start,Actuators) ; true),
    actuator_handler(T).

% actuator_start/1
% If the reading is not normal and if the actuator is able to handle the condition, activates it.
% Logs the update only if the actual status was modified.
actuator_start((Timestamp,Plant,ActuatorID,Metric,Condition,Reading,ActivationStatus)) :-
    Reading \= normal,
    Condition = ActivationStatus,
    actuator_save(ActuatorID,on,Action),
    (Action \= none -> actuator_log(Timestamp,Plant,Metric,Reading,ActuatorID,on) ; true).
% The actuator is turned off if the reading is normal or if it cannot handle the condition.
actuator_start((Timestamp,Plant,ActuatorID,Metric,Condition,Reading,ActivationStatus)) :-
    actuator_save(ActuatorID,off,Action),
    (Action \= none -> actuator_log(Timestamp,Plant,Metric,Reading,ActuatorID,off) ; true).

% actuator_log/6
actuator_log(Timestamp,Plant,Metric,Reading,ActuatorID,Status) :-
    atomic_concat([Timestamp,',',Plant,',',Metric,',',Reading,',',ActuatorID,',',Status], Message),
    log(actuator,Message).

% greenhouse_status/0
% Shows a report for all plants' conditions.
greenhouse_status :-
    plants(Plants),
    all(Plant-Condition, (member(Plant,Plants),usedfact(_,condition(Plant,Condition))),PlantConditions),
    maplist(writeln,PlantConditions).