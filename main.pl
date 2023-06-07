:- prolog_flag(unknown,_,fail).

:- dynamic asked/2.

:- leash(none).

:- [engine, utils, logger].
:- [kb/rules, kb/signs, kb/treatments, kb/messages, kb/devices, kb/plants, kb/species, kb/growth_stages].

% start/0
start :- 
    welcome, 
    init.

% init/0 - clears cache, sets running mode and asks to restart the program.
init :- 
    init_cleanup,
    set_fruition_mode,
    restart.

% init_cleanup/0
init_cleanup :-
    retractall(asked(_,_)),
    retractall(symptom(_,_,_)).

% set_fruition_mode/0
set_fruition_mode :- 
    mode_user,
    ensure_loaded(mode_user),
    user_start,
    forward.
set_fruition_mode :-
    \+ (mode_user),
    mode_kb,
    ensure_loaded(mode_kb),
    kb_start.
set_fruition_mode :-
    \+ (mode_user),
    \+ (kb_mode),
    mode_monitor,
    ensure_loaded(mode_monitor),
    monitor_start.

% restart/0
restart :- 
    askif(start_again),
    init.
restart :-
    init_cleanup,
    goodbye.

% mode_user/0
mode_user :- askif(fruition_mode(mode_user)).

% kb_mode/0
kb_mode :- askif(fruition_mode(kb_mode)).

% mode_monitor/0
mode_monitor :- askif(fruition_mode(mode_monitor)).

% health_issues/1
health_issues(HealthIssues) :- 
    all(
        Issue-Problem,
        clause(health_issue(Issue), problem(Problem)),
        HealthIssues).

% conditions/1
conditions(ProblemsConditions) :-
    all(
        Problem-Condition,
        (
            clause(health_issue(Issue), problem(Problem)),
            clause(problem(Problem), condition(Condition))
        ),
        ProblemsConditions).

% locations/1
locations(Locations) :- all(Location, sign_location(_, Location), Locations).

% signs/1
signs(Signs) :- all(Sign, sign_location(Sign, _), Signs).

% colors/1
colors(Colors) :- all(Color, sign_color(_, Color), Colors).

% treatments/1
treatments(Treatments) :-
    all(Condition-Treatment, treatment(Condition, Treatment), Treatments).

% rules/1
rules(Rules) :-
    all(
        (Issue, Problem, Condition, Symptoms),
        (
            clause(health_issue(Issue), problem(Problem)),
            clause(problem(Problem), condition(Condition)),
            clause(condition(Condition), SymptomsConj),
            conj_to_list(SymptomsConj, Symptoms)
        ),
        Rules).

% problem_condition/2
problem_condition(Problem, Condition) :-
    clause(problem(Problem), condition(Condition)).

% issue_problem/2
issue_problem(Issue, Problem) :-
    clause(health_issue(Issue), problem(Problem)).

% plants/1 - Gets all plants with installed sensors
plants(SortedPlants) :-
    all(Plant, plant_sensor(Plant, _), Plants),
    sort(Plants, SortedPlants).

% plants_reading_ranges/1 
plants_reading_ranges(SortedPlants) :-
    all(
        Plant-Species-temperature_range-TemperatureMin-TemperatureMax-growth_humidity-GrowthStage-HumidityMin-HumidityMax,
        (plant(Plant, Species, GrowthStage), species(Species, TemperatureMin, TemperatureMax), growth_humidity(GrowthStage, HumidityMin, HumidityMax)),
        Plants
    ),
    sort(Plants, SortedPlants).



