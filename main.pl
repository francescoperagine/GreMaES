:- prolog_flag(unknown,_,fail).

:- dynamic asked/2.

:- leash(none).

:- [store,engine,utils,logger].
:- [mode_user,mode_monitor,mode_kb].
:- [kb/rules,kb/signs,kb/treatments,kb/messages,kb/devices,kb/plants,kb/species,kb/growth_stages].

:- initialization(index_init).
% :- initialization(logger_init).

debug(on).

% start/0
start :- 
    welcome,
    init.

% init/0 - clears cache,sets running mode and asks to restart the program.
init :- 
    init_cleanup,
    set_fruition_mode,
    restart.

% init_cleanup/0
init_cleanup :-
    retractall(asked(_,_)).

% set_fruition_mode/0
set_fruition_mode :- 
    mode_user,
    ensure_loaded(mode_user),
    user_start.
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

% plants/1 - Gets all plants with installed sensors
plants(SortedPlants) :-
    all(Plant,plant_sensor(Plant,_),Plants),
    sort(Plants,SortedPlants).

% plants_reading_ranges/1 
plants_reading_ranges(SortedPlants) :-
    all(
        Plant-Species-temperature_range-TemperatureMin-TemperatureMax-growth_humidity-GrowthStage-HumidityMin-HumidityMax,
        (plant(Plant,Species,GrowthStage),species(Species,TemperatureMin,TemperatureMax),growth_humidity(GrowthStage,HumidityMin,HumidityMax)),
        Plants
    ),
    sort(Plants,SortedPlants).