:- use_module(library(apply_macros)).
:- use_module(library(ordsets)).
:- use_module(library(system)).
:- use_module(library(lists)).

:- prolog_flag(unknown,_,fail).

:- leash(none).

:- [store,engine,diagnosis,utils,logger].
:- [mode_user,mode_monitor,mode_kb].
:- [kb/rules,kb/signs,kb/treatments,kb/messages,kb/devices,kb/plants,kb/species,kb/growth_stages].

:- initialization(init).

% init/0
init :- 
    engine_init,
    utils_init,
    logger_init.

% start/0
start :- 
    cleanup,
    welcome,
    fruition_mode,
    restart.

% cleanup/0
cleanup :-
    unset(asked/2),
    unset(fact_history/2),
    unset(fact/2),
    unset(usedfact/2),
    unset(actuator_status/2),
    unset(observation/6).

% fruition_mode/0
fruition_mode :- 
    mode_user,
    ensure_loaded(mode_user),
    user_start.
fruition_mode :-
    \+ (mode_user),
    mode_monitor,
    ensure_loaded(mode_monitor),
    monitor_start.
fruition_mode :-
    \+ (mode_user),
    \+ (mode_monitor),
    mode_kb,
    ensure_loaded(mode_kb),
    kb_start.

% mode_user/0
mode_user :- askif(fruition_mode(mode_user)).
% mode_monitor/0
mode_monitor :- askif(fruition_mode(mode_monitor)).
% kb_mode/0
mode_kb :- askif(fruition_mode(mode_kb)).

% restart/0
restart :- 
    start_again,
    cleanup,
    start.
restart :-
    \+ start_again,
    goodbye.

% start_again/0
start_again :- askif(start_again).