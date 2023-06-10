:- use_module(library(lists)).
:- use_module(library(apply_macros)).

:- dynamic fact/1.
:- dynamic usedfact/1.

% user_start/0
user_start :-
    retractall(fact(_)),
    retractall(usedfact(_)),
    symptomatology,
    forward,
    user_diagnosis.

% symptomatology/0
symptomatology :-
    repeat,
    ask_symptom,
    !,
    not(once_again).

% once_again/0
once_again :- askif(new_symptom).

% ask_symptom/0
% Asks the user to provide informations about the manifested symptoms
ask_symptom :- 
    retractall(asked(new_symptom,_)),
    all(Sign,sign_location(Sign,_),Signs),
    ask_menu(Signs,Sign),
    all(Location,sign_location(Sign,Location),Locations),
    length(Locations,L),
    (L > 1 -> ask_menu(Locations,Location) ; nth1(1,Locations,Location)),
    write(Sign),write(' located on '),write(Location),writeln(' has been selected'),
    ask_symptom_forward(Sign,Location).
% ask_symptom_forward/2
ask_symptom_forward(Sign,Location) :-
    all(Color,sign_color(Sign,Color),Colors),
    ask_menu(Colors,Color),
    write(Sign),write(' manifests '),write(Color),writeln(' coloring'),
    save_symptom(curr,symptom(Location,Sign,Color)).
ask_symptom_forward(Sign,Location) :-
    \+ all(Color,sign_color(Sign,Color),Colors),
    write(Sign),writeln(' has no color.'),nl,
    save_symptom(curr,symptom(Location,Sign,none)).
    
% user_diagnosis/0
% No symptoms case
user_diagnosis :-
    not(usedfact(_,_)),
    writeln_message(no_symptom).
% If there are symptoms but there's no clear diagnosis
user_diagnosis :-
    usedfact(_,_),
    not(usedfact(_,condition(_,_))),
    writeln_message(no_condition).
% If there are clear conditions,explains them
user_diagnosis :-
    usedfact(_,condition(_,_)),
    explain_diagnosis.

% explain_diagnosis/0
explain_diagnosis :-
    history(HistoriesList),
    member([Plant|History],HistoriesList),
    plant_history(Plant,History,Facts),
    all(Condition,(
        member(ID,History),
        usedfact(ID,condition(Plant,Condition))
    ),Conditions),
    (mode_user,need_explanation -> (writeln('\nReasoning performed by the engine (ID-Inference step):\n'),maplist(writeln,Facts)) ; true),
    forall(member(Condition,Conditions),show_treatment(Condition)).

% need_explanation/0
need_explanation :- askif(need_explanation).

% show_treatment/1
show_treatment(Condition) :-
    problem_condition(nutrient_deficiency,Condition),
    writeln_message(missing_nutrient).
show_treatment(Condition) :-
    \+ problem_condition(nutrient_deficiency,Condition),
    \+ treatment(Condition,_),
    writeln_message(treatment_none).
show_treatment(Condition) :-
    \+ problem_condition(nutrient_deficiency,Condition),
    all(Treatment,treatment(Condition,Treatment),Treatments),
    nl,write('* How to treat '),write(Condition),writeln(':'),
    maplist(writeln,Treatments).