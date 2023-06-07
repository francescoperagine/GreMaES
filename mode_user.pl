:- use_module(library(lists)).
:- use_module(library(apply_macros)).

:- dynamic fact/1.
:- dynamic usedfact/1.

% user_start/0
user_start :-
    retractall(fact(_)),
    retractall(usedfact(_)),
    symptomatology,
    user_diagnosis.

% symptomatology/0
symptomatology :-
    repeat,
    ask_symptom,
    \+ (once_again).

% once_again/0
once_again :- askif(new_symptom).

% ask_symptom/0
% Asks the user to provide informations about the manifested symptoms
ask_symptom :- 
    all(Sign, sign_location(Sign, _), Signs),
    ask_menu(Signs, Sign),
    all(Location, sign_location(Sign, Location), Locations),
    ask_menu(Locations, Location),
    ask_symptom_forward(Sign, Location),
    retractall(asked(_,_)).
% ask_symptom_forward/2
ask_symptom_forward(Sign, Location) :-
    all(Color, sign_color(Sign, Color), Colors),
    ask_menu(Colors, Color),
    assert_symptom(curr, Location, Sign, Color).
ask_symptom_forward(Sign, Location) :-
    \+ all(Color, sign_color(Sign, Color), Colors),
    assert_symptom(curr, Location, Sign, none).
% assert_symptom/4
assert_symptom(Plant, Location, Sign, Color) :-
    assert_fact(manifests(Plant, symptom(Location, Sign, Color))).
    
% user_diagnosis/0
% No symptoms case
user_diagnosis :-
    \+ has_symptoms,
    writeln_message(no_symptom).
% If there are symptoms but there's no clear diagnosis, the system extracts the conditions that may be involved (partial match with symptoms)
user_diagnosis :-
    has_symptoms,
    \+ has_condition,
    writeln_message(no_condition).
% Matches the symptoms of every condition with the observed ones
user_diagnosis :-
    has_symptoms,
    all(
        symptom(Location, Sign, Color),
        symptom(Location, Sign, Color),
        ObservedSymptoms
    ),
    all(diagnosis(Condition, ConditionSymptoms),
        (
            condition_symptoms(Condition, ConditionSymptoms),
            match(ConditionSymptoms, ObservedSymptoms)
        ),
        Diagnoses),
    maplist(explain, Diagnoses).

% has_symptoms/0
has_symptoms :- symptom(_,_,_).

% has_condition/0
has_condition :- condition(_).

% condition_symptoms/2
% Unifies ConditionSymptoms with the right side of the condition rule
condition_symptoms(Condition, ConditionSymptoms) :-
    condition(Condition),
    clause(condition(Condition), ConditionBody),
    conj_to_list(ConditionBody, ConditionSymptoms).

% explain/1 If a diagnosis is reached, the plant is sick. The diagnosis is explained and the treatment is shown, if present.
explain(Diagnosis) :-
    Diagnosis = diagnosis(Condition, [ConditionSymptoms]),
    show_diagnosis(Condition, ConditionSymptoms),
    show_treatment(Condition).
explain(Diagnosis) :-
    Diagnosis \= diagnosis(Condition, [ConditionSymptoms]),
    writeln_message(treatment_healthy).

% show_diagnosis/2
show_diagnosis(Condition, ConditionSymptoms) :-
    problem_condition(Problem, Condition),
    write_message(because_of), write(ConditionSymptoms), write_message(diagnosis_of), write(Problem), write(' - '), writeln(Condition).

% show_treatment/1
show_treatment(Condition) :-
    problem_condition(nutrient_deficiency, Condition),
    writeln_message(missing_nutrient).
show_treatment(Condition) :-
    \+ problem_condition(nutrient_deficiency, Condition),
    bagof(Treatment, treatment(Condition, Treatment), Treatments),
    writeln_message(treatment),
    maplist(writeln, Treatments).
show_treatment(Condition) :-
    \+ problem_condition(nutrient_deficiency, Condition),
    \+ treatment(Condition),
    writeln_message(treatment_none).