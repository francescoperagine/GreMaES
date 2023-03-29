% user_mode_start/0
user_mode_start :-
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
ask_symptom :- 
    user_input(signs, Sign),
    assertz(current_sign(Sign)),
    symptomatology_forward,
    symptomatology_cleanup.

% symptomatology_forward/0
% If the sign is not associated to a color, sets it to none.
symptomatology_forward :-
    current_sign(Sign),
    \+ sign_color(Sign, _),
    user_input(sign_locations, Location),
    assertz(symptom(Location, Sign, none)).
% If the sign is associated to a color, asks it.
symptomatology_forward :-
    current_sign(Sign),
    sign_color(Sign, _),
    user_input(sign_colors, Color), 
    user_input(sign_locations, Location),
    assertz(symptom(Location, Sign, Color)).

% symptomatology_cleanup/0
% Removes temporary informations to prepare to eventually register the next symptom.
symptomatology_cleanup :- 
    retractall(asked(_,_)),
    retractall(current_sign(_)).

% user_input/2
% Unifies Relation with a list of options, reads the user selection from that list and returns it as UserChoice.
user_input(Relation, UserChoice) :-
    call(Relation, Options), 
    show_title(Relation), nl,
    show_options(Options),
    read(UserInput),
    input_choice(Options, UserInput, UserChoice),
    write_message(option_selected), write(UserInput), write(': '), writeln(UserChoice), nl.

% sign_location/1
% Unifies Locations with the list of all possible locations in which the temporary stored sign can manifest.
sign_locations(Locations) :-
    current_sign(Sign),
    all(Location, sign_location(Sign, Location), Locations).

% sign_colors/1
sign_colors(Colors) :-
    current_sign(Sign),
    all(Color, sign_color(Sign, Color), Colors).

% show_title/1 - Shows the title if present, otherwise prints the relation's name.
show_title(Relation) :- 
    writeln_message(Relation).
show_title(Relation) :-
    \+ writeln_message(Relation),
    writeln(Relation).

% show_options/1
% Shows a numbered list of ordered options, stripping atom names from their underscores.
show_options(Options) :- 
    show_options(Options, 1),
    !. 

% show_options/2
show_options([], _).
show_options([H|T], N) :-
    atomic_concat([N, '. ', H], A),
    writeln(A),
    N1 is N + 1,
    show_options(T, N1).

% input_choice/3 - The entry number UserInput of list Options unifies with list entry UserChoice.
input_choice(Options, UserInput, UserChoice) :-
    integer(UserInput),
    nth1(UserInput, Options, UserChoice),
    validate_input(Options, UserChoice).

% validate_input/2
validate_input(Options, UserChoice) :- 
    member(UserChoice, Options),
    !.
validate_input(Options, UserChoice) :-
    not(member(UserChoice, Options)),
    writeln_message(not_recognized_value).

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

% condition_symptoms/2 Unifies ConditionSymptoms with the right side of the condition rule
condition_symptoms(Condition, ConditionSymptoms) :-
    condition(Condition),
    clause(condition(Condition), ConditionBody),
    conj_to_list(ConditionBody, ConditionSymptoms).

% explain/1 If a diagnosis is reachehd, the plant is sick. The diagnosis is explained and the treatment is shown, if present.
explain(Diagnosis) :-
    Diagnosis = diagnosis(Condition, [ConditionSymptoms]),
    show_diagnosis(Condition, ConditionSymptoms),
    show_treatment(Condition).

% show_diagnosis/2
show_diagnosis(Condition, ConditionSymptoms) :-
    problem_condition(Problem, Condition),
    status_problem(Status, Problem),
    write_message(because_of), write(ConditionSymptoms), write_message(diagnosis_of), write(Status), write(' '), write(Problem), write(' - '), writeln(Condition).

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