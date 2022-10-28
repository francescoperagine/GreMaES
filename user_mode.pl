user_start :-
    symptomatology,
    user_diagnosis.

% symptomatology/0
symptomatology :-
    repeat,
    get_symptom,
    \+ (once_again).

% once_again/0
once_again :- askif(new_symptom).

% get_symptom/0
get_symptom :- 
    get_user_choice(signs, M),
    assertz(current_sign(M)),
    symptomatology_forward,
    symptomatology_cleanup.

% symptomatology_cleanup/0
symptomatology_cleanup :- 
    retractall(asked(_, _)),
    retractall(current_sign(_)).
    
% symptomatology_forward/0
symptomatology_forward :-
    current_sign(M),
    \+ sign_color(M, _),
    get_user_choice(current_sign_sections, S),
    assertz(symptom(S, M)).
symptomatology_forward :-
    current_sign(M),
    sign_color(M, _),
    get_user_choice(current_sign_colors, C), 
    get_user_choice(current_sign_sections, S),
    assertz(symptom(S, M, C)).

% get_user_choice/2
% Unifies the concept X with a list of options L, reads the user choice from that list and unifies it with Y.
get_user_choice(X, Y) :- 
    call(X, L), 
    menu_ask(X, L, Y).

% current_sign_sections/2
current_sign_sections(L) :-
    current_sign(M),
    all(S, sign_section(M, S), L).

% current_sign_colors/2
current_sign_colors(L) :- 
    current_sign(M),
    all(C, sign_color(M, C), L).

% Progressive building of the signed symptom.
% Each symptom signed on a section, and may have an appereance, an appereance with a color or a behaviour.

% menu_ask/3 - Menu title T, options' list L, entry name X
menu_ask(T, L, Y) :-
    menu_with_title(T, L), nl,
    message_code(item_number_no_exit, M),
    read(X),
    menu_input_handler(L, X, Y),
    message_code(option_selected, N),
    write(N), write(X), write(': '), writeln(Y), nl.

% menu_display/2 - Shows the title and goes ahead with the diplaying part.
menu_with_title(T, L) :- 
    message_code(T, X),
    writeln(X),
    menu_display(L).
menu_with_title(T, L) :-
    \+ message_code(T, X),
    writeln(T),
    menu_display(L).

% menu_display/1 - Options' list L, counter starts from 1
% Shows a numbered list of ordered options, stripping atom names from their underscores.
menu_display(L) :- menu_display(L, 1), !. 
menu_display([], _).
menu_display([H|T], N) :-
    atomic_concat([N, '. ', H], A),
    writeln(A),
    N1 is N + 1,
    menu_display(T, N1).

% menu_input_handler/3 - The entry number X of list L unifies with list entry name Y.
menu_input_handler(L, X, Y) :-
    integer(X),
    nth1(X, L, Y),
    validate_entry(L, Y).

% validate_entry/2
validate_entry(L, X) :- 
    member(X, L),
    !.
validate_entry(L, X) :-
    not(member(X, L)),
    message_code(not_recognized_value, M),
    writeln(M).

% user_diagnosis/0    
user_diagnosis :-
    observed_symptoms,
    all(diagnosis(T, B), (type_body(T, B), observed_symptoms(S), match(B, S)), D),
    maplist(explain_diagnosis, D).
user_diagnosis :-
    \+ observed_symptoms,
    message_code(no_symptom, M),
    writeln(M).
user_diagnosis :-
    observed_symptoms,
    \+ (condition(X)),
    message_code(no_diagnosis, M),
    writeln(M).

% observed_symptoms/0
observed_symptoms :- symptom(_,_).
observed_symptoms :- symptom(_,_,_).

% explain_diagnosis/1 - X Right side of the condition(X) rule, to be checked vs the stored symptoms.
explain_diagnosis(X) :-
    X = diagnosis(T, [B]),
    T \= healthy,
    problem_card(T, H, C),
    atomic_concat([H, ' - ', T, ' ', C], A),
    message_code(diagnosis_of, M),
    message_code(because_of, N),
    nl, write(M), write(A), write(N), writeln(B),
    explain_treatment(T), nl.
explain_diagnosis(X) :-
    X = diagnosis(T, [B]),
    T = healthy,
    message_code(treatment_healthy, M),
    writeln(M), nl.

% explain_treatment/1
explain_treatment(X) :-
    problem_condition(nutrient_deficiency, X),
    message_code(missing_nutrient, M),
    writeln(M).
explain_treatment(X) :-
    \+ problem_condition(nutrient_deficiency, X), % nutrient deficiencies have no direct treatment.
    bagof(Y, treatment(X, Y), L),
    message_code(treatment, M),
    writeln(M),
    maplist(writeln, L).
explain_treatment(X) :-
    \+ problem_condition(nutrient_deficiency, X),
    \+ treatment(X),
    message_code(treatment_none, M),
    writeln(M).

% observed_symptoms/1 Unifies S with the aggregated lists of symptoms
observed_symptoms(S) :- 
    findall(symptom(M1, S1), (condition(T1), symptom(M1, S1)), R1), 
    findall(symptom(M2, S2, C), (condition(T2), symptom(M2, S2, C)), R2),
    append([R1, R2], L),
    all(X, member(X, L), S).

% type_body/2 Unifies B with the right side of the type predicate
type_body(T, B) :-
    condition(T),
    clause(condition(T), R),
    conj_to_list(R, B).
    
conj_to_list((H, C), [H|T]) :-
    !,
    conj_to_list(C, T).
conj_to_list(H, [H]).

list_to_conj([H|T], (H, C)) :-
    !,
    list_to_conj(T, C).
list_to_conj([H], H).

