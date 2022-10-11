:- prolog_flag(unknown,_,fail).

:- use_module(library(lists)).
:- use_module(library(apply_macros)).

:- dynamic asked/2.
:- dynamic current_section/1.
:- dynamic symptom/2.
:- dynamic symptom/3.
:- dynamic type/1.

:- [greenhouse, deficiencies, diseases, infestations, messages].

test :-
    assertz(symptom(leaves, circular_lesions)),
    assertz(symptom(fruits, spots, red)),
    assertz(symptom(fruits, clear_gummy_substance)).

% start/0
start :- 
    welcome, 
    init.

% init/0 - clears cache, sets running mode and asks to restart the program.
init :- 
    cleanup_init,
    set_fruition_mode,
    restart.

% cleanup_init/0
cleanup_init :-
    retractall(asked(_, _)),
    retractall(symptom(_,_)),
    retractall(symptom(_,_,_)).

% set_fruition_mode/0
set_fruition_mode :- 
    user_consult,
    symptomatology,
    diagnosis.
set_fruition_mode :-
    \+ (user_consult),
    kb_consult,
    kb_browse.

% restart/0
restart :- 
    askif(start_again),
    init.
restart :-
    cleanup_init,
    goodbye.

user_consult :- askif(fruition_mode(user_consult)).
kb_consult :- askif(fruition_mode(kb_consult)).

% kb_browse/0
kb_browse :- 
    L = [health_problems, sections, manifestations, colors, treatments, rules],
    maplist(kb_browse_full, L).

% kb_browse_full/1
kb_browse_full(X) :- 
    askif(view(X)),
    browse(X).
kb_browse_full(X) :-
    asked(view(X), A),
    negative(A).

% health_problems/1
health_problems(L1) :- 
    all(T, class(C, T), L),
    maplist(problem_card, L, L1).

rules(L) :- all((P, X, R), (class(P, X), clause(type(X), R)), L).

deficiencies(L) :- all(X, class(nutrient_deficiency, X), L).
diseases(L) :- all(X, class(disease, X), L).
infestations(L) :- all(X, class(infestation, X), L).

% manifestations/1
manifestations(L) :- all(M, manifest_section(M, S), L).

% sections/1
sections(L) :- all(S, manifest_section(_, S), L).

% colors/1
colors(L) :- all(C, manifest_color(_, C), L).

% section_manifestations/2
current_manifestation_sections(L) :-
    current_manifestation(M),
    all(S, manifest_section(M, S), L).

% manifest_colors/2
current_manifest_colors(L) :- 
    current_manifestation(M),
    all(C, manifest_color(M, C), L).

% treatments/1
treatments(L2) :- 
    all(T, treatment(T, _), L),
    maplist(treatment_card, L, L1),
    flatten(L1, L2).

% problem/3
problem(P, T, C) :- 
    health_problem(P, C),
    class(C, T).

% browse/1
browse(X) :-
    kb_consult,
    wwuln(X),
    call(X, L),
    menu_display(L).
browse(X) :-
    kb_consult,
    wwuln(X),
    call(X, L),
    maplist(is_list, L),
    flatten(L, L1),
    menu_display(X, L1).
#TODO browse rules

browse(X) :-
    user_consult,
    call(X, L),
    menu_with_title(X, L).
browse(X) :-
    user_consult,
    call(X, L),
    maplist(is_list, L),
    flatten(L, L1),
    menu_with_title(X, L1).

% symptomatology/0
symptomatology :- \+ (symptomatology_rule).
symptomatology :- \+ (once_again).
once_again :- askif(new_symptom).

% symptomatology_rule/0
symptomatology_rule :- 
    askif(has(manifestation)),
    get_user_choice(manifestations, M),
    assertz(current_manifestation(M)),
    symptomatology_forward.
    
% symptomatology_forward/0
symptomatology_forward :-
    \+ manifest_color(M, _),
    current_manifestation(M),
    get_user_choice(current_manifestation_sections, S),
    store(S, M).
symptomatology_forward :-
    manifest_color(M, _),
    current_manifestation(M),
    get_user_choice(current_manifest_colors, C), 
    get_user_choice(current_manifestation_sections, S),
    store(S, M, C).

% cleanup_symptomatology/0
cleanup_symptomatology :- 
    retractall(asked(new_symptom, _)),
    retractall(asked(has(manifestation), _)),
    retractall(asked(has(current_manifestation), _)).

% get_user_choice/2
% Unifies the concept X with a list of options L, reads the user choice from that list and unifies it with Y.
get_user_choice(X, Y) :- 
    call(X, L), 
    menu_ask(X, L, Y).

% Progressive building of the manifested symptom.
% Each symptom manifested on a section, and may have an appereance, an appereance with a color or a behaviour.

% store/2 - Asserts a manifestation of the symptom.
store(S, M) :-
    assertz(symptom(S, M)),
    cleanup_symptomatology.
% store/3 - Section S, manifestation M, color C
store(S, M, C) :-
    assertz(symptom(S, M, C)),
    cleanup_symptomatology.

% menu_ask/3 - Menu title T, options' list L, entry name X
menu_ask(T, L, Y) :-
    menu_with_title(T, L), nl,
    message_code(item_number_no_exit, M),
    wwuln(M),  nl,
    read(X),
    menu_input_handler(L, X, Y).

% menu_display/2 - Shows the title and goes ahead with the diplaying part.
menu_with_title(T, L) :- 
    message_code(T, X),
    wwuln(X),
    menu_display(L).
menu_with_title(T, L) :-
    \+ message_code(T, X),
    wwuln(T),
    menu_display(L).

% menu_display/1 - Options' list L, counter starts from 1
% Shows a numbered list of ordered options, stripping atom names from their underscores.
menu_display(L) :- menu_display(L, 1), !. 
menu_display([], _).
menu_display([H|T], N) :-
    atomic_concat([N, '. ', H], A),
    wwuln(A),
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
    wwuln(M).

% string_replace/4 Retrieves the codes to allow a proper replacement within the 3rd argument list.
string_replace(X, Y, L1, Z) :-
    \+ atom(L),
    char_code(X, X1),
    char_code(Y, Y1),
    replace(X1, Y1, L1, L2),
    atom_codes(Z, L2).
string_replace(X, Y, L, L2) :-
    atom(L),
    atom_codes(L, L1),
    string_replace(X, Y, L1, L2).
string_replace(X, Y, L1, Z).

% replace/4 - Replaces the X in the 3rd argument list with the Y. Returns the new updated list in the 4th argument.
replace(_, _, [], []).
replace(X, Y, [X|T1], [Y|T2]) :-
    replace(X, Y, T1, T2). % X is in the first position and it's replaced.
replace(X, Y, [H|T1], [H|T2]) :- % X is not in the first position, do nothing
    dif(X, H),
    replace(X, Y, T1, T2).

% diagnosis/0    
diagnosis :-
    \+ observed_symptoms,
    message_code(no_symptom, M),
    wwuln(M).
diagnosis :-
    observed_symptoms,
    \+ (type(X)),
    message_code(no_diagnosis, M),
    wwuln(M).
diagnosis :-
    observed_symptoms,
    all(R, (type(X), clause(type(X), R)), C),
    maplist(explain_diagnosis, C).

% observed_symptoms/0
observed_symptoms :- symptom(_,_).
observed_symptoms :- symptom(_,_,_).
    
% explain_diagnosis/1 - X Right side of the type(X) rule, to be checked vs the stored symptoms.
explain_diagnosis(X) :- 
    \+ call(X).
explain_diagnosis(X) :-
    call(X),
    clause(type(T), X),
    problem_card(T, A),
    wwu('- Because you have selected '), wwu(X), wwu(', the diagnosis is '), wwuln(A),
    explain_treatment(T).
    
% conj_to_list((H, C), [H|T]) :-
%     !,
%     conj_to_list(C, T).
% conj_to_list(H, [H]).

% match([H|T], L):-
%     member(H, L),
%     match(T, L).

% problem_card/4 - The predicate holds when the first argument is a KB 'type' ground atom,
% and other arguments unify with the KB atoms. Unifies A1 with the concatenate terms
% Type T, health problem H, class C
problem_card(T, A) :-
    class(C, T),
    health_problem(H, C),
    atomic_concat([H, ' problem, ', T, ' ', C, '.'], A).

treatment_card(T, L) :- all(A, (treatment(T, D), atomic_concat([T, ': ', D], A)), L).

% wwu/1 - wwu Without Underscore predicate
wwu(X) :- 
    % string_replace('_', ' ', X, X1),
    write(X).
% wwuln/1
wwuln(X) :- wwu(X), nl.

% explain_treatment/1
explain_treatment(X) :-
    class(nutrient_deficiency, X),
    wwuln('Treatment: provide the missing nutrient to the plant.').
explain_treatment(X) :-
    \+ class(nutrient_deficiency, X), % nutrient deficiencies have no direct treatment.
    bagof(Y, treatment(X, Y), L),
    wwuln('Treatment: '),
    maplist(wwuln, L).
explain_treatment(X) :-
    \+ class(nutrient_deficiency, X),
    \+ treatment(X),
    wwuln('Treatment: There a no treatments').

% askif/1
askif(Q) :-
    ask(Q, A),
    positive_answer(Q, A).

% ask/2
ask(Qcode, A) :- asked(Qcode, A).
ask(Qcode, A) :- \+ (asked(Qcode, A)),
    question_code(Qcode, Q),
    wwu(Q), wwu('?'), nl,
    read(A2),
    ask2(Q, Qcode, A2, A).

% positive_answer/2
positive_answer(Q, A) :- affirmative(A).
positive_answer(Qcode, A) :-
    \+ (negative(A)),
    \+ (affirmative(A)),
    message_code(yes_or_no, M),
    wwu(M),
    read(A2),
    retract(asked(Qcode, A)),
    asserta(asked(Qcode, A2)),
    affirmative(A2).

% askifnot/1
askifnot(Q) :- not(askif(Q)).

% ask/2
ask(Qcode, A) :- asked(Qcode, A).
ask(Qcode, A) :- \+ (asked(Qcode, A)),
    question_code(Qcode, Q),
    wwu(Q), wwu('?'), nl,
    read(A2),
    ask2(Q, Qcode, A2, A).

% ask2/4
ask2(Q, Qcode, '?', A) :-
    explain(Qcode),
    ask(Qcode, A).

ask2(Q, Qcode, A, A) :-
    \+ (A = '?'),
    asserta(asked(Qcode, A)).

% explain/1
explain(X) :- 
    explanation(X, Y),
    wwuln(Y).
explain(X) :-
    \+ explanation(X, Y),
    message_code(no_explanation, M),
    wwuln(M).

% affirmative/1
affirmative(yes).
affirmative(y).
affirmative(ye).
affirmative(right).
affirmative(ok).
affirmative(uhhuh).

% negative/1
negative(no).
negative(n).
negative(not).
negative(never).
negative(impossible).
negative(haha).

% match/2
% match([H|T], L):-
%     member(H, L),
%     match(T, L).

% remove_string_prefix(S, P, S1) :-
%     atom_length(P, L),
%     sub_atom(S, L, _, 0, S1).