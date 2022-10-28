% kb_start/0
kb_start :- 
    L = [health_problems, sections, signs, colors, treatments, rules],
    maplist(kb_browse, L).

% kb_browse/1
kb_browse(X) :- 
    askif(view(X)),
    browse(X).
kb_browse(X) :-
    asked(view(X), A),
    negative(A).

% health_problems/1
health_problems(L1) :- 
    all(T, (problem_type(C, T), C \= healthy), L),
    maplist(problem_card, L, L1).

% sections/1
sections(L) :- all(S, sign_section(_, S), L).

% colors/1
colors(L) :- all(C, sign_color(_, C), L).

% rules/
rules(L) :- all((P, X, R), (problem_type(P, X), clause(type(X), R)), L).

% browse/1
browse(X) :-
    X \= rules,
    writeln(X),
    call(X, L),
    menu_display(L).
browse(X) :-
    X \= rules,
    writeln(X),
    call(X, L),
    maplist(is_list, L),
    flatten(L, L1),
    menu_display(X, L1).
browse(X) :-
    X = rules,
    rules(L),
    maplist(writeln, L).