% kb_mode_start/0
kb_mode_start :- 
    L = [status_problems, locations, signs, colors, treatments, rules],
    maplist(kb_browse, L).

% kb_browse/1
kb_browse(X) :- 
    askif(view(X)),
    browse(X).
kb_browse(X) :-
    asked(view(X), A),
    negative(A).

% status_problems/1
status_problems(L1) :- 
    all(C, condition(C), L),
    maplist(problem_card, L, L1).

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