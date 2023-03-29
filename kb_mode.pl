% kb_mode_start/0
kb_mode_start :- 
    L = [health_issues, conditions, locations, signs, colors, treatments, rules],
    maplist(kb_browse, L).

% kb_browse/1
kb_browse(X) :- 
    askif(view(X)),
    browse(X).
kb_browse(X) :-
    asked(view(X), A),
    negative(A).

% health_issues/1
health_issues(L) :- 
    all(Issue-Problem, clause(health_issue(Issue), problem(Problem)), L).

treatments(L) :-
    all(Condition-Treatment, treatment(Condition, Treatment), L).

% conditions/1
conditions(L) :-
    all(
        Problem-Condition,
        (
            clause(health_issue(Issue), problem(Problem)),
            clause(problem(Problem), condition(Condition))
        ),
        L).
    

% browse/1
browse(X) :-
    X \= rules,
    writeln(X),
    call(X, L),
    maplist(writeln, L).

browse(X) :-
    X = rules,
    rules(L),
    maplist(writeln, L).