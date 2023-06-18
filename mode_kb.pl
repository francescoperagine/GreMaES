% kb_start/0
kb_start :- 
    L = [rules,treatments],
    maplist(kb_browse,L).

% kb_browse/1
kb_browse(X) :- 
    askif(view(X)),
    browse(X).
kb_browse(X) :-
    asked(view(X),A),
    negative(A).
    
% browse/1
browse(X) :-
    X = rules,
    listing(rule).
browse(X) :-
    X = treatments,
    all(Condition-Treatment,treatment(Condition,Treatment),Treatments),
    maplist(writeln,Treatments).