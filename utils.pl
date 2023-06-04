:- use_module(library(system)).

% Predicate to display a menu and get user's selection
ask_menu(Menu, Selection) :-
    write('Select an option:'), nl,
    display_menu(Menu, 1),
    read(Index),
    nth1(Index, Menu, Selection).

% Helper predicate to display the menu options with their indices
display_menu([], _).
display_menu([Option|Rest], Index) :-
    write(Index), write('. '), write(Option), nl,
    NewIndex is Index + 1,
    display_menu(Rest, NewIndex).

% timestamp/1
timestamp(T) :- 
    datime(datime(Year, Month, Day, Hour, Minute, Second)),
    T = timestamp(Year-Month-Day, Hour:Minute:Second).

% write_message/1
write_message(MessageCode) :-
    message_code(MessageCode, Message),
    write(Message).
% writeln_message/1
writeln_message(MessageCode) :-
    message_code(MessageCode, Message),
    writeln(Message).

% match/2 Checks whether every member of L1 is in L2
match(L1, L2):- forall(member(X, L1), member(X, L2)).

% conj_to_list/2
conj_to_list((H, C), [H|T]) :-
    !,
    conj_to_list(C, T).
conj_to_list(H, [H]).

% list_to_conj/2
list_to_conj([H|T], (H, C)) :-
    !,
    list_to_conj(T, C).
list_to_conj([H], H).

% askif/1
askif(Q) :-
    ask(Q, A),
    positive_answer(Q, A).

% ask/2
ask(Qcode, A) :- asked(Qcode, A).
ask(Qcode, A) :- \+ (asked(Qcode, A)),
    question_code(Qcode, Q),
    write(Q), write('?'), nl,
    read(A2),
    ask2(Q, Qcode, A2, A).

% positive_answer/2
positive_answer(Q, A) :- affirmative(A).
positive_answer(Qcode, A) :-
    \+ (negative(A)),
    \+ (affirmative(A)),
    message_code(yes_or_no, M),
    write(M), nl,
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
    write(Q), write('?'), nl,
    read(A2),
    ask2(Q, Qcode, A2, A).

% ask2/4
ask2(Q, Qcode, '?', A) :-
    explain(Qcode),
    ask(Qcode, A).

ask2(Q, Qcode, A, A) :-
    \+ (A = '?'),
    asserta(asked(Qcode, A)).

% % explain/1
% explain(X) :- 
%     explanation(X, Y),
%     writeln(Y).
% explain(X) :-
%     \+ explanation(X, Y),
%     writeln_message(no_explanation).

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