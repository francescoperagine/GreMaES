debug(off).

% is_debug/0
is_debug :- debug(on).

% timer/2
% Estimates the CPU time for evaluating a Goal.
timer(Goal, Time) :-
    statistics(runtime, [Start|_]),
    Goal,
    statistics(runtime, [End|_]),
    Time is End - Start.

% utils_init
% Initializes the most frequently used lists to improve performances and improve tracing readeability
utils_init :-
    signs_init,
    plants_init.

% signs/0
% Stores in the working memory a list of all signs for future samplings.
signs_init :-
    all(Sign,sign_location(Sign,_),Signs),
    delete(Signs,'none',Signs1),
    list_to_ord_set(Signs1,Set),
    saveln(signs(Set)).

% plants_init/0
% Stores in the working memory a list of all plants for future samplings.
plants_init :-
    all(Plant,plant_sensor(Plant,_,_),Plants),
    list_to_ord_set(Plants,Set),
    saveln(plants(Set)).

% history/0
history :-
    all([P|H2],
            (fact_history(P,H1),
            usedfact(ID,manifests(P,S)),
            memberchk(ID,H1),
            reverse(H1,H2)),
        Hs),
    maplist(writeln,Hs).
% history/1 (-Hs)
% Returns the reversed plants' histories
history(Hs) :-
    all([P|H2],
            (fact_history(P,H1),
            usedfact(ID,manifests(P,S)),
            memberchk(ID,H1),
            reverse(H1,H2)),
        Hs).


% plant_history_id/2(+P,-H1)
% Returns the reversed plant's history
plant_history_id(P,H1) :-
    fact_history(P,H),
    reverse(H,H1).

% plant_history/3 (+Plant,-History,-Facts)
% Returns plant's history and facts
plant_history(Plant,History,Facts) :-
    plant_history_id(Plant,History),
    all(X-Fact,(
        member(X,History),
        (usedfact(X,Fact) ; (rule(X,(Head),Body),Fact = Head-Body))
        ),Facts).

% timestamp_utc/1
timestamp_utc(Now) :-
    datime(datime(Year, Month, Day, Hour, Minute, Second)),
    add_zero(Month, PaddedMonth),
    add_zero(Day, PaddedDay),
    add_zero(Hour, PaddedHour),
    add_zero(Minute, PaddedMinute),
    add_zero(Second, PaddedSecond),
    atomic_list_concat([Year, PaddedMonth, PaddedDay], '-', Date),
    atomic_list_concat([PaddedHour, PaddedMinute, PaddedSecond, 'Z'], ':', Time),
    atomic_list_concat([Date, 'T', Time], DateTime),
    format(atom(Now), '~w', [DateTime]).
    
% add_zero/2
add_zero(N, Padded) :-
    (N < 10 -> atomic_concat([0, N], Padded) ; Padded = N).

% write_message/1 (+MessageCode)
% Retrieves the string message from its code and prints it out.
write_message(MessageCode) :-
    message_code(MessageCode,Message),
    write(Message).
% writeln_message/1
writeln_message(MessageCode) :-
    write_message(MessageCode),nl.

% match/2 (+X,+L)
% Checks whether the X matches any element within L
match(X,[X|L]).
match(X,[Y|L]) :- match(X,L).

% match/2 (+L1,+L1)
% Checks every member of L1 in L2
match([], _).
match([X|L1], L2) :- 
    member(X, L2),
    match(L1, L2).

% askif/1
askif(Q) :-
    ask(Q,A),
    positive_answer(Q,A).

% ask/2
ask(Qcode,A) :- asked(Qcode,A).
ask(Qcode,A) :- \+ (asked(Qcode,A)),
    question_code(Qcode,Q),
    write(Q),write('?\n'),
    read(A2),
    ask2(Q,Qcode,A2,A).

% positive_answer/2
positive_answer(Q,A) :- affirmative(A).
positive_answer(Qcode,A) :-
    \+ (negative(A)),
    \+ (affirmative(A)),
    message_code(yes_or_no,M),
    write(M),nl,
    read(A2),
    saveln(asked(Qcode,A)),
    affirmative(A2).

% ask/2
ask(Qcode,A) :- asked(Qcode,A).
ask(Qcode,A) :- \+ (asked(Qcode,A)),
    question_code(Qcode,Q),
    write(Q),write('?'),nl,
    read(A2),
    ask2(Q,Qcode,A2,A).

% ask2/4
ask2(Q,Qcode,'?',A) :-
    explain(Qcode),
    ask(Qcode,A).

ask2(Q,Qcode,A,A) :-
    \+ (A = '?'),
    saveln(asked(Qcode,A)).

% explain/1
explain(X) :- 
    explanation(X,Y),
    writeln(Y).
explain(X) :-
    \+ explanation(X,Y),
    writeln_message(no_explanation).

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