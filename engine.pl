% engine_init/0
engine_init :-
    not(last_index(_)),
    all(ID,rule(ID,_,_),IDs),
    max_list(IDs,LastID),
    NextID is LastID + 1,
    save_index(NextID).

% next_index/1 (-NextID)
next_index(NextID) :-
    last_index(LastID),
    NextID is LastID + 1,
    save_index(NextID).

% backward/1 (+Fact)
% Backward chaining uses as KB the output of the forward one, therefore starts from evaluating conditions.
backward(Fact) :-
    usedfact(_,Fact),
    \+ functor(Fact,manifests,_).
backward(Fact) :-
    usedfact(_,Fact),
    functor(Fact,condition,_).
% Evaluates all rules and stores the proved facts on top of the KB then adds them to the plant history.
backward(Fact) :-
    rule(RuleID,Fact,[Condition]),
    backward(Condition),
    \+ usedfact(_,Fact),
    next_index(FactID),
    saveln(usedfact(FactID,Fact)),
    Fact =.. [_,X,_],
    save_history(X,RuleID),
    save_history(X,FactID).

% backward/0
% Initializes the inference.
backward :-
    all(Fact,(backward(Fact)),Facts).

% forward/0
forward :- done.
forward :- 
    fact(ID,Fact),
    not(pursuit(ID,Fact)),
    save_usedfact(ID,Fact),
    forward.

% done/0
done :- not(fact(_,_)).

% pursuit/2 (+FactID,+Fact)
pursuit(FactID,Fact) :-
    rule(RuleID,Head,Conditions),
    functor(Head,condition,_),
    rule_pursuit(FactID,Fact,RuleID,Head,Conditions),
    fail.
    
% rule_pursuit/5 (+FactID,+Fact,+RuleID,+Head,+Conditions)
% Searches through rules conditions,deleting entries that match the fact F
rule_pursuit(FactID,Fact,RuleID,Head,Conditions) :-
    match(Fact,Conditions),
    delete_fact(Fact,Conditions,ConditionsNew),
    new_rule(FactID,RuleID,Head,ConditionsNew).

% delete_fact/3 (+X,+List,-Rest)
% Unifies the fact by binding the variable within the conditions and it always succedees
delete_fact(X,[],[]).
delete_fact(X,[X|L],M) :- delete_fact(X,L,M).
delete_fact(X,[Y|L],[Y|M]) :- 
    not(X=Y),
    delete_fact(X,L,M).

% new_rule/4 (+FactID,+RuleID,+Head,+List)
% When the right-hand sided of a rule is empty a new fact is made,otherwise a new updated rule is made.
new_rule(FactID,RuleID,Head,[]) :-
    save_fact(Head,FactIDNew),
    save_trail(FactID,[RuleID,FactIDNew]).
% (+FactID,-RuleID,+Head,+List)
new_rule(FactID,_,Head,Conditions) :-
    not(Conditions=[]),
    save_rule(RuleID,Head,Conditions),
    save_trail(FactID,RuleID).
new_rule(_,RuleID,Head,Conditions) :-
    fact(_,Head),
    Conditions=[].