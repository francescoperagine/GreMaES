% diagnosis/0
% No symptoms case
diagnosis :-
    not(usedfact(_,_)),
    writeln_message(no_symptom).
% If there are symptoms but there's no clear diagnosis
diagnosis :-
    usedfact(_,_),
    not(usedfact(_,condition(_,_))),
    writeln_message(no_condition).
% If there are clear conditions,explains them
diagnosis :-
    usedfact(_,condition(_,_)),
    diagnosis_forward.

% diagnosis_forward/0
diagnosis_forward :-
    history(HistoriesList),
    (member([Plant|History],HistoriesList),
    plant_history(Plant,History,Facts),
    all(Plant-Condition,(member(ID,History), usedfact(ID,condition(Plant,Condition))),PlantsConditions),
    explain(PlantsConditions),
    explain_inference(Facts)).

% explain_inference/1 (+Facts)
explain_inference(Facts) :-
    need_explanation,
    writeln('\nReasoning performed by the forward engine (ID-Inference step):\n'),
    maplist(writeln,Facts).
explain_inference(_) :-
    \+ need_explanation.

% need_explanation/0
need_explanation :- askif(need_explanation).

% explain/1
explain([]).
explain([Plant-Condition|T]) :-
    explain_diagnosis(Plant-Condition),
    explain_treatment(Condition),
    explain(T).

% explain_diagnosis/1 (+Plant-Condition)
explain_diagnosis(Plant-Condition) :-
    rule(_,problem(_,Problem),[condition(_,Condition)]),
    rule(_,issue(_,Issue),[problem(_,Problem)]),
    usedfact(_,issue(Plant,Issue)),
    (mode_user -> X = 'Your plant'; X = Plant),
    atomic_concat([X,' is affected by the ',Issue,' disorder - ',Condition,' ',Problem], Message),
    writeln(Message).

% explain_treatment/1 (+Condition)
explain_treatment(Condition) :-
    clause(problem('climate'),condition(Condition)),
    writeln('Preserve the plant\'s environment accordingly to its needs.').
explain_treatment(Condition) :-
    problem_condition('nutrient deficiency',Condition),
    writeln_message(missing_nutrient).
explain_treatment(Condition) :-
    \+ problem_condition('nutrient deficiency',Condition),
    \+ treatment(Condition,_),
    write_message(treatment_none), writeln(Condition).
explain_treatment(Condition) :-
    \+ problem_condition('nutrient deficiency',Condition),
    all(Treatment,treatment(Condition,Treatment),Treatments),
    write('* How to treat '),write(': '),
    maplist(writeln,Treatments).