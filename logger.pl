:- use_module(library(system)).

log_file(main,'logs/GreMaES.log').
log_file(observations,'logs/observations.pl').
log_file(symptoms,'kb/symptoms.pl').
log_file(rules,'knowledge_base/rules.pl').

% logger_init/0 - Deletes every log at the start
logger_init :-
    all(LogFile,log_file(File,LogFile),LogFiles),
    (member(LogFile,LogFiles) ->  file_exists(LogFile),delete_file(LogFile)).

% log/1
log(Message) :-
    log_file(main,File),
    open(File,append,Stream),
    write(Stream,Message),
    close(Stream),
    write(Message).

% logln/1
logln(Message) :-
    log_file(main,File),
    open(File,append,Stream),
    write(Stream,Message),
    nl(Stream),
    close(Stream),
    writeln(Message).

% log_observation/1
log_observation(symptom(X,Y,Z)) :-
    log_file(observations,File),
    (file_exists(File) ->
        open(File,read,Stream),
        read_file(Stream,Symptoms),
        close(Stream)
    ;
        Symptoms = []
    ),
    update_symptom(Symptoms,symptom(X,Y,Z),UpdatedSymptoms),
    open(File,write,Stream2),
    write_symptoms(Stream2,UpdatedSymptoms),
    close(Stream2).

% log_conditions/1
log_conditions(conditions(Conditions,N)) :-
	retractall(conditions(_,_)),
	assertz(conditions(Conditions,N)).

% log_symptoms/1
log_symptoms(symptoms(Symptoms,N)) :-
	retractall(symptoms(_,_)),
	assertz(symptoms(Symptoms,N)).

% log_prior/1
log_prior(prior(Condition,Prior)) :-
    prior(Condition,Prior).
log_prior(prior(Condition,Prior)) :-
    prior(Condition,_),
    retractall(prior(Condition,_)),
    assertz(prior(Condition,Prior)).
log_prior(prior(Condition,Prior)) :-
    \+ prior(Condition,Prior),
    assertz(prior(Condition,Prior)).

% log_likelihood/3
log_likelihood(Symptom,Condition,Likelihood) :-
    likelihood(Symptom,Condition,Likelihood).
log_likelihood(Symptom,Condition,Likelihood) :-
    Symptom = symptom(Location,Sign,Color),
    \+ likelihood(Symptom,Condition,_),
    assertz(likelihood(Symptom,Condition,Likelihood)).
log_likelihood(Symptom,Condition,Likelihood) :-
    Symptom = symptom(Location,Sign,Color),
    likelihood(Symptom,Condition,_),
    retractall(likelihood(Symptom,Condition,_)),
    assertz(likelihood(Symptom,Condition,Likelihood)).

% log_posterior/1
log_posterior(posterior(Condition,Posterior)) :-
    posterior(Condition,Posterior).
log_posterior(posterior(Condition,Posterior)) :-
    posterior(Condition,_),
    retractall(posterior(Condition,_)),
    assertz(posterior(Condition,Posterior)).
log_posterior(posterior(Condition,Posterior)) :-
    \+ posterior(Condition,Posterior),
    assertz(posterior(Condition,Posterior)).

% read_file/2
read_file(Stream,[]) :-
    at_end_of_stream(Stream).
read_file(Stream,Symptoms) :-
    \+ at_end_of_stream(Stream),
    read(Stream,Symptom),
    (Symptom == end_of_file ->
        Symptoms = []
    ;
        Symptoms = [Symptom|Rest],
        read_file(Stream,Rest)
    ).

% update_symptom/3
update_symptom([],symptom(X,Y,Z),[observation(symptom(X,Y,Z),1)]).
update_symptom([observation(symptom(X,Y,Z),N)|Symptoms],symptom(X,Y,Z),[observation(symptom(X,Y,Z),N1)|Symptoms]) :-
    !,
    N1 is N + 1.
update_symptom([Symptom|Symptoms],Symptom2,[Symptom|UpdatedSymptoms]) :-
    update_symptom(Symptoms,Symptom2,UpdatedSymptoms).

% write_symptoms/2
write_symptoms(_,[]).
write_symptoms(Stream,[Symptom|Symptoms]) :-
    writeq(Stream,Symptom),
    write(Stream,'.\n'),
    write_symptoms(Stream,Symptoms).

% logSymptom/1 - Rewrites the KB facts' bodies as lists
logSymptom((Condition,BodyConj)) :-
    log_file(symptoms,File),
    conj_to_list(BodyConj,BodyList),
    open(File,append,Stream),
    atomic_concat(['condition(',Condition,') :- '],Message),
    write(Stream,Message),
    write(Stream,BodyList),
    write(Stream,'.'),
    nl(Stream),
    close(Stream).
