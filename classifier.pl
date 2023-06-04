% Dynamic predicates for the knowledge base
:- dynamic(prior/2).
:- dynamic(likelihood/3).
:- dynamic(posterior/2).
:- dynamic(conditions/2).
:- dynamic(symptoms/2).

:- [logs/observations].

% probability_start/0
classifier_start :-
	unique_conditions,
    listing(UniqueConditions),
	unique_symptoms,
    listing(UniqueSymptoms),
    compute_priors,
    listing(prior),
    compute_likelihoods,
    listing(likelihood),
    compute_posteriors.

unique_conditions :-
	findall(Condition, condition(Condition, _), Conditions),
	list_to_ord_set(Conditions, UniqueConditions),
	length(UniqueConditions, N),
	log_conditions(conditions(UniqueConditions, N)).

unique_symptoms :-
    findall(Symptom, (condition(_, Symptoms), member(Symptom, Symptoms)), AllSymptoms),
	list_to_ord_set(AllSymptoms, UniqueSymptoms),
	length(UniqueSymptoms, N),
	log_symptoms(symptoms(UniqueSymptoms, N)).

% compute_priors/0
% Predicate to compute prior probabilities given a list of conditions
compute_priors :-
	conditions(Conditions, N),
    P is 1 / N,
    forall(member(Condition, Conditions), log_prior(prior(Condition, P))).

% conditional_probability/3
conditional_probability(Symptom, Condition, Probability) :-
    condition(Condition, Symptoms),
    member(Symptom, Symptoms),  % Controlla se il sintomo appartiene alla lista dei sintomi della condizione
    symptoms(AllSymptoms, AllSymptomsCount),  % Conta il numero totale di occorrenze di sintomi
    observation(Symptom, SymptomsCount),  % Conta il numero di occorrenze del sintomo
    Probability is SymptomCount / AllSymptomsCount.  % Calcola la probabilità come il rapporto delle occorrenze del sintomo sul totale

posterior :-
	conditions(Conditions, _),
	findall(Symptom, observation(Symptom, Count), Symptoms),
	all([Condition, Posterior], (
		observation(Symptom, _),
		member(Symptom, Symptoms),
		member(Condition, Conditions),
		conditional_probability(Symptom, Condition, Probability),
		prior(Condition, Prior),
		Posterior is Prior * Probability
	), Classification),
	maplist(writeln, Classification).


% compute_likelihoods/0
% Predicate to compute likelihoods
compute_likelihoods :-
    symptoms(Symptoms, _),
	conditions(Conditions, N),
    K = 1,
    D = 2,
    forall(member(Symptom, Symptoms),
        forall(member(Condition, Conditions),
			condition(Condition, ConditionSymptoms),
            count_occurrences(Symptom, ConditionSymptoms, Count),
            compute_likelihood(Symptom, Condition, K, D, N, Count)
        )
    ).

% compute_likelihood/6
% compute_likelihood(Symptom, Condition, K, D, N, Count)
% Predicate to compute likelihood for a symptom and condition
compute_likelihood(_, _, _, _, _, 0).
compute_likelihood(Symptom, Condition, K, D, N, Count) :-
    condition(Condition, SymptomsList),
    count_occurrences(Symptom, SymptomsList, TotalCount),
    L is (Count + K) / (TotalCount + (K * D)),
    log_likelihood(likelihood(Symptom, Condition, L)).

% Utility predicate to count occurrences of an element in a list
count_occurrences(_, [], 0).
count_occurrences(Element, [Element | Rest], Count) :-
    count_occurrences(Element, Rest, Count1),
    Count is Count1 + 1.
count_occurrences(Element, [_ | Rest], Count) :-
    count_occurrences(Element, Rest, Count).

% compute_posteriors/0
% Predicate to compute posterior probabilities given a list of conditions
compute_posteriors :-
	conditions(Conditions, _),
    forall(member(Condition, Conditions),
        compute_posterior(Condition)
    ).

% compute_posterior/1
% Predicate to compute posterior probability for a condition
compute_posterior(Condition) :-
    prior(Condition, Prior),
    findall(Likelihood, likelihood(_, Condition, Likelihood), Likelihoods),
    product_list(Likelihoods, LikelihoodProduct),
    Posterior is Prior * LikelihoodProduct,
    log_posterior(posterior(Condition, Posterior)).

% Auxiliary predicate to compute the product of elements in a list
product_list([], 1).
product_list([X | Xs], Product) :-
    product_list(Xs, Product1),
    Product is X * Product1.