% status_problem/2
status_problem(pests, infestation).

% problem_condition/2
problem_condition(infestation, apthids).
problem_condition(infestation, thrips).
problem_condition(infestation, spider_mites).
problem_condition(infestation, scale_insects).
problem_condition(infestation, white_flies).
problem_condition(infestation, cutworms).
problem_condition(infestation, fungus_gnats).
problem_condition(infestation, mealy_bugs).

% type/1
% treatment/1

% apthids
condition(apthids) :- symptom(all, sticky_honeydew, none).
condition(apthids) :- symptom(all, deformed, none).
condition(apthids) :- symptom(soil, husks, white).
condition(apthids) :- symptom(soil, husks, grey).
condition(apthids) :- symptom(leaves, random_chlorosis, none).
treatment(apthids, 'spray with neem oil and insecticidal soap').

% thrips
condition(thrips) :- symptom(leaves, mottled, none).
condition(thrips) :- symptom(leaves, streaking, none).
condition(thrips) :- symptom(leaves, altered_color, brown).
treatment(thrips, 'cut off and remove the infected leaves or flowers').
treatment(thrips, 'spray with neem oil or natural pyrethrum').

% spider_mites
condition(spider_mites) :- symptom(all, sticky_webbing, none).
condition(spider_mites) :- symptom(leaves, mottled, none), symptom(leaves, spots, brown).
treatment(spider_mites, 'shower the plant once a week').
treatment(spider_mites, 'purchasing the predatory mite Phytosieulus persimilis').
treatment(spider_mites, 'spray with neem oil and insecticidal soap').

% scale_insects
condition(scale_insects) :- symptom(all, sticky_honeydew, none).
treatment(scale_insects, 'spray with neem oil and insecticidal soap').
treatment(scale_insects, 'dab individual scales with alcohol').

% white_flies
condition(white_flies) :- symptom(leaves, flies, white), symptom(leaves, altered_color, chlorotic).
condition(white_flies) :- symptom(leaves, flies, white), symptom(leaves, dry).
treatment(white_flies, 'apply sticky traps').
treatment(white_flies, 'spray with neem oil and insecticidal soap').

% cutworms
condition(cutworms) :- symptom(leaves, cutworms_under_leaves, none).
condition(cutworms) :- symptom(all, cutworms_under_debris, none).
treatment(cutworms, 'Create barriers with cardboard collars or gritty substances like eggshells, coffee grounds, or diatomaceous earth').
treatment(cutworms, 'encourage birds to visit the garden').

% fungus_gnats
condition(fungus_gnats) :- symptom(all, small_flies, black).
treatment(fungus_gnats, 'keep the soil less moist until they leave').
treatment(fungus_gnats, 'use the bottom watering method').
treatment(fungus_gnats, 'mix the nematodes with water and water directly').

% mealy_bugs
condition(mealy_bugs) :- symptom(all, stunted_growth, none).
condition(mealy_bugs) :- symptom(all, withering, none).
condition(mealy_bugs) :- symptom(leaves, altered_color, chlorotic).
condition(mealy_bugs) :- symptom(lower_side_leaves, clustering_cottony, none).
condition(mealy_bugs) :- symptom(leaves, altered_color, chlorotic), symptom(leaves, early_fall, none).
condition(mealy_bugs) :- symptom(all, dry, none).
condition(mealy_bugs) :- symptom(all, sticky_honeydew, none).
treatment(mealy_bugs, 'poke them off shaking the plant').
treatment(mealy_bugs, 'spraying with water').
treatment(mealy_bugs, 'spray with neem oil and insecticidal soap').