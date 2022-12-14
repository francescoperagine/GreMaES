% health_problem/2
health_problem(pests, infestation).

% problem_type/2
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
condition(apthids) :- symptom(all, sticky_honeydew).
condition(apthids) :- symptom(all, deformed).
condition(apthids) :- symptom(soil, husks, white).
condition(apthids) :- symptom(soil, husks, grey).
condition(apthids) :- symptom(leaves, random_chlorosis).
treatment(apthids, 'spray with neem oil and insecticidal soap').

% thrips
condition(thrips) :- symptom(leaves, mottled).
condition(thrips) :- symptom(leaves, streaking).
condition(thrips) :- symptom(leaves, altered_color, brown).
treatment(thrips, 'cut off and remove the infected leaves or flowers').
treatment(thrips, 'spray with neem oil or natural pyrethrum').

% spider_mites
condition(spider_mites) :- symptom(all, sticky_webbing).
condition(spider_mites) :- symptom(leaves, mottled), symptom(leaves, spots, brown).
treatment(spider_mites, 'shower the plant once a week').
treatment(spider_mites, 'purchasing the predatory mite Phytosieulus persimilis').
treatment(spider_mites, 'spray with neem oil and insecticidal soap').

% scale_insects
condition(scale_insects) :- symptom(all, sticky_honeydew).
treatment(scale_insects, 'spray with neem oil and insecticidal soap').
treatment(scale_insects, 'dab individual scales with alcohol').

% white_flies
condition(white_flies) :- symptom(leaves, flies, white), symptom(leaves, altered_color, chlorotic).
condition(white_flies) :- symptom(leaves, flies, white), symptom(leaves, dry).
treatment(white_flies, 'apply sticky traps').
treatment(white_flies, 'spray with neem oil and insecticidal soap').

% cutworms
condition(cutworms) :- symptom(leaves, cutworms_under_leaves).
condition(cutworms) :- symptom(all, cutworms_under_debris).
treatment(cutworms, 'Create barriers with cardboard collars or gritty substances like eggshells, coffee grounds, or diatomaceous earth').
treatment(cutworms, 'encourage birds to visit the garden').

% fungus_gnats
condition(fungus_gnats) :- symptom(all, small_flies, black).
treatment(fungus_gnats, 'keep the soil less moist until they leave').
treatment(fungus_gnats, 'use the bottom watering method').
treatment(fungus_gnats, 'mix the nematodes with water and water directly').

% mealy_bugs
condition(mealy_bugs) :- symptom(all, stunted_growth).
condition(mealy_bugs) :- symptom(all, withering).
condition(mealy_bugs) :- symptom(leaves, altered_color, chlorotic).
condition(mealy_bugs) :- symptom(lower_side_leaves, clustering_cottony).
condition(mealy_bugs) :- symptom(leaves, altered_color, chlorotic), symptom(leaves, early_fall).
condition(mealy_bugs) :- symptom(all, dry).
condition(mealy_bugs) :- symptom(all, sticky_honeydew).
treatment(mealy_bugs, 'poke them off shaking the plant').
treatment(mealy_bugs, 'spraying with water').
treatment(mealy_bugs, 'spray with neem oil and insecticidal soap').