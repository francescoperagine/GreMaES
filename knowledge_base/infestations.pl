% health_problem/2
health_problem(biotic, infestation).

% class/2
class(infestation, apthids).
class(infestation, thrips).
class(infestation, spider_mites).
class(infestation, scale_insects).
class(infestation, white_flies).
class(infestation, cutworms).
class(infestation, fungus_gnats).
class(infestation, mealy_bugs).

% type/1
% treatment/1

% apthids
type(apthids) :- symptom(all, sticky_honeydew).
type(apthids) :- symptom(all, deformed).
type(apthids) :- symptom(soil, husks, white).
type(apthids) :- symptom(soil, husks, grey).
type(apthids) :- symptom(leaves, random_chlorosis).
treatment(apthids, 'spray with neem oil and insecticidal soap').

% thrips
type(thrips) :- symptom(leaves, mottled).
type(thrips) :- symptom(leaves, streaking).
type(thrips) :- symptom(leaves, altered_color, brown).
treatment(thrips, 'cut off and remove the infected leaves or flowers').
treatment(thrips, 'spray with neem oil or natural pyrethrum').

% spider_mites
type(spider_mites) :- symptom(all, sticky_webbing).
type(spider_mites) :- symptom(leaves, mottled), symptom(leaves, spots, brown).
treatment(spider_mites, 'shower the plant once a week').
treatment(spider_mites, 'purchasing the predatory mite Phytosieulus persimilis').
treatment(spider_mites, 'spray with neem oil and insecticidal soap').

% scale_insects
type(scale_insects) :- symptom(all, sticky_honeydew).
treatment(scale_insects, 'spray with neem oil and insecticidal soap').
treatment(scale_insects, 'dab individual scales with alcohol').

% white_flies
type(white_flies) :- symptom(leaves, flies, white), symptom(leaves, altered_color, chlorotic).
type(white_flies) :- symptom(leaves, flies, white), symptom(leaves, dry).
treatment(white_flies, 'apply sticky traps').
treatment(white_flies, 'spray with neem oil and insecticidal soap').

% cutworms
type(cutworms) :- symptom(leaves, cutworms_under_leaves).
type(cutworms) :- symptom(all, cutworms_under_debris).
treatment(cutworms, 'Create barriers with cardboard collars or gritty substances like eggshells, coffee grounds, or diatomaceous earth').
treatment(cutworms, 'encourage birds to visit the garden').

% fungus_gnats
type(fungus_gnats) :- symptom(all, small_flies, black).
treatment(fungus_gnats, 'keep the soil less moist until they leave').
treatment(fungus_gnats, 'use the bottom watering method').
treatment(fungus_gnats, 'mix the nematodes with water and water directly').

% mealy_bugs
type(mealy_bugs) :- symptom(all, stunted_growth).
type(mealy_bugs) :- symptom(all, withering).
type(mealy_bugs) :- symptom(leaves, altered_color, chlorotic).
type(mealy_bugs) :- symptom(lower_side_leaves, clustering_cottony).
type(mealy_bugs) :- symptom(leaves, altered_color, chlorotic), symptom(leaves, early_fall).
type(mealy_bugs) :- symptom(all, dry).
type(mealy_bugs) :- symptom(all, sticky_honeydew).
treatment(mealy_bugs, 'poke them off shaking the plant').
treatment(mealy_bugs, 'spraying with water').
treatment(mealy_bugs, 'spray with neem oil and insecticidal soap').














