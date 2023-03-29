% health_issue/1

health_issue(abiotic) :- problem(wet).
health_issue(abiotic) :- problem(dry).
health_issue(abiotic) :- problem(hot).
health_issue(abiotic) :- problem(cold).
health_issue(abiotic) :- problem(nutrient_deficiency).

% problem/1

problem(wet) :- reading(humidity, high).
problem(dry) :- reading(humidity, low).
problem(hot) :- reading(temperature, high).
problem(cold) :- reading(temperature, low).


problem(nutrient_deficiency) :- condition(nitrogen).
problem(nutrient_deficiency) :- condition(phosphorus).
problem(nutrient_deficiency) :- condition(potassium).
problem(nutrient_deficiency) :- condition(sulfur).
problem(nutrient_deficiency) :- condition(magnesium).
problem(nutrient_deficiency) :- condition(calcium).
problem(nutrient_deficiency) :- condition(boron).
problem(nutrient_deficiency) :- condition(chloride).
problem(nutrient_deficiency) :- condition(copper).
problem(nutrient_deficiency) :- condition(iron).
problem(nutrient_deficiency) :- condition(manganese).
problem(nutrient_deficiency) :- condition(zinc).

% nitrogen
condition(nitrogen) :- symptom(lower_leaves, altered_color, chlorotic).
condition(nitrogen) :- symptom(leaves, early_fall, none).
condition(nitrogen) :- symptom(roots, increased_growth, none).
condition(nitrogen) :- symptom(shoot, stunted_growth, none).

% phosphorus
condition(phosphorus) :- symptom(leaves, poor_quantity, none).
condition(phosphorus) :- symptom(leaves, altered_color, dark_green).
condition(phosphorus) :- symptom(leaves, small_size, none).
condition(phosphorus) :- symptom(roots, increased_growth, none).
condition(phosphorus) :- symptom(shoot, stunted_growth, none).

% potassium
condition(potassium) :- symptom(lower_leaves, altered_color, blotchy_chlorosis).
condition(potassium) :- symptom(leaves, altered_color, interveinal_chlorosis).
condition(potassium) :- symptom(upper_leaves, stunted_growth, none).

% sulfur
condition(sulfur) :- symptom(upper_leaves, altered_color, chlorotic).
condition(sulfur) :- symptom(upper_leaves, stunted_growth, none).
condition(sulfur) :- symptom(leaves, thin_size, none).

% magnesium
condition(magnesium) :- symptom(lower_leaves, altered_color, interveinal_chlorosis).
condition(magnesium) :- symptom(leaves, altered_color, chlorotic).
condition(magnesium) :- symptom(leaves, altered_color, reddish_purple).

% calcium
condition(calcium) :- symptom(leaves, deformed, none).
condition(calcium) :- symptom(leaves, altered_color, dark_veins).
condition(calcium) :- symptom(upper_leaves, spots, brown).
condition(calcium) :- symptom(upper_leaves, stunted_growth).
condition(calcium) :- symptom(leaves, parachute_shape, none).
condition(calcium) :- symptom(all, deformed, none).

% boron
condition(boron) :- symptom(buds, fail_to_form, none).
condition(boron) :- symptom(leaves, irregular_lesions, none).
condition(boron) :- symptom(shoot, death_of_growing_point, none).
condition(boron) :- symptom(upper_leaves, altered_color, chlorotic).
condition(boron) :- symptom(leaves, spots, whitish_yellow).
condition(boron) :- symptom(stem, stunted_growth, none).
condition(boron) :- symptom(leaves, deformed, none).
condition(boron) :- symptom(buds, deformed, none).

% chloride
condition(chloride) :- symptom(leaves, spots, necrotic).
condition(chloride) :- symptom(leaves, spots, chlorotic).
condition(chloride) :- symptom(leaves, wilting_margins, none).
condition(chloride) :- symptom(roots, highly_branched_growth, none).

% copper
condition(copper) :- symptom(upper_leaves, altered_color, chlorotic).
condition(copper) :- symptom(all, altered_color, brown_discoloration).
condition(copper) :- symptom(all, stunted_growth, none).
condition(copper) :- symptom(all, delayed_growth, none).
condition(copper) :- symptom(all, excessive_tillering_growth, none).

% iron
condition(iron) :- symptom(fruits, poor_quantity, none).
condition(iron) :- symptom(fruits, poor_quality, none).
condition(iron) :- symptom(upper_leaves, altered_color, angular_chlorosis).
condition(iron) :- symptom(roots, altered_color, brown).
condition(iron) :- symptom(root, smell_strange, none).

% manganese
condition(manganese) :- symptom(leaves, withering, none), symptom(leaves, altered_color, brown).
condition(manganese) :- symptom(lower_leaves, altered_color, chlorotic).
condition(manganese) :- symptom(leaves, altered_color, interveinal_chlorosis).
condition(manganese) :- symptom(leaves, spots, brown).

% zinc
condition(zinc) :- symptom(upper_leaves, altered_color, rounded_chlorosis).
condition(zinc) :- symptom(leaves, altered_color, bronze).
condition(zinc) :- symptom(leaves, stunted_growth, none).
condition(zinc) :- symptom(leaves, deformed, none).