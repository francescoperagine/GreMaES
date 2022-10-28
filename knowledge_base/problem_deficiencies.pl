% health_problem/2
health_problem(abiotic, nutrient_deficiency).

% problem_condition/2
problem_condition(nutrient_deficiency, nitrogen).
problem_condition(nutrient_deficiency, phosphorus).
problem_condition(nutrient_deficiency, potassium).
problem_condition(nutrient_deficiency, sulfur).
problem_condition(nutrient_deficiency, magnesium).
problem_condition(nutrient_deficiency, calcium).
problem_condition(nutrient_deficiency, boron).
problem_condition(nutrient_deficiency, chloride).
problem_condition(nutrient_deficiency, copper).
problem_condition(nutrient_deficiency, iron).
problem_condition(nutrient_deficiency, manganese).
problem_condition(nutrient_deficiency, zinc).

% condition/1

% nitrogen
condition(nitrogen) :- symptom(lower_leaves, altered_color, chlorotic).
condition(nitrogen) :- symptom(leaves, early_fall).
condition(nitrogen) :- symptom(roots, increased_growth).
condition(nitrogen) :- symptom(shoot, stunted_growth).

% phosphorus
condition(phosphorus) :- symptom(leaves, poor_quantity).
condition(phosphorus) :- symptom(leaves, altered_color, dark_green).
condition(phosphorus) :- symptom(leaves, small_size).
condition(phosphorus) :- symptom(roots, increased_growth).
condition(phosphorus) :- symptom(shoot, stunted_growth).

% potassium
condition(potassium) :- symptom(lower_leaves, altered_color, blotchy_chlorosis).
condition(potassium) :- symptom(leaves, altered_color, interveinal_chlorosis).
condition(potassium) :- symptom(upper_leaves, stunted_growth).

% sulfur
condition(sulfur) :- symptom(upper_leaves, altered_color, chlorotic).
condition(sulfur) :- symptom(upper_leaves, stunted_growth).
condition(sulfur) :- symptom(leaves, thin_size).

% magnesium
condition(magnesium) :- symptom(lower_leaves, altered_color, interveinal_chlorosis).
condition(magnesium) :- symptom(leaves, altered_color, chlorotic).
condition(magnesium) :- symptom(leaves, altered_color, reddish_purple).

% calcium
condition(calcium) :- symptom(leaves, deformed).
condition(calcium) :- symptom(leaves, altered_color, dark_veins).
condition(calcium) :- symptom(upper_leaves, spots, brown).
condition(calcium) :- symptom(upper_leaves, stunted_growth).
condition(calcium) :- symptom(leaves, parachute_shape).
condition(calcium) :- symptom(all, deformed).

% boron
condition(boron) :- symptom(buds, fail_to_form).
condition(boron) :- symptom(leaves, irregular_lesions).
condition(boron) :- symptom(shoot, death_of_growing_point).
condition(boron) :- symptom(upper_leaves, altered_color, chlorotic).
condition(boron) :- symptom(leaves, spots, whitish_yellow).
condition(boron) :- symptom(stem, stunted_growth).
condition(boron) :- symptom(leaves, deformed).
condition(boron) :- symptom(buds, deformed).

% chloride
condition(chloride) :- symptom(leaves, spots, necrotic).
condition(chloride) :- symptom(leaves, spots, chlorotic).
condition(chloride) :- symptom(leaves, wilting_margins).
condition(chloride) :- symptom(roots, highly_branched_growth).

% copper
condition(copper) :- symptom(upper_leaves, altered_color, chlorotic).
condition(copper) :- symptom(all, altered_color, brown_discoloration).
condition(copper) :- symptom(all, stunted_growth).
condition(copper) :- symptom(all, delayed_growth).
condition(copper) :- symptom(all, excessive_tillering_growth).

% iron
condition(iron) :- symptom(fruits, poor_quantity).
condition(iron) :- symptom(fruits, poor_quality).
condition(iron) :- symptom(upper_leaves, altered_color, angular_chlorosis).
condition(iron) :- symptom(roots, altered_color, brown).
condition(iron) :- symptom(root, smell_strange).

% manganese
condition(manganese) :- symptom(leaves, withering), symptom(leaves, altered_color, brown).
condition(manganese) :- symptom(lower_leaves, altered_color, chlorotic).
condition(manganese) :- symptom(leaves, altered_color, interveinal_chlorosis).
condition(manganese) :- symptom(leaves, spots, brown).

% zinc
condition(zinc) :- symptom(upper_leaves, altered_color, rounded_chlorosis).
condition(zinc) :- symptom(leaves, altered_color, bronze).
condition(zinc) :- symptom(leaves, stunted_growth).
condition(zinc) :- symptom(leaves, deformed).