% health_problem/2
health_problem(abiotic, nutrient_deficiency).

% problem_type/2
problem_type(nutrient_deficiency, nitrogen).
problem_type(nutrient_deficiency, phosphorus).
problem_type(nutrient_deficiency, potassium).
problem_type(nutrient_deficiency, sulfur).
problem_type(nutrient_deficiency, magnesium).
problem_type(nutrient_deficiency, calcium).
problem_type(nutrient_deficiency, boron).
problem_type(nutrient_deficiency, chloride).
problem_type(nutrient_deficiency, copper).
problem_type(nutrient_deficiency, iron).
problem_type(nutrient_deficiency, manganese).
problem_type(nutrient_deficiency, zinc).

% type/1

% nitrogen
type(nitrogen) :- symptom(lower_leaves, altered_color, chlorotic).
type(nitrogen) :- symptom(leaves, early_fall).
type(nitrogen) :- symptom(roots, increased_growth).
type(nitrogen) :- symptom(shoot, stunted_growth).

% phosphorus
type(phosphorus) :- symptom(leaves, poor_quantity).
type(phosphorus) :- symptom(leaves, altered_color, dark_green).
type(phosphorus) :- symptom(leaves, small_size).
type(phosphorus) :- symptom(roots, increased_growth).
type(phosphorus) :- symptom(shoot, stunted_growth).

% potassium
type(potassium) :- symptom(lower_leaves, altered_color, blotchy_chlorosis).
type(potassium) :- symptom(leaves, altered_color, interveinal_chlorosis).
type(potassium) :- symptom(upper_leaves, stunted_growth).

% sulfur
type(sulfur) :- symptom(upper_leaves, altered_color, chlorotic).
type(sulfur) :- symptom(upper_leaves, stunted_growth).
type(sulfur) :- symptom(leaves, thin_size).

% magnesium
type(magnesium) :- symptom(lower_leaves, altered_color, interveinal_chlorosis).
type(magnesium) :- symptom(leaves, altered_color, chlorotic).
type(magnesium) :- symptom(leaves, altered_color, reddish_purple).

% calcium
type(calcium) :- symptom(leaves, deformed).
type(calcium) :- symptom(leaves, altered_color, dark_veins).
type(calcium) :- symptom(upper_leaves, spots, brown).
type(calcium) :- symptom(upper_leaves, stunted_growth).
type(calcium) :- symptom(leaves, parachute_shape).
type(calcium) :- symptom(all, deformed).

% boron
type(boron) :- symptom(buds, fail_to_form).
type(boron) :- symptom(leaves, irregular_lesions).
type(boron) :- symptom(shoot, death_of_growing_point).
type(boron) :- symptom(upper_leaves, altered_color, chlorotic).
type(boron) :- symptom(leaves, spots, whitish_yellow).
type(boron) :- symptom(stem, stunted_growth).
type(boron) :- symptom(leaves, deformed).
type(boron) :- symptom(buds, deformed).

% chloride
type(chloride) :- symptom(leaves, spots, necrotic).
type(chloride) :- symptom(leaves, spots, chlorotic).
type(chloride) :- symptom(leaves, wilting_margins).
type(chloride) :- symptom(roots, highly_branched_growth).

% copper
type(copper) :- symptom(upper_leaves, altered_color, chlorotic).
type(copper) :- symptom(all, altered_color, brown_discoloration).
type(copper) :- symptom(all, stunted_growth).
type(copper) :- symptom(all, delayed_growth).
type(copper) :- symptom(all, excessive_tillering_growth).

% iron
type(iron) :- symptom(fruits, poor_quantity).
type(iron) :- symptom(fruits, poor_quality).
type(iron) :- symptom(upper_leaves, altered_color, angular_chlorosis).
type(iron) :- symptom(roots, altered_color, brown).
type(iron) :- symptom(root, smell_strange).

% manganese
type(manganese) :- symptom(leaves, withering), symptom(leaves, altered_color, brown).
type(manganese) :- symptom(lower_leaves, altered_color, chlorotic).
type(manganese) :- symptom(leaves, altered_color, interveinal_chlorosis).
type(manganese) :- symptom(leaves, spots, brown).

% zinc
type(zinc) :- symptom(upper_leaves, altered_color, rounded_chlorosis).
type(zinc) :- symptom(leaves, altered_color, bronze).
type(zinc) :- symptom(leaves, stunted_growth).
type(zinc) :- symptom(leaves, deformed).