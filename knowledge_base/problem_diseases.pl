% status_problem/2
status_problem(biotic, disease).

% problem_condition/2
problem_condition(disease, black_spot).
problem_condition(disease, leaf_spot).
problem_condition(disease, powdery_mildew).
problem_condition(disease, downy_mildew).
problem_condition(disease, blight).
problem_condition(disease, canker).
problem_condition(disease, shot_hole).
problem_condition(disease, early_blight).
problem_condition(disease, late_blight).
problem_condition(disease, grey_mold).
problem_condition(disease, verticilium_wilt).

% type/1
% treatment/1

% black_spot
condition(black_spot) :- symptom(leaves, chlorotic, none), symptom(leaves, early_fall, none).
condition(black_spot) :- symptom(upper_side_leaves, spots, black, none).
treatment(black_spot, 'plant in well-draining soil. Provide regular feedings of organic fertilizer. Remove dead leaves and infected canes and do not add them to the compost pile. Avoid applying water on the leaves, as it spreads the fungal spores.').

% leaf_spot
condition(leaf_spot) :- symptom(leaves, visible_spores, none), symptom(leaves, spots, brown), symptom(leaves, spots, dark_margin).
condition(leaf_spot) :- symptom(leaves, visible_spores, none), symptom(leaves, spots, tan), symptom(leaves, spots, dark_margin).
condition(leaf_spot) :- symptom(leaves, visible_spores, none), symptom(leaves, spots, reddish), symptom(leaves, spots, dark_margin).
condition(leaf_spot) :- symptom(leaves, angular_lesions, none).
condition(leaf_spot) :- symptom(leaves, angular_lesions, none), symptom(leaves, altered_color, yellowish_outline).
condition(leaf_spot) :- symptom(leaves, circular_lesions, none).
condition(leaf_spot) :- symptom(leaves, circular_lesions, none), symptom(leaves, altered_color, yellowish_outline).
condition(leaf_spot) :- symptom(leaves, necrotic_lesions, none).
condition(leaf_spot) :- symptom(leaves, necrotic_lesions, none), symptom(leaves, altered_color, yellowish_outline).
treatment(leaf_spot, 'plant in well-draining soil. Provide regular feedings of organic fertilizer. Remove dead leaves and infected canes and do not add them to the compost pile. Avoid applying water on the leaves, as it spreads the fungal spores.').

% powdery_mildew
condition(powdery_mildew) :- symptom(all, white_powdery_substance, none).
treatment(powdery_mildew, 'trim and remove infected parts. Do not till the debris into the soil or use in compost pile. Space plants far enough apart to increase air circulation and reduce humidity').

% downy_mildew
condition(downy_mildew) :- symptom(upper_side_lower_leaves, spots, pale_green).
condition(downy_mildew) :- symptom(upper_side_lower_leaves, spots, yellow).
condition(downy_mildew) :- symptom(lower_side_leaves, white_cotton_like_downy_substance, none).
condition(downy_mildew) :- symptom(lower_side_leaves, greysh_cotton_like_downy_substance, none).
condition(downy_mildew) :- symptom(lower_side_leaves, grayish_fuzzy_looking_spores, none).
treatment(downy_mildew, 'keep water off leaves as much as possible, as downy mildew need water to survive and spread. Be sure to clean around the plants in the fall to help prevent the biotic').

% blight
condition(blight) :- symptom(leaves, altered_color, chlorotic).
condition(blight) :- symptom(all, spot, none).
condition(blight) :- symptom(all, withering, none).
condition(blight) :- symptom(all, death_of_plant_tissue, none).
treatment(blight, 'destroy any blight-infected plant parts.  Keep the area clean of fallen debris from your diseased plants and discard in the trash.  Do not add to your compost pile.').

% canker
condition(canker) :- symptom(leaves, small_holes, none).
condition(canker) :- symptom(bark, dead_patches, none).
treatment(canker, 'remove diseased parts in dry weather').
treatment(canker, 'avoid overwatering and overcrowding').

% shot_hole
condition(shot_hole) :- symptom(leaves, spots, tan).
condition(shot_hole) :- symptom(leaves, spots_drop_out, none).
condition(shot_hole) :- symptom(leaves, shot_holes, none).
condition(shot_hole) :- symptom(leaves, spots, purplish).
condition(shot_hole) :- symptom(leaves, spots_drop_out, none).
condition(shot_hole) :- symptom(leaves, shot_holes, none).
condition(shot_hole) :- symptom(fruits, spots, red).
condition(shot_hole) :- symptom(fruits, spots, red), symptom(fruits, clear_gummy_substance, none).
condition(shot_hole) :- symptom(fruits, spots, purplish).
condition(shot_hole) :- symptom(fruits, spots, purplish), symptom(fruits, clear_gummy_substance, none).
treatment(shot_hole, 'destroy fallen leaves, spray with copper spray').

% early_blight
condition(early_blight) :- symptom(leaves, spots, dark_brown). 
condition(early_blight) :- symptom(leaves, spots, black). 
condition(early_blight) :- symptom(stem, spots, black). 
condition(early_blight) :- symptom(fruits, sunken_spot, black).
condition(early_blight) :- symptom(fruits, leathery_spot, black).
condition(early_blight) :- symptom(fruits, large_spots, black).
treatment(early_blight, 'Space and prune plants for good air curculation. Avoid overhead watering. Destroy infected leaves').

% late_blight
condition(late_blight) :- symptom(lower_leaves, irregular_water_soaked_blotches, greenish_black).
condition(late_blight) :- symptom(stem, irregular_water_soaked_blotches, greenish_black).
condition(late_blight) :-  symptom(lower_leaves, irregular_water_soaked_blotches, greenish_black), symptom(fruits, irregular_water_soaked_blotches, greenish_black).
condition(late_blight) :- symptom(stem, irregular_water_soaked_blotches, greenish_black), symptom(fruits, irregular_water_soaked_blotches, greenish_black).
treatment(late_blight, 'remove diseased leaves or entire plants immediately, seal in a plastic bag and send to the landfill. Do not compost late blight infected plants. Apply a copper spray at every 5 to 10 days till allowed days before harvest').

% grey_mold
condition(grey_mold) :- symptom(all, fuzzy_mold, grey).
condition(grey_mold) :- symptom(stem, water_soaked_spots, none).
condition(grey_mold) :- symptom(flower, water_soaked_spots, none).
condition(grey_mold) :- symptom(lower_leaves, water_soaked_spots, none).
treatment(grey_mold, 'Thoroughly clean and discard garden debris and refuse in the fall to reduce the level of grey mold in your garden. Susceptible plants (that are sun loving) should be grown in sunny areas with good air circulation. If practical water at the base of plants not over the foliage. If botrytis appears, remove infected leaves and fruit.').

% verticilium_wilt
condition(verticilium_wilt) :- symptom(branches, altered_color, chlorotic).
condition(verticilium_wilt) :- symptom(branches, wilting, none).
condition(verticilium_wilt) :- symptom(branches, dying, none).
treatment(verticilium_wilt, 'Remove dead and dying plants including the infested roots and the soil and replant with tolerant or resistant species. When pruning trees that may have this biotic, sterilize your pruning tools between trees to prevent spreading it to an and noninfected tree').