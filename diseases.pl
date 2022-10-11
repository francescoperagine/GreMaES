% health_problem/2
health_problem(biotic, disease).

% class/2
class(disease, black_spot).
class(disease, leaf_spot).
class(disease, powdery_mildew).
class(disease, downy_mildew).
class(disease, blight).
class(disease, canker).
class(disease, shot_hole).
class(disease, early_blight).
class(disease, late_blight).
class(disease, grey_mold).
class(disease, verticilium_wilt).

% type/1
% treatment/1

% black_spot
type(black_spot) :- symptom(leaves, chlorosis), symptom(leaves, early_fall).
type(black_spot) :- symptom(upper_side_leaves, spots, black).
treatment(black_spot, 'plant in well-draining soil. Provide regular feedings of organic fertilizer. Remove dead leaves and infected canes and do not add them to the compost pile. Avoid applying water on the leaves, as it spreads the fungal spores.').

% leaf_spot
type(leaf_spot) :- symptom(leaves, visible_spores), symptom(leaves, spots, brown), symptom(leaves, spots, dark_margin).
type(leaf_spot) :- symptom(leaves, visible_spores), symptom(leaves, spots, tan), symptom(leaves, spots, dark_margin).
type(leaf_spot) :- symptom(leaves, visible_spores), symptom(leaves, spots, reddish), symptom(leaves, spots, dark_margin).
type(leaf_spot) :- symptom(leaves, angular_lesions).
type(leaf_spot) :- symptom(leaves, angular_lesions), symptom(leaves, altered_color, yellowish_outline).
type(leaf_spot) :- symptom(leaves, circular_lesions).
type(leaf_spot) :- symptom(leaves, circular_lesions), symptom(leaves, altered_color, yellowish_outline).
type(leaf_spot) :- symptom(leaves, necrotic_lesions).
type(leaf_spot) :- symptom(leaves, necrotic_lesions), symptom(leaves, altered_color, yellowish_outline).
treatment(leaf_spot, 'plant in well-draining soil. Provide regular feedings of organic fertilizer. Remove dead leaves and infected canes and do not add them to the compost pile. Avoid applying water on the leaves, as it spreads the fungal spores.').

% powdery_mildew
type(powdery_mildew) :- symptom(all, white_powdery_substance).
treatment(powdery_mildew, 'trim and remove infected parts. Do not till the debris into the soil or use in compost pile. Space plants far enough apart to increase air circulation and reduce humidity').

% downy_mildew
type(downy_mildew) :- symptom(upper_side_lower_leaves, spots, pale_green).
type(downy_mildew) :- symptom(upper_side_lower_leaves, spots, yellow).
type(downy_mildew) :- symptom(lower_side_leaves, cotton_like_downy_substance, white).
type(downy_mildew) :- symptom(lower_side_leaves, cotton_like_downy_substance, greysh).
type(downy_mildew) :- symptom(lower_side_leaves, grayish_fuzzy_looking_spores).
treatment(downy_mildew, 'keep water off leaves as much as possible, as downy mildew need water to survive and spread. Be sure to clean around the plants in the fall to help prevent the disease').

% blight
type(blight) :- symptom(leaves, altered_color, chlorosis).
type(blight) :- symptom(all, spot).
type(blight) :- symptom(all, withering).
type(blight) :- symptom(all, death_of_plant_tissue).
treatment(blight, 'destroy any blight-infected plant parts.  Keep the area clean of fallen debris from your diseased plants and discard in the trash.  Do not add to your compost pile.').

% canker
type(canker) :- symptom(leaves, small_holes).
type(canker) :- symptom(bark, dead_patches).
treatment(canker, 'remove diseased parts in dry weather').
treatment(canker, 'avoid overwatering and overcrowding').

% shot_hole
type(shot_hole) :- symptom(leaves, spots, tan), symptom(leaves, spots_drop_out), symptom(leaves, shot_holes).
type(shot_hole) :- symptom(leaves, spots, purplish), symptom(leaves, spots_drop_out), symptom(leaves, shot_holes).
type(shot_hole) :- symptom(fruits, spots, red).
type(shot_hole) :- symptom(fruits, spots, red), symptom(fruits, clear_gummy_substance).
type(shot_hole) :- symptom(fruits, spots, purplish).
type(shot_hole) :- symptom(fruits, spots, purplish), symptom(fruits, clear_gummy_substance).
treatment(shot_hole, 'destroy fallen leaves, spray with copper spray').

% early_blight
type(early_blight) :- symptom(leaves, spots, dark_brown). 
type(early_blight) :- symptom(leaves, spots, black). 
type(early_blight) :- symptom(stem, spots, black). 
type(early_blight) :- symptom(fruits, sunken_spot, black), symptom(fruits, leathery_spot, black), symptom(fruits, large_spots, black).
treatment(early_blight, 'Space and prune plants for good air curculation. Avoid overhead watering. Destroy infected leaves').

% late_blight
type(late_blight) :- symptom(lower_leaves, irregular_water_soaked_blotches, greenish_black).
type(late_blight) :- symptom(stem, irregular_water_soaked_blotches, greenish_black).
type(late_blight) :- 
    symptom(lower_leaves, irregular_water_soaked_blotches, greenish_black),
    symptom(fruits, irregular_water_soaked_blotches, greenish_black).
type(late_blight) :-
    symptom(stem, irregular_water_soaked_blotches, greenish_black),
    symptom(fruits, irregular_water_soaked_blotches, greenish_black).
treatment(late_blight, 'remove diseased leaves or entire plants immediately, seal in a plastic bag and send to the landfill. Do not compost late blight infected plants. Apply a copper spray at every 5 to 10 days till allowed days before harvest').

% grey_mold
type(grey_mold) :- symptom(all, grey_fuzzy_mold).
type(grey_mold) :- symptom(stem, water_soaked_spots).
type(grey_mold) :- symptom(flower, water_soaked_spots).
type(grey_mold) :- symptom(lower_leaves, water_soaked_spots).
treatment(grey_mold, 'Thoroughly clean and discard garden debris and refuse in the fall to reduce the level of grey mold in your garden. Susceptible plants (that are sun loving) should be grown in sunny areas with good air circulation. If practical water at the base of plants not over the foliage. If botrytis appears, remove infected leaves and fruit.').

% verticilium_wilt
type(verticilium_wilt) :- symptom(branches, altered_color, chlorosis).
type(verticilium_wilt) :- symptom(branches, wilting).
type(verticilium_wilt) :- symptom(branches, dying).
treatment(verticilium_wilt, 'Remove dead and dying plants including the infested roots and the soil and replant with tolerant or resistant species. When pruning trees that may have this disease, sterilize your pruning tools between trees to prevent spreading it to an and noninfected tree').