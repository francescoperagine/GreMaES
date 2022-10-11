% health_problem/1
health_problem(abiotic_problem).
health_problem(biotic_problem).

% manifest_section/2
manifest_section(altered_color, all).
manifest_section(altered_color, branches).
manifest_section(altered_color, leaves).
manifest_section(altered_color, lower_leaves).
manifest_section(altered_color, roots).
manifest_section(altered_color, upper_leaves).
manifest_section(angular_lesions, leaves).
manifest_section(black_leathery_spot, fruits).
manifest_section(black_sunken_spot, fruits).
manifest_section(circular_lesions, leaves).
manifest_section(clear_gummy_substance, fruits).
manifest_section(clustering_cottony, lower_side_leaves).
manifest_section(cotton_like_downy_substance, lower_side_leaves).
manifest_section(cutworms_under_debris, all).
manifest_section(cutworms_under_leaves, leaves).
manifest_section(dead_patches, bark).
manifest_section(death_of_growing_point, shoot).
manifest_section(death_of_plant_tissue, all).
manifest_section(deformed, all).
manifest_section(deformed, buds).
manifest_section(deformed, leaves).
manifest_section(delayed_growth, all).
manifest_section(dry, all).
manifest_section(dry, leaves).
manifest_section(dying, branches).
manifest_section(early_fall, leaves).
manifest_section(excessive_tillering_growth, all).
manifest_section(fail_to_form, buds).
manifest_section(flies, leaves).
manifest_section(fuzzy_looking_spores, lower_side_leaves).
manifest_section(highly_branched_growth, roots).
manifest_section(husks, soil).
manifest_section(increased_growth, roots).
manifest_section(irregular_lesions, leaves).
manifest_section(irregular_water_soaked_blotches, fruits).
manifest_section(irregular_water_soaked_blotches, lower_leaves).
manifest_section(irregular_water_soaked_blotches, stem).
manifest_section(large_spots, fruits).
manifest_section(mottled, leaves).
manifest_section(necrotic_lesions, leaves).
manifest_section(parachute_shape, leaves).
manifest_section(poor_quality, fruits).
manifest_section(poor_quantity, fruits).
manifest_section(poor_quantity, leaves).
manifest_section(shot_holes, leaves).
manifest_section(small_flies, all).
manifest_section(small_holes, leaves).
manifest_section(small_size, leaves).
manifest_section(smell_strange, root).
manifest_section(spots_drop_out, leaves).
manifest_section(spots, fruits).
manifest_section(spots, leaves).
manifest_section(spots, stem).
manifest_section(spots, upper_side_lower_leaves).
manifest_section(sticky_honeydew, all).
manifest_section(sticky_webbing, all).
manifest_section(streaking, leaves).
manifest_section(stunted_growth, all).
manifest_section(stunted_growth, leaves).
manifest_section(stunted_growth, shoot).
manifest_section(stunted_growth, stem).
manifest_section(stunted_growth, upper_leaves).
manifest_section(thin_size, leaves).
manifest_section(visible_spores, leaves).
manifest_section(water_soaked_spots, flower).
manifest_section(water_soaked_spots, lower_leaves).
manifest_section(water_soaked_spots, stem).
manifest_section(white_powdery_substance, all).
manifest_section(wilting_margins, leaves).
manifest_section(withering, all).
manifest_section(withering, leaves).

% manifest_color/2
manifest_color(altered_color, chlorotic).
manifest_color(altered_color, dark_green).
manifest_color(altered_color, blotchy_chlorosis).
manifest_color(altered_color, interveinal_chlorosis).
manifest_color(altered_color, chlorotic).
manifest_color(altered_color, reddish_purple).
manifest_color(altered_color, dark_veins).
manifest_color(altered_color, brown_discoloration).
manifest_color(altered_color, angular_chlorosis).
manifest_color(altered_color, brown).
manifest_color(altered_color, rounded_chlorosis).
manifest_color(altered_color, bronze).
manifest_color(altered_color, yellowish_outline).
manifest_color(cotton_like_downy_substance, greysh).
manifest_color(cotton_like_downy_substance, white).
manifest_color(flies, white).
manifest_color(fuzzy_looking_spores, greysh).
manifest_color(fuzzy_mold, grey).
manifest_color(husks, grey).
manifest_color(husks, white).
manifest_color(irregular_water_soaked_blotches, greenish_black).
manifest_color(irregular_water_soaked_blotches, greenish_black).
manifest_color(large_spots, black).
manifest_color(leathery_spot, black).
manifest_color(small_flies, black).
manifest_color(spots, black).
manifest_color(spots, black).
manifest_color(spots, brown).
manifest_color(spots, chlorosis).
manifest_color(spots, dark_brown).
manifest_color(spots, dark_margin).
manifest_color(spots, necrotic).
manifest_color(spots, pale_green).
manifest_color(spots, purplish).
manifest_color(spots, red).
manifest_color(spots, reddish).
manifest_color(spots, tan).
manifest_color(spots, whitish_yellow).
manifest_color(spots, yellow).
manifest_color(sunken_spot, black).

% humidity_range
humidity_range(flowering_mature, 40, 50).
humidity_range(vegetative_growing, 50, 60).
humidity_range(greenhouse, 60, 80).
humidity_range(seed_germination, 90, 100).
humidity_range(succulents, 20, 30).

% monitor/4 - Reading received from sensors
% monitor(Plant, temperature(TemperatureDevice, temperature_reading), humidity(HumidityDevice, humidity_reading), caption(CaptionDevice, Section, caption_reading)).

% plant/4 - Defines the variables ranges for each plant
% plant(Plant, TemperatureMin, TemperatureMax, humidity_range).

plants(L) :- all(X, plant(X, _, _, _), L).

plant(trinidad1, 24, 28, greenhouse).
plant(sunflower, 25, 30, flowering_mature).
plant(p3, 10, 16, succulents).

% devices/4 - Associates sensors with plants
% devices(Plant, TemperatureDevice, HumidityDevice, CaptionDevice)

devices(trinidad1, t11, h12, c11).
devices(sunflower, t21, h21, c21).
devices(p3, t31, h31, c31).

% temperature_reading(Device, temperature_reading) :- ask(temperature_reading, X).

% humidity_reading(Device, humidity_reading) :- 

% caption_reading(Device, /*CaptionSection,*/ caption_reading) :- 

% temperature/2
temperature(Plant) :- 
    devices(Plant, Device, _, _),
    temperature_reading(Device, temperature_reading),
    ask(temperature_reading, X).

% humidity/2
humidity(Plant, humidity_reading) :-
    devices(Plant, _, Device, _),
    humidity_reading(Device, humidity_reading),
    ask(humidity_reading, X).

%  caption/3 - device X image of section Y reports caption Z
caption(Plant, /*CaptionSection,*/ caption_reading) :- 
    devices(Plant, _, _, Device),
    caption_reading(Device, /*CaptionSection,*/ caption_reading),
    ask(caption_reading, X).

% nutrient_deficiency/2
% nutrient_deficiency(Plant, nutrient_deficiency) :-
%     devices(Plant, _, _, Device),
%     caption(Device, /*CaptionSection,*/ caption_reading),
%     deficiency(nutrient_deficiency, /*CaptionSection,*/ caption_reading).

% dry/1 - Soil is dry if the humidity reading of the sensor is lower than the ideal range of the plant's species.
dry(Plant) :- 
    plant(Plant, _, _, HumidityRef),
    humidity_range(HumidityRef, HumidityMin, _),
    humidity(Plant, humidity_reading),
    humidity_reading < HumidityMin.

% wet/1 - Soil is wet if  the humidity reading of the sensor is higher than the ideal range of the plant's species.
wet(Plant) :-
    plant(Plant, _, _, HumidityRef),
    humidity_range(HumidityRef, _, HumidityMax),
    humidity(Plant, humidity_reading),
    humidity_reading > HumidityMax.

% cold/1 - Environment temperature is too low if  the temperature reading of the sensor is lower than the ideal range
cold(Plant) :- 
    plant(Plant, TemperatureMin, _, _),
    temperature(Plant, temperature_reading),
    temperature_reading < TemperatureMin.

% hot/1 - Environment temperature is too high if  the temperature reading of the sensor is higher than the ideal range
hot(Plant) :- 
    plant(Plant, _, TemperatureMax, _),
    temperature(Plant, temperature_reading),
    temperature_reading > TemperatureMax.

% abiotic/2
abiotic(X, dry) :- dry(X).
abiotic(X, wet) :- wet(X).
abiotic(X, cold) :- cold(X).
abiotic(X, hot) :- hot(X).
% abiotic(X, nutrient_deficiency) :- nutrient_deficiency(X, _).

% biotic/2
biotic(X, disease) :- disease(X, Y).
biotic(X, infestation) :- infestation(X, Y).

% status/2
status(X, sick) :- abiotic(X, _).
status(X, sick) :- biotic(X, _).
status(X, healthy) :- \+ status(X, sick).