% health_problem/1
health_problem(biotic_problem).
health_problem(abiotic_problem).

% section/1
section(all).
section(branches).
section(shoot).
section(leaves).
section(lower_leaves).
section(upper_leaves).
section(upper_side_leaves).
section(lower_side_leaves).
section(upper_side_lower_leaves).
section(roots).
section(stem).
section(buds).
section(fruits).
section(bark).

% appearance/1
appearance(altered_color, [lower_leaves, leaves, upper_leaves, all, roots, branches]).
appearance(deformed, [leaves, all, buds]).
appearance(angular_lesions, [leaves]).
appearance(circular_lesions, [leaves]).
appearance(irregular_lesions, [leaves]).
appearance(necrotic_lesions, [leaves]).
appearance(mottled, [leaves]).
appearance(streaking, [leaves]).

appearance(parachute, [leaves]).
appearance(thin_size, [leaves]).
appearance(small_size, [leaves]).
appearance(large_spots, [leaves, fruits]).

appearance(poor_quality, [fruits]).
appearance(poor_quantity, [leaves, fruits]).
appearance(visible_spores, [leaves]).

appearance(small_holes, [leaves]).
appearance(spots, [leaves, upper_side_lower_leaves, fruits, stem]).
appearance(shot_holes, [leaves]).
appearance(sunken_spot, [fruits]).
appearance(leathery_spot, [fruits]).
appearance(dead_patches, [bark]).


appearance(white_powdery_substance, [all]).
appearance(cotton_like_downy_substance, [lower_side_leaves]).
appearance(grayish_fuzzy_looking_spores, [lower_side_leaves]).
appearance(water_soaked_spots, [stem, flower, lower_leaves]).
appearance(clear_gummy_substance, [fruits]).
appearance(irregular_water_soaked_blotches, [lower_leaves, stem, fruits]).
appearance(dying, [branches]).
appearance(sticky_honeydew, [all]).
appearance(husks, [soil]).

appearance(sticky_webbing, [all]).
appearance(white_flies, [leaves]).
appearance(dry, [all, leaves]).
appearance(cutworms_under_leaves, [leaves]).
appearance(cutworms_under_debris, [all]).
appearance(small_black_flies, [all]).
appearance(clustering_cottony, [lower_side_leaves]).

% color/1
color(angular_chlorosis).
color(blotchy_chlorosis).
color(interveinal_chlorosis).
color(rounded_chlorosis).
color(spots_chlorosis).
color(chlorosis).
color(random_chlorosis).

color(bronze).
color(brown).
color(brown_discoloration).
color(black).
color(chlorotic).
color(dark_green).
color(dark_brown).
color(dark_margin).
color(dark_veins).
color(greysh).
color(greenish_black).
color(necrotic).
color(pale_green).
color(reddish_purple).
color(red).
color(purplish).
color(tan).
color(white).
color(whitish_yellow).
color(yellowish_outline).

% behaviour/1
behaviour(early_fall, [leaves]).
behaviour(death_of_growing_point, [shoot]).
behaviour(death_of_plant_tissue, [all]).
behaviour(fail_to_form, [buds]).
behaviour(wilting_margins, [leaves]).
behaviour(withering, [leaves, all]).
behaviour(spots_drop_out, [leaves]).

behaviour(smell_strange, [root]).

behaviour(stunted_growth, [all, stem, leaves, shoot, upper_leaves]).
behaviour(increased_growth, [roots]).
behaviour(delayed_growth, [all]).
behaviour(excessive_tillering_growth, [all]).
behaviour(highly_branched_growth, [roots]).

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