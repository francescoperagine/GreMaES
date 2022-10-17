% species/3 - Name, minimum temperature, maximum temperature, avg temperature
species(capsicum_chinense, 22, 26, 24).
species(helianthus_annuus, 18, 30, 24).
species(phalaenopsis, 16, 24, 20).
species(spathiphyllum_wallisii, 18, 26, 22).
species(paphiopedilum_platyphyllum, 16, 25, 20.5).

% plant/3 Plant, species, stage.
plant(trinidad1, capsicum_chinense, greenhouse).
plant(sunflower, helianthus_annuus, flowering_mature).
plant(p1, phalaenopsis, orchid_growth_development).
plant(p2, phalaenopsis, orchid_growth_development).
plant(p3, phalaenopsis, orchid_growth_development).
plant(p4, paphiopedilum_platyphyllum, orchid_growth_development).
plant(spatafillo, spathiphyllum_wallisii, spathiphyllum).

% device/3
device(t11, temperature, trinidad1).
device(t12, temperature, sunflower).
device(t13, temperature, p1).
device(t14, temperature, p2).
device(t15, temperature, p3).
device(t16, temperature, p4).
device(t17, temperature, spatafillo).

device(h11, humidity, trinidad1).
device(h12, humidity, sunflower).
device(h13, humidity, p1).
device(h14, humidity, p2).
device(h15, humidity, p3).
device(h16, humidity, p4).
device(h17, humidity, spatafillo).

device(c11, caption, trinidad1).
device(c12, caption, sunflower).
device(c13, caption, p1).
device(c14, caption, p2).
device(c15, caption, p3).
device(c16, caption, p4).
device(c17, caption, spatafillo).

% stage/4 Stage, Min, Max, Avg
stage(flowering_mature, 40, 50, 45).
stage(vegetative_growing, 50, 60, 55).
stage(orchid_growth_development, 50, 70, 60).
stage(greenhouse, 60, 80, 75).
stage(seed_germination, 90, 100, 95).
stage(spathiphyllum, 50, 80, 65).

plant_actuator(trinidad1, act1).
plant_actuator(trinidad1, act2).
plant_actuator(trinidad1, act3).
plant_actuator(trinidad1, act4).
plant_actuator(sunflower, act1).
plant_actuator(sunflower, act2).
plant_actuator(sunflower, act3).
plant_actuator(p1, act1).
plant_actuator(p1, act2).
plant_actuator(p1, act3).
plant_actuator(p1, act4).
plant_actuator(p2, act1).
plant_actuator(p2, act2).
plant_actuator(p2, act3).
plant_actuator(p3, act3).
plant_actuator(p3, act4).
plant_actuator(p4, act4).
plant_actuator(p4, act1).
plant_actuator(p4, act2).

plant_actuator(spatafillo, act1).
plant_actuator(spatafillo, act2).

actuator(act1, temperature, low, thermostat).
actuator(act2, temperature, high, fan).
actuator(act2, humidity, low, sprinkler).
actuator(act4, humidity, high, fan).