% species/3 - Name, minimum temperature, maximum temperature
species(capsicum_chinense, 22, 26).
species(helianthus_annuus, 18, 30).
species(phalaenopsis, 16, 24).
species(spathiphyllum_wallisii, 18, 26).
species(paphiopedilum_platyphyllum, 16, 25).

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

% stage/3
stage(flowering_mature, 40, 50).
stage(vegetative_growing, 50, 60).
stage(orchid_growth_development, 50, 70).
stage(greenhouse, 60, 80).
stage(seed_germination, 90, 100).
stage(spathiphyllum, 50, 80).