% plant/3 Plant, species, stage.
plant(trinidad1, capsicum_chinense, greenhouse).
plant(sunflower, helianthus_annuus, flowering_mature).
plant(p1, phalaenopsis, orchid_growth_development).
plant(p2, phalaenopsis, orchid_growth_development).
plant(p3, phalaenopsis, orchid_growth_development).
plant(p4, paphiopedilum_platyphyllum, orchid_growth_development).
plant(spatafillo, spathiphyllum_wallisii, spathiphyllum).

% species/3 - Name, minimum temperature, maximum temperature, avg temperature
species(capsicum_chinense, 22, 26, 24).
species(helianthus_annuus, 18, 30, 24).
species(phalaenopsis, 16, 24, 20).
species(spathiphyllum_wallisii, 18, 26, 22).
species(paphiopedilum_platyphyllum, 16, 25, 20.5).

% stage/4 Stage, Min, Max, Avg
stage(flowering_mature, 40, 50, 45).
stage(vegetative_growing, 50, 60, 55).
stage(orchid_growth_development, 50, 70, 60).
stage(greenhouse, 60, 80, 75).
stage(seed_germination, 90, 100, 95).
stage(spathiphyllum, 50, 80, 65).