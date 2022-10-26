% actuator/4
actuator(act1, temperature, low, thermostat).
actuator(act2, temperature, high, fan).
actuator(act3, humidity, low, sprinkler).
actuator(act4, humidity, high, fan).

% plant_actuator/2
plant_actuator(trinidad1, act1).
plant_actuator(trinidad1, act3).
plant_actuator(trinidad1, act4).
plant_actuator(sunflower, act1).
plant_actuator(sunflower, act2).
plant_actuator(sunflower, act4).
plant_actuator(p1, act1).
plant_actuator(p1, act2).
plant_actuator(p1, act3).

plant_actuator(p3, act1).
plant_actuator(p3, act2).
plant_actuator(p3, act3).
plant_actuator(p3, act4).
plant_actuator(p4, act3).
plant_actuator(p4, act4).
