% plant_sensor/3 - plant,sensorID,metric
plant_sensor(trinidad1,t11,temperature).
plant_sensor(sunflower,t12,temperature).
plant_sensor(p1,t13,temperature).
plant_sensor(p2,t14,temperature).
plant_sensor(p3,t15,temperature).
plant_sensor(p4,t16,temperature).
plant_sensor(spatafillo,t17,temperature).

plant_sensor(trinidad1,h11,humidity).
plant_sensor(sunflower,h12,humidity).
plant_sensor(p1,h13,humidity).
plant_sensor(p2,h14,humidity).
plant_sensor(p3,h15,humidity).
plant_sensor(p4,h16,humidity).
plant_sensor(spatafillo,h17,humidity).
plant_sensor(trinidad1,c11,caption).
plant_sensor(sunflower,c12,caption).
plant_sensor(p1,c13,caption).
plant_sensor(p2,c14,caption).
plant_sensor(p3,c15,caption).
plant_sensor(p4,c16,caption).
plant_sensor(spatafillo,c17,caption).

% plant_actuator/4 - plant,actuatorID,metric,usage,type
plant_actuator(trinidad1,thermostat1,temperature,cold).
plant_actuator(trinidad1,fan2,temperature,hot).
plant_actuator(trinidad1,sprinkler3,humidity,dry).
plant_actuator(trinidad1,fan4,humidity,wet).
plant_actuator(sunflower,thermostat5,temperature,cold).
plant_actuator(sunflower,fan6,temperature,hot).
plant_actuator(sunflower,sprinkler7,humidity,dry).
plant_actuator(sunflower,fan8,humidity,wet).
plant_actuator(p1,thermostat9,temperature,cold).
plant_actuator(p1,fan10,temperature,hot).
plant_actuator(p3,sprinkler11,humidity,dry,sprinkler).
plant_actuator(p3,fan12,humidity,wet).
plant_actuator(spatafillo,fan13,temperature,hot).
plant_actuator(spatafillo,sprinkler14,humidity,dry).
