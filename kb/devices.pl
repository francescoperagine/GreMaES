% sensor/2
sensor(t11,temperature).
sensor(t12,temperature).
sensor(t13,temperature).
sensor(t14,temperature).
sensor(t15,temperature).
sensor(t16,temperature).
sensor(t17,temperature).

sensor(h11,humidity).
sensor(h12,humidity).
sensor(h13,humidity).
sensor(h14,humidity).
sensor(h15,humidity).
sensor(h16,humidity).
sensor(h17,humidity).

sensor(c11,caption).
sensor(c12,caption).
sensor(c13,caption).
sensor(c14,caption).
sensor(c15,caption).
sensor(c16,caption).
sensor(c17,caption).

% plant_sensor/2
plant_sensor(trinidad1,t11).
plant_sensor(sunflower,t12).
plant_sensor(p1,t13).
plant_sensor(p2,t14).
plant_sensor(p3,t15).
plant_sensor(p4,t16).
plant_sensor(spatafillo,t17).

plant_sensor(trinidad1,h11).
plant_sensor(sunflower,h12).
plant_sensor(p1,h13).
plant_sensor(p2,h14).
plant_sensor(p3,h15).
plant_sensor(p4,h16).
plant_sensor(spatafillo,h17).
plant_sensor(trinidad1,c11).
plant_sensor(sunflower,c12).
plant_sensor(p1,c13).
plant_sensor(p2,c14).
plant_sensor(p3,c15).
plant_sensor(p4,c16).
plant_sensor(spatafillo,c17).

% actuator/4
% ActuatorID,SensorType,Activation,Class
actuator(act1,temperature,low,thermostat).
actuator(act2,temperature,high,fan).
actuator(act3,humidity,low,sprinkler).
actuator(act4,humidity,high,fan).

% plant_actuator/2
plant_actuator(trinidad1,act1).
plant_actuator(trinidad1,act2).
plant_actuator(trinidad1,act3).
plant_actuator(trinidad1,act4).
plant_actuator(sunflower,act1).
plant_actuator(sunflower,act2).
plant_actuator(sunflower,act3).
plant_actuator(sunflower,act4).
plant_actuator(spatafillo,act1).
plant_actuator(spatafillo,act2).
plant_actuator(spatafillo,act3).
plant_actuator(spatafillo,act4).

plant_actuator(p1,act1).
plant_actuator(p1,act2).
plant_actuator(p1,act3).
plant_actuator(p1,act4).

plant_actuator(p3,act1).
plant_actuator(p3,act2).
plant_actuator(p3,act3).
plant_actuator(p3,act4).
plant_actuator(p4,act3).
plant_actuator(p4,act4).
