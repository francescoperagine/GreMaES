% plant/4 - Defines the variables ranges for each plant
% plant(Plant, TemperatureMin, TemperatureMax, humidity_range).
plant(trinidad1, 24, 28, greenhouse).
plant(sunflower, 25, 30, flowering_mature).
plant(p3, 10, 16, succulents).

% device/4 - Associates sensors with plants
% device(Plant, TemperatureDevice, HumidityDevice, CaptionDevice)

device(trinidad1, t11, h12, c11).
device(sunflower, t21, h21, c21).
device(p3, t31, h31, c31).

sensor_class(caption).
sensor_class(humidity).
sensor_class(temperature).

% humidity_range
humidity_range(flowering_mature, 40, 50).
humidity_range(vegetative_growing, 50, 60).
humidity_range(greenhouse, 60, 80).
humidity_range(seed_germination, 90, 100).
humidity_range(succulents, 20, 30).