# European Air Quality Interpolation

European environmental agency, EEA, collects the air quality sensor data and distributes it online. From that dataset, we can measure different components in the air such as the level of Nitrogen Dioxide, `NO2` in the air in any European area. Also, as this data is time bound and forms a time series, we try to do interpolation to measure whether the air quality conditions are met for a particular area.

## Getting nearby stations

The `R` script in the directory can auto-download the air quality data from the nearest stations within a user defined radius. For that, the stations data is read from `AirBase_v8_stations.csv` in the `data` folder. 

After getting the data, it them pre-processes before interpolating the results. See the comments in the code for more details