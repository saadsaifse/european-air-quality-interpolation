## Getting Nearby Stations

library(sf)
radius = 150000 # Define radius to get stations in
pointLngLat = c(7.6, 52) # Define Longitude and Latitude of the desired point
stations = read.csv("AirBase_v8_stations.csv", sep = "\t", stringsAsFactors = FALSE)
stations.sf = st_as_sf(stations, coords = c("station_longitude_deg", "station_latitude_deg"), crs = 4326)
point.sfc = st_sfc(st_point(pointLngLat), crs = 4326) # Center point with 4326 EPSG
distances = st_distance(stations.sf, point.sfc) # All distances from the stations
sel = as.numeric(distances) < radius
stationsInRadius = stations.sf[sel,]
stationsInRadius

# automatically download data

downloadData = T

if (downloadData){
  
library(urltools)
library(httr)

# TODO: make loops parallel
# library(doParallel)

requestTimeoutInSec = 5
pollutantCode = 8 # 8 is the pollutant code for NO2
fromYear = 2017
tillYear = 2017
source = "E1a"
restrictCountryDataTo = "DE"

#stationsInRadius = stations[ stations$country_iso_code == "DE", ]

for (i in c(1:(length(stationsInRadius[[1]])))){
  print(paste0("In iteration", i))
  countryCode = stationsInRadius[i,]$country_iso_code
  cityName = stationsInRadius[i,]$station_city
  
  if (is.na(countryCode) || countryCode == "" || is.na(cityName) || cityName == ""){
    print("City or country was not found, skipping..")
    next
  }
  if (restrictCountryDataTo != ""){
    countryCode = restrictCountryDataTo
  }
  result = NA
  getOperation = try({
    
    request = GET("https://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw",
              query = list(CountryCode = countryCode,
                           CityName = cityName,
                           Pollutant=pollutantCode,
                           Year_from=fromYear,
                           Year_to=tillYear,
                           Source=source,
                           Output="TEXT"), 
              timeout(requestTimeoutInSec),
              verbose())
    
    if (status_code(request) != 200){
      print("Request failed")
      stop("Error")
    }
    
    result = httr::content(request, encoding = "UTF-8")
  })
  
  if (inherits(getOperation, "try-error")){
    print(paste0("For city ", cityName))
    print("Status code was not 200, skipping..")
    next
  }
  
  library(XML)
  parsed = xmlToList(xmlParse(result)) # Parse HTML document
  links = parsed$body$p;
  links = strsplit(links, split = "\n");
    
  for (i in c(1:length(links[[1]]))){
    link = links[[1]][i]
    print(paste0("Link is", link));
    data = NA
    getOperation = try({
      linkPath = url_parse(link)$path
      fileName = tail(strsplit(linkPath, "/")[[1]], n=1)
      download.file(link, destfile = fileName)
      print(paste0("For city ", cityName))
      print(paste0("Wrote file named ", fileName))
    })
    
    if (inherits(getOperation, "try-error")){
      print(paste0("For city ", cityName))
      print("File was not downloaded, skipping..")
      next
    }
  }
}
}
```

## Pre-processing the Dataset

Read Files

```{r}
files = list.files(".", pattern = "DE_5*", full.names = TRUE)
data = lapply(files, function(x)read.csv(x, fileEncoding = "UTF-16", header = TRUE, sep = ","))
print(paste0("Number of files read: ", length(files)))
```

Convert DatetimeBegin to POSIXct timr from factor

```{r}
Sys.setenv(TZ = "UTC") # make sure times are not interpreted as DST
data = lapply(data, function(f) {
        f$t = as.POSIXct(f$DatetimeBegin)
        f[order(f$t), ] 
    })
```

Deselect smaller datasets that do not contain hourly data

```{r}
data = data[sapply(data, nrow) > 1000]
names(data) =  sapply(data, function(f) unique(f$AirQualityStationEoICode))
length(data) == length(unique(names(data)))
```

Combine all files using `xts::cbind`, so that they are matched based on time

```{r}
library(xts)
data = lapply(data, function(f) xts(f$Concentration, f$t))
aq = do.call(cbind, data)
```

Remove stations with more than 75% missing values:

```{r}
sel = apply(aq, 2, function(x) sum(is.na(x)) < 0.75 * 365 * 24)
aqsel = aq[, sel] # stations are in columns
```

Filter urban stations

```{r}
library(tidyverse)
read.csv("AirBase_v8_stations.csv", sep = "\t", stringsAsFactors = FALSE) %>%
  as_tibble %>% 
  filter(station_type_of_area == "urban", type_of_station == "Background") -> backgroundAndUrban

library(stars)
backgroundAndUrban.sf = st_as_sf(backgroundAndUrban, coords = c("station_longitude_deg", "station_latitude_deg"), crs = 4326)
```

Filter our selected stations as well

```{r}
sel =  colnames(aqsel) %in% backgroundAndUrban$station_european_code
aqsel = aqsel[,sel]

if (is.null(colnames(aqsel))){
  stop("No stations match urban and background stations in the processed stations list")
}
```


Compute station means and show them on a map

```{r}
tb = tibble(NO2 = apply(aqsel, 2, mean, na.rm = TRUE), station_european_code = colnames(aqsel))
crs = 32632
right_join(backgroundAndUrban.sf, tb) %>% st_transform(crs) -> no2.sf

# load German boundaries
data(air, package = "spacetime")
de <- st_transform(st_as_sf(DE_NUTS1), crs)
ggplot() + geom_sf(data = de) +  geom_sf(data = no2.sf, mapping = aes(col = NO2))
```


## Sample Variogram

To identify a model for the mean and for the spatial correlations. Here we use the `variogram` method from the `gstat` package.
The formula `NO2~1` is used to select the variable of interest from the data file (NO2), and to specify the mean model: `~1` refers to an intercept-only (unknown, constant mean) model.

``` {r}
# Sample Variogram

library(gstat)
v = variogram(NO2~1, no2.sf)
plot(v, plot.numbers = TRUE)

```

This chooses default maximum distance (`cutoff`: one third of the length of the bounding box diagonal) and (constant) interval widths (`width`: cutoff divided by 15). These defaults can be changed, e.g. by

```{r}
# With custom cutoffs
v0 = variogram(NO2~1, no2.sf, cutoff = 100000, width = 10000)
plot(v0, plot.numbers = TRUE)
```

### Fitting Variogram Models

In order to progress toward spatial predictions, we need a variogram model for (potentially) all distances rather than the set of estimates derived above. We can fit a model to estimate all distance values. Feel free to change `vgm` parameters. Here we used 30000 becuase that best fits the data in our case.

```{r}
v.m = fit.variogram(v, vgm(1, "Exp", 30000, 1))
plot(v, v.m, plot.numbers = TRUE)
```


## Kirging Interpolation

Kriging involves the prediction of a phenomenon at arbitrary locations. Typically, when we interpolate a variable, we do that on points on a regular grid covering the target area. We first create a stars object with a raster covering the target area, and NA’s outside it:

```{r}
# build a grid over Germany:
st_bbox(de) %>%
  st_as_stars(dx = 10000) %>%
  st_set_crs(crs) %>%
  st_crop(de) -> grd
grd

k = krige(NO2~1, no2.sf, grd, v.m)
#> [using ordinary kriging]
ggplot() + geom_stars(data = k, aes(fill = var1.pred, x = x, y = y)) + 
    geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
    geom_sf(data = no2.sf)

```

## Spatio-temporal Interpolation

Here we do the interpolation on the urban stations data gathered above

```{r}
aqx = aq[,colnames(aq) %in% backgroundAndUrban$station_european_code]
sfc = st_geometry(backgroundAndUrban.sf)[match(colnames(aqx), backgroundAndUrban.sf$station_european_code)]
st_as_stars(NO2 = as.matrix(aqx)) %>%
    st_set_dimensions(names = c("time", "station")) %>%
    st_set_dimensions("time", index(aqx)) %>%
    st_set_dimensions("station", sfc) -> no2.st
v.st = variogramST(NO2~1, no2.st[,1:(24*31)], tlags = 0:48)

v1 = plot(v.st)
v2 = plot(v.st, map = FALSE)
print(v1, split = c(1,1,2,1), more = TRUE)
print(v2, split = c(2,1,2,1), more = FALSE)

# product-sum
prodSumModel <- vgmST("productSum",
    space=vgm(150, "Exp", 200, 0),
    time= vgm(20, "Sph",   40, 0),
    k=2)
StAni = estiStAni(v.st, c(0,200000))
(fitProdSumModel <- fit.StVariogram(v.st, prodSumModel, fit.method = 7,
    stAni = StAni, method = "L-BFGS-B",
    control = list(parscale = c(1,10,1,1,0.1,1,10)),
    lower = rep(0.0001, 7)))
plot(v.st, fitProdSumModel, wireframe=FALSE, all=TRUE, scales=list(arrows=FALSE), zlim=c(0,150))

plot(v.st, model=fitProdSumModel, wireframe=TRUE, all=TRUE, scales=list(arrows=FALSE), zlim=c(0,185))
```


The below code is giving error `Error in make.unique(row.names(sp)) : 'names' must be a character vector`. We tried multiple ways to fix this but could not. Probably an implementation error of the `KrigeST` function.

```{r}
set.seed(123)
pt = st_sample(de, 2)
t = st_get_dimension_values(no2.st, 1)
st_as_stars(list(pts = matrix(1, length(t), length(pt)))) %>%
    st_set_dimensions(names = c("time", "station")) %>%
    st_set_dimensions("time", t) %>%
    st_set_dimensions("station", pt) -> new_pt

new_pt = st_transform(new_pt, crs)
no2.st <- st_transform(no2.st, crs)

data = no2.st["NO2"]

# error in below line
#new_ts <- krigeST(NO2~1, data = data, newdata = new_pt,
#         nmax = 50, stAni = StAni, modelList = fitProdSumModel,
#         progress = FALSE)
#plot(xts(t(new_ts[[2]]), t), type = 'l')
```


## References
https://keen-swartz-3146c4.netlify.com/interpolation.html