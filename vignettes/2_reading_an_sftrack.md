---
title: "2. Reading in an sftrack"
output:
  pdf_document: default
  html_document: default
vignette: |
  %\VignetteIndexEntry{2. Reading in an sftrack}
   %\VignetteEncoding{UTF-8}
   %\VignetteEngine{knitr::rmarkdown}
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
devtools::load_all("/home/matt/r_programs/sftrack")
#library(sftrack)

```
Creating sftrack objects is relatively straight forward and can be read in a variety of ways including as a standard `data.frame`, `sf` object, or `ltraj` object (from `adehabitatLT`).  


## Loading in raw data

To create `sftrack` objects data we use the `as_sftrack()` or `as_sftraj()` function, depending on your desired output. Both have the same arguments but differ in the way the geometry field is calculated.  

### Vector vs. data.frame inputs

`as_sftrack()/as_sftraj` accepts 2 kinds of raw data for each of the 4 required parts. Either a vector/list representing the data where length = nrow(data), or it the column name where the data exists. For any `sftrack` component you can input either vector data or the column name for any variable, and can mix types between arguments.

**Vector** inputs to `as_sftrack` in general involve feeding as_sftrack the data itself where length(vector) == nrow(data). Or a list where each component adheres to this rule. If using entirely vector inputs for burst, geometry, time, and error then `data` is not required. 

**data.frame** inputs on the other hand are simply character vectors describing the column name in `data` where the appropriate data is found.

### Arguments 

**data** - is a data.frame containing your data. At present we are reserving 'burst' as a column name, so data will be overwritten if this column name exists. Data is optional if all inputs are done in vector mode where the appropriate vectors are given for geometry, burst, and time.   

**burst** (required) - a list with named vectors to group the sftrack. One group must be named `id`, but otherwise can be infinite number of grouping variables. Or a vector naming the column names for each burst category.     

**coords** (required) - The x,y,z coordinates to calculate geometries from. Accepts either a vector of `c(x,y,z)` describing which column the coordinates can be found, or a list(x=, y=, z=) with vectors for each coordinate. z is optional. NAs are allowed, alhough NAs must exist through the entire row otherwise an error is thrown.  

**time** (required) - Time information in either `POSIXct` or as an `integer`. Accepts either a vector of time, or the column name found in `data`. The outputed object will be sorted by the time column.  

**error** - Error information for the associated xyz point. Accepts either a vector of the error, or the column name found in `data`. If not given, default = NA.

**crs** - the coordinate references system/projection of the data, as implemented by rgdal. see ?rgdal::`CRS-class` for more information. If none is supplied crs is set as NA and can be set later using `sf::st_crs()` from `sf`.  

**active_burst** (required) - This is a vector containing what bursts are 'active'. Meaning calculations and graphing will be grouped by these bursts. If no value is supplied it defaults to all bursts. Can change active_burst later using `active_burst()<-'myvalue'`. 


### Vector inputs

In the case of vector inputs, the vectors are cbinded to `data` if data is supplied. Therefore you may experience duplicate columns if you did not subset appropriately. Column names are created for that vector data that was supplied where burst = 'burst', time = 'reloc_time', error = 'sftrack_error'. Geometry = 'Geometry' regardless of the input type.


```{r}
raccoon_data <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))

#data
data = raccoon_data
#xyz
coords = data[,c('longitude','latitude')]
crs = '+init=epsg:4326'
#bursts
burst = list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon+1)
active_burst = c('id','month')
#time
time = as.POSIXct(raccoon_data$acquisition_time, tz='EST')
#error
error = data$fix
my_sftrack <- as_sftrack(data = data, coords = coords, burst = burst, 
                         active_burst = active_burst, time = time, 
                         crs = crs, error = error)

head(my_sftrack)

```

As you can see in this case the data is not overwritten, but extra columns added with the correct data.

-------------

### data.frame inputs  

Data.frame inputs generally describe the columns in the data that represent each field. If the columns are not found in the data, an error is thrown. 


```{r}
data$time <- as.POSIXct(data$acquisition_time, tz='EST')
data$month <- as.POSIXlt(data$acquisition_time)$mon+1

coords = c('longitude','latitude')
burst = c(id = 'sensor_code', month = 'month')
time = 'time'
error = 'fix'

my_sftraj <- as_sftraj(data = data, coords = coords, burst = burst, time = time, error = error)

head(my_sftraj)
```

## Conversion mode

`as_sftrack()` and `as_sftraj()` also accept other data types, and the arguments differ depending on the class. It currenly accepts, `sf`, `ltraj`, and eventually `tibbles`. 

### Import from ltraj
To read in an ltraj object all you need is an ltraj object created in `adehabitatLT`. All relevant information is taken from the object. The burst as defined in an ltraj is slightly different than in an sftrack, so it assumes the ltraj 'burst' is the `id` field of the sftrack object.

```{r, message = FALSE}
library(adehabitatLT)

ltraj_df <- as.ltraj(xy=raccoon_data[,c('longitude','latitude')], date = as.POSIXct(raccoon_data$acquisition_time),
 id = raccoon_data$sensor_code, typeII = TRUE,
 infolocs = raccoon_data[,1:6] )

my_sf <- as_sftrack(ltraj_df)
head(my_sf)


```

### sf objects
`sf` objects are handled similiarly to the standard way you input raw data, except you do not need to input any information about the coordinates or projection. Burst and time are still required.

```{r}
library(sf)
df1 <- data[!is.na(raccoon_data$latitude),]
sf_df <- st_as_sf(df1, coords=c('longitude','latitude'), crs = crs)
burst = c(id = 'sensor_code')
time_col = 'time'

new_sftraj <- as_sftraj(sf_df,burst = burst, time = time_col) 
head(new_sftraj)

new_sftrack <- as_sftrack(sf_df,burst = burst, time= time_col) 
head(new_sftrack)

```

### Inter-class conversion
Additionally `as_sftrack` and `as_sftraj` can convert back and forth between each other with no loss in information.

```{r}
# Make tracks from raw data
coords = c('longitude','latitude')
burst = c(id = 'sensor_code', month = 'month')
time = 'time'
error = 'fix'

my_sftraj <- as_sftraj(data = data, coords = coords, burst = burst, time = time, error = error)
my_sftrack <- as_sftrack(data = data, coords = coords, burst = burst, time = time, error = error)

# Convert between types
new_sftrack <- as_sftrack(my_sftraj)
#head(new_sftrack)
new_sftraj <- as_sftraj(my_sftrack)
#head(new_sftraj)

identical(my_sftraj,new_sftraj)
identical(my_sftrack,new_sftrack)
```
### Duplicated data
A common issue with movement data is when duplicated gps time stamps are logged for a sensor. When this happens it can be impossible for `sftrack` know which point to use. For this reason, sftrack returns and error if any `burst` + `time` combinations are duplicated. 


```{r}

data$time[1] <- data$time[2]
 try(as_sftrack(data = data, coords = coords, burst = burst, time = time, error = error))

```
To help determine which rows are duplicated you can use the `which_duplicated` function to check your inputs. After which you can delete the superfluous rows and try again:

```{r}
which_duplicated(data = data , burst = burst, time = time)
data <- data[-2,]
my_sftrack <- as_sftrack(data = data, coords = coords, burst = burst, time = time, error = error)

```
