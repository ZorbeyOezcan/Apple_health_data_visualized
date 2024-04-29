Data_import_and_wrangling
================
Zorbey Özcan
2024-04-22

# Introduction

Like many other people, recently I started monitoring my daily step
count and paying more attention to its trends. I just found out, that
you can easily export the data from the iPhone and import it to R, using
the AppleHealthAnalysis package.

So, I decided to export my Apple Health data from my iPhone, which I
have been using for many years now. The imported data dates back to July
2019 and ends in April 2024. In this analysis, I want to focus on my
walking behavior analyze and visualize it, simply for fun.

In the following parts I document my process step by step.

# Importing the data

``` r
# Laoding packages 

library(devtools)
```

    ## Loading required package: usethis

    ## Warning: package 'usethis' was built under R version 4.2.3

``` r
library(tidyverse)
```

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## Warning: package 'tidyr' was built under R version 4.2.3

    ## Warning: package 'readr' was built under R version 4.2.3

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## Warning: package 'stringr' was built under R version 4.2.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(lubridate)
library(readr)
library(devtools)
library(AppleHealthAnalysis)

# Importing with AppleHealthAnalysis
health_data <- ah_import_xml("/Users/zorbeyozcan/Desktop/apple_health/Apple_health_data/apple_health_export/Export.xml")
```

    ## File size: 138.9 MiB
    ## Estimated import time at 5MiB/s: 28 seconds
    ## Import started at: 2024-04-29 13:46:34

    ## Warning in
    ## ah_import_xml("/Users/zorbeyozcan/Desktop/apple_health/Apple_health_data/apple_health_export/Export.xml"):
    ## NAs introduced by coercion

    ## Import finished at: 2024-04-29 13:46:52
    ## Time for import: 18 seconds
    ## File size/running time: 7.9 MiB per second

# Exploring the data set

## What data do I need?

In this analysis, I focus on my walking behavior. Any data not
correlated to walking is removed in this step, forming a new data frame.

``` r
# Checking what types of data this data frame contains: 
type_values <- unique(health_data$type)
print(type_values)
```

    ##  [1] Height                         BodyMass                      
    ##  [3] StepCount                      DistanceWalkingRunning        
    ##  [5] BasalEnergyBurned              ActiveEnergyBurned            
    ##  [7] FlightsClimbed                 HeadphoneAudioExposure        
    ##  [9] WalkingDoubleSupportPercentage WalkingSpeed                  
    ## [11] WalkingStepLength              WalkingAsymmetryPercentage    
    ## [13] HKDataTypeSleepDurationGoal    AppleWalkingSteadiness        
    ## [15] SleepAnalysis                  HeadphoneAudioExposureEvent   
    ## 16 Levels: ActiveEnergyBurned AppleWalkingSteadiness ... WalkingStepLength

``` r
## The relevant data is: 

# StepCount 
# DistanceWalkingRunnig 
# FlightsClimbed 
# WalkingDoubleSupportPercantage
# WalkingSpeed
# WalkingStepLength 
# WalkingAsymmetryPercantage 
```

## What does the data mean?

The seven identified types of data are all related to my walking
behavior Most of them are self-explanatory, but others might need some
clarification. The data is gathered through a combination of the inbuilt
accelerometer, barometer, 3-axis rotation rate sensor, compass and the
GPS tracking data.

Note: The iPhone only collects the data, when I am carrying my phone
with me and on me. I do not own an Apple Watch. This means, the data is
naturally missing inputs.

### StepCount

The total steps my phone has recorded, ordered by date and time of day.
Measured in absolute numbers.

### DistanceWalkingRunnig

The total distance I walked or ran, ordered by date and time of day.
Measured in meters.

### FlightsClimbed

The total floghts I climbed, ordered by date and time of day. Measured
in absolute numbers. One flight consists of approximately 16 stairs or
three meters.

### WalkingDoubleSupportPercantage

The percentage of time, where both feet are steady on the ground
(effectively percentage of time standing on both feet). Measured in
percentages.

### WalkingSpeed

The average walking speed, ordered by date and time of day. Measured in
kilometers per hour (Km/h).

### WalkingStepLength

The average length of one step, ordered by date and time of day.
Measured in centimeters.

### WalkingAsymmetryPercantage

The asymmetry of the left and right leg in my walking pattern.

# Cleaning the data according to my needs.

## Creating a new data frame with only the relevant data.

``` r
# Creating a vector with the relevant data.

walking_data <-  c("StepCount", "DistanceWalkingRunning", "FlightsClimbed",
                   "WalkingDoubleSupportPercentage", "WalkingSpeed", 
                   "WalkingStepLength", "WalkingAsymmetryPercentage")

# Applying the vector to form a new data frame.
apple_walking_data <- health_data[health_data$type %in% walking_data, ]
```

\##Cleaning up the data frame

On further inspection, some of the columns of variables in the data
frame are irrelevant to this analysis. For sake of simplicity, those are
removed too, but without creating a new data frame. Also the order of
the columns is changed, for the same reason.

``` r
# Creating a vector with the irrelevant columns.
colnames_removed <- c("sourceName", "endDate", "month_name")

# Applying the vector to form a cleaned data frame.
apple_walking_data[, colnames_removed] <- NULL

# Creating a vector with the right order of columns.
col_order <- c("type","date","year","month","day","day_name","hour","minutes","unit","value")

# Applying the vector to form a data frame with the right order of columns. 
apple_walking_data <- apple_walking_data[, col_order]
```

# Treatment of missing values.

Now I am confronted with a problem, that most data analysts have: what
to do with missing values? To approach this issue, I decided to check
for missing values first and find out how much data is missing in every
column. After that, I will decide what to do with it.

``` r
# Checking every column and calculate percentages of missing value. 
missing_values_percentages_per_column <- sapply(apple_walking_data, function(x){
  sum(is.na(x)) / length(x)* 100
})

print(missing_values_percentages_per_column)
```

    ##     type     date     year    month      day day_name     hour  minutes 
    ##        0        0        0        0        0        0        0        0 
    ##     unit    value 
    ##        0        0

``` r
## No missing values.
```

The data set has no missing values.

But there is one case stands out: Only one day in 2018 was recorded, the
rest of the data is starting on July 4th 2019. I will remove this case.
Also the day of exporting is removed, because this day is only recorded
until 3 pm.

``` r
# Deleting the faulty cases. 
apple_walking_data$date <- as.Date(apple_walking_data$date)

faulty_dates <- as.Date( c ("2018-08-06", "2024-04-15"))

apple_walking_data <- apple_walking_data %>%
  filter(!date %in% faulty_dates)
```

# Creating separate data frames for every type, which lists the data only by day.

We are now left with a data frame, that contains my walking data, which
is not only displayed by date, but also by the hour. For my first,
simple analysis, I want to display the data only by day. For that
reason, I will create a new data frame, containing values displayed by
day by summing up the values of each day. Because the data is calculated
in different units, the data is combined separately for every type. An
additional reason for this, is that the data is accumulated in different
time zones. When exported, the current time zone is used to format the
data which can falsify the data. I am choosing to ignore this, because
in the respecting time frame, I very sparsely traveled to regions with a
time zone difference of more than two hours. Further analysis can
correlate the walking data with my travel destinations.

``` r
# Combining the values of each day: StepCount
StepCount_by_day <- apple_walking_data %>%
  filter( type == "StepCount" ) %>%
  group_by( type, date, year, month, day, day_name, unit) %>%
  summarize( StepCountDaily = sum(value))
```

    ## `summarise()` has grouped output by 'type', 'date', 'year', 'month', 'day',
    ## 'day_name'. You can override using the `.groups` argument.

``` r
# Combining the values of each day: FlightsClimbed
FlightsClimbed_by_day <- apple_walking_data %>%
  filter( type == "FlightsClimbed" ) %>%
  group_by( type, date, year, month, day, day_name, unit) %>%
  summarize( FlightsClimbedDaily = sum(value))
```

    ## `summarise()` has grouped output by 'type', 'date', 'year', 'month', 'day',
    ## 'day_name'. You can override using the `.groups` argument.

``` r
# Combining the values of each day and changing the unit to meters: DistanceWalkingRunning
DistanceWalkingRunning_by_day <- apple_walking_data %>%
  filter( type == "DistanceWalkingRunning" ) %>%
  group_by( type, date, year, month, day, day_name, unit) %>%
  summarize( DistanceWalkingRunningDaily = ceiling( sum(value) * 1000 )) %>%
  mutate(unit = "meters")
```

    ## `summarise()` has grouped output by 'type', 'date', 'year', 'month', 'day',
    ## 'day_name'. You can override using the `.groups` argument.

``` r
# Combining the values of each day and calculating the mean: WalkingDoubleSupportPercentage
WalkingDoubleSupportPercentage_by_day <- apple_walking_data %>%
  filter( type == "WalkingDoubleSupportPercentage" ) %>%
  group_by( type, date, year, month, day, day_name, unit) %>%
  summarize( WalkingDoubleSupportPercentageDaily = mean(value))
```

    ## `summarise()` has grouped output by 'type', 'date', 'year', 'month', 'day',
    ## 'day_name'. You can override using the `.groups` argument.

``` r
# Combining the values of each day and calculating the mean: WalkingSpeed
WalkingSpeed_by_day <- apple_walking_data %>%
  filter( type == "WalkingSpeed" ) %>%
  group_by( type, date, year, month, day, day_name, unit) %>%
  summarize( WalkingSpeedDaily = mean(value))
```

    ## `summarise()` has grouped output by 'type', 'date', 'year', 'month', 'day',
    ## 'day_name'. You can override using the `.groups` argument.

``` r
# Combining the values of each day and calculating the mean: WalkingStepLength
WalkingStepLength_by_day <- apple_walking_data %>%
  filter( type == "WalkingStepLength" ) %>%
  group_by( type, date, year, month, day, day_name, unit) %>%
  summarize( WalkingStepLengthDaily = mean(value))
```

    ## `summarise()` has grouped output by 'type', 'date', 'year', 'month', 'day',
    ## 'day_name'. You can override using the `.groups` argument.

``` r
# Combining the values of each day and calculating the mean: WalkingAsymmetryPercentage 
WalkingAsymmetryPercentage_by_day <- apple_walking_data %>%
  filter( type == "WalkingAsymmetryPercentage" ) %>%
  group_by( type, date, year, month, day, day_name, unit) %>%
  summarize( WalkingAsymmetryPercentageDaily = mean(value))
```

    ## `summarise()` has grouped output by 'type', 'date', 'year', 'month', 'day',
    ## 'day_name'. You can override using the `.groups` argument.

# Exporting the data for further analysis

In this last step of wrangling, I am creating rds files for each data
frame and cleaning up the environment

``` r
# Creating a list for all data frames.
all_dataframes <- list(StepCount_by_day = StepCount_by_day, 
                   FlightsClimbed_by_day = FlightsClimbed_by_day,
                   DistanceWalkingRunning_by_day = DistanceWalkingRunning_by_day,
                   WalkingDoubleSupportPercentage_by_day = WalkingDoubleSupportPercentage_by_day,
                   WalkingSpeed_by_day = WalkingSpeed_by_day,
                   WalkingStepLength_by_day = WalkingStepLength_by_day, 
                   WalkingAsymmetryPercentage_by_day = WalkingAsymmetryPercentage_by_day
                   ) 

# Saving all data frames as rds files.
for (name in names (all_dataframes)) {
  saveRDS(all_dataframes[[name]], file = paste0("/Users/zorbeyozcan/Desktop/apple_health/Apple_health_data", name, ".rds"))
}

# Cleaning up unused values and data frames.
rm(col_order, faulty_dates, colnames_removed, type_values, name, missing_values_percentages_per_column, walking_data)

rm(health_data, all_dataframes, apple_walking_data)
```
