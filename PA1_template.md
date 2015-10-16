# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(data.table)
library(dplyr)
library(ggplot2)
library(lattice)

unzip("activity.zip")
dt <- data.table(read.csv("activity.csv"))
dt$date <- as.Date(dt$date, "%Y-%m-%d")
```



## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
