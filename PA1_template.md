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


```r
dt %>% select(date, steps) %>%
    group_by(date) %>%
    summarise(nsteps = sum(steps, na.rm=FALSE)) %>%
    filter(!is.na(nsteps)) -> steps

rng = range(steps$nsteps)
ggplot(data=steps, aes(steps$nsteps)) +
    geom_histogram(breaks=seq(rng[1], rng[2], by=2000), fill="Brown") +
    xlab("Steps Per Day") +
    ylab("Total") +
    ggtitle("Total Steps Taken Per Day")
```

![](PA1_template_files/figure-html/total_steps_per_day-1.png) 

```r
mean(steps$nsteps)
```

```
## [1] 10766.19
```

```r
median(steps$nsteps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
ts <- tapply(dt$steps, dt$interval, mean, na.rm = TRUE)
plot(row.names(ts), ts, type = "l",
     xlab = "5-minute interval", ylab = "Average across all Days",
     main = "Average number of steps taken",
     col = "Blue")
```

![](PA1_template_files/figure-html/avg_daily_activity-1.png) 

```r
interval <- which.max(ts)
names(interval)
```

```
## [1] "835"
```



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
