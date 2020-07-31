 
---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


##This is a R Markdown File for Assignment #1

## Load and Process the Data

```r
    activity <- read.csv('~/activity.csv', header= TRUE)
    activity$date <- as.Date(activity$date, "%Y-%m-%d")
    library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Overview of the Data

```r
dim(activity)
```

```
## [1] 17568     3
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```
##What is mean total number of steps taken per day?
##Calculate the total number of steps per day

```r
    daily_steps <- aggregate(steps~date, data=activity, sum, na.rm=TRUE)

##Plot Histogram
    hist(daily_steps$steps, main = "Mean total steps per day", xlab = "steps", ylab ="# of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
    mean_daily_steps <- mean(daily_steps$steps)
    median_daily_steps <- median(daily_steps$steps)
```
The mean of the total steps taken per day: 'mean_daily_steps'
The median of the total steps taken per day: 'median_daily_steps'

#What is the average daily activity pattern?

# Calculate average daily steps per interval

```r
    library(ggplot2)
    steps_interval <- aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
    ggplot(data=steps_interval, aes(x=interval, y=steps))+geom_line()+
    xlab("Interval")+
    ylab("Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
max_avg_steps <- steps_interval[which.max(steps_interval$steps),]$interval
```

The 5-minute inerval on average contains the max # of steps is: 'max_avg_steps
    

##Imputing missing values

Out of 17568 total rows, 2304 have NA value instead of valid steps recorded. These values will be replaced with average/mean of the 5-minute interval data.

###fill in the missing values with data


```r
fillings <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (steps_interval[steps_interval$interval==interval, "steps"])
    return(filled)
    }

##Create a new dataset that is equal to the original dataset but with the missing data filled in.

filldata <- activity
filldata$steps <- mapply(fillings, filldata$steps, filldata$interval)
```

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
totalsteps <- tapply(filldata$steps, filldata$date, FUN=sum)
qplot(totalsteps, binwidth=1000, xlab="total # of steps per day", ylab="# of times in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
mean(totalsteps)
```

```
## [1] 10766.19
```

```r
median(totalsteps)
```

```
## [1] 10766.19
```
The mean is `mean(totalsteps)` and median is `median(totalsteps)` and there is an impact with the missing value included, as median with the NA were `interval`.



##Are there differences in activity patterns between weekdays and weekends?


```r
weekend_days <- c("Saturday", "Sunday")
filldata$dow = as.factor(ifelse(is.element(weekdays(as.Date(filldata$date)),weekend_days), "Weekend", "Weekday"))
filldata_interval <- aggregate(steps ~ interval + dow, filldata, mean)
```

##Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
ggplot(filldata_interval, aes(x=interval, y=steps)) + 
        geom_line(color="blue") + 
        facet_wrap(~ dow, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->






