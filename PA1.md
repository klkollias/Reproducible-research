---
title: "Reproducible Research - Programming Assignment 1"
author: "Konstantinos Kollias"
date: "4/14/2019"
output: html_document
---


This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals throughout the day. 

The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012, including the number of steps taken in 5 minute intervals each day.

The first step of the analysis is to read the CSV file from the local working directory.


```r
setwd("~/Documents/Reproducible Research")
activity <- read.csv("activity.csv")
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
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

The variables included in this dataset are:
  
* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

Next, with the R script below, create a histogram of the total steps taken each day of the study period and calculate the mean and median of the total number of steps taken per day. 


```r
daily_activity <- aggregate(activity$steps,by = list(activity$date), sum, na.rm = TRUE)
hist(daily_activity$x, xlab = "Number of Steps", main = "Daily Activity", ylim = c(0,30))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
mean_daily <- mean(daily_activity$x)
median_daily <- median(daily_activity$x)

mean_daily
```

```
## [1] 9354.23
```

```r
median_daily
```

```
## [1] 10395
```


Next make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days and find which 5-minute interval contains the maximum number of steps.


```r
interval_activity <- aggregate(activity$steps, by = list(activity$interval), mean, na.rm = TRUE)
plot(interval_activity$Group.1,interval_activity$x, type = "l", main = "Average steps taken during each interval", xlab = "Interval", ylab = "Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
max_interval <- max(interval_activity$x)
max_interval
```

```
## [1] 206.1698
```

Calculate and report the total number of missing values in the dataset. Devise a strategy for filling in all of the missing values in the dataset, then create a new dataset that is equal to the original dataset but with the missing data filled in. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
na_count <- sum(is.na(activity$steps))

colnames(interval_activity) <- c("interval", "Average_Interval_Steps")
activity_imputed <- merge(activity,interval_activity,by="interval")
activity_imputed[is.na(activity_imputed$steps),]$steps <- activity_imputed[is.na(activity_imputed$steps),]$Average_Interval_Steps

daily_activity_imputed <- aggregate(activity_imputed$steps, by = list(activity_imputed$date), sum)

hist(daily_activity_imputed$x, xlab = "Number of Steps", main = "Daily Activity with Imputed Data")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
mean_daily_imputed <- mean(daily_activity_imputed$x)
median_daily_imputed <- median(daily_activity_imputed$x)

mean_daily_imputed
```

```
## [1] 10766.19
```

```r
median_daily_imputed
```

```
## [1] 10766.19
```

Finally, make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.


```r
activity_imputed$weekdayInd<-with(activity_imputed, !(weekdays(as.Date(activity_imputed$date))=="Saturday" | weekdays(as.Date(activity_imputed$date))=="Sunday")) 

activity_imputed$TimePeriod[activity_imputed$weekdayInd==0]<-"Weekend"
activity_imputed$TimePeriod[activity_imputed$weekdayInd==1]<-"Weekday"
library(dplyr)
activity_imputed.SMRY <- group_by(activity_imputed, TimePeriod,interval)
activity_imputed.SMRY<-summarize(activity_imputed.SMRY, AveSteps = mean(steps, na.rm = TRUE))
library(lattice)
xyplot(AveSteps ~ interval | TimePeriod, data = activity_imputed.SMRY, layout = c(1, 2),type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


