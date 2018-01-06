---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
      keep_md: true
      
author: "Marcel Boers"
date: "6 jan 2017"
---


## Loading and preprocessing the data



First I set the workingdirectory on my laptop:
D:/coursera/reproducible research
Please adjust the directory to your directorry containing the file: activity.csv.

```r
setwd("D:/coursera/reproducible research")
```

Then we load the data from csv: 

```r
activity <- read.csv("activity.csv") 
```

I load libraries for the plots:

```r
library(ggplot2)
library(plyr)
```

## What is mean total number of steps taken per day?

Calculate total steps per day:

```r
stepsday <- na.omit(activity)
head(stepsday)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

```r
stepsday <- tapply (activity$steps, activity$date, sum, na.rm=TRUE)

qplot(stepsday)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/mean-1.png)<!-- -->
Calculate the mean and median steps per day:

```r
mean(stepsday)
```

```
## [1] 9354.23
```

```r
median(stepsday)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Plot the daily activity pattern:

```r
plot(aggregate(steps ~ interval, data = activity, FUN = mean), type = "l")
```

![](PA1_template_files/figure-html/dailypatt-1.png)<!-- -->
Print the 5-minute interval which contains the maximum number of steps: 

```r
max(activity$steps, na.rm = TRUE)
```

```
## [1] 806
```

## Imputing missing values
Calculate the number of missing values in the dataset: 

```r
sum(is.na(activity))
```

```
## [1] 2304
```
I will substitute each NA with a fixed value. I set the fixed value equivalent to the overall mean of the variable activity$steps.
Create a new dataset with the missing data filled: 

```r
activity2 <- activity
activity2$steps[is.na(activity2$steps)] <- mean(na.omit(activity$steps))
activity2$date <- as.Date(activity2$date, format = "%Y-%m-%d")
```


```r
steps_day2 <- aggregate(steps ~ date, rm.na = TRUE, data = activity2, FUN = sum)

par(mfrow = c(1, 2))
plot(stepsday, type = "h", lwd = 5,lend = "square", main = "With NAs")
abline(h = seq(0, 20000, 2500), lty = "dashed")
plot(steps_day2, type = "h", lwd = 5, lend = "square", main = "NAs filled")
abline(h = seq(0, 20000, 2500), lty = "dashed")
```

![](PA1_template_files/figure-html/dailypatt5-1.png)<!-- -->

```r
dev.off()
```

```
## null device 
##           1
```
Filling the NA makes the distributions more homogeneous since there are no longer deep drops in value to zero.

The mean and median are higher that the values in the first part of the assignment:

```r
mean(steps_day2$steps)
```

```
## [1] 10767.19
```

```r
median(steps_day2$steps)
```

```
## [1] 10767.19
```

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
        if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })


activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```

![](PA1_template_files/figure-html/dailypatt7-1.png)<!-- -->

