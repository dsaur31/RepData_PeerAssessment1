---
title: "PA1_template"
author: "dsaur31"
date: "02/03/2022"
output: html_document
---

Loading and Processing the Data
================================================
### The Process Code  

```r
activity <- read.csv("activity.csv")
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
activity_clean <- activity[with(activity, {
        !(is.na(steps))
        }), ]
```

What is mean total number of steps taken per day?
================================================
### Calculating the Total Steps per Day

```r
steps_by_date <- aggregate(steps ~ date, activity_clean, sum)
head(steps_by_date)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
### A historgram of the Total number of steps taken each day

```r
hist(steps_by_date$steps, main = "Sum of Steps per Day",
     xlab="Sum of Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
### The Mean and Median of Total Steps per Day  

```r
summary(steps_by_date)
```

```
##      date               steps      
##  Length:53          Min.   :   41  
##  Class :character   1st Qu.: 8841  
##  Mode  :character   Median :10765  
##                     Mean   :10766  
##                     3rd Qu.:13294  
##                     Max.   :21194
```
**The mean of total steps taken per day is 10,765 and the median is 10,766.**    

What is the average daily activity pattern?
================================================  
### Time Series  

```r
steps_by_interval <- aggregate(steps ~ interval, activity_clean, mean)
plot(steps_by_interval$interval,
     steps_by_interval$steps,
     type='l',
     main="Avg. # of steps over all days",
     xlab="Intervals (by 5)", 
     ylab="Avg. number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

### Which interval has the max number of steps?   

```r
max_interval_step <- which.max(steps_by_interval$steps)
steps_by_interval[max_interval_step, ]
```

```
##     interval    steps
## 104      835 206.1698
```
**Interval 835 has the maximum number of steps with 206.2**  

Imputing Missing Values  
================================================  
### Calculating the total number of NAs in the dataset 

```r
activity_fixed <- activity
sum(is.na(activity_fixed))
```

```
## [1] 2304
```
**The total volume of missing data is 2304**   

### Creating a new dataset  

```r
for (i in 1:nrow(activity_fixed)) {
        if (is.na(activity_fixed$steps[i])) {
                interval_value <- activity_fixed$interval[i]
                steps_value <- steps_by_interval[
                        steps_by_interval$interval == interval_value,]
                activity_fixed$steps[i] <- steps_value$steps
        }
}

sum(is.na(activity_fixed))
```

```
## [1] 0
```

### Histogram & Summary

```r
steps_by_date_fixed <- aggregate(steps ~ date, activity_fixed, sum)

hist(steps_by_date_fixed$steps, main = "Sum of Steps per Day (Imputed)",
     xlab="Sum of Steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
summary(steps_by_date_fixed)
```

```
##      date               steps      
##  Length:61          Min.   :   41  
##  Class :character   1st Qu.: 9819  
##  Mode  :character   Median :10766  
##                     Mean   :10766  
##                     3rd Qu.:12811  
##                     Max.   :21194
```

```r
summary(steps_by_date)
```

```
##      date               steps      
##  Length:53          Min.   :   41  
##  Class :character   1st Qu.: 8841  
##  Mode  :character   Median :10765  
##                     Mean   :10766  
##                     3rd Qu.:13294  
##                     Max.   :21194
```
**The mean remains unchanged, however the median does slightly increase**  

Are there differences between weekdays/weekends?
================================================   
### Creating a new factor variable 

```r
activity_fixed$date <- as.Date(activity_fixed$date)
activity_fixed$weekdays <- weekdays(activity_fixed$date)
activity_fixed$day_type <- ""
activity_fixed[activity_fixed$weekdays == "Saturday" | activity_fixed$weekdays == "Sunday", ]$day_type <- "weekend"
activity_fixed[!(activity_fixed$weekdays == "Saturday" | activity_fixed$weekdays == "Sunday"), ]$day_type <- "weekday"
head(activity_fixed)
```

```
##       steps       date interval weekdays day_type
## 1 1.7169811 2012-10-01        0   Monday  weekday
## 2 0.3396226 2012-10-01        5   Monday  weekday
## 3 0.1320755 2012-10-01       10   Monday  weekday
## 4 0.1509434 2012-10-01       15   Monday  weekday
## 5 0.0754717 2012-10-01       20   Monday  weekday
## 6 2.0943396 2012-10-01       25   Monday  weekday
```

```r
activity_fixed$day_type <- factor(activity_fixed$day_type)
```

### Panel Plot

```r
avg_step_int_fixed <- aggregate(steps ~ interval + day_type, activity_fixed, mean)
library(lattice)
xyplot(steps ~ interval | day_type, activity_fixed, type = "l", lwd = 2,
       layout = c(1, 2), 
       xlab = "5-minute interval", 
       ylab = "Average number of steps",
       main = "Average Number of Steps Taken (across all weekday days or weekend days)")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
