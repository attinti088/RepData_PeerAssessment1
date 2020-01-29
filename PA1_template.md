---
title: "Reproducible Research Project 1"
author: "Venkatesh"
date: "25 January 2020"
output: html_document
---

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as ????????) </br>
date: The date on which the measurement was taken in YYYY-MM-DD format </br>
interval: Identifier for the 5-minute interval in which measurement was taken </br>
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

##loading requried Libraries

```r
library("data.table")
```

```
## data.table 1.12.8 using 2 threads (see ?getDTthreads).  Latest news: r-datatable.com
```

```r
library(ggplot2)
```

##Loading and preprocessing the data

```r
stepsData <- read.csv("activity.csv", sep=",")
cleanedData <- stepsData[complete.cases(stepsData), ]
```

##What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day


```r
totalStepsperDay <- aggregate(steps ~ date, cleanedData, sum)
head(totalStepsperDay, 10)
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
```
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day. 


```r
hist(totalStepsperDay$steps, main = "Histogram of total number of steps per day", xlab = "Steps per day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-27-1.png" width="672" />

3. Calculate and report the mean and median of the total number of steps taken per day

```r
round(mean(totalStepsperDay$steps))
```

```
## [1] 10766
```

```r
round(median(totalStepsperDay$steps))
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg_steps_per_interval <- aggregate(steps ~ interval, cleanedData, mean)
plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type='l', col=1, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average number of steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-29-1.png" width="672" />

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval_idx <- which.max(avg_steps_per_interval$steps)
print (paste("The interval with the highest avg steps is ", avg_steps_per_interval[interval_idx, ]$interval, " and the no of steps for that interval is ", round(avg_steps_per_interval[interval_idx, ]$steps, digits = 1)))
```

```
## [1] "The interval with the highest avg steps is  835  and the no of steps for that interval is  206.2"
```
## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s)


```r
missing_value_act <- stepsData[!complete.cases(stepsData), ]
nrow(missing_value_act)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
for (i in 1:nrow(stepsData)) {
  if(is.na(stepsData$steps[i])) {
    val <- avg_steps_per_interval$steps[which(avg_steps_per_interval$interval == stepsData$interval[i])]
    stepsData$steps[i] <- val 
  }
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Aggregate the steps per day with the imputed values
steps_per_day_impute <- aggregate(steps ~ date, stepsData, sum)
# Draw a histogram of the value 
hist(steps_per_day_impute$steps, main = "Histogram of total number of steps per day (IMPUTED)", xlab = "Steps per day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-33-1.png" width="672" />

```r
# Compute the mean and median of the imputed value
# Calculate the mean and median of the total number of steps taken per day
round(mean(steps_per_day_impute$steps))
```

```
## [1] 10766
```

```r
median(steps_per_day_impute$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
week_day <- function(date_val) {
  wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
  if  (!(wd == 'Saturday' || wd == 'Sunday')) {
    x <- 'Weekday'
  } else {
    x <- 'Weekend'
  }
  x
}
stepsData$day_type <- as.factor(sapply(stepsData$date, week_day))
steps_per_day_impute <- aggregate(steps ~ interval+day_type, stepsData, mean)
```

2. Make a panel plot containing a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
plt <- ggplot(steps_per_day_impute, aes(interval, steps)) +
  geom_line(stat = "identity", aes(colour = day_type)) +
  theme_gray() +
  facet_grid(day_type ~ ., scales="fixed", space="fixed") +
  labs(x="Interval", y=expression("No of Steps")) +
  ggtitle("No of steps Per Interval by day type")
print(plt)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-35-1.png" width="672" />
