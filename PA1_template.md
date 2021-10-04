---
title: "PA1_template"
output: 
        html_document:
                keep_md: true
---



# Reproducible Research: Course Project 1
## Loading and preprocessing the data
## 1. Load the data
Create a data directory if it does not already exist, and unzip
the data into that directory.

```r
if(!file.exists("data")) {dir.create("data")}
unzip("./data/repdata_data_activity.zip", exdir = "./data")
setwd("./data")
```
Read in the data to an object called activity. 

```r
activity <- read.csv("./data/activity.csv", stringsAsFactors = TRUE)
```

## What is the mean total number of steps taken per day? (ignoring missing values)
### 1. Calculate the total number of steps taken per day.
We can use the aggregate() function to sum the steps for each date. The
result will be stored in an object called tSteps

```r
tSteps <- aggregate(activity$steps, list(activity$date), FUN = sum)
colnames(tSteps) <- c("date", "steps")
```

### 2. Make a histogram of the total number of steps taken each day
We can use the hist() function to accomplish this task

```r
hist(tSteps$steps, xlab = "Total Daily Steps", 
     main = "Histogram of Total Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### 3. Calculate and report the mean and median of the total number of steps  
### taken per day.
We can just call the functions mean() and median() on the steps column
of our tSteps object.

```r
tMean <- mean(tSteps$steps, na.rm = TRUE)
tMedian <- median(tSteps$steps, na.rm = TRUE)
tMean
```

```
## [1] 10766.19
```

```r
tMedian
```

```
## [1] 10765
```
Of the total number of steps taken per day: the mean is 10766.19, and 
the median is 10765.

## What is the average daily activity pattern?
### 1. Make a time series plot of the 5-minute interval and the average number
### of steps taken, averaged across all days.
We want the daily average amount of steps for each interval. We can calculate
this by grouping the activity data set by interval and calculating the
mean for each interval (after removing the NAs)

```r
aInt <- activity %>% 
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarize(aInt <- mean(steps))
plot(aInt, type = "l", xlab = "Interval", ylab = "Average Daily Steps", 
     main = "Average Daily Steps for each 5-Minute Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### 2. Which 5-minute interval, on average across all the days in the dataset, 
### contains the maximum number of steps?
All we have to do here is sort our previous data set by decreasing and
select the first row.

```r
a <- as.data.frame(aInt)
colnames(a) <- c("interval", "ave.steps")
a <- a[with(a, order(ave.steps, decreasing = TRUE)),]
topInt <- a[1, ]
topInt
```

```
##     interval ave.steps
## 104      835  206.1698
```
The interval across all days that contains the average maximum number of steps
(~ 206 steps) is interval 835.

## Imputing missing values
### 1. Calculate and report the total number of missing values in the dataset.
We can do this by summing the TRUE values returned by is.na() on the steps
variable

```r
mis <- sum(is.na(activity$steps))
mis
```

```
## [1] 2304
```
There are 2304 missing values in the dataset.

### 2. Devise a strategy for filling in all of the missing values in the dataset.
We can impute the missing values using the average daily value for the
5-minute interval where the NA is located.

### 3. Create a new dataset that is equal to the original dataset but with the
### missing data filled in.
Using our devised strategy as stated above:

```r
colnames(aInt) <- c("interval", "steps")
activity$srep <- aInt$steps
activity$steps[is.na(activity$steps)] <- activity$srep[is.na(activity$steps)]
repAct <- select(activity, c("steps", "date", "interval"))
```

### 4. Make a histogram of the total number of steps taken each day. Calculate
### and report the mean and median total number of steps taken per day.
### Do these values differ from the estimate from the first part of the
### assignment? What is the impact of imputing missing data on the estimates of 
### the total daily number of steps?

```r
imSteps <- aggregate(repAct$steps, list(repAct$date), FUN = sum)
colnames(imSteps) <- c("date", "steps")

hist(imSteps$steps, xlab = "Total Daily Steps", 
     main = "Histogram of Total Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
imMean <- mean(imSteps$steps)
imMedian <- median(imSteps$steps)
imMean
```

```
## [1] 10766.19
```

```r
imMedian
```

```
## [1] 10766.19
```
The mean value has stayed the same (10766.19) after imputing missing data, but the median value has increased (from 10765 to 10766.19). These results are expected. The mean should not have changed much, if at all, due to the relatively low ratio of missing values to total values contained in our dataset. The median should have increased due to the fact that there are now numerical values in our dataset where there were previously none.

## Are there differences in activity patterns between weekdays and weekends?
### 1. Create a new factor variable in the dataset with two levels - 
### "weekday" and "weekend" - indicating whether a given date is a weekday
### or weekend day.
We first have to change all of the dates to a proper date class. Then we
can find the specific days using the weekdays() function and appropriately assign
"weekday" (Monday - Friday) and "weekend" (Saturday & Sunday).

```r
dAct <- repAct 
dAct$date <- as.Date(as.character(repAct$date))
dAct$weekday <- weekdays(dAct$date)
days <- dAct$weekday
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekends <- c("Saturday", "Sunday")
dAct$weekday[days %in% weekdays] <- "weekday"
dAct$weekday[days %in% weekends] <- "weekend"
dAct$weekday <- as.factor(dAct$weekday)
colnames(dAct)[4] <- "dayclass"
```

### 2. Make a panel plot containing a time series plot of the 5-minute interval
### and the average number of steps taken, averaged accross all weekday days
### or weekend days.
To accomplish this, we must first create 2 new datasets. The first will contain
the data for weekdays and the second will contain data for weekend days.

```r
aDay <- dAct %>%
        filter(as.character(dayclass) == "weekday") %>%
        group_by(interval) %>%
        summarize(aDay <- mean(steps))
aDay <- as.data.frame(aDay)
colnames(aDay)[2] <- "steps"

aEnd <- dAct %>%
        filter(as.character(dayclass) == "weekend") %>%
        group_by(interval) %>%
        summarize(aEnd <- mean(steps))
aEnd <- as.data.frame(aEnd)
colnames(aEnd)[2] <- "steps"
```

Now we can recombine these two new datasets into one.

```r
aDayend <- as.data.frame(cbind(aDay$interval, aDay$steps, aEnd$steps))
colnames(aDayend) <- c("interval", 
                       "average.weekday.steps", 
                       "average.weekend.steps")
```

Now we are ready to make our panel plot.

```r
par(mfrow = c(2,1))
par(mar = c(4, 5, 4, 5))
plot(aDayend$interval, aDayend$average.weekday.steps, type = "l", 
     ylab = "Average Steps", xlab = "5-Minute Interval", 
     main = c("Average Daily Steps in Each 5-Minute Interval: Weekday & Weekend",
     "\nWeekday Steps"),
     ylim = c(0, 250))
plot(aDayend$interval, aDayend$average.weekend.steps, type = "l", 
     ylab = "Average Steps", xlab = "5-Minute Interval",
     main = "Weekend Steps",
     ylim = c(0, 250))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
