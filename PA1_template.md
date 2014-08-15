Reproducible Research--Peer Assignment 1
========================================================
**Loading and Preprocessing the Data**

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```


**What is mean total number of steps taken per day?**
 
*1. Make a histogram of the total number of steps taken each day*

```r
steps_date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps_date$steps, names.arg = steps_date$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


*2. Calculate and report the mean and median total number of steps taken per day*

```r
mean(steps_date$steps)
```

```
## [1] 10766
```

```r
median(steps_date$steps)
```

```
## [1] 10765
```


**What is the average daily activity pattern?**

*1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*
 

```r
steps_interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(steps_interval, type = "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


*2. Which 5-minute interval, on average across all the days in the dataset,contains the maximum number of steps?*
 

```r
steps_interval$interval[which.max(steps_interval$steps)]
```

```
## [1] 835
```


**Imputing missing values**
 
*1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA`s)*


```r
sum(is.na(activity))
```

```
## [1] 2304
```


*2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

Strategy used: Means for the 5-minute intervals will serve as fillers for missing values.

*3. Create a new dataset that is equal to the original dataset but with the missing data filled in.*  

```r
activity <- merge(activity, steps.interval, by = "interval", suffixes = c("", 
    ".y"))
NAs <- is.na(activity$steps)
activity$steps[NAs] <- activity$steps.y[NAs]
activity <- activity[, c(1:3)]
```


*4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.*
  

```r
steps_date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps_date$steps, names.arg = steps_date$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r

mean(steps_date$steps)
```

```
## [1] 10766
```

```r

median(steps_date$steps)
```

```
## [1] 10766
```


*Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

The impact of the missing data appears to be low when estimating the total number of steps per day.

**Are there differences in activity patterns between weekdays and weekends?**

*1. Create a new factor variable in the dataset with two levels --"weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*


```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```


*2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*


```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = activity, subset = activity$daytype == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

 
