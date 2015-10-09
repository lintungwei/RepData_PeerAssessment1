# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Assume data fle in the local working directory  
Read the data to variable mydata

```r
myRawData <- read.csv("activity.csv", header=TRUE)
myTBLData <- tbl_df(myRawData)
```


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
dailySum <- myTBLData %>% group_by(date) %>% summarize(totalSteps=sum(steps,na.rm=TRUE))
```

2. Make a histogram of the total number of steps taken each day

```r
hist(dailySum$totalSteps, xlab="Total Steps", main = "Total Steps Per Day Histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

3. Calulate mean and median of the total number of steps taken per day

```r
mean(dailySum$totalSteps)
```

```
## [1] 9354.23
```

```r
median(dailySum$totalSteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intervalMean <- myTBLData %>% group_by(interval) %>% summarize(meanSteps=mean(steps,na.rm=TRUE))

plot(x=intervalMean$interval, y=intervalMean$meanSteps, xlab="Time Interval", ylab="Average Steps", main="Average Steps by Time Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? Below is the answer.

```r
intervalMean[ intervalMean$meanSteps >= max(intervalMean$meanSteps), "interval"][[1]]
```

```
## [1] 835
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(myTBLData$steps))
```

```
## [1] 2304
```

2. Use the average steps of the interval to replace NA. 
3. Create a new dataset (nonNAData) that is equal to the original dataset but with the missing data filled in.

```r
nonNAData <- myTBLData
x <- which(is.na(nonNAData$steps))
x <- lapply(x, FUN = function (i) { nonNAData[i, "steps"] <<- intervalMean[ intervalMean$interval == nonNAData[i,"interval"][[1]], "meanSteps"] })
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
dailySum2 <- nonNAData %>% group_by(date) %>% summarize(totalSteps=sum(steps))
hist(dailySum2$totalSteps, xlab="Total Steps (NA replaced)", main = "Total Steps Per Day Histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
mean(dailySum2$totalSteps)
```

```
## [1] 10766.19
```

```r
median(dailySum2$totalSteps)
```

```
## [1] 10766.19
```

What is the impact of imputing missing data on the estimates of the total daily number of steps?

*Ans : Both mean and median increased and the data become more close to normal distribution.*


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
nonNAData$date <- as.Date(nonNAData$date)
nonNAData <- cbind(nonNAData, day_type = ifelse(is.element(weekdays(nonNAData$date), c("Sunday","Saturday")), "Weekend", "Weekday"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
intervalMean2 <- nonNAData %>% group_by(interval, day_type) %>% summarize(meanSteps=mean(steps))

xyplot(meanSteps~interval | day_type, data=intervalMean2, type="l", ylab="Number of Steps", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
