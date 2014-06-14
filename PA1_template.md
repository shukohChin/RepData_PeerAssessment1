# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
We assume that activity.zip is in your working directory.  

Before doing any processing, set system locale.

```r
Sys.setlocale(locale = "C")
```

```
## [1] "C/C/C/C/C/ja_JP.UTF-8"
```

1.Unzip the file.  

```r
unzip("activity.zip")
```

2.Load the data.

```r
activity <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
```

3.We know that interval represents identifier for the 5-minute interval. For example, 5 means 00:05 and 1215 means 12:15. So we create a new colum named "time" to represent time like "2012-10-01 00:05:00".

```r
activity$time <- strptime(paste(activity$date, sapply(activity$interval, formatC, 
    width = 4, flag = 0)), format = "%Y-%m-%d %H%M")
```



## What is mean total number of steps taken per day?
1.In this step, we ignore the missing values in dataset activity.

```r
clean.act <- activity[!is.na(activity$steps), ]
```

2.Calculate the total numbers of steps taken each day, and then make a plot.

```r
sum.act <- by(clean.act$steps, clean.act$date, sum)
hist(sum.act, breaks = 10, main = "total numbers of steps taken each day", xlab = "total steps per day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

3.Calculate **mean** and **median**.

```r
mean(sum.act)
```

```
## [1] 10766
```

```r
median(sum.act)
```

```
## 2012-11-12 
##      10765
```

* mean total number of steps taken each day is 10766.
* median total number of steps taken each day is 10765.


## What is the average daily activity pattern?
1.Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days.

```r
ave.act <- by(clean.act$steps, clean.act$interval, mean)
plot(ave.act, type = "l", xlab = "5-minute interval", ylab = "average number of steps taken")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

2.Calculate which 5-minute interval, on average across all the days in the dataset, contains the maximun number of steps.

```r
ave.act[which.max(ave.act)]
```

```
##   835 
## 206.2
```

As you can see above, 835 interval contain the maximun number of steps. 


## Imputing missing values
1.Calculate the total number of NA rows.

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Total number of NA rows is 2304.  

2.Devise a strategy for filling in all of the missing values in the datasets. Here I will fill these missing values using the mean for that 5-minute interval.  

3.Create a new dataset with the missing data filled in.

```r
mean.act <- aggregate(steps ~ interval, data = activity, FUN = mean)
new.act <- merge(activity, mean.act, by = "interval")
nar <- is.na(new.act$steps.x)
new.act$steps.x[nar] <- new.act$steps.y[nar]
activity <- new.act[, 1:3]
colnames(activity) <- c("interval", "steps", "date")
```

4.Make the plot.

```r
new.sum.act <- by(activity$steps, activity$date, sum)
hist(new.sum.act, breaks = 10, main = "total numbers of steps taken each day", 
    xlab = "total steps per day")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

```r
mean(new.sum.act)
```

```
## [1] 10766
```

```r
median(new.sum.act)
```

```
## 2012-11-04 
##      10766
```

From the result we can see above, these value are almost same with the result of the first part. The impact of imputing missing data is quite low and can be ignored.

## Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor indicating whether a given date is a weekday or weekend day.

```r
activity$daytype <- ifelse(weekdays(activity$date) %in% c("Saturday", "Sunday"), 
    "weekend", "weekday")
```

2.Make a panel plot.

```r
type.act <- aggregate(activity$steps, by = list(activity$interval, activity$daytype), 
    FUN = "mean")
colnames(type.act) <- c("interval", "daytype", "steps")
library(lattice)
xyplot(steps ~ interval | daytype, data = type.act, layout = c(1, 2), type = "l")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

Finally, set system locale back.

```r
Sys.setlocale(locale = "")
```


