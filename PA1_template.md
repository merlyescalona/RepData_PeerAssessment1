# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
zipfile<-"activity.zip"
csvfile<-"activity.csv"
if (!file.exists(csvfile)){unzip(zipfile)}
activity<-read.csv(csvfile, colClasses = c("numeric","Date","numeric"))
```

## What is mean total number of steps taken per day?



```r
activityNA<-subset(activity,!is.na(activity$steps))
splitAc<-split(activityNA,activityNA$date)
meanPerDay<-sapply(splitAc,function(x){mean(x$steps)})
meanActivity<-mean(meanPerDay)
medianPerDay<-sapply(splitAc,function(x){median(x$steps)})
medianActivity<-median(activityNA$steps)
```

- Mean total number of steps taken per day: 37.3825996

- Median total number of steps taken per day: 0


```r
with(activityNA,hist(date,steps, freq=T,breaks = "days", col="darkblue",las=2 ))
abline(h=medianActivity, col="red")
abline(h=meanActivity, col="yellow")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)\



## What is the average daily activity pattern?


```r
with(activityNA,plot(date,steps, type="l", col="black",las=2))
abline(h=meanActivity, col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
missingData<-(1-(nrow(activityNA)/nrow(activity)))*100
missingData
```

```
## [1] 13.11475
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```
Best way to fill all the missing values would be to set it the mean per day
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity2<-activity
activity2[is.na(activity2$steps),]$steps=meanActivity
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
media


```r
with(activity2,hist(date,steps,breaks = "days", freq=T,col="darkblue",las=2))
splitAc2<-split(activity2, activity2$date)
meanPerDay<-sapply(splitAc,function(x){mean(x$steps)})
medianPerDay<-sapply(splitAc,function(x){median(x$steps)})
meanActivity2<-mean(meanPerDay)
medianActivity2<-median(medianPerDay)
abline(h=medianActivity2, col="red")
abline(h=meanActivity2, col="yellow")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)\


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity2$day<-weekdays(activity2$date)
activity2$week<-activity2$day
activity2$week[activity2$day %in% c("Saturday","Sunday")]<-"weekend"
activity2$week[!activity2$day %in% c("Saturday","Sunday")]<-"weekday"
activity2$week<-factor(activity2$week)
```

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:



```r
library(ggplot2)
qplot(interval,steps,data=activity2,facets=week~.)+geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)\
