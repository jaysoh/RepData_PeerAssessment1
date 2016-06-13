---
title: "Reproducible Research Week 2 Project"
output: html_document
---

Read and format the data


```r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
```

Calculate the total number of steps per day

```r
daily_total <- aggregate(steps ~ date, data, sum)
```

Create histogram for total number of steps taken each day

```r
hist(daily_total$steps)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-3-1.png" title="" alt="" width="672" />

Calculate the mean and median steps taken per day


```r
summary(daily_total)
```

```
##       date                steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

Create a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
interval_mean <- aggregate(steps ~ interval, data, mean)
plot(interval_mean$interval, interval_mean$steps, type = "l")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-5-1.png" title="" alt="" width="672" />

Find which interval contains the maximum number of steps

```r
interval_mean[which.max(interval_mean$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Replace missing data with mean for that interval

```r
newdata <- data
for (i in 1:nrow(newdata)) {
  if (is.na(newdata$steps[i])) {   
    interval <- newdata$interval[i]
    steps <- interval_mean[
      interval_mean$interval == interval,]
    newdata$steps[i] <- steps$steps
  }
}
```

Make a histogram of the new data of the total number of steps taken each day

```r
newdaily_total <- aggregate(steps ~ date, newdata, sum)
hist(newdaily_total$steps)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-9-1.png" title="" alt="" width="672" />

Report the mean and median of the new data and compare to the original

```r
summary(newdaily_total)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

```r
summary(daily_total)
```

```
##       date                steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
newdata$Week <- factor((weekdays(newdata$date) %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
newdata$Week <- as.factor(newdata$Week)
weekday_mean <- aggregate(steps ~ interval + Week, newdata, mean)
library(ggplot2)
qplot(interval, steps, data = newdata, geom=c("line")) + facet_wrap(~ Week, ncol = 1)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-12-1.png" title="" alt="" width="672" />









