---
title: "Reproducible Research Week 2 Project"
output: html_document
---

Read and format the data

```{r, echo=TRUE}
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
```

Calculate the total number of steps per day
```{r, echo=TRUE}
daily_total <- aggregate(steps ~ date, data, sum)
```

Create histogram for total number of steps taken each day
```{r, echo=TRUE}
hist(daily_total$steps)
```

Calculate the mean and median steps taken per day

```{r, echo=TRUE}
summary(daily_total)
```

Create a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r, echo=TRUE}
interval_mean <- aggregate(steps ~ interval, data, mean)
plot(interval_mean$interval, interval_mean$steps, type = "l")
```

Find which interval contains the maximum number of steps
```{r, echo=TRUE}
interval_mean[which.max(interval_mean$steps), ]
```

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r, echo=TRUE}
sum(is.na(data$steps))
```

Replace missing data with mean for that interval
```{r, echo=TRUE}
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
```{r, echo=TRUE}
newdaily_total <- aggregate(steps ~ date, newdata, sum)
hist(newdaily_total$steps)
```

Report the mean and median of the new data and compare to the original
```{r, echo=TRUE}
summary(newdaily_total)
summary(daily_total)
```

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r, echo=TRUE}
newdata$Week <- factor((weekdays(newdata$date) %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r, echo=TRUE}
newdata$Week <- as.factor(newdata$Week)
weekday_mean <- aggregate(steps ~ interval + Week, newdata, mean)
library(ggplot2)
qplot(interval, steps, data = newdata, geom=c("line")) + facet_wrap(~ Week, ncol = 1)
```









