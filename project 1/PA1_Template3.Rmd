#Reproducible Research Project 1

#1. Code for reading in the dataset and/or processing the data

Unzip and read the data.
```{r, echo=TRUE}
unzip("repdata%2Fdata%2Factivity (1).zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
```

#2. Histogram of the total number of steps taken each day

Aggregate steps per day and plot a histogram.
```{r}
total <- aggregate(steps ~ date, activity, sum)
hist(total$steps, breaks = 20)
```

#3. Mean and median number of steps taken each day

Calculate mean and median of steps taken each day.
```{r}
mean(total$steps)
median(total$steps)
```

#4. Time series plot of the average number of steps taken

Remove the NAs, calculate average steps per interval, and plot the dataframed data.
```{r}
noNa <- activity[complete.cases(activity),]
avgInt <- tapply(noNa$steps, noNa$interval, mean, na.rm=TRUE, simplify=T)
avgIntDf <- data.frame(interval=as.integer(names(avgInt)), avg=avgInt)
with(avgIntDf, 
     plot(interval,
          avg,
          type = "l",
          xlab = "5 min intervals",
          ylab = "avg steps"))
```

#5. The 5-minute interval that, on average, contains the maximum number of steps

Match the average interval with its max for looking up the value.
```{r}
avgIntDf[avgIntDf$avg == max(avgIntDf$avg),]
```

#6. Code to describe and show a strategy for imputing missing data

Use the mean of intervals with NAs.
```{r}
naSteps <- is.na(activity$steps)
avgInt <- tapply(noNa$steps, noNa$interval, mean, na.rm=TRUE, simplify=T)
activity$steps[naSteps] <- avgInt[as.character(activity$interval[naSteps])]
```

#7. Histogram of the total number of steps taken each day after missing values are imputed

Plot a histogram on data with NAs imputed.
```{r}
activityImp <- tapply(activity$steps, activity$date, sum, na.rm=TRUE, simplify=T)

hist(activityImp,
     breaks = 20,
     xlab = "steps per day",
     main = "NA Imputed Steps per Day")
```

#8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Classify weekday vs weekend and plot the data.
```{r}
weekday <- function(d) {
    w <- weekdays(d)
    ifelse (w == "Saturday" | w == "Sunday", "weekend", "weekday")
}

w2 <- sapply(activity$date, weekday)
activity$w3 <- as.factor(w2)
weekdayAgg <- aggregate(steps ~ w3+interval, data=activity, FUN=mean)

library(lattice)
xyplot(steps ~ interval | factor(w3),
       layout = c(1, 2),
       type="l",
       lty=1,
       data=weekdayAgg)
```
