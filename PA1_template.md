# Reproducible Research: Peer Assessment 1
## Loading and preprocessing the data


```r
dataset <- read.csv("activity.csv", head = T)
```


Check the dimensions of dataset

```r
dim(dataset)
```

```
## [1] 17568     3
```

Check the head and tail records of the full dataset

```r
head(dataset)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
tail(dataset)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

There are NAs in the full dataset
## What is mean total number of steps taken per day?
First, only consider records with complete data(steps, date, interval).
Thus, take the subset with complete data, stored in goodata

```r
goodata <- subset(dataset, !is.na(dataset$steps))
```

Check the dimensions of goodata

```r
dim(goodata)
```

```
## [1] 15264     3
```

Compute the total number of steps for each day in the goodata, the result of which is stored in stepsperday

```r
total <- tapply(goodata$steps, goodata$date, sum, simplify = F)
stepsperday <- integer()
for (t in total) stepsperday <- c(stepsperday, t[[1]])
```

Get unique days in goodata

```r
uniquedate <- unique(goodata$date)
```

Generate the histogram of the total number of steps for each day

```r
barplot(stepsperday, main = "Histogram of Total Number of Steps vs. Date (Ignore NAs)", 
    ylab = "Total Number of Steps", names.arg = uniquedate, cex.names = 0.8, 
    cex.axis = 0.6, cex.lab = 1, space = c(0, 0), las = 2)
```

![plot of chunk totalvsdatewthtna](figure/totalvsdatewthtna.png) 

Output the mean step and median step across all days

```r
meanstep <- mean(stepsperday)
meanstep
```

```
## [1] 10766
```

```r
medianstep <- median(stepsperday)
medianstep
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Get interval in goodata

```r
interval <- as.factor(goodata$interval)
```

Compute the average steps for each interval across all days

```r
meanstepsperitvl <- tapply(goodata$steps, goodata$interval, mean, simplify = F)
meanstepitvl <- numeric()
for (m in meanstepsperitvl) meanstepitvl <- c(meanstepitvl, m[[1]])
```

Get interval levels

```r
interval <- levels(as.factor(goodata$interval))
```

Generate the time series for the average steps of each interval accross all days

```r
plot(interval, meanstepitvl, type = "l", xlab = "Interval", ylab = "Mean Number of Steps across All Days ", 
    main = "Mean Number of Steps across All Days vs. Interval (Without NAs)")
```

![plot of chunk meanstepvsintervalnna](figure/meanstepvsintervalnna.png) 

Output the interval with max mean steps across all days

```r
maxmeanstep <- max(meanstepitvl)
maxstepinterval <- interval[match(maxmeanstep, meanstepitvl)]
maxstepinterval
```

```
## [1] "835"
```

## Imputing missing values
Compute the number of missing value

```r
numofmissing <- length(which(is.na(dataset$steps)))
numofmissing
```

```
## [1] 2304
```

Fill each NA with the average number of steps for that interval across all days

```r
completestep <- integer()
for (i in 1:nrow(dataset)) {
    stp <- 0
    if (is.na(dataset[i, 1])) {
        stp <- meanstepitvl[match(dataset[i, 3], interval)]
    } else {
        stp <- dataset[i, 1]
    }
    completestep <- c(completestep, stp)
}
```

Create a new dataset with all NA value filled in

```r
newset <- data.frame(steps = completestep, date = dataset$date, interval = dataset$interval)
```

Compute the total number of steps taken each day

```r
total <- tapply(newset$steps, newset$date, sum, simplify = F)
stepsperday <- integer()
uniquedate <- unique(newset$date)
for (t in total) stepsperday <- c(stepsperday, t[[1]])
```

Generate histogram

```r
barplot(stepsperday, main = "Histogram of Total Number of Steps vs. Date (Fill NAs)", 
    ylab = "Total Number of Steps", names.arg = uniquedate, cex.names = 0.6, 
    cex.axis = 0.6, cex.lab = 1, space = c(0, 0), las = 2)
```

![plot of chunk totalvsdatewna](figure/totalvsdatewna.png) 

Output the mean step and median step across all days

```r
meanstep <- mean(stepsperday)
meanstep
```

```
## [1] 10766
```

```r
medianstep <- median(stepsperday)
medianstep
```

```
## [1] 10766
```

Compare data with NA filled and that without NA filled in, we can discover that the mean value of total number of steps for each day stays the same, but the median of total number to steps for each day is different. With NA value filled, the median goes a little higher than before.

## Are there differences in activity patterns between weekdays and weekends?
Create a new variable indicating if a date is weekday or weekend

```r
weekday <- character()
for (i in 1:nrow(newset)) {
    if (weekdays(as.Date(newset[i, 2])) %in% c("Sunday", "Saturday")) {
        weekday <- c(weekday, "weekend")
    } else {
        weekday <- c(weekday, "weekday")
    }
}
```

Get weekday's and weekend's data respectively

```r
newset$weekday <- as.factor(weekday)
weekdaydata <- subset(newset, newset$weekday == "weekday")
weekenddata <- subset(newset, newset$weekday == "weekend")
```

Compute the mean step time series for weekdays and weekends respectively

```r
intervalwd <- as.factor(weekdaydata$interval)
meanstepsperitvlwd <- tapply(weekdaydata$steps, intervalwd, mean, simplify = F)
meanstepitvlwd <- numeric()
for (m in meanstepsperitvlwd) meanstepitvlwd <- c(meanstepitvlwd, m[[1]])
intervalwd <- levels(intervalwd)

intervalwn <- as.factor(weekenddata$interval)
meanstepsperitvlwn <- tapply(weekenddata$steps, intervalwn, mean, simplify = F)
meanstepitvlwn <- numeric()
for (m in meanstepsperitvlwn) meanstepitvlwn <- c(meanstepitvlwn, m[[1]])
intervalwn <- levels(intervalwn)
```

Display the results

```r
par(mfrow = c(2, 1), mar = c(5, 4, 2, 1))
plot(intervalwd, meanstepitvlwd, type = "l", xlab = "Interval", ylab = "Mean Number of Steps (Weekdays)", 
    main = "Mean Number of Steps across All Days vs. Interval (Fill NAs)", cex.axis = 0.8, 
    cex.lab = 1)
plot(intervalwn, meanstepitvlwn, type = "l", xlab = "Interval", ylab = "Mean Number of Steps (Weekends)", 
    main = "Mean Number of Steps across All Days vs. Interval (Fill NAs)", cex.axis = 0.8, 
    cex.lab = 1)
```

![plot of chunk weekdayvsweekend](figure/weekdayvsweekend.png) 

We can discover that people tend to exercise more in the weekends than weekdays.
