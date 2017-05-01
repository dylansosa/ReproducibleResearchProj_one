---
    title: "Reproducible Research Project One"
author: "Dylan S"
date: "April, 2017"
---
    ### About
    This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

# Data
* Dataset: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip


## Code for reading in the dataset and/or processing the data

Load data into data frame `data`. 

```r
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
data <- datafile()
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",data)
unzip(data)
unlink(data)
}
```

```
## Error in eval(expr, envir, enclos): could not find function "datafile"
```

```r
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
### Histogram of the total number of steps taken each day

```r
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)
```

`mean` is 1.0766189 &times; 10<sup>4</sup> and `median` is 10765.

## What is the average daily activity pattern?
### Mean and median number of steps taken each day

```r
steps_by_interval <- aggregate(steps ~ interval, data, mean)

plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
```

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is 835.

## Impute missing values. Compare imputed to non-imputed data. 
Missing data needed to be imputed. Only a simple imputation approach was required for this assignment. 
Missing values were imputed by inserting the average for each interval. Thus, if interval 10 was missing on 10-02-2012, the average for that interval for all days (0.1320755), replaced the NA. 

```r
incomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))
```

Zeroes were imputed for 10-01-2012 because it was the first day and would have been over 9,000 steps higher than the following day, which had only 126 steps. NAs then were assumed to be zeros to fit the rising trend of the data. 

```r
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```

Recount total steps by day and create Histogram. 

```r
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

# Histogram of the total number of steps taken each day after missing values are imputed
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

Calculate new mean and median for imputed data. 

```r
rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)
```

Calculate difference between imputed and non-imputed data.

```r
mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian
```

Calculate total difference.

```r
total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)
```
* The imputed data mean is 1.0589694 &times; 10<sup>4</sup>
* The imputed data median is 1.0766189 &times; 10<sup>4</sup>
* The difference between the non-imputed mean and imputed mean is -176.4948964
* The difference between the non-imputed mean and imputed mean is 1.1886792
* The difference between total number of steps between imputed and non-imputed data is 7.5363321 &times; 10<sup>4</sup>. Thus, there were 7.5363321 &times; 10<sup>4</sup> more steps in the imputed data.

## Are there differences in activity patterns between weekdays and weekends?
### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)

library(lattice)

xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
require(knitr) # required for knitting from rmd to md
require(markdown) # required for md to html 
knit('test.rmd', 'test.md') # creates md file
markdownToHTML('test.md', 'test.html') # creates html file
