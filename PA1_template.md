---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Show any code that is needed to

### 1. Load the data (i.e. read.csv())


```r
unzip("activity.zip")
activity_data <- read.csv("activity.csv")
```

### 2. Process/transform the data (if necessary) into a format suitable for your analysis


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

### 1. Calculate the total number of steps taken per day


```r
steps_date <- aggregate(steps ~ date, data = activity_data, sum, na.rm = TRUE)
```

### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(steps_date$steps, main = "Histogram", xlab = "Total number of steps taken each day", ylab = "Frequency (days)", col = "Blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### 3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean_steps <- round(mean(steps_date$steps))
median_steps <- round(median(steps_date$steps))
```

The mean and median total number of steps taken per day are 10,766 and 10,765 respectively.

## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_interval <- aggregate(steps ~ interval, data = activity_data, mean, na.rm = TRUE)
plot(steps ~ interval, data = steps_interval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_steps_interval <- steps_interval$interval[which.max(steps_interval$steps)]
```

The interval 835 contains on average the maximum number of steps.

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
num_NA <- sum(is.na(activity_data))
```

The total number of missing values in the data set is 2304.

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy will be to fill all missing values in the datasets with zeros.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_data_2 <- activity_data
activity_data_2 <- replace(activity_data_2, is.na(activity_data_2), 0)
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_date_2 <- aggregate(steps ~ date, data = activity_data_2, sum, na.rm = TRUE)
hist(steps_date_2$steps, main = "Histogram", xlab = "Total number of steps taken each day", ylab = "Frequency (days)", col = "Red")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean_steps_2 <- round(mean(steps_date_2$steps))
median_steps_2 <- round(median(steps_date_2$steps))
mean_red <- mean_steps_2/mean_steps - 1
median_red <- median_steps_2/median_steps - 1
```

Now the mean and median total number of steps taken per day are 9,354 and 10,395 respectively.

These values do differ from the estimates from the first part of the assignment. There has been a change of -13.1 % in the mean and a change of -3.4 % in the median. 

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity_data_3 <- activity_data_2
day <- weekdays(as.Date(activity_data_3$date))
day_type <- vector()
for (i in 1:nrow(activity_data_3)) {
    if (day[i] == "Saturday" | day[i] == "Sunday") {
        day_type[i] <- "weekend"
    } else {
        day_type[i] <- "weekday"
    }
}
activity_data_3$day_type <- day_type
activity_data_3$day_type <- factor(activity_data_3$day_type)
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
steps_interval_day_type <- aggregate(steps ~ interval + day_type, data = activity_data_3, mean)
library("lattice")
xyplot(steps ~ interval | day_type, steps_interval_day_type, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

