#Assignment 1 Reprodcible Research 

Loading File 
```{r,echo=TRUE}

# Loading CSV file
activity_raw <- read.csv("activity.csv", stringsAsFactors=FALSE)
```

Process/transform the data (if necessary) into a format suitable for analysis
    
```{r,echo=TRUE}

activity_raw$date <- as.POSIXct(activity_raw$date, format="%Y-%m-%d")

# Computing the weekdays
activity_raw <- data.frame(date=activity_raw$date, 
                           weekday=tolower(weekdays(activity_raw$date)), 
                           steps=activity_raw$steps, 
                           interval=activity_raw$interval)

#weekend or weekday
activity_raw <- cbind(activity_raw, 
                      daytype=ifelse(activity_raw$weekday == "saturday" | 
                                     activity_raw$weekday == "sunday","weekend","weekday"))


activity <- data.frame(date=activity_raw$date, 
                       weekday=activity_raw$weekday, 
                       daytype=activity_raw$daytype, 
                       interval=activity_raw$interval,
                       steps=activity_raw$steps)

```


Make a histogram of the total number of steps taken each day
```{r,echo=TRUE}
# Compute the total number of steps each day (NA values removed)
sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)

# Rename the attributes
names(sum_data) <- c("date", "total")
```

The histogram is given by the following lines of code:

```{r,echo=TRUE}
# Compute the histogram of the total number of steps each day
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="grey", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day\n(NA removed)")
```     

The mean and median are computed like
```{r,echo=TRUE}
print(mean(sum_data$total))
print(median(sum_data$total))
```

What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r,echo=TRUE}


# Computing means of steps accross all days for each interval
mean_data <- aggregate(activity$steps, 
                       by=list(activity$interval), 
                       FUN=mean, 
                       na.rm=TRUE)


names(mean_data) <- c("interval", "mean")
```

The time series plot is created by the following lines of code
```{r,echo=TRUE}
# Compute the time series plot
plot(mean_data$interval, 
     mean_data$mean, 
     type="l", 
     col="grey", 
     lwd=2, 
     xlab="Interval in Minutes", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals\n(NA removed)")
```     
     
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r,echo=TRUE}
#Position of maximum Mean
max_pos <- which(mean_data$mean == max(mean_data$mean))

max_interval <- mean_data[max_pos, 1]
print(max_interval)
```

Inputing the missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA's)

```{r,echo=TRUE}

NA_count <- sum(is.na(activity$steps))
NA_count
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r,echo=TRUE}

na_pos <- which(is.na(activity$steps))


mean_vec <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_pos))
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r,echo=TRUE}
# Replacing the NAs by the means
activity[na_pos, "steps"] <- mean_vec

```

Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r,echo=TRUE}

sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum)

names(sum_data) <- c("date", "total")


hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="grey", 
     xlab="Total Steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")
```

The mean and median are 
```{r,echo=TRUE}
print(mean(sum_data$total))
print(median(sum_data$total))
```

These values differ greatly from the estimates from the first part of the assignment. The impact of imputing the missing values is to have more data, hence to obtain a bigger mean and median value.
Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekdays" and "weekend" indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. type = "l") of the 5- minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r,echo=TRUE}

library(lattice)
mean_data <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$weekday, activity$interval), mean)

names(mean_data) <- c("daytype", "weekday", "interval", "mean")
```

The time series plot take the following form:
```{r,echo=TRUE}
# Compute the time series plot
xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```