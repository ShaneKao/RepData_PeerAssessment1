# Reproducible Research Peer Assessment 1

## Loading and preprocessing the data

Show any code that is needed to

1.Load the data (i.e. `read.csv()`)


```r
activity = read.table("activity.csv", header = TRUE, sep = ",")
```

```
## Warning: 無法開啟檔案 'activity.csv' ：No such file or directory
```

```
## Error: 無法開啟連結
```

```r
head(activity)
```

```
## Error: 找不到物件 'activity'
```


2.Process/transform the data (if necessary) into a format suitable for your analysis

**Remove missing values of steps**
  
  
  ```r
  activity.na.rm = activity[!is.na(activity$steps), ]
  ```
  
  ```
  ## Error: 找不到物件 'activity'
  ```
  
  ```r
  head(activity.na.rm)
  ```
  
  ```
  ## Error: 找不到物件 'activity.na.rm'
  ```


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1.Make a histogram of the total number of steps taken each day


```r
hist(activity.na.rm$steps, main = "Histogram of the total number of steps", 
    xlab = "steps")
```

```
## Error: 找不到物件 'activity.na.rm'
```


2.Calculate and report the **mean** and **median** total number of steps taken per day


```r
(tapply(activity.na.rm$steps, activity.na.rm$date, mean))
```

```
## Error: 找不到物件 'activity.na.rm'
```

```r
(tapply(activity.na.rm$steps, activity.na.rm$date, median))
```

```
## Error: 找不到物件 'activity.na.rm'
```


## What is the average daily activity pattern?

1.Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(unique(activity.na.rm$interval), as.numeric(tapply(activity.na.rm$steps, 
    activity.na.rm$interval, mean, simplify = FALSE)), type = "l", main = "Time Series Plot", 
    xlab = "interval", ylab = "The average number of steps across all days")
```

```
## Error: 找不到物件 'activity.na.rm'
```


2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
activity.na.rm$interval[which(as.numeric(tapply(activity.na.rm$steps, activity.na.rm$interval, 
    mean, simplify = FALSE)) == max(as.numeric(tapply(activity.na.rm$steps, 
    activity.na.rm$interval, mean, simplify = FALSE))))]
```

```
## Error: 找不到物件 'activity.na.rm'
```


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
sum(is.na(activity$steps))
```

```
## Error: 找不到物件 'activity'
```


2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

**Use the mean for that 5-minute interval to replace missing data**
  
  3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
rep.val = numeric(sum(is.na(activity$steps)))
```

```
## Error: 找不到物件 'activity'
```

```r
for (i in 1:sum(is.na(activity$steps))) {
    rep.val[i] = as.numeric(tapply(activity.na.rm$steps, activity.na.rm$interval, 
        mean, simplify = FALSE))[which(unique(activity.na.rm$interval) == activity[is.na(activity$steps), 
        3][i])]
}
```

```
## Error: 找不到物件 'activity'
```

```r
activity[is.na(activity$steps), 1] = rep.val
```

```
## Error: 找不到物件 'rep.val'
```

```r
head(activity)
```

```
## Error: 找不到物件 'activity'
```


4.Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(activity$steps, main = "Histogram of the total number of steps for new dataset", 
    xlab = "steps")
```

```
## Error: 找不到物件 'activity'
```



```r
(tapply(activity$steps, activity$date, mean))
```

```
## Error: 找不到物件 'activity'
```

```r
(tapply(activity$steps, activity$date, median))
```

```
## Error: 找不到物件 'activity'
```


**The values differ from the estimates from the first part of the assignment and the distribution of steps is more positive skew**
  
  ## Are there differences in activity patterns between weekdays and weekends?
  
  For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

**The new factor variable is 1 if a given date is a weekend day,0 otherwise**
  
  
  ```r
  activity$week = ifelse(weekdays(as.Date(activity$date, "%Y-%m-%d")) %in% c("星期六", 
      "星期日"), 1, 0)
  ```
  
  ```
  ## Error: 找不到物件 'activity'
  ```
  
  ```r
  head(activity)
  ```
  
  ```
  ## Error: 找不到物件 'activity'
  ```


2.Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using **simulated data**:
  
  
  ```r
  par(mfrow = c(2, 1))
  activity.weekend = activity[activity$week == 1, ]
  ```
  
  ```
  ## Error: 找不到物件 'activity'
  ```
  
  ```r
  activity.weekday = activity[activity$week == 0, ]
  ```
  
  ```
  ## Error: 找不到物件 'activity'
  ```
  
  ```r
  plot(unique(activity.weekend$interval), as.numeric(tapply(activity.weekend$steps, 
      activity.weekend$interval, mean, simplify = FALSE)), type = "l", main = "Time Series Plot on Weekend", 
      xlab = "interval", ylab = "The average number of steps across all days")
  ```
  
  ```
  ## Error: 找不到物件 'activity.weekend'
  ```
  
  ```r
  
  plot(unique(activity.weekday$interval), as.numeric(tapply(activity.weekday$steps, 
      activity.weekday$interval, mean, simplify = FALSE)), type = "l", main = "Time Series Plot on Weekdays", 
      xlab = "interval", ylab = "The average number of steps across all days")
  ```
  
  ```
  ## Error: 找不到物件 'activity.weekday'
  ```

knit2html()
