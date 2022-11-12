---
title: "Reproducible Research - Project 1"
autor: Angel Villamizar
date: "12/11/2022"
output:
  html_document:
    keep_md: true
    theme: cerulean
    toc: yes
    toc_depth: 1
editor_options: 
  chunk_output_type: console
---



1.- Code for reading in the dataset and/or processing the data.

```r
fichero <- list.files(directory, pattern = ".zip" )

data <- unzip(fichero, 
      files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, 
      exdir = directory, unzip = "internal", setTimes = FALSE)

activity <- read.csv(data, sep = ",")

head(activity)
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

2.- Histogram of the total number of steps taken each day.

```r
steps_by_day <- activity %>%
    group_by(date) %>%
    summarize(total = sum(steps))  %>%  as.data.table()

hist(steps_by_day$total, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3.- Mean and median number of steps taken each day.

```r
summary(steps_by_day)
```

```
##      date               total      
##  Length:61          Min.   :   41  
##  Class :character   1st Qu.: 8841  
##  Mode  :character   Median :10765  
##                     Mean   :10766  
##                     3rd Qu.:13294  
##                     Max.   :21194  
##                     NA's   :8
```

Mean of total number of steps per day is 10766, median is 10765.

4.- Time series plot of the average number of steps taken.

```r
steps_by_interval <- aggregate(steps ~ interval, activity, mean)

plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

5.- The 5-minute interval that, on average, contains the maximum number of steps.

```r
max_steps_row <- which.max(steps_by_interval$steps)

steps_by_interval[max_steps_row, ]
```

```
##     interval    steps
## 104      835 206.1698
```

The interval 835 has the maximum average value of steps = 206.1698.

6.- Code to describe and show a strategy for imputing missing data.

```r
sum(is.na(activity))
```

```
## [1] 2304
```

Total number of rows with NA’s is 2304.

Replacing NA’s with the mean for that 5-minute interval.

```r
aux_steps <- data.table()
for (i in 1:nrow(activity)) {
  # i <- 2
  activity2 <- activity[i,]
  
  if (is.na(activity2$steps)) {
    
    interv <- activity2[,c("interval")]
  
    mean_interv <- steps_by_interval[steps_by_interval$interval == interv,c("steps")]
    
    activity2$steps <- mean_interv
    
  }else{ activity2$steps <- activity2$steps }
  
  aux_steps <- rbind(aux_steps,activity2)
}

aux_steps2 <- aux_steps
aux_steps2$steps <- round(aux_steps2$steps, 0)
```

7.- Mean and median number of steps taken each day after missing values are imputed.

```r
aux_steps2_steps_by_day <- aggregate(steps ~ date, aux_steps2, sum)

mean(aux_steps2_steps_by_day$steps)
```

```
## [1] 10765.64
```

```r
median(aux_steps2_steps_by_day$steps)
```

```
## [1] 10762
```

8.- Histogram of the total number of steps taken each day after missing values are imputed.

```r
df_imputed <- aggregate(steps ~ date, aux_steps2, sum)
head(df_imputed)
```

```
##         date steps
## 1 2012-10-01 10762
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
hist(df_imputed$steps, main="Histogram of total number of steps per day (imputed)", 
     xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

9.- Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
setDF(aux_steps2)
aux_steps2['day'] <- weekdays(as.Date(aux_steps2$date))

aux_steps2$day[aux_steps2$day  %in% c('sábado','domingo') ] <- "weekend"
aux_steps2$day[aux_steps2$day != "weekend"] <- "weekday"

aux_steps2$day <- as.factor(aux_steps2$day)

aux_steps2_interval <- aggregate(steps ~ interval + day, aux_steps2, mean)

qplot(interval, 
      steps, 
      data = aux_steps2_interval, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
  facet_wrap(~ day, ncol = 1)
```

```
## Warning: `qplot()` was deprecated in ggplot2 3.4.0.
```

```
## Warning in geom_line(type = "l"): Ignoring unknown parameters: `type`
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->




