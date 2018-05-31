---
title: "Reproducible Research Assignment 1"
author: "Lim Wee Pynn"
date: "31 May 2018"
output: 
  html_document: 
    keep_md: yes

---



## Loading and preprocessing the data


```r
act_data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```

```r
total_steps <- tapply(act_data$steps, act_data$date, FUN=sum, na.rm=TRUE)
qplot(total_steps, binwidth=500, xlab="number of steps")
```

![](Assignment1_files/figure-html/total_steps-1.png)<!-- -->

```r
ggsave("hist.png")
```

```
## Saving 7 x 5 in image
```

```r
mean(total_steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(total_steps, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
avg_steps_interval <- aggregate(steps ~ interval, act_data, FUN=mean, na.rm=TRUE)

ggplot(data=avg_steps_interval, aes(x=interval, y=steps)) + geom_line() + xlab("5-minute interval") +
      ylab("average steps taken")
```

![](Assignment1_files/figure-html/avg_steps_interval-1.png)<!-- -->

```r
ggsave("time_series.png")
```

```
## Saving 7 x 5 in image
```

```r
avg_steps_interval[which.max(avg_steps_interval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values


```r
act_data_merge <- merge(x=act_data, y=avg_steps_interval, by="interval")
act_data_merge$steps <- ifelse(is.na(act_data_merge$steps.x), act_data_merge$steps.y, act_data_merge$steps.x)
act_data_na_removed <- subset(act_data_merge, select = c("steps", "date", "interval"))
total_steps_na_removed <- tapply(act_data_na_removed$steps, act_data_na_removed$date, FUN=sum, na.rm=TRUE)
qplot(total_steps_na_removed, binwidth=500, xlab="total number of steps each day")
```

![](Assignment1_files/figure-html/total_steps_na_removed-1.png)<!-- -->

```r
ggsave("hist_na_removed.png")
```

```
## Saving 7 x 5 in image
```

```r
mean(total_steps_na_removed, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(total_steps_na_removed, na.rm=TRUE)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?


```r
library(chron)
```

```
## Warning: package 'chron' was built under R version 3.4.4
```

```r
act_data_na_removed$weekday_weekend <- ifelse(is.weekend(act_data_na_removed$date), "weekend", "weekday")
avg_steps_interval_2 <- aggregate(steps ~ interval + weekday_weekend, act_data_na_removed, FUN=mean, na.rm=TRUE)
ggplot(avg_steps_interval_2, aes(interval, steps)) + geom_line() + facet_wrap(~weekday_weekend, nrow=2) +
  xlab("5-minute interval") + ylab("Average number of steps")
```

![](Assignment1_files/figure-html/avg_steps_interval_2-1.png)<!-- -->

```r
ggsave("time_series3.png")
```

```
## Saving 7 x 5 in image
```
Yes. 

