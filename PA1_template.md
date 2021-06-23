---
title: "Reproducible Research - Week 2 assignment"
author: "Matthew Peck"
date: "6/23/2021"
output: 
  html_document: 
    keep_md: true 
---

## Overview

This assignment conducts a series of analysis on personal movement data, collected via activity monitoring devices. The devices collect data at 5 minute intervals throughout the day. This data consists of 2 months of anonymous data collected between October and November 2012, and measure the number of steps taken during each interval. 

The data can be found here: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included are:

* **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* **date**: The date on which the measurement was taken in YYYY-MM-DD format
* **interval**: Identifier for the 5-minute interval in which measurement was taken

This analysis proceeds through the following steps:

1. Load and process the data 
2. Investigate mean total number of steps per day 
3. Look at average daily pattern across intervals
4. Investigate / impute missing values
5. Analyze differences across weekends and weekdays

## 1) Load and process data 

We'll first download and read in the csv file into a dataframe, and then look into some summary information.


```r
ProjectfileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if(!file.exists("repdata_data_2Factivity.zip")){
        download.file(ProjectfileURL, "repdata_data_2Factivity.zip", method = "curl")
        unzip("repdata_data_2Factivity.zip")
} #download and unzip file if not exists
rawdata <- read.csv("activity.csv") #load data into df
head(rawdata)
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
str(rawdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Two observations from the above: 1) steps has missing values (we will come to those later), and 2) date is not formatted as a date - let's address that now.


```r
rawdata$date <- as.Date(rawdata$date, format = "%Y-%m-%d") #convert date column to date
str(rawdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## 2) Investigate mean total number of steps per day
First, we'll calculate total step count by day, then will create a histogram of step count to show frequency, and then calculate the mean/median of step count.


```r
steps_perday <- with(rawdata, aggregate(steps, by=list(date), FUN=sum, na.rm = FALSE)) #aggregate by day
head(steps_perday)
```

```
##      Group.1     x
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
hist(steps_perday$x, breaks = 10, main = "Total Steps per Day", xlab = "Step Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean(steps_perday$x, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(steps_perday$x, na.rm = TRUE)
```

```
## [1] 10765
```

## 3) Look at average daily pattern across intervals
Here we'll first calculate the average step count by interval, and then look at a time-series plot by interval across a typical day. Then we'll identify the 5-minute interval with the maximum number of steps.


```r
steps_byinterval <- with(rawdata, aggregate(steps, by=list(interval), FUN=mean, na.rm = TRUE)) # aggregate by interval
head(steps_byinterval)
```

```
##   Group.1         x
## 1       0 1.7169811
## 2       5 0.3396226
## 3      10 0.1320755
## 4      15 0.1509434
## 5      20 0.0754717
## 6      25 2.0943396
```

```r
plot(steps_byinterval$Group.1, steps_byinterval$x, type="l", 
        main="Average Number of Steps Taken - By Interval", xlab="Interval", 
        ylab="Average Step Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
steps_byinterval[which.max(steps_byinterval$x), 1] # identify interval with max step count
```

```
## [1] 835
```

## 4) Investigate / impute missing values
To handle the missing step count values, we'll first look into how many there are, and then replace them with the average number of steps for that interval. This strategy is not perfect, but should serve as a reasonable proxy for the missing information to reduce the chance of bias entering our analysis. We'll create a new dataset, and see how the mean/median compares to our previous calculations. 

First, look at how many missing values there are:

```r
table(is.na(rawdata$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

We see there are 2304 missing values.

Next, merge with the dataset created as part of step (3) above, and use it to replace the missing values.

```r
merged_byinterval <- merge(x=rawdata, y=steps_byinterval, by.x="interval", by.y="Group.1") # merge data to identify average step count by interval
index <- c(1:length(merged_byinterval$interval))
for(i in index){
        if(is.na(merged_byinterval$steps[i])==TRUE){
                merged_byinterval$steps[i] <- merged_byinterval$x[i]}
        rawdata_noNA <- merged_byinterval
}
```

Aggregate, create a histogram, and look at median/mean:

```r
steps_perday_noNA <- with(rawdata_noNA, aggregate(steps, by=list(date), FUN=sum, na.rm = TRUE))
hist(steps_perday_noNA$x, breaks = 10, main = "Total Steps per Day", xlab = "Step Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
mean(steps_perday_noNA$x, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(steps_perday_noNA$x, na.rm = TRUE)
```

```
## [1] 10766.19
```

We see that the mean has stayed the same, but the median has shifted slightly - after imputing missing values, the mean and median for the updated dataset are the same. Overall the values are very close.

## 5) Analyze differences across weekends and weekdays
We'll use the weekdays() function here to create a new factor variable that groups weekends and weekdays together, to analyze behavior differences across weekends/weekdays. We'll then plot the two to compare.


```r
wd_index <- c(1:length(rawdata_noNA$date)) 
weekdays_factor <- c()
for(i in wd_index){
if(weekdays(rawdata_noNA$date[i]) == "Monday" | 
   weekdays(rawdata_noNA$date[i]) == "Tuesday" |
   weekdays(rawdata_noNA$date[i]) == "Wednesday" |
   weekdays(rawdata_noNA$date[i]) == "Thursday" |
   weekdays(rawdata_noNA$date[i]) == "Friday"){
        weekdays_factor <- c(weekdays_factor, "weekday")
} else if (weekdays(rawdata_noNA$date[i]) == "Saturday" | 
           weekdays(rawdata_noNA$date[i]) == "Sunday") {
        weekdays_factor <- c(weekdays_factor, "weekend")
}
        weekdays_factor
}
data_inter <- cbind(rawdata_noNA, weekdays_factor) # Create new dataset adding the new factor variable
head(data_inter)
```

```
##   interval    steps       date        x weekdays_factor
## 1        0 1.716981 2012-10-01 1.716981         weekday
## 2        0 0.000000 2012-11-23 1.716981         weekday
## 3        0 0.000000 2012-10-28 1.716981         weekend
## 4        0 0.000000 2012-11-06 1.716981         weekday
## 5        0 0.000000 2012-11-24 1.716981         weekend
## 6        0 0.000000 2012-11-15 1.716981         weekday
```

Aggregate and create a plot of weekends vs. weekdays:


```r
library(lattice)
steps_int_wd <- with(data_inter, aggregate(steps, by=list(weekdays_factor, interval), FUN=mean, na.rm = TRUE))
xyplot(x ~ Group.2 | Group.1, data = steps_int_wd, layout = c(1,2), 
       type = "l", ylab="Average Step Count", xlab="Interval", 
       main="Average Steps Taken - By Interval and weekend/weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
