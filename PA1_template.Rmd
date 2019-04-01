---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
activityData <- read.csv('activity.csv')
str(activityData)


## What is mean total number of steps taken per day?
stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)

# 2b. Make a histogram of the total number of steps taken each day
qplot(stepsByDay, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)

#2c. Calculate and report the mean and median of the total number of steps taken per day
stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)


## What is the average daily activity pattern?
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activityData$steps), by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)

# 3a.  Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
library(ggplot2)
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")

# 3b. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])


## Imputing missing values

numMissingValues <- length(which(is.na(activityData$steps)))
library(Hmisc)
activityDataImputed <- activityData
activityDataImputed$steps <- impute(activityData$steps, fun=mean)

#4c. Make a histogram of the total number of steps taken each day. And Calculate and report the mean and median total number of steps taken per day.
stepsByDayImputed <- tapply(activityDataImputed$steps, activityDataImputed$date, sum)
stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)

## Are there differences in activity patterns between weekdays and weekends?
activityDataImputed$dateType <-  ifelse(as.POSIXlt(activityDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
# 5b.  Make a panel plot containing a time series plot
averagedActivityDataImputed <- aggregate(steps ~ interval + dateType, data=activityDataImputed, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(dateType ~ .) +
  xlab("5-minute interval") + 
  ylab("avarage number of steps")
