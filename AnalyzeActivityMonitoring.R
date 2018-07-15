## Analyze Activity Monitoring Data

## Repository Content Requirements

## Valid GitHub URL
## At least one commit beyond the original fork
## Valid SHA-1
## SHA-1 corresponds to a specific commit
## Commit containing full submission


## Required in submission
## Code for reading in the dataset and/or processing the data
## Histogram of the total number of steps taken each day
## Mean and median number of steps taken each day
## Time series plot of the average number of steps taken
## The 5-minute interval that, on average, contains the maximum number of steps
## Code to describe and show a strategy for imputing missing data
## Histogram of the total number of steps taken each day after missing values are imputed
## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
## All of the R code needed to reproduce the results (numbers, plots, etc.) in the report


# Loading and preprocessing the data
library(dplyr)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
file <- "repdata data activity.zip"

if(!file.exists(file)) {
      download.file(url, file, mode="wb")
}

path <- ""
if(!file.exists(path)) {
      unzip(file)
}

activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, format="%Y-%m-%d")


# What is mean total number of steps taken per day?
stepsPerDay <- aggregate(steps ~ date, data=activity, rm.NA = TRUE, FUN = sum)
png("HistSteps.png")
hist(stepsPerDay$steps, main="Frequency of Step Totals per Day", xlab="Total Step Ranges", ylab="Number of Days",
     col = rainbow(5))
dev.off()
meanSteps <- mean(stepsPerDay$steps)
medianSteps <- median(stepsPerDay$steps)
print(meanSteps)
print(medianSteps)

## What is the average daily activity pattern?
meanStepsPerInterval <- aggregate(steps ~ interval, data=activity, rm.NA = TRUE, FUN = mean)
png("MeanStepsInterval.png")
plot(meanStepsPerInterval$interval, meanStepsPerInterval$steps, type = "l",
     main = "Means Steps Per Interval", xlab = "Daily 5-Minute Interval", ylab="Mean Steps",
     col = "blue")
dev.off()
meanStepsPerInterval[which.max(meanStepsPerInterval$steps), ]

## Imputing missing values
sum(is.na(activity$steps))
activity$weekday <- weekdays(activity$date)
meanStepsDay <- aggregate(steps ~ weekday, data=activity, rm.NA = TRUE, FUN=mean)
activity$replace <- as.numeric(as.character(factor(activity$weekday, levels=meanStepsDay$weekday, labels=meanStepsDay$steps)))
activity$steps <- ifelse(is.na(activity$steps), activity$replace, activity$steps)
stepsPerDay <- aggregate(steps ~ date, data=activity, rm.NA = TRUE, FUN = sum)
png("HistStepsNoMissing.png")
hist(stepsPerDay$steps, main="Frequency of Step Totals per Day (no NAs)", xlab="Total Step Ranges", ylab="Number of Days",
     col = rainbow(5))
dev.off()
meanSteps <- mean(stepsPerDay$steps)
medianSteps <- median(stepsPerDay$steps)
print(meanSteps)
print(medianSteps)


## Are there differences in activity patterns between weekdays and weekends?
activity$dayType <- as.factor(ifelse(activity$weekday %in% c("Saturday", "Sunday"), "weekend", "weekday"))
meanStepsPerIntervalWeekend <- aggregate(steps ~ interval, data=activity[activity$dayType=="weekend", ], rm.NA = TRUE, FUN = mean)
meanStepsPerIntervalWeekday <- aggregate(steps ~ interval, data=activity[activity$dayType=="weekday", ], rm.NA = TRUE, FUN = mean)
png("panelMeanSteps.png")
par(mfrow = c(2,1))
plot(meanStepsPerIntervalWeekend$interval, meanStepsPerIntervalWeekend$steps, type = "l",
     main = "Means Steps Per Interval (Weekend)", xlab = "Daily 5-Minute Interval", ylab="Mean Steps",
     col = "blue")
plot(meanStepsPerIntervalWeekday$interval, meanStepsPerIntervalWeekday$steps, type = "l",
     main = "Means Steps Per Interval (Weekday)", xlab = "Daily 5-Minute Interval", ylab="Mean Steps",
     col = "red")
dev.off()














