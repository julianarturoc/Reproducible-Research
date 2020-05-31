## ------------------------------------------------------------------------
# 0. loading libraries
library(ggplot2)
library(dplyr)
library(lubridate)

## ------------------------------------------------------------------------
# 1. Reading Data
unzip(zipfile="activity.zip")
data<-read.csv(file="activity.csv", header=TRUE, sep=",")
head(data)
# 2. Graph steps per day
t_steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(t_steps, binwidth=1000, xlab="Total number of steps taken per day")
mean(t_steps, na.rm=TRUE)
median(t_steps, na.rm=TRUE)

## ------------------------------------------------------------------------
# 3. Graph average steps taken
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
        geom_line() +
        xlab("interval [every 5 minutes]") +
        ylab("Number of steps taken [average]")

## ------------------------------------------------------------------------
# Calculating metrics for filling values
averages[which.max(averages$steps),]
missing <- is.na(data$steps)
table(missing)

## ------------------------------------------------------------------------
# Replace each missing value with the mean calculated above
fill <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps))
                filled <- c(steps)
        else
                filled <- (averages[averages$interval==interval, "steps"])
        return(filled)
}
completed.data <- data
completed.data$steps <- mapply(fill, completed.data$steps, completed.data$interval)
t_steps <- tapply(completed.data$steps, completed.data$date, FUN=sum)
qplot(t_steps, binwidth=1000, xlab="total number of steps taken each day")
mean(t_steps)
median(t_steps)

## ------------------------------------------------------------------------
# checking the day 
completed.data$date<-as.Date(completed.data$date)
completed.data<-mutate(completed.data, weekdayType=0)
completed.data$weekdayType <- ifelse(weekdays(completed.data$date) %in% c("sabado", "domingo"), 
                              "weekend", "weekday")

## ------------------------------------------------------------------------
day <- weekdays(completed.data$date)
averages <- aggregate(steps ~ interval + day, data=completed.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("steps (Q)")

