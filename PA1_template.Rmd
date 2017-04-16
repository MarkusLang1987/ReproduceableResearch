---
title: "KnitR"
author: "Markus"
date: "15 April 2017"
output: html_document
---


<strong>Loading and preprocess the data</strong>

```{r setoptions, echo=TRUE}
library(dplyr)
library(readr)
library(ggplot2)

if(!file.exists("./data")){dir.create("./data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL,destfile="./data/Factivity.zip",method="curl")

unzip(zipfile="./data/Factivity.zip",exdir="./data")

data_row <- read_csv("D:/BVM-DatScienceCup/Aufgabe1/data/activity.csv")

data <- data_row[!is.na(data_row$steps), ]

steps_per_day <- summarise(group_by(data, date), total = sum(steps))


hist(steps_per_day$total, main="Histogram of steps per day", xlab = "Steps per day", ylab = "Frequency")

median(steps_per_day$total)
mean(steps_per_day$total)

steps_by_interval <- data %>%
    group_by(interval) %>%
    summarise(AVGSteps = mean(steps))

plot(steps_by_interval$interval, steps_by_interval$AVGSteps, type='l', 
     main="Average number of steps per day", xlab="Interval", 
     ylab="Average steps")

max_step <- which.max(steps_by_interval$AVGSteps)
steps_by_interval[max_step, ]

data_imputed <- data_row


for (i in 1:nrow(data_imputed)) {
    if(is.na(data_imputed$steps[i])) {
        value <- steps_by_interval$AVGSteps[which(steps_by_interval$interval == data_imputed$interval[i])]
        data_imputed$steps[i] <- value
    } 
}



steps_by_interval_imputed <- summarise(group_by(data_imputed, date), total = sum(steps))


hist(steps_by_interval_imputed$total, main = "Histogram total steps imputed", xlab = "Total number of steps per day")

data_imputed$weekday <- weekdays(as.Date(data_imputed$date))
data_imputed$weekday <- as.character(data_imputed$weekday)
data_imputed$weekday[data_imputed$weekday  %in% c('Samstag','Sonntag') ] <- "weekend"
data_imputed$weekday[data_imputed$weekday != "weekend"] <- "weekday"

data_imputed$weekday <- as.factor(data_imputed$weekday)
table(data_imputed$weekday)

data_imputed_steps_by_interval <- aggregate(steps ~ interval + weekday, data_imputed, mean)
names(data_imputed_steps_by_interval)

ggplot(data = data_imputed_steps_by_interval, aes(x = interval, y = steps)) + geom_line() + facet_wrap(~ weekday, ncol = 1)


```



