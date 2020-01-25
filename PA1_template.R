
//read dataset
stepsData <- read.csv("activity.csv", sep=",")

//cleansing data
cleanedData <- stepsData[complete.cases(stepsData), ]

head(cleanedData)

totalStepsperDay <- aggregate(steps ~ date, cleanedData, sum)
# Create a histogram of no of steps per day
hist(totalStepsperDay, main = "Histogram of total number of steps per day", xlab = "Steps per day")
head(totalStepsperDay)
# Calculate the mean and median of the total number of steps taken per day
round(mean(totalStepsperDay$steps))
round(median(totalStepsperDay$steps))

# Calculate average steps per interval for all days
avg_steps_per_interval <- aggregate(steps ~ interval, cleanedData, mean)

# Calculate average steps per day for all intervals - Not required, but for my own sake 
avg_steps_per_day <- aggregate(steps ~ date, cleanedData, mean)

# Plot the time series with appropriate labels and heading
plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type='l', col=1, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average number of steps")

# Identify the interval index which has the highest average steps
interval_idx <- which.max(avg_steps_per_interval$steps)


# Identify the specific interval and the average steps for that interval
print (paste("The interval with the highest avg steps is ", avg_steps_per_interval[interval_idx, ]$interval, " and the no of steps for that interval is ", round(avg_steps_per_interval[interval_idx, ]$steps, digits = 1)))

# Calculate the number of rows with missing values
missing_value_act <- stepsData[!complete.cases(stepsData), ]
nrow(missing_value_act)

for (i in 1:nrow(stepsData)) {
  if(is.na(stepsData$steps[i])) {
    val <- avg_steps_per_interval$steps[which(avg_steps_per_interval$interval == stepsData$interval[i])]
    stepsData$steps[i] <- val 
  }
}

# Aggregate the steps per day with the imputed values
steps_per_day_impute <- aggregate(steps ~ date, stepsData, sum)

# Draw a histogram of the value 
hist(steps_per_day_impute$steps, main = "Histogram of total number of steps per day (IMPUTED)", xlab = "Steps per day")

# Compute the mean and median of the imputed value
# Calculate the mean and median of the total number of steps taken per day
round(mean(steps_per_day_impute$steps))

median(steps_per_day_impute$steps)


##Are there differences in activity patterns between weekdays and weekends?
week_day <- function(date_val) {
  wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
  if  (!(wd == 'Saturday' || wd == 'Sunday')) {
    x <- 'Weekday'
  } else {
    x <- 'Weekend'
  }
  x
}

# Apply the week_day function and add a new column to stepsData dataset
stepsData$day_type <- as.factor(sapply(stepsData$date, week_day))

#load the ggplot library
library(ggplot2)

# Create the aggregated data frame by intervals and day_type
steps_per_day_impute <- aggregate(steps ~ interval+day_type, stepsData, mean)

# Create the plot
plt <- ggplot(steps_per_day_impute, aes(interval, steps)) +
  geom_line(stat = "identity", aes(colour = day_type)) +
  theme_gray() +
  facet_grid(day_type ~ ., scales="fixed", space="fixed") +
  labs(x="Interval", y=expression("No of Steps")) +
  ggtitle("No of steps Per Interval by day type")
print(plt)


