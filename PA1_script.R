library(readr)
library(dplyr)
library(zoo)
## Loading and preprocessing the data

getwd()
filename = "activity.csv"
if (!file.exists(filename)){
    urlzip <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(urlzip, destfile = "./activity.zip" )
    unzip("./activity.zip", exdir = getwd() )
  }

path <- setwd("C:/Users/jeff/Documents/R/Repro_Research/RepData_PeerAssessment1/")
getwd()
main_activity <- read.csv("./activity.csv", header = TRUE)
head(main_activity,100)

activity <- main_activity[!is.na(main_activity[, 1]), ]
act_mean <- mean(activity[, 1])
act_mean
act_sum <- sum(activity[, 1])
act_sum


dim(activity)
str(activity)
head(activity)
summary(activity)


steps_by_date1 <- select(activity, steps, date, interval) %>%
  filter(!is.na(steps)) %>% # removed filter to include NAs as 0-step days in histogram
  group_by(date) %>% 
  summarize(total_steps = sum(steps, na.rm = TRUE))




# Mean-Median number of Steps per day

steps_by_date <- activity[complete.cases(activity),] %>% 
  group_by(date) %>% 
  summarize(total_steps=sum(steps))
steps_by_date

# Plot the histogram
hist(steps_by_date$total_steps, 
     main = "Histogram of total steps per day", 
     xlab = "Total steps per day", 
     ylab = "Frequency [number of days]", 
     breaks = 20,
     border = "cadetblue4",
     col = "cadetblue3"
)

# Calculate and report the mean and median of the total number of steps taken per day

## Calculate but save for use later
MeanMedian_Raw <- 
  activity[complete.cases(activity),] %>% 
  group_by(date) %>% 
  summarize(average_steps=mean(steps),"median_of_steps"=median(steps)) %>%
as.data.frame(MeanMedian_Raw)

MeanMedian_Raw["average_steps"]

mean_total_steps <- mean(steps_by_date$total_steps)
median_total_steps <- median(steps_by_date$total_steps)
abline(v = mean_total_steps, lwd = 1, lty = 2, col = "red")
abline(v = median_total_steps, lwd = 1, lty = 2, col = "red")
print(mean_total_steps)
print(median_total_steps)

# Average daily activity pattern 
steps_by_interval <- select(activity, steps, date, interval) %>% 
  group_by(interval) %>% 
  summarize(average_steps = mean(steps, na.rm = TRUE))
dim0 <- dim(steps_by_interval)
cat("The dimension of steps_by_interval is: ", dim0)

x <- steps_by_interval[steps_by_interval$average_steps==max(steps_by_interval$average_steps),1]
cat("The 5-minute interval with the maximum steps is: ", as.character(x))   

new_steps_100da <- zoo::rollmean(activity["steps"], k = 100, fill = NA)
plot(steps_by_interval$interval, steps_by_interval$average_steps, 
     type = "l",
     lwd  = 3, 
     col  = 'royalblue4',
     main = 'Average Steps by 5-minute Interval', cex = .85,
     xlab = "Interval",
     ylab = "Average number of steps")
lines(new_steps_100da, lwd = 4, col = 'red2')
legend('topright', deaths.xts,
       c('Steps', '100-period MA'),
       fill = c('blue2', 'red2', 'green2'), cex = .85,
       box.lty = 1)

plot(steps_by_interval$interval, steps_by_interval$average_steps, 
     type = "l",
     lwd  =  2,
     main = "Average steps by 5-minute interval",
     xlab = "Interval",
     ylab = "Average number of steps",
     col  = "royalblue4"
)
max_average_steps <- max(steps_by_interval$average_steps)
max_average_steps_interval <- steps_by_interval[
  steps_by_interval$average_steps == max_average_steps,
]$interval
abline(v = max_average_steps_interval, lwd = 1, lty = 2, col = "red")
abline(h = max_average_steps, lwd = 1, lty = 2, col = "red")


# Sample of some averages in steps_by_interval:
steps_by_interval[steps_by_interval$interval %in% c(600,1000,2000),]

head(activity[which(is.na(activity$steps) & activity$interval %in% c(600,1000,2000)),], n = 30)


# Calculate and report the total number of missing values
x<-sum(is.na(activity[, 1]))
cat("Number of rows with missing values =",as.character(x))

## This uses the *mean* for a given interval to replace missing steps values (NAs)
## 'Summarized' contains means of steps for each interval (see above)
steps_by_date <- as.data.frame(steps_by_interval)

## Create something to hold activity and imputed activity
Imputed <- activity

## Find rows with missing (NA) data and replace with mean for the interval
for (i in 1:nrow(Imputed)) { 
  if (is.na(Imputed$steps[i])){
    n <- steps_by_date[steps_by_date$interval==Imputed$interval[i],average_steps]
                           Imputed$steps[i]<- as.integer(round(n))
                           }
}

Summarized <- Imputed %>% 
  group_by(date) %>% 
  summarize(imp_total_steps=sum(steps))

steps_by_date_na <- select(main_activity, steps, date, interval) %>%
  #filter(!is.na(steps)) %>% # removed filter to include NAs as 0-step days in histogram
  group_by(date) %>% 
  summarize(total_steps = sum(steps, na.rm = TRUE)) #has the same effect as `!is.na`

steps_by_date <- select(activity, steps, date, interval) %>%
  filter(!is.na(steps)) %>% # removed filter to include NAs as 0-step days in histogram
  group_by(date) %>% 
  summarize(total_steps = sum(steps, na.rm = TRUE)) #has the same effect as `!is.na`

par(mfrow=c(1,3))

hist(steps_by_date_imp$imp_total_steps, 
     main = "Histogram of Inputed Total Steps per Day (Avg/Interval)", 
     xlab = "Total steps per day", 
     ylab = "Frequency [number of days]", 
     breaks = 20,
     border = "slateblue4",
     col = "slateblue3"
)
hist(steps_by_date$total_steps, 
     main = "Histogram of Imputed Total Steps per Day (NAs Removed)",
     xlab = "Total steps per day", 
     ylab = "Frequency [number of days]", 
     breaks = 20,
     border = "cadetblue4",
     col = "cadetblue3"
)
hist(steps_by_date_na$total_steps, 
     main = "Histogram of Total Steps per Day", 
     xlab = "Total steps per day", 
     ylab = "Frequency [number of days]", 
     breaks = 20,
     border = "dodgerblue4",
     col = "dodgerblue3"
)

#Calculate and report the mean and median total number of steps taken per day.

MeanMedian_Imputed <- 
  Imputed %>% 
  group_by(date) %>% 
  summarize(average_steps=mean(steps),"median_of_steps"=median(steps))
MeanMedian_Imputed

#Do these values differ from the estimates from the first part of the assignment?

summary(MeanMedian_Raw,2:3)
summary(MeanMedian_Imputed,2:3)

# Are there differences in activity patterns between weekdays and weekends?

#  For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

# Create the names, doesnt seem to be an R constant available
DayOfWeek <- data.frame(
  name=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
  part=c("weekday","weekday","weekday","weekday","weekday","weekend","weekend"))

Imputed$date<-as.Date(Imputed$date)

## Create the column
Imputed$WeekPart <- as.factor(weekdays(Imputed$date))
## Set the value
Imputed$WeekPart <- DayOfWeek[Imputed$WeekPart,2]

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

## Calculate and store the means by group
PlotSummary <- Imputed %>% 
  group_by(WeekPart,interval) %>% 
  summarize(StepsMean=mean(steps))

## Use the lattice graphics for easier multiple panels
library(lattice)
xyplot(PlotSummary$StepsMean ~ PlotSummary$interval | PlotSummary$WeekPart, 
       data=PlotSummary, type="l", layout=c(1:2),
       ylab="Number of Steps (mean)",
       xlab="Interval")
