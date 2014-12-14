library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
require(devtools)
install_github('rCharts', 'ramnathv')
library(rCharts)
library(reshape2)
library(lattice)


####################################################
###Loading and preprocessing the data

Show any code that is needed to



##Load the data (i.e. read.csv())

getfile <- tempfile()
download.file ("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" , getfile)
ac = read.csv(unz(getfile, "activity.csv"))
unlink(getfile)

###############################################################

##Process/transform the data (if necessary) into a format suitable for your analysis

#Changing date to actual date 
str(ac)
ac$Date = as.Date(ac$date)

## I code  transfrom the interval as a massive factor variable but decided to leave as is

###########################################################

##What is mean total number of steps taken per day?
ac_days = ac %>% group_by(Date) %>% 
  summarize( D_steps = sum(steps, na.rm=T)) 

#Make a histogram of the total number of steps taken each day

ggplot(ac_days, aes(x=D_steps)) + geom_histogram(bindwidth=1 , colour="black", fill="orange" ,  position="identity") +
 geom_vline(aes(xintercept=mean(D_steps, na.rm=T)),  color="black", linetype="dashed", size=1) +
 geom_vline(aes(xintercept=median(D_steps, na.rm=T)),  color="blue", linetype="dashed", size=1) +
  xlab("Steps Per Day") + ylab("Count") + ggtitle("Total Steps a Day") +
  theme_bw()
  
#Calculate and report the mean and median total number of steps taken per day

# The median and mean are listed above in the histogram.
# The mean is colored in black and the median is colored in blue
# Here they are in actual hard numbers
mean(ac_days$D_steps)
median(ac_days$D_steps)

###########################################################
#What is the average daily activity pattern?

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of 
#steps taken, averaged across all days (y-axis)

time = ac2 %>% select(interval, Steps ) %>% group_by (interval) %>%
  summarize( Avg_Steps = mean(Steps , na.rm=T)) %>%
  arrange(desc(interval)) 

time_P  = xPlot(Avg_Steps ~ interval, data=time ,type="line")

time_P$show('iframesrc',cdn=TRUE)

##The above chart has the 5 Minute Interval as the horizontal axis and the Average steps per day
## as the vertical axis. Can't wait for Rcharts to make labeling easier.

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
Max_num = max(time$Avg_Steps)

time %>% filter( Avg_Steps ==  Max_num ) %>% select(interval)

########################################################################
#Imputing missing values


##Calculate and report the total number of missing values in the dataset
sum(is.na(ac))

## 13% of the data has missing values

##Devise a strategy for filling in all of the missing values in the dataset. 
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
## Normally I would just exclude or if modeling, I would create a new var MISSING
## Since this is a peer review exercise imputing with the mean

#ac_copy$steps[is.na(ac_copy$steps)] = mean(ac_copy$steps, na.rm=TRUE)

miss_val = merge(ac, time , by ="interval", sort=F )
miss_val$Steps = ifelse( miss_val$steps == 0 , miss_val$Avg_Steps, miss_val$steps)
miss_val$steps = NULL ; miss_val$Avg_Steps = NULL;

#head(miss_val) ; head(ac)
#sum(is.na(miss_val))
#names(miss_val)

#Make a histogram of the total number of steps taken each day and Calculate 
#and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

miss_val %>% group_by(Date) %>% 
  summarize( D_steps = sum(Steps, na.rm=T))  %>%
ggplot( aes(x=D_steps)) + geom_histogram(bindwidth=1 , colour="black", fill="orange" ,  position="identity") +
 geom_vline(aes(xintercept=mean(D_steps, na.rm=T)),  color="black", linetype="dashed", size=1) +
 geom_vline(aes(xintercept=median(D_steps, na.rm=T)),  color="blue", linetype="dashed", size=1) +
  xlab("Steps Per Day") + ylab("Count") + ggtitle("Total Steps a Day") +
  theme_bw()
  
## Here are the new mean and median
ac2 = miss_val %>% select(Date, Steps) %>% group_by(Date)

mean(ac2$Steps, na.rm=T) ; median(ac2$Steps, na.rm=T)

## Comparing to what was in the orginal with NAs
mean(ac$steps, na.rm=T) ; median(ac$steps, na.rm=T)

###########################################################################
#Are there differences in activity patterns between weekdays and weekends?

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating 
miss_val$Wk = weekdays(miss_val$Date)
table(miss_val$Wk)

miss_val$Wkday = factor(ifelse(miss_val$Wk %in% c("Saturday" , "Sunday"), "Weekend", "Weekday"))
table(miss_val$Wk , miss_val$Wkday)

Wks = miss_val %>% select(Wkday, interval, Steps) %>% 
  group_by(Wkday, interval) %>%
  summarize( W_steps = mean(Steps, na.rm=T)) 
  
xyplot( W_steps~ interval | Wkday, data=Wks, layout=c(1,2), 
        type="l",    xlab = "Interval",  ylab = "Number of steps")



