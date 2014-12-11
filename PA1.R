## Week5 - Assessment 1
## Read File
amdata<-read.csv("activity.csv")
##Convert date to date type
amdata[,"date"]<-as.Date(amdata[,"date"],"%Y-%m-%d")

##Get Complete Cases
cc<-complete.cases(amdata)
ccdata<-amdata[cc,]

library("doBy")
##Use SummaryBy to get the total steps by day
sumbyday<-summaryBy(list(c("steps"),c("date")),ccdata,FUN=c(sum))
sumbydayWithNA<-summaryBy(list(c("steps"),c("date")),amdata,FUN=c(sum))

##Change the column name
names(sumbyday)[2]<-"Total Steps per Day"
names(sumbydayWithNA)[2]<-"Total Steps per Day"

#What is mean total number of steps taken per day?

##1.Make a histogram of the total number of steps taken each day

plot(sumbyday,type="h")



##2.Calculate and report the mean and median total number of steps taken per day
mean(sumbyday[,"Total Steps per Day"])
median(sumbyday[,"Total Steps per Day"])

mean(sumbydayWithNA[,"Total Steps per Day"],na.rm=T)
median(sumbydayWithNA[,"Total Steps per Day"],na.rm=T)


#What is the average daily activity pattern?
avgsteps5minInterval<-summaryBy(list(c("steps"),c("interval")),amdata,FUN=c(mean),na.rm=T)

##1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
##and the average number of steps taken, averaged across all days (y-axis)
plot(avgsteps5minInterval,type="l")

##2.Which 5-minute interval, on average across all the days in the dataset, 
##contains the maximum number of steps?
subset(avgsteps5minInterval,avgsteps5minInterval$steps.mean==max(avgsteps5minInterval[,2]))[,"interval"]

#################

#Imputing missing values

##Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

##1.Calculate and report the total number of missing values in the dataset 
##(i.e. the total number of rows with NAs)
length(subset(amdata,is.na(amdata$steps))[,1])


##2.Devise a strategy for filling in all of the missing values in the dataset.
##The strategy does not need to be sophisticated. For example, you could use 
##the mean/median for that day, or the mean for that 5-minute interval, etc.

##3.Create a new dataset that is equal to the original dataset but with the 
##missing data filled in.

##make copy of original data
amdataClean<-amdata
##find matching averages based on interval
m<-match(amdataClean[!cc,"interval"],avgsteps5minInterval$interval)
##apply the averages to the data frame
amdataClean[!cc,1]<-avgsteps5minInterval[m,][,2]




##4.Make a histogram of the total number of steps taken each day and Calculate 
##and report the mean and median total number of steps taken per day. Do these values differ 
##from the estimates from the first part of the assignment? What is the impact of imputing 
##missing data on the estimates of the total daily number of steps?

##Use SummaryBy to get the total steps by day
sumbydayClean<-summaryBy(list(c("steps"),c("date")),amdataClean,FUN=c(sum))

##Change the column name
names(sumbydayClean)[2]<-"Total Steps per Day"

#What is mean total number of steps taken per day?

##1.Make a histogram of the total number of steps taken each day

plot(sumbydayClean,type="h")



##2.Calculate and report the mean and median total number of steps taken per day
mean(sumbydayClean[,"Total Steps per Day"])
median(sumbydayClean[,"Total Steps per Day"])

################

###############
##Are there differences in activity patterns between weekdays and weekends?

#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

##1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
amdataClean<-cbind(amdataClean,wkday=factor(as.POSIXlt(amdataClean$date)$wday %in% c(0,6),levels = c(TRUE,FALSE),labels=c("weekend","weekday")))

#Group data by interval,weekday
avgStepsByIntervalWkday<-summaryBy(list(c("steps"),c("interval","wkday")),amdataClean,FUN=c(sum))

##2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
library(lattice)
xyplot(steps.sum~interval|wkday,data=avgStepsByIntervalWkday,type="l",layout=c(1,2),xlab="interval",ylab="average steps")

##############
