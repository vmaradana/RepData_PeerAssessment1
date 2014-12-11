## Loading and preprocessing the data
## Read File
amdata<-read.csv("activity.csv")
##Convert date to date type
amdata[,"date"]<-as.Date(amdata[,"date"],"%Y-%m-%d")

## What is mean total number of steps taken per day?
##Summarize data using doBy package to produce the histogram

library("doBy")
##Use SummaryBy to get the total steps by day
sumbydayWithNA<-summaryBy(list(c("steps"),c("date")),amdata,FUN=c(sum))
##Change the column name
names(sumbydayWithNA)[2]<-"Total steps per day"


##1.Make a histogram of the total number of steps taken each day
##Use the base plotting systemm to produce a histogram
plot(sumbydayWithNA,type="h",main="Total number of steps taken each day")



##2.Calculate and report the mean and median total number of steps taken per day, exclude NAs
## Mean before imputation
meanBeforeImpute<-mean(sumbydayWithNA[,"Total steps per day"],na.rm=T)

meanBeforeImpute

## Median before imputation

medianBeforeImpute<-median(sumbydayWithNA[,"Total steps per day"],na.rm=T)
medianBeforeImpute

## What is the average daily activity pattern?
##1.Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

##calculate the average steps across all days for each 5 min interval using the summaryBy functuion in doBy package
avgsteps5minInterval<-summaryBy(list(c("steps"),c("interval")),amdata,FUN=c(mean),na.rm=T)
##generate a line plot
plot(avgsteps5minInterval,type="l",ylab="Avg Steps across all days",xlab="5 min interval", main="Avg steps(across all days) per 5 min interval")


##2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

##Use max function to find the 5-minute interval with maximum number of steps
subset(avgsteps5minInterval,avgsteps5minInterval$steps.mean==max(avgsteps5minInterval[,2]))[,"interval"]



## Imputing missing values
##1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

##use the is.na method to get subset of missing records
length(subset(amdata,is.na(amdata$steps))[,1])



##2.Devise a strategy for filling in all of the missing values in the dataset.
##Strategy: Make a copy of the exisiting data, indentify the observations with NA and substitute with respective 5 min interval averages

##make copy of original data
amdataClean<-amdata

##Get Complete Cases
cc<-complete.cases(amdata)

##find matching averages based on interval
m<-match(amdataClean[!cc,"interval"],avgsteps5minInterval$interval)

##apply the averages to the data frame
amdataClean[!cc,1]<-avgsteps5minInterval[m,][,2]




##4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

## Histogram of the total number of steps taken each day

##Use SummaryBy to get the total steps by day
sumbydayClean<-summaryBy(list(c("steps"),c("date")),amdataClean,FUN=c(sum))

##Change the column name
names(sumbydayClean)[2]<-"Total steps per day"

##What is mean total number of steps taken per day?
plot(sumbydayClean,type="h")

##  Mean and median total number of steps taken per day after imputation
##Mean After Imputation
meanAfterImpute<-mean(sumbydayClean[,"Total steps per day"])
meanAfterImpute

##Median After Imputation
medianAfterImpute<-median(sumbydayClean[,"Total steps per day"])
medianAfterImpute

## Compare before and After
cdf<-data.frame(cbind(c("mean","median"),c(meanBeforeImpute,medianBeforeImpute),c(meanAfterImpute,medianAfterImpute)))
names(cdf)<-c("Measure", "Before Impute ", "After Impute")
cdf

## Are there differences in activity patterns between weekdays and weekends?
##For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
##1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
amdataClean<-cbind(amdataClean,wkday=factor(as.POSIXlt(amdataClean$date)$wday %in% c(0,6),levels = c(TRUE,FALSE),labels=c("weekend","weekday")))

##Group data by interval,weekday
avgStepsByIntervalWkday<-summaryBy(list(c("steps"),c("interval","wkday")),amdataClean,FUN=c(sum))

##2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
library(lattice)
xyplot(steps.sum~interval|wkday,data=avgStepsByIntervalWkday,type="l",layout=c(1,2),xlab="interval",ylab="average steps")
