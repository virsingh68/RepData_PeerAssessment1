# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
require(plyr)
```

```
## Loading required package: plyr
```

```r
activity_df<-read.csv("activity.csv")
full_activity_df<-activity_df
activity_df<-subset(activity_df,!is.na(steps))
daily_steps<-ddply(activity_df,.(date), summarize, sum=sum(steps))
```
## Histogram of total number of step taken per day

```r
hist(daily_steps$sum,main="Daily Steps Histogram",xlab="Daily Steps",breaks=10)
```

![plot of chunk histogram](figure/histogram.png) 

```r
mean_steps_per_day<-mean(activity_df$steps)
median_setps_per_day<-median(activity_df$steps)
total_setps<-sum(activity_df$steps)
```
## What is mean total number of steps taken per day? 

### Mean Value - 37.3826
### Median Value - 0

## What is the average daily activity pattern?


```r
average_steps_interval<-ddply(activity_df,.(interval), summarize, mean_steps=mean(steps))
plot(average_steps_interval$interval,average_steps_interval$mean_steps,type="l",
     main="Average Steps Interval Wise", xlab="5 Minutes Interval",ylab='Average Steps Taken')
```

![plot of chunk averagedailyactivity](figure/averagedailyactivity.png) 



```r
intervalmax<-subset(average_steps_interval,mean_steps==max((average_steps_interval$mean_steps)))$interval
```
## Invterval with maximum average number of steps across all days - 835

## Imputing missing values

```r
na_activity_df<-subset(full_activity_df,is.na(steps))
#Create new data set with replacing NA values with mean steps for the interval
na_activity_df_values<-merge(na_activity_df,average_steps_interval)[,c("mean_steps","date","interval")]
colnames(na_activity_df_values)<-c("steps","date","interval")
activity_df_new<-rbind(activity_df,na_activity_df_values)
# creating historgram of new activity data
hist(daily_steps$sum,main="Daily Steps Histogram with NA values Replaced",xlab="Daily Steps",breaks=10)
```

![plot of chunk imputingmissingvalues](figure/imputingmissingvalues.png) 


```r
mean_setps_per_day_imv<-mean(activity_df_new$steps)
median_setps_per_day_imv<-median(activity_df_new$steps)
total_setps_imv<-sum(activity_df_new$steps)
```
### Mean and Median steps taken per day after imputing missing values with average interval value
### Mean Value - 37.3826
### Median Value -0
Mean and Median values are not impacted by imputing missing values
Total steps taken with imputing missing values 6.5674 &times; 10<sup>5</sup> is greater than
Total steps taken withoput imputing missing values 570608
## Are there differences in activity patterns between weekdays and weekends?

### More steps are taken during weekend vs weekdays across all intervals


```r
library(lattice)
activity_df_new$date1<-as.Date(activity_df_new$date,format="%Y-%m-%d")
activity_df_new$weekday<-weekdays(activity_df_new$date1)
activity_df_new$wd_we<-activity_df_new$weekday
for (i in 1:nrow(activity_df_new)) {
        if (activity_df_new[i,]$weekday %in% c("Saturday","Sunday")){
                activity_df_new[i,]$wd_we="weekend"
        }
        else {activity_df_new[i,]$wd_we="weekday" }
}
wd_we_avg_steps<-ddply(activity_df_new,.(wd_we,interval), summarize, mean=mean(steps))
wd_we_avg_steps<-transform(wd_we_avg_steps,wd_we=factor(wd_we))
xyplot(mean~interval | wd_we,data=wd_we_avg_steps, layout=c(1,2),ylab="Number of Steps",type="l")
```

![plot of chunk weekdayweekend](figure/weekdayweekend.png) 
