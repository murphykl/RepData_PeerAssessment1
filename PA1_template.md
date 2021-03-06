---
title: "Assignment1"
output: html_document
---


First, I am loading and processing the data for analysis. This involves reformatting the date column to be a date rather than a factor, and the counts to numbers rather than integers:

```r
setwd("~/r_coursera/RepData_PeerAssessment1")
activity<-read.csv("activity.csv")
activity$date<-as.Date(activity$date, format="%Y-%m-%d")
activity$steps<-as.numeric(activity$steps)
```

For the first section of the assighment, I am melting the data to allow for aggregation by date and reporting of the mean and median values.


```r
options(digits=2, scipen=5)
library(reshape2)
day<-melt(activity, c("interval","date"))
avday<-dcast(day, date ~., sum)
avday$count<-avday[,2]
mnday<-round(mean(avday$count, na.rm=TRUE), 2)
mdday<-median(avday$count, na.rm=TRUE)
```

The resulting histogram can be found below. Note that the mean value for step counts is ~10766.19 and the median value is 10765


```r
hist(avday$count, breaks=12, col="green", main="Total Steps Per Day", xlab = "step count")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

## Average daily time series data
For this analysis, I am melting the original dataset to allow aggregation by time bin


```r
tim<-melt(activity, id=c("interval","date"))
avtim<-dcast(tim, interval ~., mean, na.rm=TRUE)
avtim$ct<-avtim[,2]
plot(avtim$interval, avtim$ct, type="l", main="Time Series of Average Daily Activity", xlab="interval", ylab="step count")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
bin<-which.max(avtim$ct)
avtim$interval1<-sprintf("%04d",avtim$interval)
maxtim<-avtim[bin,4]
```

The time bin with the greatest activity on average occurs at 0835 hours.

##Imputing missing values

Here I am replacing missing values with the average for that time bin, and calling the new data frame newact1.


```r
actavg<-merge(activity, avtim, by.x="interval", by.y="interval")
actavg[is.na(actavg$steps),]$steps<-actavg[is.na(actavg$steps),]$ct
newact<-activity
newact$steps<-actavg$steps
newact$date<-actavg$date
newact$interval<-actavg$interval
newact1<-newact[order(newact$date),]
```

Now I am repeating the analyses from part one with the dataset containing the imputed values


```r
daynew<-melt(newact1, c("interval","date"))
avdaynew<-dcast(daynew, date ~., sum)
avdaynew$count<-avdaynew[,2]
mndaynew<-round(mean(avdaynew$count, na.rm=TRUE), 2)
mddaynew<-median(avdaynew$count, na.rm=TRUE)
```

The new histogram can be found below. Note that the mean value for step counts is ~10766.19 and the median value is 10766.19


```r
hist(avdaynew$count, breaks=12, col="green", main="Updated Total Steps Per Day", xlab = "step count")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

BONUS: Here is an overlay of the two histograms:


```r
ign<-hist(avday$count, breaks=12, plot=FALSE)
rep<-hist(avdaynew$count, breaks=12, plot=FALSE)
plot(ign, col=rgb(0,0,1,1/4), main="Comparison of step counts", xlab = "step count")
plot(rep, col=rgb(1,0,0,1/4), add=TRUE)
legend('topleft',c('ignoring na values','replaced na values'),
       fill = c(rgb(0,0,1,1/4), rgb(1,0,1,1/4)), bty = 'n',
       border = NA)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

To address the question asked in the assignment, yes, adding in the missing values makes some difference, but because the missing values seem to largely represent entire days and I replaced the missing values with the average for that time bin, it just increases the number of days with the average number of steps. It's questionable whether this is a reasonable data manipulation given the circumstances.

##Weekdays vs. Weekends
To examine whether the weekdays differ in profile from the weekends, I'm adding a weekday column to the original data and will filter by that to create two datasets: one with the weekday time series, and one with the weekend time series.


```r
 activity$day<- ifelse(weekdays(activity$date) %in% c("Saturday", "Sunday") ,"weekend", "weekday")
 weekends<-activity[activity$day=="weekend",]
 we<-melt(weekends, id=c("interval","date", "day"))
  avwe<-dcast(we, interval ~., mean, na.rm=TRUE)
  avwe$ct<-avwe[,2]
  bin<-which.max(avwe$ct)
  avwe$interval1<-sprintf("%04d",avtim$interval)
  maxwe<-avwe[bin,4]
  weekday<-activity[activity$day=="weekday",]
  wd<-melt(weekday, id=c("interval","date", "day"))
  avwd<-dcast(wd, interval ~., mean, na.rm=TRUE)
  avwd$ct<-avwd[,2]
  bin<-which.max(avwd$ct)
  avwd$interval1<-sprintf("%04d",avwd$interval)
  maxwd<-avwd[bin,4]
```

And the creation of the two-panel figure:


```r
par(mfrow=c(2,1), 
    mar = c(2, 2, 1, 0), oma=c(0,0,0,0))
plot(avwe$interval, avwe$ct, type="l", xaxt="n", ylim=c(0,200), main="Time Series of Average Weekend Daily Activity", xlab="", ylab="step count")
  plot(avwd$interval, avwd$ct, type="l", main="Time Series of Average Weekday Daily Activity", xlab="interval", ylab="step count")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

