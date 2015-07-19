# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
if(!file.exists("./activity.csv") & file.exists("./activity.zip"))
{
  unzip("activity.zip", setTimes=T)
}
rawdata <- read.csv("activity.csv", colClasses=c("integer", "Date", "factor"))
```

## What is mean total number of steps taken per day?


```r
spd <- tapply(rawdata$steps, rawdata$date, sum)

library(lattice)
histogram(spd, type="count", nint=45, na.rm=T, xlab="steps per day", ylab="frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
meanspd <- round(mean(spd, na.rm=T), 2)
medspd <- median(spd, na.rm=T)
```

The mean number of steps taken by this subject per day is 1.076619\times 10^{4}.  The median number of steps taken per day by this subject is 1.0765\times 10^{4}.

## What is the average daily activity pattern?


```r
intmeansteps <- sapply(split(rawdata$steps, rawdata$interval), function(x) {round(mean(x, na.rm=T), 2)})

intmeansteps2 <- data.frame(interval = as.numeric(names(intmeansteps)), meansteps = intmeansteps)
intmeansteps2 <- intmeansteps2[order(intmeansteps2[,1]),]

xyplot(meansteps ~ interval, data=intmeansteps2, type="l", xlab="Time", ylab="Mean Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maxmeaninterval <- names(intmeansteps[which.max(intmeansteps)])
```

The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps for this subject is 835


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
