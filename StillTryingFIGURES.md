Untitled
================

R Markdown
----------

This is an R Markdown document
==============================

Loading and preprocessing the data

Show any code that is needed to

\*\* 1.Load the data (i.e. read.csv() )

Read in file and view

``` r
setwd("~/R")
Data <- read.csv("Activity.csv")
head(Data)
```

    ##   steps      date interval
    ## 1    NA 10/1/2012        0
    ## 2    NA 10/1/2012        5
    ## 3    NA 10/1/2012       10
    ## 4    NA 10/1/2012       15
    ## 5    NA 10/1/2012       20
    ## 6    NA 10/1/2012       25

View Data Types

``` r
str(Data)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "10/1/2012","10/10/2012",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

\*\* 2.Process/transform the data (if necessary) into a format suitable for your analysis

``` r
#Transforming date into a "date format"
Data$date <- as.Date(Data$date, "%m/%d/%Y")
str(Data)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

I am going to remove the entries where steps = NA and create a data set to build by histogram from called StepByDay

``` r
StepByDay <- aggregate(steps ~ date, data=Data, sum, na.rm = TRUE)

head(StepByDay)
```

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

\*\* 1.Make a histogram of the total number of steps taken each day

\*\* 2.Calculate and report the mean and median total number of steps taken per day

``` r
hist(StepByDay$steps, col = "green", breaks = 20, main = "Steps By Day", xlab = "Steps", ylab = "Days")
```

![](StillTryingFIGURES_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
mean(StepByDay$steps)
```

    ## [1] 10766.19

``` r
median(StepByDay$steps)
```

    ## [1] 10765

``` r
J<- mean(StepByDay$steps)
hist(StepByDay$steps, col = "green", breaks = 20, main = "Steps By Day", xlab = "Steps (mean in blue)", ylab = "Days")
abline(v=J, col = "blue", lwd=3)
```

![](StillTryingFIGURES_files/figure-markdown_github/unnamed-chunk-5-2.png)

What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l" ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
IntData <- aggregate (steps ~ interval, data = Data, mean, na.rm = TRUE)

## Let's look at it
dim(IntData)
```

    ## [1] 288   2

``` r
head(IntData)
```

    ##   interval     steps
    ## 1        0 1.7169811
    ## 2        5 0.3396226
    ## 3       10 0.1320755
    ## 4       15 0.1509434
    ## 5       20 0.0754717
    ## 6       25 2.0943396

``` r
plot(IntData$interval, IntData$steps, type = "l", col = "green", lwd = 2, main = "Average Daily Activity Pattern by Interval", xlab = "Interval", ylab = "STEPS")
```

![](StillTryingFIGURES_files/figure-markdown_github/unnamed-chunk-6-1.png)

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
head(IntData)
```

    ##   interval     steps
    ## 1        0 1.7169811
    ## 2        5 0.3396226
    ## 3       10 0.1320755
    ## 4       15 0.1509434
    ## 5       20 0.0754717
    ## 6       25 2.0943396

``` r
SortIntData <- IntData[order(-IntData$steps), ]
head(SortIntData)
```

    ##     interval    steps
    ## 104      835 206.1698
    ## 105      840 195.9245
    ## 107      850 183.3962
    ## 106      845 179.5660
    ## 103      830 177.3019
    ## 101      820 171.1509

``` r
## Here we go :-)
print(paste("The interval with the most steps is ", SortIntData[1,1], " it has ", SortIntData[1,2], " steps"))
```

    ## [1] "The interval with the most steps is  835  it has  206.169811320755  steps"

Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA ). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA s)

``` r
sum(is.na(Data$steps))
```

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the dataset. \*\*\* We will use the mean values \*\*\*

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
AllData <- Data
head(AllData)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
NAs <- is.na(AllData$steps)

Average <- tapply(AllData$steps, AllData$interval, mean, na.rm=TRUE)

AllData$steps[NAs] <- Average[as.character(AllData$interval[NAs])]

## Look to see it worked
head(AllData)
```

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

1.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
StepByDay2 <- aggregate(steps ~ date, data=AllData, sum, na.rm = TRUE)

head(StepByDay2)
```

    ##         date    steps
    ## 1 2012-10-01 10766.19
    ## 2 2012-10-02   126.00
    ## 3 2012-10-03 11352.00
    ## 4 2012-10-04 12116.00
    ## 5 2012-10-05 13294.00
    ## 6 2012-10-06 15420.00

``` r
hist(StepByDay2$steps, col = "orangered", breaks = 20, main = "Steps By Day without NA", xlab = "Steps", ylab = "Days")
```

![](StillTryingFIGURES_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
mean(StepByDay2$steps)
```

    ## [1] 10766.19

``` r
median(StepByDay2$steps)
```

    ## [1] 10766.19

``` r
## The impact of imputing is that the mean and median are now the same value.
```

Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part. 1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
head(AllData)
```

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

``` r
# Adding Day indicator
AllData$Day <- weekdays(AllData$date)
head(AllData)
```

    ##       steps       date interval    Day
    ## 1 1.7169811 2012-10-01        0 Monday
    ## 2 0.3396226 2012-10-01        5 Monday
    ## 3 0.1320755 2012-10-01       10 Monday
    ## 4 0.1509434 2012-10-01       15 Monday
    ## 5 0.0754717 2012-10-01       20 Monday
    ## 6 2.0943396 2012-10-01       25 Monday

``` r
## Adding Weekend by Weekday indicator
AllData$Type <- as.factor(ifelse(AllData$Day == "Saturday" | AllData$Day == "Sunday", "Weekend", "Weekday"))
head(AllData)
```

    ##       steps       date interval    Day    Type
    ## 1 1.7169811 2012-10-01        0 Monday Weekday
    ## 2 0.3396226 2012-10-01        5 Monday Weekday
    ## 3 0.1320755 2012-10-01       10 Monday Weekday
    ## 4 0.1509434 2012-10-01       15 Monday Weekday
    ## 5 0.0754717 2012-10-01       20 Monday Weekday
    ## 6 2.0943396 2012-10-01       25 Monday Weekday

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
DayType4Plot <- aggregate(steps~ interval + Type, AllData, mean)
head(DayType4Plot)
```

    ##   interval    Type      steps
    ## 1        0 Weekday 2.25115304
    ## 2        5 Weekday 0.44528302
    ## 3       10 Weekday 0.17316562
    ## 4       15 Weekday 0.19790356
    ## 5       20 Weekday 0.09895178
    ## 6       25 Weekday 1.59035639

``` r
library(lattice)

xyplot(DayType4Plot$steps ~ DayType4Plot$interval | DayType4Plot$Type, DayType4Plot, type = "l", layout = c(1,2), xlab = "5 Minute Interval", ylab = "Steps")
```

![](StillTryingFIGURES_files/figure-markdown_github/unnamed-chunk-12-1.png)
