The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data \[52K\] The variables included in this
dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are
coded as NA) date: The date on which the measurement was taken in
YYYY-MM-DD format interval: Identifier for the 5-minute interval in
which measurement was taken.

1.  Loading the data

<!-- -->

    if (!"file.zip" %in% dir()){
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url, destfile = "file.zip")
    unzip("file.zip", exdir = "data")
    }
    filepath <- paste(getwd(), "data", dir("data")[1], sep = "/")
    r <- read.csv(filepath)

1.  Histogram of the total number of steps taken each day

<!-- -->

    agg <- with(r, aggregate(steps~date, FUN = sum))
    hist(agg$steps, breaks = 20, col = "green", main = "Total steps per day(NAs ignored)", xlab = "Step")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

1.  Mean and median number of steps taken each day

<!-- -->

    data.frame(mean = mean(agg$steps), median = median(agg$steps))

    ##       mean median
    ## 1 10766.19  10765

1.  Time series plot of the average number of steps taken

<!-- -->

    agg2 <- with(r, aggregate(steps~interval, FUN = mean, rm.na = T))
    library(ggplot2)
    gg <- ggplot(agg2, aes(interval, steps)) + geom_line(col = "brown") +
        labs(y = "Time in a day", title = "Average steps per day")
    gg

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

1.  The 5-minute interval that, on average, contains the maximum number
    of steps

<!-- -->

    agg2[which(agg2$steps == max(agg2$steps, na.rm = T)),]

    ##     interval    steps
    ## 104      835 206.1698

1.  Code to describe and show a strategy for imputing missing data a.The
    number of missing values

<!-- -->

    paste("The number of missing values is", sum(is.na(r$steps)))

    ## [1] "The number of missing values is 2304"

b.Replace the missing values with the median of the non-missing values
which is 0

    r1 <- r
    r1[is.na(r1$steps),]$steps <- median(r1[!is.na(r1$steps),]$steps)

c.Histogram of the total number of steps taken each day after missing
values are imputed

    agg3 <- with(r1, aggregate(steps~date, FUN = sum))
    hist(agg3$steps, breaks = 20, col = "green", main = "Total steps per day(0 for NAs)", xlab = "Step")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

d.Calculate the mean and median total number of steps taken per day

    paste("The mean is", mean(agg3$steps))

    ## [1] "The mean is 9354.22950819672"

    paste("The meadian is", median(agg3$steps))

    ## [1] "The meadian is 10395"

e.The mean and the median both decrease because the missing values are
replaced with 0

1.  Panel plot comparing the average number of steps taken per 5-minute
    interval across weekdays and weekends

<!-- -->

    r2 <- r
    r2$date <- as.POSIXct(r2$date, format = "%Y-%m-%d")
    r2.0 <- cbind(r2, "day" = (weekdays(r2$date)))
    r2.0$day <- as.character(r2.0$day)
    ## Group the weekday and weekend
    r2.0[which(r2.0$day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")), "day"] = "Weekday"
    r2.0[which(r2.0$day %in% c("Saturday", "Sunday")), "day"] = "Weekend"
    ## Calculate the mean for weekdays and weekends at the same time
    library(dplyr)
    r3 <- group_by(r2.0, day, interval) %>% summarise(mean = mean(steps, na.rm = T)) %>% data.frame
    head(r3, 5)

    ##       day interval      mean
    ## 1 Weekday        0 2.3333333
    ## 2 Weekday        5 0.4615385
    ## 3 Weekday       10 0.1794872
    ## 4 Weekday       15 0.2051282
    ## 5 Weekday       20 0.1025641

    g_plot <- ggplot(r3, aes(interval, mean, col = day)) + 
        geom_line() + 
        facet_grid(day~.,) + 
        labs(y = "steps", color = "day in a week", title = "The average number of steps in one day ")
    g_plot

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)
