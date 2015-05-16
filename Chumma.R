library(stringr)
library(lubridate)


filename <- "activity/activity.csv"
Data <- read.csv(filename, header = TRUE)

Num.missing.values <- length(which(is.na(Data$steps)))

## Remove NA values
Data2 <- Data[complete.cases(Data), ]

spData2 <- split(Data2$steps, Data2$date)
sum.of.steps.per.day <- sapply(spData2, sum) 

hist(sum.of.steps.per.day)

mean.steps <- mean(sum.of.steps.per.day)
median.steps <- median(sum.of.steps.per.day)


spData2 <- split(Data2$steps, Data2$interval)
steps.per.interval <- sapply(spData2, mean)

y <- as.numeric(names(steps.per.interval)) 

mins <- vector("character", length(y))
hours <- vector("character", length(y))
time <- vector("character", length(y))

for (i in 1:length(y)) {
  
  if (y[i] < 100) {
    
    mins[i] <- as.character(y[i])
    hours[i] <- "00"
    time[i] <- paste(hours[i], ":", mins[i], ":", "00", sep="")
    
    
  } else if (y[i] < 1000) {
    
    zz <- as.character(y[i])
    hours[i] <- str_sub(zz, start=1, end=1)
    mins[i] <- str_sub(zz, start=2)  
    time[i] <- paste("0", hours[i], ":", mins[i], ":", "00", sep = "")      
    
  } else {
    
    zz <- as.character(y[i])
    hours[i] <- str_sub(zz, start=1, end=2)
    mins[i] <- str_sub(zz, start=3)
    time[i] <- paste(hours[i], ":", mins[i], ":", "00", sep="")      
    
  }
  
}

Time <- strptime(time, "%H:%M:%S")

plot(Time, steps.per.interval, type="l", main = "Average daily activity pattern", ylab = "Avg. no. of steps over 2 month period")
ind <- which(steps.per.interval == max(steps.per.interval))
time_max <- names(steps.per.interval[ind])


## Imputing the missing values

for (i in 1:length(Data$steps))  {
  
    if (is.na(Data$steps[i])) {
      
        j <- length(Time)
        index <- i%%j
        
        if (index != 0) {
          
            Data$steps[i] <- steps.per.interval[index]
      
        } else {
          
            Data$steps[i] <- steps.per.interval[j]
          
        }
        
    }
  
}


y <- as.Date(Data$date)

inds.wkends <- which(weekdays(y) == "Saturday" | weekdays(y) == "Sunday")
inds.wkdays <- which(weekdays(y) != "Saturday" & weekdays(y) != "Sunday")

daytype <- vector("character", length(y))
daytype[inds.wkdays] = "Weekdays"
daytype[inds.wkends] = "weekends"

dayType <- data.frame(daytype = daytype)
Data.V2 <- cbind(Data, dayType)

Data.V2$daytype <- as.factor(Data.V2$daytype)
spData3 <- split(Data.V2$steps, Data.V2$daytype)

#average steps: weekdays vs weekends
mean.daytype <- sapply(spData3, mean)
print(mean.daytype)

#average steps per time interval: weekdays vs weekends
spData4 <- split(Data.V2, Data.V2$daytype)


#1. Weekdays

weekd <- spData4[[1]]
spData5 <- split(weekd$steps, weekd$interval)
avg.wd.interval <- sapply(spData5, mean) 

#.2. weekends
weeknd <- spData4[[2]]
spData6 <- split(weeknd$steps, weeknd$interval)
avg.wnd.interval <- sapply(spData6, mean)

par(mfrow = c(2,1), mar = c(3,4,2,2) + 0.1)
plot(Time, avg.wd.interval, type = "l", main = "Weekdays", cex.main = 0.8, ylim = c(0,250), ylab = "Average steps over 2 months", cex.lab = 0.7, cex.axis = 0.7)
plot(Time, avg.wnd.interval, type = "l", main = "Weekends", cex.main= 0.8, ylim = c(0,250), ylab = "Average steps over 2 months", cex.lab = 0.7, cex.axis = 0.7)



