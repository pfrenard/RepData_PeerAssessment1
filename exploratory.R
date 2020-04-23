library("readr")
library("dplyr")
library("ggplot2")

setwd("~/lang/R/Coursera/Cours5_ReproductibleResearch/RepData_PeerAssessment1")
dataset <- read_delim("activity.zip",delim=",")

avg_by_interval <- dataset %>% group_by(interval) %>% summarise(avg_steps=mean(steps, na.rm=TRUE))
newdataset <- dataset %>% mutate(new_steps=ifelse(is.na(steps),pull(avg_by_interval[interval,2]),steps) )


#sum_by_date <- dataset %>% group_by(date) %>% summarize_all(sum)
#hist(mean_by_date$steps, main="Steps mean by date", xlab="date", ylab="steps mean",breaks=51)

#barplot(sum_by_date$steps, main="Steps by date", xlab="date", ylab="steps")


#p <- ggplot(sum_by_date, aes(x=date,y=steps) ) 
#p <- p + geom_col()
#p <- p + xlab("Dates") + ylab("Steps") 
#p <- p + ggtitle("Sum of steps per day")

#print(p)

#my_mean <- mean(sum_by_date$steps,na.rm=TRUE)
#print(my_mean)


setday <- function(r) {
    dayname <- weekdays(r)
    if (all(dayname == "Saturday") ) {
        ret <- "Weekend" 
    } else if (all(dayname == "Sunday")) {
        ret <- "Weekend" 
    } else {
        ret <- "Weekday"
    }

    ret
}

week_days <- c('Monday','Tuesday','Wednesday','Thursday','Friday')
weekend_days <- c('Saturday','Sunday')
new <- newdataset %>% mutate(day = factor( case_when( weekdays(date) %in% week_days ~ "Weekday",  weekdays(date) %in% weekend_days ~ "Weekend") )) 


#newdataset$day <- setday(newdataset$date)
#newdataset$day <- as.factor(newdataset$day)

#avg_by_interval <- newdataset %>% group_by(interval,day) %>% summarise(avg_steps=mean(new_steps, na.rm=TRUE))



