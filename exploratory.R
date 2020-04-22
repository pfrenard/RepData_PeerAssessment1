library("readr")

setwd("~/lang/R/Coursera/Cours5_ReproductibleResearch/RepData_PeerAssessment1")
dataset <- read_delim("activity.zip",delim=",")

summary(dataset)

library("dplyr")

sum_by_date <- dataset %>% group_by(date) %>% summarize_all(sum)
#hist(mean_by_date$steps, main="Steps mean by date", xlab="date", ylab="steps mean",breaks=51)

barplot(sum_by_date$steps, main="Steps by date", xlab="date", ylab="steps")


library("ggplot2")
p <- ggplot(sum_by_date, aes(x=date,y=steps) ) 
p <- p + geom_col()
p <- p + xlab("Dates") + ylab("Steps") 
p <- p + ggtitle("Sum of steps per day")

print(p)

my_mean <- mean(sum_by_date$steps,na.rm=TRUE)
print(my_mean)