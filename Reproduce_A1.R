setwd("c://temp")
activity <- read.csv("activity.csv")

activity_nona <- na.omit(activity)
daysum<-aggregate(activity_nona$steps,by=list(activity_nona$date), FUN=sum)
barplot(daysum[,2],names.arg=daysum[,1],main="Steps by Date (NA removed)", col="blue", xlab="Date", ylab="Steps", ylim=c(0,25000))
box()
mean_act<-mean(daysum[,2])
median_act<-median(daysum[,2])
segments(-5,mean_act,70,mean_act,col="Green")
segments(-5,median_act,70,median_act,col="Red")

timesum<-aggregate(activity_nona$steps,by=list(activity_nona$interval), FUN=sum)
plot(timesum[,1],timesum[,2],type="l",main="Steps by Time",xlab="Time", ylab="Steps", col="dark red")

missingvalues = dim(activity)[1]-dim(activity_nona)[1]

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
activity1 <- ddply(activity, ~interval, transform, steps  = impute.mean(steps))

daysum1<-aggregate(activity1$steps,by=list(activity1$date), FUN=sum)
barplot(daysum1[,2],names.arg=daysum1[,1],main="Steps by Date (NA replaced)", col="blue", xlab="Date", ylab="Steps", ylim=c(0,25000))
box()
mean_act1<-mean(daysum1[,2])
median_act1<-median(daysum1[,2])

weekday <- as.POSIXlt(as.Date(activity1$date))$wday
activity1$weekend <- (weekday%%6==0)

par(mfrow = c(2,1), mar=c(6,8,2,4) )

activity_weekend <- subset(activity1,weekend==TRUE)
activity_weekday <- subset(activity1,weekend==FALSE)

timesum_weekend<-aggregate(activity_weekend$steps,by=list(activity_weekend$interval), FUN=sum)
plot(timesum_weekend[,1],timesum_weekend[,2],type="l",main="Weekend", ylab="Steps", col="blue", lwd=2, xlab="", ylim=c(0,10000))

timesum_weekday<-aggregate(activity_weekday$steps,by=list(activity_weekday$interval), FUN=sum)
plot(timesum_weekday[,1],timesum_weekday[,2],type="l",main="Weekday",xlab="Interval", ylab="Steps", col="blue", lwd=2)


