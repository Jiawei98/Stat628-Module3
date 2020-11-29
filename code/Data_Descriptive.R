setwd("C:/Users/13989/Desktop/Stat628-Module3/data")  #set your own path
data=read.csv("burger_noreview.csv")
dim(data) #65154 reviews
head(data)
data[1,]
length(unique(data$address)) #1405 burger businesses
65154/1405 #an average of 46 reviews for a burger business
colnames(data)
summary(data$stars_x)
boxplot(data$stars_x)
hist(data$stars_x, main="Histogram of the Review Scores", xlab="Review Score")
hist(data$stars_x, main="Histogram of the Business Scores", xlab="Business Score")

