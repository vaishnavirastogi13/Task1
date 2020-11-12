#PRESDICTION USING SUPERVISED ML

df=read.csv(file.choose(),header=TRUE,sep=",")
df
scores=df[,2]
hours=df[,1]
scatter.smooth(y=scores,x=hours,col="purple",main="plot of data")
#positive correlation as expected

#EDA
par(mfrow=c(1,2))
boxplot(df$Scores,ylab="scores",main="boxplot")
boxplot(df$Hours,ylab="hours of study",main="boxplot")
#no outliers are present

plot(density(scores),main="Scores")
plot(density(hours),main="Hours of study")
#data is roughly normal since the number of obs is small

#Modeling
par(mfrow=c(1,1))
model=lm(scores~hours)
model

plot(x=hours,y=model$fitted.values,xlab="hours",ylab="scores",ylim=c(0,100),main="actual and predicted values")
par(new=TRUE)
plot(hours,scores,xlab="hours",ylab="scores",col="red",ylim=c(0,100),main="actual and predicted values")

summary(model)
#adj rsquared=0.9509
#the explained variance is quite large
model$coefficients
#intercept=2.483673
#slope=9.775803

data.frame(hours,scores,model$fitted.values)
# predicted score for 9.25 hours of study
predict_val=2.483673+9.775803*9.25
predict_val
#92.90985
#if a person studies 9.25 hrs a day, the predicted score is 92.91
