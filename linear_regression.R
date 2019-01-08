setwd("~/Desktop/rcode")
data(anscombe)
attach(anscombe)
par(mfrow=c(2,2))
plot(x1,y1, main="plot1")
plot(x2,y2, main="plot2")
plot(x3, y3, main="plot3")
plot(x4, y4, main="plot4")
#########################
install.packages("alr3")
library(alr3)
data(snake)
cor(snake$X, snake$Y)
names(snake)<-c("Content", "yield")
plot(snake$Content, snake$yield, main="snake x and y")
line.snake<-lm(yield~Content, data=snake)
abline(line.snake)
summary(line.snake)
qqPlot(line.snake)
##########################
data(water)
str(water)
social.water<-water[,-1] # 행은 다 포함 -1 즉 일열을 빼고
head(social.water)
install.packages("corrplot")
library(corrplot) ######correlation plot
water.cor<-cor(social.water)
water.cor
corrplot(water.cor, method="ellipse")
pairs(~., data=social.water)
install.packages("leaps")
library(leaps)
fit<-lm(BSAAM~., data=social.water)
summary(fit)
sub.fit<-regsubsets(BSAAM~., data=social.water) ### 어떤 variables을 추가할지 regsubsets을 이용함
best.summary<-summary(sub.fit)
names(best.summary)
which.min(best.summary$rss)
plot(best.summary$cp, xlab="number of features", ylab="cp") #features 가 3일떄 낮기에 3개를 선택
plot(sub.fit, scale="Cp")
best.fit<-lm(BSAAM~APSLAKE+OPRC+OPSLAKE, data=social.water)
plot(best.fit)
vif(best.fit)  #1이 최소값이며 이 경우 공선성이 없다. 5를 넘으면 문제가 있다고 봄
#위의 경우에서 oprc 와 opslake 간에 공선성이 있을 가능성을 발견
plot(social.water$OPRC, social.water$OPSLAKE, xlab="oprc", ylab="opslake")
abline(social.water$OPSLAKE~social.water$OPRC)
library(lmtest)
bptest(best.fit)

fit.lm2<-lm(BSAAM~APSLAKE+OPRC, data=social.water)
summary(fit.lm2)
vif(fit.lm2)
bptest(fit.lm2)

fit.lm3<-lm(BSAAM~APSLAKE+OPSLAKE, data=social.water)
summary(fit.lm3)
vif(fit.lm3)
bptest(fit.lm3)  # p-value 공분산성이 존재하지 않는다는 결론을 내릴수 있음
par(mfrow=c(1,1))
plot(fit.lm3$fitted.values, social.water$BSAAM, xlab="predicted", ylab="actual")
social.water["Actual"]=water$BSAAM  #각각을 다른 백터로 변환 abline이 아니라
social.water$Forecast = predict(fit.lm3)
library(ggplot2); library(dplyr)
social.water %>% ggplot(aes(x=Forecast, y=Actual))+geom_point()+geom_smooth(method=lm)
