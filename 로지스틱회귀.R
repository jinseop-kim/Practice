rm(list=ls())
setwd("~/Desktop/rcode")
library(MASS)
data("biopsy")
str(biopsy)
biopsy$ID=NULL
names(biopsy)<-c("Tich", "u.size", "u.shape", "adhsn", "s.size", "nucl", "chrom", "n.nuc", "mit", "class")
names(biopsy)
biopsy.v2<-na.omit(biopsy)
y<-ifelse(biopsy.v2$class =="malignant", 1,0)
library(reshape2)
library(ggplot2)
biop.m<-melt(biopsy.v2, id.var="class")
ggplot(data=biop.m, aes(x=class, y=value))+ geom_boxplot()+facet_wrap(~variable, ncol=3)
library(corrplot)
bc<-cor(biopsy.v2[,1:9])
corrplot.mixed(bc)

## partition data set. sample 함수를 사용하여 Training set 과 Testing Set 으로 분류
set.seed(123)
ind<-sample(2, nrow(biopsy.v2), replace=T,prob=c(0.7,0.3))
train<- biopsy.v2[ind==1,]
test<-biopsy.v2[ind==2,]
table(train$class)
table(test$class)
full.fit<-glm(class~. , family= binomial, data=train)
summary(full.fit)
confint(full.fit)  #95%신뢰구간 점검
library(car)
vif(full.fit)  #5가 넘는 결과값이 없으므로 다중공선성에는 문제가 없음.
train.probs<-predict(full.fit, type="response")
train.probs[1:5]
library(InformationValue); library(caret)
confusionMatrix(trainY, train.probs)
misClassError(trainY, train.probs)

test.probs<-predict(full.fit, newdata=test, type="response")
confusionMatrix(testY, test.probs)

#K-fold cross validation
install.packages("bestglm")
library(bestglm)
library(leaps)
library(lda)
lda.fit<-lda(class~., data=train)
lda.fit #선형 로지스틱 분석
plot(lda.fit, type="both")
library(earth)
set.seed(1)
earth.fit<-earth(class~., data=train, pmethod="cv", nfold=5, ncross=3, degree=1, ,minspan=-1, glm=list(family=binomial))
summary(earth.fit)










