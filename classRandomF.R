library(rminer)
library(rpart)
library(rpart.plot)
library(ROSE)
# classification demo, using wine quality from UCI:
w <- read.csv("bank-full.csv", sep=";",header=TRUE, stringsAsFactors=TRUE)
w= subset(w, select = -c(day, month, duration) )
#load the target as a no and yes countable 
#w$y <- ifelse(w$y =='yes',1,0)
#w$y=cut(w$y, c(-1,0.5,1), c("no","yes"))
w1 <- w[25001:35000,]
s1=sample(1:15000,1000)
s2=sample(35001:45000,200)
w2 <- w[s2,]


#w1 <- ovun.sample(y~., data=w1, method="both", p=0.5, seed=1)$data

holdoutM=holdout(w1$y, 2/3, seed=1)
# select random sample of 500 examples (to speedup execution)
print(summary(w1))

#mpause("fit a lr")
lr=fit(y~.,w1[holdoutM$tr,],model="lr")
#mpause("get predictions:")



plr=predict(lr,w2)
Y=w2[,]$y

#mpause("show some lr metrics:")
print(mmetric(Y,plr,metric=c("ACC","AUC","ACCLASS","AUCCLASS"), D=0.3, TC=2))
print(mmetric(Y,plr,metric="CONF" ))
#mpause("show ROC for svm:")
mgraph(Y,plr,graph="ROC",baseline=TRUE,leg="yes",Grid=10)

