library(rminer)
library(rpart)
library(rpart.plot)

# classification demo, using wine quality from UCI:
w1=read.csv("~/universidade/4 ano/1 semestre/SINO/pl/codigo/bank-full.csv",header=TRUE, stringsAsFactors=TRUE)
w1= subset(w1, select = -c(day, month, duration ) )
#mpause("load wines and transform quality into (bad,medium,good):")
#w1$y <- ifelse(w1$y =='yes',1,0)
#w1$y=cut(w1$y, c(-1,0.5,1), c("no","yes"))
#mpause("random selection of 500 examples:")
# select random sample of 500 examples (to speedup execution)
s1=sample(1:nrow(w1),500)
ws1=w1[s1,]
#mpause("transform quality into (bad,medium,good):")
print(summary(ws1))
plot(ws1$y)
#mpause("save transformed data into a new csv: wq3.csv (bad,medium,good):")
write.table(file="wq3.csv",ws1,row.names=FALSE,col.names=TRUE,sep=",") # , is for weka

#mpause("fit of a decision tree:")
dt1=fit(y~.,ws1,model="dt")
#mpause("show decision tree:")
plot(dt1@object);text(dt1@object)
#mpause("show same decision tree in a nicer way:")
rpart.plot(dt1@object)

#mpause("fit a SVM:")
svm1=fit(y~.,ws1,model="ksvm",search="heuristic5")
#mpause("get predictions:")
pdt1=predict(dt1,w1[-s1,])
psvm1=predict(svm1,w1[-s1,])
Y1=w1[-s1,]$y
#mpause("show some dt metrics:")
print(mmetric(Y1,pdt1,metric=c("ACC","AUC","ACCLASS","AUCCLASS")))
print(mmetric(Y1,pdt1,metric="CONF"))
#mpause("show some svm metrics:")
print(mmetric(Y1,psvm1,metric=c("ACC","AUC","ACCLASS","AUCCLASS")))
print(mmetric(Y1,psvm1,metric="CONF"))
#mpause("show ROC for svm:")
mgraph(Y1,psvm1,graph="ROC",baseline=TRUE,leg="yes",Grid=10)
#mpause("show ROC for svm and dt:")
L1=vector("list",2)
testl1=vector("list",1);testl1[[1]]=Y1
p11=vector("list",1);p11[[1]]=psvm1
p21=vector("list",1);p21[[1]]=pdt1
L1[[1]]=list(pred=p11,test=testl1,runs=1)
L1[[2]]=list(pred=p21,test=testl1,runs=1)
mgraph(L1,graph="ROC",baseline=TRUE,leg=c("svm1","dt1"),main="yes",Grid=10)

