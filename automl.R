library(rminer)
library(ROSE)
# classification demo, using wine quality from UCI:
w1=read.csv("bank-full.csv", sep=";",header=TRUE, stringsAsFactors=TRUE)
w1= subset(w1, select = -c(day, month, duration ) )




wr <- w1[30000:40000,]
s2=sample(35000:45000,200)
w2 <- w1[s2,]

wr <- ovun.sample(y~., data=wr, method="both", p=0.5, seed=1)$data

#wr <- holdout(wr$y, ratio = 2/3, mode = "stratified")

table(wr$y)



print(summary(wr))



print("### automl3 (might provide better results, requires more computation) ###")
# use of auto-ml, mode 3: "automl3"
inputs=ncol(wr)-1 # number of inputs, needed for random forest
metric="AUC" 

sm3=mparheuristic(model="automl3",task="prob",inputs=inputs)
# internal validation method (used within the training data)
imethod=c("kfold",3,123)
search3=list(search=sm3,smethod="auto",method=imethod,metric=metric,convex=0)

# external validation method:
emethod=holdout(wr$y,2/3,seed=12345) # object with tr and ts indexes
# emethod$tr - training data rows

# execution of 1 run of the fast automl1:
print("automl3 fit:")
M3=fit(y~.,data=wr[emethod$tr,],model="auto",search=search3,fdebug=TRUE)
# emethod$ts - test data rows

P3=predict(M3,wr[emethod$ts,])

# show leaderboard:
cat("> leaderboard models:",M3@mpar$LB$model,"\n")
cat(">  validation values:",round(M3@mpar$LB$eval,4),"\n")
cat("best model is:",M3@model,"\n")
cat("test set ",metric,"=",round(mmetric(wr$y[emethod$ts],P3,metric=metric),2),"\n")


pse=predict(M3,w2[,])
Y=w2$y

print(mmetric(Y,pse,metric=c("ACC","AUC","ACCLASS","AUCCLASS")))
print(mmetric(Y,pse,metric="CONF" ))

mgraph(Y, pse, graph = "ROC",baseline=TRUE,leg="yes",Grid=10)
  