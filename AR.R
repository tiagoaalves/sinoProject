install.packages("arules")
library(arules)
banco <- read.csv("bank-full.csv", sep=";")

banco$saldos <- 0
saldos <- banco$saldos 


for (i in 1:nrow(banco)) {
  if(banco$balance[i]<0) {
    banco$saldos[i] <- "negativo"
  } else if(banco$balance[i]>=0 & banco$balance[i]<=1000) {
    banco$saldos[i] <- "ate1000"
  } else if(banco$balance[i]>1000 & banco$balance[i] <= 5000) {
    banco$ saldos[i] <- "ate5000"
  } else if(banco$balance[i]>5000 & banco$balance[i] <= 25000){
    banco$saldos[i] <- "ate25000"
  } else if(banco$balance[i]>25000 & banco$balance[i] <= 50000){
    banco$saldos[i] <- "ate50000"
  }else{
    banco$saldos[i] <- "mais de 50000"
  }
}

banco = subset(banco, select = -c(day, month, duration) )
banco= as(banco,"transactions")



class(banco)

# show most frequent items:
itemFrequencyPlot(banco,support=0.1)

#todas as regras
allRules <- apriori(banco, parameter = list(supp = 0.001, conf = 0.5))

inspect(head(allRules, 100))

#rules with high confidence
rules_Highconf <- sort (allRules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_Highconf, 50)) # show the support, lift and confidence for all rules

#rules of saying yes
rulesYes <- apriori(data=banco, parameter=list (supp=0.001,conf = 0.08, maxlen= 4), appearance = list (default="lhs",rhs="y=yes"), control = list (verbose=F)) # get rules that lead to buying the product
rules_confYes <- sort (rulesYes, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_confYes))


#rules of saldos ate 1000
rulesBal <- apriori(data=banco, parameter=list (supp=0.001,conf = 0.08, maxlen= 3), appearance = list (default="lhs",rhs="saldos=ate1000"), control = list (verbose=F)) # get rules that lead to buying the product
rules_confBal <- sort (rulesBal, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_confBal))

# external evaluation demo:
g1=apriori(banco[1:1000,],parameter=list(support=0.006,confidence=0.25,minlen=2))
ext=interestMeasure(g1,transactions=banco[1001:2000,])
print(ext[1:10,1:5]) # 10 rules x 5 measures


library(arulesViz)
plot(rulesYes)
# interactive exploration:
ruleExplorer(rulesYes)


ruledfYes = data.frame(
  lhs = labels(lhs(rules_confYes)),
  rhs = labels(rhs(rules_confYes)), 
  rules_confYes@quality)
head(ruledfYes)

 ruledfBal = data.frame(
  lhs = labels(lhs(rules_confBal)),
  rhs = labels(rhs(rules_confBal)), 
  rules_confBal@quality)
head(ruledfBal)


