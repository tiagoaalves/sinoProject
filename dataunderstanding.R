bank_full <- read.csv("bank-full.csv", sep=";")

library("ggplot2")
library("mltools")
library("data.table")
library("scales")

bank <- bank_full

age <- bank$age
balance <- bank$balance
bank$insistencia <- 0
insistencia <- bank$insistencia
campaign <- bank$campaign
balance <- bank$balance
bank$saldos <- ""
saldos <- bank$saldos 

summary(saldos)

#catgoric transformation in column "balance"
for (i in 1:nrow(bank)) {
  if(balance[i]<0) {
    bank$saldos[i] <- "negativo"
  } else if(balance[i]>=0 & balance[i]<=1000) {
    bank$saldos[i] <- "ate 1000"
  } else if(balance[i]>1000 & balance[i] <= 5000) {
    bank$saldos[i] <- "ate 5000"
  } else if(balance[i]>5000 & balance[i] <= 25000){
    bank$saldos[i] <- "ate 25000"
  } else if(balance[i]>25000 & balance[i] <= 5000){
    bank$saldos[i] <- "ate 50000"
  }else{
    bank$saldos[i] <- "mais de 50000"
  }
}
summary(saldos)
head(bank)

targetPorFaixaEtaria <- ggplot(bank, 
                               aes(x = factor(insistencia,
                                              levels = c("unica", "segunda", 
                                                         "ate 5", "ate 10", 
                                                         "ate 20", "mais de 20")),
                                   fill = factor(y, 
                                                 levels = c("y", "n"),
                                                 labels = c("yes", 
                                                            "no")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent", 
       fill = "y",
       x = "Class",
       title = "Automobile Drive by Class") +
  theme_minimal()

targetPorFaixaEtaria

