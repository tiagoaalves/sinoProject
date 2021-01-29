install.packages("rpart.plot")
library(dplyr)
library(VIM)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(factoextra)
library(devtools)
library(ggbiplot)
library(party)
library(rpart.plot)
library(rminer)

summary(bank)

#show missing values
aggr(bank)

set.seed(20)
clusters <- kmeans(bank[,c(1,6,12)], 5)


# Save the cluster number in the dataset as column 'cage'
bank$cage <- as.factor(clusters$cluster)
str(clusters)

# plot age vs. balance (color represents cluster)
ggplot(bank, aes(x = age, 
                     y = balance, 
                     color=cage)) +
  geom_point() +
  labs(title = "Clusters by age and balance")

# Compute k-means with k = 3
set.seed(123)
res.km <- kmeans(scale(bank[, c(1,6)]), 5, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster

fviz_cluster(res.km, data = bank[, c(1,6)], 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

#####################################

cats <- factor(bank$job)
bank$jobsNumeric <- as.numeric(cats)

cats <- factor(bank$education)
bank$educationNumeric <- as.numeric(cats)

set.seed(50)
res.km <- kmeans(scale(bank[, c(1,6,21)], 5, nstart = 25))
                 # K-means clusters showing the group of each individuals
                 res.km$cluster
                 str(res.km)
                 
                 fviz_cluster(res.km, data = bank[, c(1,6,21)],
                              geom = "point",
                              ellipse.type = "convex", 
                              ggtheme = theme_bw()
                 )
bank$cluster <- res.km$cluster

#age vs job vs trabalho

banco <- bank
banco$trabalho <- 0
trabalho <- banco$trabalho


for (i in 1:nrow(banco)) {
  if(banco$job[i]=="entrepreneur") {
    banco$trabalho[i] <- 1
  } else if(banco$job[i]=="management") {
    banco$trabalho[i] <- 1
  } else if(banco$job[i]=="self-employed") {
    banco$trabalho[i] <- 1
  } else if(banco$job[i]=="blue-collar") {
    banco$trabalho[i] <- 1
  } else if(banco$job[i]=="admin.") {
    banco$trabalho[i] <- 1
  } else if(banco$job[i]=="technician") {
    banco$trabalho[i] <- 1
  } else if(banco$job[i]=="housemaid") {
    banco$trabalho[i] <- 1
  } else if(banco$job[i]=="technician") {
    banco$trabalho[i] <- 1
  } else if(banco$job[i]=="services") {
    banco$trabalho[i] <- 1
  } else if(banco$job[i]=="student") {
    banco$trabalho[i] <- 0
  } else if(banco$job[i]=="retired") {
    banco$trabalho[i] <- 2
  } else if(banco$job[i]=="unemployed") {
    banco$trabalho[i] <- 3
  } else if(banco$job[i]=="unknown") {
    banco$trabalho[i] <- 3
  }
}

set.seed(123)
res.km <- kmeans(scale(banco[, c(1,6,22)]), 5, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster

fviz_cluster(res.km, data = banco[, c(1,6,22)], 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
                 
1 #Elbow method
                
#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- bank[, c(1,6,21)]
wss <- sapply(1:k.max, 
  function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
  wss
  plot(1:k.max, wss,
    type="b", pch = 19, frame = TRUE, 
    xlab="Number of clusters K",
    ylab="Total within-clusters sum of squares")
  
#verificar utilidade dos clusters
  
  bank$result <- 0
  sp <- ggplot(bank, 
         aes(x = res.km$cluster, 
             fill = y)) + 
    geom_bar(position = "fill") +
    labs(y = "Proportion")
  sp + geom_hline(yintercept=0.117)

#classification

  bank$result <-3
  
  for (i in 1:nrow(bank)) {
    if(bank$y[i]=="yes") {
      bank$result[i] <- 1
    } else if(bank$y[i]== "no") {
      bank$result[i] <- 0
    }
  }
  
  # Create the input data frame. 
  input.dat <- bank[, c(1,6,21,23)] 
  
  # Give the chart file a name. 
  png(file = "decision_tree.png") 
  
  # Create the tree. 
  output.tree <- ctree( 
    result ~ age + balance,  
    data = input.dat) 
  
  # Plot the tree. 
  plot(output.tree) 
  
  # Save the file. 
  dev.off()
  
#classification 2
  
  cals <- bank[, c(1,6,14,21,23)]
  
  n <- nrow(cals)
  n_train <- round(0.8 * n) 
  set.seed(123)
  train_indices <- sample(1:n, n_train)
  train <- cals[train_indices, ]  
  test <- cals[-train_indices, ]

  model <- rpart(formula = result ~ age + balance,
                 data = cals, 
                 method = "class")  
  rpart.plot(model)

#classification 3
  #cals <- bank[, c(1,6,14,21,23)]
  cals <- banco[, c(1,6,22)]
  cals$result <- as.factor(bank$result)
  summary(cals)
  head(cals)
  #cals$result <- ifelse(cals$result =='yes',1,0)
  #cals$result=cut(cals$result, c(-1,0.5,1), c("no","yes"))
  s1=sample(1:nrow(cals),500)
  ws1=cals[s1,]
  dt=fit(result~.,ws1,model="dt")
  plot(dt@object);text(dt@object)
  rpart.plot(dt@object)
  head(cals)
  summary(cals)
  
#Naive Bayes
    
  # Loading package 
  library(e1071) 
  library(caTools) 
  library(caret) 

  # Splitting data into train 
  # and test data 
  split <- sample.split(cals, SplitRatio = 0.7) 
  train_cl <- subset(cals, split == "TRUE") 
  test_cl <- subset(cals, split == "FALSE") 
  head(train_cl)
  
  # Feature Scaling 
  train_scale <- scale(train_cl[, 1:3]) 
  test_scale <- scale(test_cl[, 1:3]) 
  head(test_cl)
  
  # Fitting Naive Bayes Model  
  # to training dataset 
  set.seed(120)  # Setting Seed 
  classifier_cl <- naiveBayes(result ~ ., data = train_cl) 
  classifier_cl 

  # Predicting on test data' 
  y_pred <- predict(classifier_cl, newdata = test_cl) 
  y_pred
  
  # Confusion Matrix 
  cm <- table(test_cl$result, y_pred) 
  cm 
  
  # Model Evauation 
  confusionMatrix(cm)   
  
  