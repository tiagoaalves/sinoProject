function(input, output, session) {
  #clustering
  x <- c(1, 6, 19, 20)
  
  selectedData <- reactive({
    x <- c(1, 6, 19, 20)
    if(input$Age == FALSE){
      x <- x[!x %in% 1]
    }
    if(input$Balance == FALSE){
      x <- x[!x %in% 6]
    }
    if(input$Job == FALSE){
      x <- x[!x %in% 19]
    }
    if(input$Education == FALSE){
      x <- x[!x %in% 20]
    }
    bank[,x]
  })
  
  rval_kclust<-reactive({
    kmeans(selectedData(), centers = input$clusters)
  })
  
  output$clustplot<-renderPlot({
    factoextra::fviz_cluster(rval_kclust(), data = selectedData()) 
  })
  
  
  
  #elbow method
  
  wssplot <- function(data, nc = 15, seed = 1234) {
    wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
    for (i in 2:nc) {
      set.seed(seed)
      wss[i] <- sum(kmeans(data, centers = i)$withinss)
    }
    plot(1:nc,
         wss,
         type = "b",
         xlab = "Number of Clusters",
         ylab = "Within groups sum of squares")
  }
  
  output$elbowplot<-renderPlot({
    
    wssplot(selectedData())
  })
  
  #Cluster confirmation
  
  conplot <- function(data) {
    resa.km <- kmeans(selectedData(), centers = input$clusters)
  sp <- ggplot(data, 
               aes(x = resa.km$cluster, 
                   fill = bank$y)) + 
    geom_bar(position = "fill") +
    labs(y = "Proportion")
  sp + geom_hline(yintercept=0.117)
  }
  
  output$confirmationplot<-renderPlot({
    conplot(selectedData())
    
  })
  
  #association rules
  
  sr <- ggplot(ruledfYes, aes(x = support, 
                       y = confidence, 
                       color=lift)) +
    geom_point() +
    labs(title = "Relação Confiança, Suporte e Lift")
  
  output$table <- DT::renderDataTable({
    DT::datatable(ruledfYes)
  })
  
  output$assoplot<-renderPlot({
    sr
  })
  
  #classification
  
  selectModel <- reactive({
    print(input$var)
    if(input$var == "Logistic Regression"){
      a <- "lr"
    }else if(input$var == "Random Forest"){
      a <- "randomForest"
    }else if(input$var == "SVM"){
      a <- "svm"
    }
    a
  })
  
  d <- 0
  e <- 0
  
  assplot <- function(selectimp) {
    #mpause("fit a lr")
    
    #mpause("get predictions:")
    if(selectimp == "auto"){
      plrt <- predict(M3,w2)
    }else{
      lrt=fit(y~.,w1[holdoutM$tr,],model=selectimp)
      plrt <- predict(lrt,w2)
    }
    Y=w2[,]$y
    
    #mpause("show some lr metrics:")
    d <- mmetric(Y,plrt,metric=c("ACC","AUC","ACCLASS","AUCCLASS"), D=0.3, TC=2)
    e <- mmetric(Y,plrt,metric="CONF" )
    #mpause("show ROC for svm:")
    
    mgraph(Y,plrt,graph="ROC",baseline=TRUE,leg="yes",Grid=10)
  }
  
  printplot1 <- function(selectimp) {
    
    #mpause("fit a lr")
    lrt=fit(y~.,w1[holdoutM$tr,],model=selectimp)
    #mpause("get predictions:")
    
    plrt <- predict(lrt,w2)
    Y=w2[,]$y
    
    #mpause("show some lr metrics:")
    print(mmetric(Y,plrt,metric=c("ACC","AUC","ACCLASS","AUCCLASS"), D=0.3, TC=2))
    
    #mpause("show ROC for svm:")
  }
  
  printplot2 <- function(selectimp) {
    
    #mpause("fit a lr")
    lrt=fit(y~.,w1[holdoutM$tr,],model=selectimp)
    #mpause("get predictions:")
    
    plrt <- predict(lrt,w2)
    Y=w2[,]$y
    
    #mpause("show some lr metrics:")
    print(mmetric(Y,plrt,metric="CONF" ))
    #mpause("show ROC for svm:")
  }
  
  
  output$distPlot<-renderPlot({
    assplot(selectModel())
  })
  
  output$summary1 <- renderPrint({
    printplot1(selectModel())
  })
  
  output$summary2 <- renderPrint({
    printplot2(selectModel())
  })
  
  
}

