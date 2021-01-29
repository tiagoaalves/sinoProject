library(markdown)
library(shinythemes)
library(ggplot2)
library(arulesViz)


vars <- setdiff(names(iris), "Species")

fluidPage(theme = shinytheme("cosmo"),
navbarPage("Grupo 26",
           tabPanel("Clustering",
                    
                    
                    pageWithSidebar(
                      headerPanel('Clustering Inputs'),
                      sidebarPanel(
                        checkboxInput("Age", label = "Age", value = TRUE),
                        checkboxInput("Balance", label = "Balance", value = TRUE),
                        checkboxInput("Job", label = "Job", value = TRUE),
                        checkboxInput("Education", label = "Education", value = TRUE),
                        numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
                      ),
                      mainPanel(
                        plotOutput('clustplot'),
                        plotOutput('elbowplot'),
                        plotOutput('confirmationplot'),
                        plotOutput('classplot')
                      )
                    )
           ),
           tabPanel("Regras de 
                    associação",
                    titlePanel("Regras de Associação para obter um Yes"),
                    DT::dataTableOutput("table"),
                    plotOutput('assoplot')
           ),
           tabPanel("Classificação",
                   # titlePanel("demoApp"), # title of the panel
                    sidebarLayout( # sidebar with input and outputs
                      sidebarPanel(  # panel for inputs
                        selectInput("var", # input: 3 character choices
                                    label="Escolha o modelo de Classificação",
                                    choices=c("Logistic Regression","Random Forest","SVM"),
                                    selected="Logistic Regression" # default choice
                        )), # end sidebarPanel
                      mainPanel( # panel for outputs
                        plotOutput(outputId="distPlot"),
                        verbatimTextOutput("summary1"),
                        verbatimTextOutput("summary2")
                      ))
           )
)
)