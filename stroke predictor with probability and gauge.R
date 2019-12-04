library(shiny)
library(dplyr)
library(ROCR)
library(caret)
library(DMwR)
library(e1071)
library(flexdashboard)
library(shinydashboard)
library(ggpubr)
library(ggplot2)
library(cowplot)

load(file = "fit.rda")

ui <- navbarPage("Stroke Riskometer",
  tabPanel("Risk Assessment Test",
  titlePanel(title = "Stroke Riskometer"),
  sidebarLayout(
  sidebarPanel("A unique tool to access individual risk of stroke"),
  mainPanel()
  ),
  radioButtons(inputId = "gender",
               label = "Gender",
               choices = list("Male", "Female"),
               inline = TRUE
  ),
  sliderInput(inputId = "age",
              label = "How old are you?",
              value = 25, min = 0, max = 100
  ),
  radioButtons(inputId = "hypertension",
               label = "Do you have hypertension?",
               choices = list(1, 0),
               inline = TRUE
  ),
  radioButtons(inputId = "heart_disease",
               label = "Do you have any heart disease?",
               choices = list(1, 0),
               inline = TRUE
  ),
  radioButtons(inputId = "ever_married",
               label = "Have you ever married?",
               choices = list("Yes", "No"),
               inline = TRUE
  ),
  selectInput(inputId = "work_type",
              label = "Select your work type",
              choices = list("Private", "Self-employed", "children", "Govt_job", "Never_worked")
  ),
  selectInput(inputId = "Residence_type",
              label = "Select your residence type",
              choices = list("Urban", "Rural")
  ),
  numericInput(inputId = "avg_glucose_level",
               label = "Average glucose level",
               value = 100, min= 70, max = 200, step = 5
  ),
  sliderInput(inputId = "bmi",
              label = "Body Mass Index (BMI)",
              value = 20, min= 15, max = 35, step = 0.5
  ),
  selectInput(inputId = "smoking_status",
              label = "Do you smoke?",
              choices = list("smokes", "formerly smoked", "never smoked")
  ),
  tableOutput(outputId = "table_testing"
  ),
  textOutput(outputId = "text_testing"
  ),
  column(6,box(flexdashboard::gaugeOutput("plt1"),width= 12,title="Stroke Risk",background = "green")),
  htmlOutput(outputId = "textlist")),
  
  tabPanel("Click Here to View the Fun Facts & Relationship between the Variables",
  column(12, htmlOutput(outputId = "history")),
  br(),
  column(12, plotOutput("Bar1", height = 500)), 
  br(),
  column(12, htmlOutput("age")), 
  br(),
  column(12, plotOutput("agegraph", height = 500)),
  br(), 
  column(12, htmlOutput("marrytext")),
  br(),
  column(12, plotOutput("BMI", height = 500)), 
  br(),
  column(12, plotOutput("marry", height = 500)), 
  br(),
  column(12, htmlOutput("glucose")), 
  br(),
  column(12, plotOutput("glucoseplot", height = 500)), 
))

server <- function(input,output) 
{
  output$table_testing <- renderTable({
    mydata <- read.csv("C:/Users/train_2v.csv", header=T)
    mydata <- subset(mydata, select = -c(id))
    mydata$stroke <- as.factor(mydata$stroke)
    mydata <- SMOTE(stroke ~ ., mydata, perc.over = 100, perc.under = 200)
    set.seed(100)
    inTrain <- createDataPartition(y = mydata$stroke, p = 0.8, list = FALSE)
    training <- mydata[inTrain,]
    testing <- mydata[-inTrain,]
    fit <- glm(stroke~., data = training, family = 'binomial')
    gender <- c(input$gender)
    age <- c(strtoi(input$age))
    hypertension <- c(strtoi(input$hypertension))
    heart_disease <-c(strtoi(input$heart_disease))
    ever_married <- c(input$ever_married)
    work_type <- c(input$work_type)
    Residence_type <- c(input$Residence_type)
    avg_glucose_level <- c(strtoi(input$avg_glucose_level))
    bmi <- c(strtoi(input$bmi))
    smoking_status <- c(input$smoking_status)
    test_one_line.data <- data.frame(gender, age, hypertension, heart_disease, ever_married, work_type, Residence_type, avg_glucose_level, bmi, smoking_status)
    predict_unseen_one_line <- predict(fit, test_one_line.data , type = 'response')*100
    predict_unseen_one_line
  })
  output$plt1 <- flexdashboard::renderGauge({
    mydata <- read.csv("C:/Users/train_2v.csv", header=T)
    mydata <- subset(mydata, select = -c(id))
    mydata$stroke <- as.factor(mydata$stroke)
    mydata <- SMOTE(stroke ~ ., mydata, perc.over = 100, perc.under = 200)
    set.seed(100)
    inTrain <- createDataPartition(y = mydata$stroke, p = 0.8, list = FALSE)
    training <- mydata[inTrain,]
    testing <- mydata[-inTrain,]
    fit <- glm(stroke~., data = training, family = 'binomial')
    gender <- c(input$gender)
    age <- c(strtoi(input$age))
    hypertension <- c(strtoi(input$hypertension))
    heart_disease <-c(strtoi(input$heart_disease))
    ever_married <- c(input$ever_married)
    work_type <- c(input$work_type)
    Residence_type <- c(input$Residence_type)
    avg_glucose_level <- c(strtoi(input$avg_glucose_level))
    bmi <- c(strtoi(input$bmi))
    smoking_status <- c(input$smoking_status)
    test_one_line.data <- data.frame(gender, age, hypertension, heart_disease, ever_married, work_type, Residence_type, avg_glucose_level, bmi, smoking_status)
    predict_unseen_one_line <- round(as.numeric(predict(fit, test_one_line.data , type = 'response')*100), digits=2)
    gauge(predict_unseen_one_line, min= 0, max= 100, symbol= '%', label=paste("Risk Level"),
    gaugeSectors(success= c(0,29.9), warning=c(30,69.9), danger=c(70,100)))
  })
  
  output$textlist <- renderText({
    mydata <- read.csv("C:/Users/train_2v.csv", header=T)
    mydata <- subset(mydata, select = -c(id))
    mydata$stroke <- as.factor(mydata$stroke)
    mydata <- SMOTE(stroke ~ ., mydata, perc.over = 100, perc.under = 200)
    set.seed(100)
    inTrain <- createDataPartition(y = mydata$stroke, p = 0.8, list = FALSE)
    training <- mydata[inTrain,]
    testing <- mydata[-inTrain,]
    fit <- glm(stroke~., data = training, family = 'binomial')
    gender <- c(input$gender)
    age <- c(strtoi(input$age))
    hypertension <- c(strtoi(input$hypertension))
    heart_disease <-c(strtoi(input$heart_disease))
    ever_married <- c(input$ever_married)
    work_type <- c(input$work_type)
    Residence_type <- c(input$Residence_type)
    avg_glucose_level <- c(strtoi(input$avg_glucose_level))
    bmi <- c(strtoi(input$bmi))
    smoking_status <- c(input$smoking_status)
    test_one_line.data <- data.frame(gender, age, hypertension, heart_disease, ever_married, work_type, Residence_type, avg_glucose_level, bmi, smoking_status)
    predict_unseen_one_line <- round(as.numeric(predict(fit, test_one_line.data , type = 'response')*100), digits=2)
    ifelse(predict_unseen_one_line >= 70, paste("<b><font color=\"#FF0000\">CAUTION! YOU ARE AT HIGH RISK OF STROKE"), 
           ifelse(predict_unseen_one_line >= 30, paste("<b><font color=\"#e86100\">YOU ARE AT MODERATE RISK"), 
                  paste("<b><font color=\"green\">CONGRATULATIONS! YOU ARE AT LOW RISK")))
      })


    output$history <- renderText({paste(
    "<b><font color=\"blue\">Do you know, person with smoking history (Formerly Smokes and Smoking) has risk of more than 2 times higher than person with no smoking history?")
      })

  output$Bar1 <- renderPlot({
    mydata <- read.csv("C:/Users/train_2v.csv", header=T)
    mydata <- mydata %>% filter(gender != "Other" & !(is.na(bmi))) %>%
      select(-id)
    
    #convert some variables to a factor
    mydata$hypertension <- as.factor(mydata$hypertension)
    mydata$heart_disease <- as.factor(mydata$heart_disease)
    mydata$stroke <- as.factor(mydata$stroke)
    nvr_smoke_percent <- (nrow(filter(mydata, smoking_status == "never smoked" & stroke == 1)) 
                          /nrow(filter(mydata, smoking_status == "never smoked")))*100
    
    smoke_percent<- (nrow(filter(mydata, smoking_status == "smokes" & stroke == 1)) 
              /nrow(filter(mydata, smoking_status == "smokes")))*100
    former_percent<-(nrow(filter(mydata, smoking_status == "formerly smoked" & stroke == 1))/
                    nrow(filter(mydata, smoking_status == "formerly smoked")))*100
    
   smoking_status <- c(nvr_smoke_percent, smoke_percent, former_percent)
   barplot(smoking_status, main = "Percentage of Stroke Risk", ylab= "Percentage(%)", xlab="Smoking Status", 
           col = "#00BFFF", names.arg = c("Never Smoke", "Smokes", "Formerly Smoke" ) )
  })
  
  
  output$agegraph <- renderPlot({
    mydata <- read.csv("C:/Users/train_2v.csv", header=T)
    mydata <- mydata %>% filter(gender != "Other" & !(is.na(bmi))) %>%
      select(-id)
    
    #convert some variables to a factor
    mydata$hypertension <- as.factor(mydata$hypertension)
    mydata$heart_disease <- as.factor(mydata$heart_disease)
    mydata$stroke <- as.factor(mydata$stroke)
    distribution_plot <- function(y){
      ## Function returns histogram and boxplot of choosen variable distribution by stroke outcome
      
      box_plt <- ggplot(mydata, aes(x=stroke, y = eval(parse(text = y)))) +
        geom_boxplot(aes(fill=stroke)) +
        stat_summary(fun.y=mean, colour="darkred", geom="point", size=2) +
        labs(x = "Stroke outcome",
             y = y) +
        theme_bw()
      
      hstgm <- ggplot(mydata, aes(x = eval(parse(text = y)), fill=stroke)) +
        geom_histogram(bins = 40, alpha=.5, position="identity") +
        geom_vline(aes(xintercept=mean(mydata[[y]][mydata$stroke == 1], na.rm = T)), 
                   colour = "blue", size=0.5) +
        geom_vline(aes(xintercept=mean(mydata[[y]][mydata$stroke == 0], na.rm = T)), 
                   colour = "red", size=0.5) +
        geom_vline(aes(xintercept=median(mydata[[y]][mydata$stroke == 1], na.rm = T)), 
                   colour = "blue", linetype="dashed", size=0.5) +
        geom_vline(aes(xintercept=median(mydata[[y]][mydata$stroke == 0], na.rm = T)), 
                   colour = "red", linetype="dashed", size=0.5) +
        labs(x = y) +
        theme_bw()
      
      return(plot_grid(hstgm, box_plt))
    }
    distribution_plot("age")
  })  
  
  output$age <- renderText({paste(
    "<b><font color=\"blue\">The older you are, the higher is your stroke risk")
  })

  output$BMI <- renderPlot({
    mydata <- read.csv("C:/Users/train_2v.csv", header=T)
    mydata <- mydata %>% filter(gender != "Other" & !(is.na(bmi))) %>%
      select(-id)
    
    #convert some variables to a factor
    mydata$hypertension <- as.factor(mydata$hypertension)
    mydata$heart_disease <- as.factor(mydata$heart_disease)
    mydata$stroke <- as.factor(mydata$stroke)
    ggplot(mydata, aes(x = bmi, fill=ever_married)) +
      geom_histogram(bins = 40, alpha=.5, position="identity") +
      geom_vline(aes(xintercept=mean(mydata$bmi[mydata$ever_married == 'Yes'], na.rm = T)), 
                 colour = "blue", size=0.5) +
      geom_vline(aes(xintercept=mean(mydata$bmi[mydata$ever_married == 'No'], na.rm = T)), 
                 colour = "red", size=0.5) +
      geom_vline(aes(xintercept=median(mydata$bmi[mydata$ever_married == 'Yes'], na.rm = T)), 
                 colour = "blue", linetype="dashed", size=0.5) +
      geom_vline(aes(xintercept=median(mydata$bmi[mydata$ever_married == 'No'], na.rm = T)), 
                 colour = "red", linetype="dashed", size=0.5)+
      theme_bw()
  }) 
  
output$marrytext <- renderText({
  paste("<b><font color=\"blue\">Histogram is actually skewed to the right. It does look like people trend to gain weight in marriage") 
})  
  
output$marry <- renderPlot({
  mydata <- read.csv("C:/Users/train_2v.csv", header=T)
  mydata <- mydata %>% filter(gender != "Other" & !(is.na(bmi))) %>%
    select(-id)
  
  #convert some variables to a factor
  mydata$hypertension <- as.factor(mydata$hypertension)
  mydata$heart_disease <- as.factor(mydata$heart_disease)
  mydata$stroke <- as.factor(mydata$stroke)
  
  ggplot(mydata, aes(x = ever_married, y = bmi)) +
    geom_boxplot(aes(fill=ever_married)) +
    stat_summary(fun.y=mean, colour="darkred", geom="point", size=2)+
    theme_bw()
})
  
  output$glucose <- renderText({
    paste("<b><font color=\"blue\">Group of people with higher glucose level will have higher risk of having stroke")
  })
    
  output$glucoseplot <- renderPlot({
    mydata <- read.csv("C:/Users/train_2v.csv", header=T)
    mydata <- mydata %>% filter(gender != "Other" & !(is.na(bmi))) %>%
      select(-id)
    
    #convert some variables to a factor
    mydata$hypertension <- as.factor(mydata$hypertension)
    mydata$heart_disease <- as.factor(mydata$heart_disease)
    mydata$stroke <- as.factor(mydata$stroke)
    distribution_plot <- function(y){
      ## Function returns histogram and boxplot of choosen variable distribution by stroke outcome
      
      box_plt <- ggplot(mydata, aes(x=stroke, y = eval(parse(text = y)))) +
        geom_boxplot(aes(fill=stroke)) +
        stat_summary(fun.y=mean, colour="darkred", geom="point", size=2) +
        labs(x = "Stroke outcome",
             y = y) +
        theme_bw()
      
      hstgm <- ggplot(mydata, aes(x = eval(parse(text = y)), fill=stroke)) +
        geom_histogram(bins = 40, alpha=.5, position="identity") +
        geom_vline(aes(xintercept=mean(mydata[[y]][mydata$stroke == 1], na.rm = T)), 
                   colour = "blue", size=0.5) +
        geom_vline(aes(xintercept=mean(mydata[[y]][mydata$stroke == 0], na.rm = T)), 
                   colour = "red", size=0.5) +
        geom_vline(aes(xintercept=median(mydata[[y]][mydata$stroke == 1], na.rm = T)), 
                   colour = "blue", linetype="dashed", size=0.5) +
        geom_vline(aes(xintercept=median(mydata[[y]][mydata$stroke == 0], na.rm = T)), 
                   colour = "red", linetype="dashed", size=0.5) +
        labs(x = y) +
        theme_bw()
      
      return(plot_grid(hstgm, box_plt))
    }
    
    distribution_plot("avg_glucose_level")
  })
}

shinyApp(ui = ui, server = server)

#Completed
