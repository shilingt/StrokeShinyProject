
library(shiny)
library(shinydashboard)
library(here)
library(shiny)
library(ggplot2)
library(ggridges)
library(rsconnect)
library(shinythemes)
library(fmsb)
library(dplyr)
library(rsconnect)
library(shinythemes)
library(shinyWidgets)
library(DT)

# ------------- Read data -------------------------

stroke_df <- read.csv("healthcare-dataset-stroke-data.csv")
#diabetes[,2:6][diabetes[,2:6]==0] <- NA # replaces all zero values from column two to six with NA
#diabetes <- na.omit(diabetes) # now we omit all NA values
stroke_df$Outcome <- as.factor(stroke_df$stroke)
levels(stroke_df$Outcome) <- c("No Stroke","Stroke")
Smoking_Status=c("formerly smoked, 17.32%","never smoked, 37.03%","smokes, 15.44%","Unknown, 30.21%")
value=c(17.32,37.03,15.44,30.21)

# ------------- Data Cleaning & Preparation -------------------------

# Imputation of missing values
typeof(stroke_df$bmi)
stroke_df$bmi <- round(as.numeric(stroke_df$bmi), digits=2)
typeof(stroke_df$bmi)
#bmi_nonum <- which(is.na(as.numeric(stroke$bmi)))
stroke_df$bmi[is.na(stroke_df$bmi)] <- mean(stroke_df$bmi, na.rm=T)

# Compute quantity for each group
stroke.quantity <- stroke_df %>%
  group_by(Outcome) %>% summarise(Count=n())

# compute the mean for each variable by each group
gender.count <- stroke_df %>%
  group_by(gender, Outcome) %>% 
  summarise(Count=n())

hypertension.count <- stroke_df %>%
  group_by(hypertension, Outcome) %>% 
  summarise(Count=n())

heart.count <- stroke_df %>%
  group_by(heart_disease, Outcome) %>% 
  summarise(Count=n())

married.count <- stroke_df %>%
  group_by(ever_married, Outcome) %>% 
  summarise(Count=n())

smoking.count <- stroke_df %>%
  group_by(smoking_status, Outcome) %>% 
  summarise(Count=n())

work.count <- stroke_df %>%
  group_by(work_type, Outcome) %>% 
  summarise(Count=n())

residence.count <- stroke_df %>%
  group_by(Residence_type, Outcome) %>% 
  summarise(Count=n())
residence.count 

mean_age <- mean(stroke_df$age)
mean_avgglu <- mean(stroke_df$avg_glucose_level)
mean_bmi <- mean(stroke_df$bmi)


# ------------- Server  -------------------------
#generic line initiating the SERVER
server <- shinyServer(function(input, output){
  output$table.1 <- renderTable(stroke.quantity)
  
  # ------------- Server  -------------------------
  
  output$summary_df1 <- renderPrint({summary(stroke_df)}) 
  output$table_df1 <- DT::renderDataTable({stroke_df })
  
  
  # ------------- Graph ploting -------------------------
  
  output$gg1 <- renderPlot({
    ggplot(stroke_df, aes(age, avg_glucose_level)) + 
      geom_point(colours='blue', height = 200, width = 300)})
  
  output$gg2 <- renderPlot({
    ggplot(stroke_df,aes(x=stroke_df[,input$table1],y= stroke_df[,input$table2],col=Outcome,fill=Outcome)) +
      geom_violin(alpha = 0.8) +
      ylab(input$table2)+
      xlab(input$table1)
    
  })
  
  output$gg3 <- renderPlot({
    
    
    ggplot(stroke_df, aes(stroke_df[,input$table3],stroke_df[,input$table4],col=Outcome,fill=Outcome))+
      geom_bar(stat = "identity")+
      
      ylab(input$table4)+
      xlab(input$table3)
  })
  
  output$gg4 <- renderPlot({
    
    ggplot(stroke_df, aes(x = stroke, y = avg_glucose_level))+
      geom_bar(aes(fill = Residence_type),stat = "identity", position = position_dodge(0.9))+
      facet_wrap(~smoking_status, scales = "free")
    
  })
  
  
  
  output$gg6 <- renderPlot({
    stroke_df %>% count(stroke) %>%
      ggplot() + geom_col(mapping = aes(x = stroke, y = n, fill = stroke), color="#110110")+
      coord_polar()+
      ggtitle(label = "Number of people who suffered a stroke.")+
      theme_minimal()
  })
  
  output$prediction<-renderPrint({
    
    # ------------- Prediction model -------------------------
    
    # Splitting the dataset into the Training set and Test set
    library(caTools)
    # Convert categorical variables to numerical
    stroke_df$ever_married <- as.numeric(factor(as.matrix(stroke_df$ever_married)))
    stroke_df$gender <- as.numeric(factor(as.matrix(stroke_df$gender)))
    stroke_df$work_type <- as.numeric(factor(as.matrix(stroke_df$work_type)))
    stroke_df$Residence_type <- as.numeric(factor(as.matrix(stroke_df$Residence_type)))
    stroke_df$smoking_status <- as.numeric(factor(as.matrix(stroke_df$smoking_status)))
    set.seed(123)
    split = sample.split(stroke_df$Outcome, SplitRatio = 0.80)
    training_set = subset(stroke_df, split==TRUE)
    test_set = subset(stroke_df, split==FALSE)
    
    # Fitting naive bayes to training set
    library(e1071)
    set.seed(123)
    classifier = naiveBayes(x = training_set[2:11],
                            y = training_set$Outcome)
    
    # Predicting the Test set results
    prob_percent=predict(classifier,
                         newdata=data.frame("gender"=input$gender,"age"=input$age, 
                                            "hypertension"=input$hypertension,
                                            "heart_disease"=input$heart_disease,
                                            "ever_married"=input$ever_married, "work_type"=input$work_type,
                                            "Residence_type"=input$Residence_type, "avg_glucose_level"=input$avg_glucose_level,
                                            "bmi"=input$bmi, "smoking_status"=input$smoking_status), type='raw')
    percent<-round(prob_percent[2]*100,3)
    
    if(percent>=50){
      cat(paste(" Your risk of getting stroke is:", percent,"%","\n",
                "Kindly visit your nearest healthcare professional for personalized medical advice."))
    }
    else if(percent<50){
      cat(paste(" Great! Your risk of getting stroke is:", percent,"%,lower than 50%.","\n",
                "Please continue your healthy lifestyle and do medical check up regularly"))
    }
  })
})

# ------------- Image -------------------------
setBackgroundImage(src = NULL, shinydashboard = FALSE)

backgroundpic <- base64enc::dataURI(file="bckgrd.png", mime="image/png")

stroke <-  base64enc::dataURI(file="stroke.jpg", mime="image/jpg")


# ------------- Shinny App -------------------------

ui <- fluidPage(
  
  setBackgroundImage( 
    src = ""
  ),
  
  navbarPage(
    theme = shinytheme("journal"),
    inverse = FALSE,
    title = (h1(strong("Stroke predictor")))
  ),
  
  tabsetPanel( 
    
    tabPanel("Overview",icon=icon("fas fa-globe"),
             fluidPage(
               
               fluidRow(column(width =5,h2(style="color:LightCoral",strong("What is Stroke?")))),
               
               br(),
               
               img(src = stroke,
                   height = "35%", width = "35%",
                   style = "position: relative;
                                    right: -400px;"),
               
               fluidRow(column(width =6,h3 ("A stroke, sometimes called a brain attack, happens in one of two ways:"))),
               fluidRow(column(width =6,h3 (" (i) Ischemic stroke-when the blood supply to the brain is blocked"))),
               fluidRow(column(width =6,h3 (" (ii) Hemorrhagic stroke-when a blood vessel in the brain bursts"))),
               fluidRow(column(width =6,h3 ("A stroke causes brain tissue to die, which can lead to brain damage, disability, and death."))),
               
               br(),
               
               fluidRow(column(width =11, h3("According to the World Stroke Organization, every 1 in 4 adults over the age of 25 will have a stroke in their lifetime and 5.5 millions people die yearly. 
                             It is estimated the deaths will rise to 6.7 million yearly without appropriate action."))),
               br(),
               
               fluidRow(column(width =12, h2(style="color:LightCoral",strong("What are the attributes related to stroke?")))),
               fluidRow(column(width =11, h3("There are several factors that can increase the risk for stroke, includes age, sex, unhealthy eating habits and smoking. 
                                           Dataset 'Stroke prediction dataset' from Kaggle will be used to study the stroke attributes"))),
               
               br(),
               fluidRow(column(width =12, h2(style="color:LightCoral",strong("What is the correlation between pre-existing health conditions and stroke risks?")))),
               fluidRow(column(width =11, h3("A number of interactive graph under Analysis tab is available for you to explore and understand the correlation between the variables."))), 
               
               br(),
               fluidRow(column(width =12, h2(style="color:LightCoral", strong("Are you at risk for stroke?")))),
               fluidRow(column(width =11, h3("Anyone, including children, can have a stroke at any time. Why wait? Check out your stroke risk now in the prediction tab!"))) 
               
               
             )),
    
    # ------------- App: Data Explorer -------------------------
    tabPanel("Data Explorer",icon=icon("fas fa-search"),
             fluidPage(
               DT::dataTableOutput("table_df1"),
               
               h3("Descriptive Statistic"),
               column(width = 12, verbatimTextOutput("summary_df1") )
               
             )),              
    
    # ------------- App: Analysis ------------------------- 
    
    tabPanel("Analysis",icon=icon("fas fa-chart-bar"),
             tabBox(
               height = "250px", width = "500px",
               
               tabPanel("Scatterplots",
                        mainPanel(h5("Number of stroke and no stroke"), h5("ggplot"),plotOutput("gg1"))),
               
               
               
               tabPanel("Violin Plot",
                        sidebarPanel(selectInput("table1","X-axis",names(stroke_df)[1:11],multiple=FALSE,selected="id"),
                                     selectInput("table2","Y-axis",names(stroke_df)[1:11],multiple=FALSE,selected="gender"),
                                     submitButton(text="Submit",icon=icon("refresh"),width=NULL)),
                        mainPanel(h5("Violin Plot of"),tableOutput("table.1"),plotOutput("gg2"))),
               
               tabPanel("Bar Chart",
                        sidebarPanel(selectInput("table3","X-axis",names(stroke_df)[1:11],multiple=FALSE,selected="id"),
                                     selectInput("table4","Y-axis",names(stroke_df)[1:11],multiple=FALSE,selected="gender"),
                                     submitButton(text="Submit",icon=icon("refresh"),width=NULL)),
                        mainPanel(h2("Marital Status Vs Stroke"),tableOutput("table.3"),plotOutput("gg3"),h2("Smoking Status Vs stroke for each Residence Type and Glucose levels"),plotOutput("gg4"))),
               
               
               tabPanel("Pie Chart",
                        mainPanel(h2("Number of people who suffered a stroke"),plotOutput("gg6"))),
               
               
             )),
    
    
    
    # ------------- App: Prediction -------------------------
    tabPanel("Prediction",icon=icon("fas fa-poll"),
             
             sidebarPanel(radioButtons("gender","Select your gender",choices = c("Male" = 2, "Female" = 1)),
                          numericInput("age","Enter your age",value=0,min=0,max=100),
                          radioButtons("hypertension","Do your have hypertension?", choices = list("Yes" = 1, "No" = 0)),
                          radioButtons("heart_disease","Do your have heart disease?",choices = list("Yes" = 1, "No" = 0)),
                          radioButtons("ever_married","Are you married?",choices = list("Yes" = 2, "No" = 1)),
                          radioButtons("work_type","Select your work types",choices = list("Student" = 1, "Government" = 2, "Never Worked" = 3, "Private" = 4, "Self-employed" = 5)),
                          radioButtons("Residence_type","Select your live area?", choices = list("Urban" = 2, "Rural" = 1)),
                          numericInput("avg_glucose_level","Enter your average glucose level (mg/dL) [Range from 50 to 500+]",value=0,min=0,max=300),
                          numericInput("bmi","Enter your bmi (bmi = kg/m2)",value=0,min=0,max=100),
                          radioButtons("smoking_status","Do you smoke", choices = list("formerly smoke" = 1, "Never Smoked" = 2, "Smokes" = 3, "Others" = 4)),
                          submitButton(text="Submit",icon=icon("refresh"),width=NULL)),
             mainPanel(h3("Stroke Risk Prediction"),verbatimTextOutput("prediction"),
                       
                       img(src = backgroundpic,
                           height = "80%", width = "80%",
                           style = "position: relative;
                                    right: -200px;")
             )),
    
    
    # ------------- App: Documentation -------------------------
    tabPanel("Documentation", icon=icon("fas fa-file-alt"),
             
             fluidPage(
               
               fluidRow(column(width = 12,h2(strong("Documentation")))),
               
               HTML("
                               <div style='background-color:white; padding: 5px 10px; border-radius: 5px; margin-bottom: 50px;'>
                                  <h4><i class=' fa fa-globe fa-fw'></i><b> Overview </b></h4>
                                  <p> This page provides an overview of stroke and overview of this application. </p>
                                  <br/>
                                  <h4><i class=' fa fa-search fa-fw'></i><b> Data Explorer </b></h4>
                                  <p> This page tabulated the data used used in analysis and prediction. Descriptive analysis is also provided.</p>
                                     <br/>
                                  <h4><i class=' fa fa-bar-chart-o fa-fw'></i><b> Analysis </b></h4>
                                  <p> This page containes the interactive graph for users to visualised the correlations between variables.</p>
                                  <br/>
                                  <h4><i class=' fa fa-poll fa-fw'></i><b> Prediction </b></h4>
                                  <p> This page allow users to predict their stroke risk by enter their information.</p>
                                </div>
                               "),
               
               fluidRow(column(width=11, h2(strong("Data")))),
               fluidRow(column(width=11, HTML("<p> The dataset 'Stroke Predictive Dataset' is obtained from Kaggle."))),
               fluidRow(column(width=11, HTML("<p> Mean imputation method is implemented in missing data handling."))),
               
               br(),
               fluidRow(column(width=11, h2(strong("Source Code")))),
               fluidRow(column(width=11, tags$a(href = "https://github.com/shilingt/StrokeShinyProject", "Github Link"))),
               
               br(),
               
               
               fluidRow(column(width=11, h2(strong("References")))),
               fluidRow(column(width=11, HTML("<p>Fedesoriano (2021). Stroke Prediction Dataset. Retrieved on Jan 10, 2022 from"),tags$a(href = "https://www.kaggle.com/fedesoriano/stroke-prediction-dataset", "https://www.kaggle.com/fedesoriano/stroke-prediction-dataset"))), 
               fluidRow(column(width=11, HTML("<p>Stroke. Centers for Disease Control and Prevention. Retrieved on Jan 10, 2022 from"),tags$a(href = "https://www.cdc.gov/stroke/facts_stroke.htm", "https://www.cdc.gov/stroke/facts_stroke.htm"))),
               
               br(),
               
               fluidRow(column(width = 12,h2(strong("Authors")))),
               fluidRow(column(width = 12,HTML("<p> by Group K stroKe"))),
               fluidRow(column(width = 12,HTML("<p>1. Ho Wei Yan (S2116489) (Team Leader)"))),         
               fluidRow(column(width = 12,HTML("<p>2. Charroogesinee (17060405)"))),         
               fluidRow(column(width = 12,HTML("<p>3. Choong Che Wei (S2106183)"))),      
               fluidRow(column(width = 12,HTML("<p>4. Hii Yew Han (S2037987)"))),
               fluidRow(column(width = 12,HTML("<p>5. Tan Shi Ling (S2115562)")))                        
               
             ))
    
    
    
    
  ))


shinyApp(ui=ui,server=server)




