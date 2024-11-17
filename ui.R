
library(shiny)
library(shinydashboard)
library(tidymodels)
library(tidyverse)
library(randomForest)



# header
header <-  dashboardHeader(title = span("Obesity Level Evaluation",style = "font-size:18px"),titleWidth = 230)
  
# side
menu_sidebar <- dashboardSidebar(tags$head
                                 (tags$style(HTML(".sidebar-menu a { font-size: 16px; }"))),
    
    sidebarMenu(id = "tabs", 
                menuItem(text = "System introduction",tabName = "introduction",icon = icon("book")),
                menuItem(text = "Obesity Assessment",tabName = "predictors",icon = icon("calculator")),
                menuItem(text = "Obesity Results",tabName = "results",icon = icon("gauge-high"))
                )
    )
  
  # body
info_introduction <- shinydashboard::box(title = "Obesity Prediction System", 
                         solidHeader = TRUE,
                         header = TRUE, 
                         status = "warning",
                         width = 13,
                         
                        fluidRow(
                          tags$style(HTML("
                            p{
                              text-align: justify;
                              font-size: 15px;
                            }
                            
                            h2{
                              font-size: 22px;
                              font-weight: bold;
                              color: #333;
                            }"
                            
                          )),
                          
                        column(7,
                        h2("System Introduction:"),
                        br(),
                        p("Obesity is a private struggle as well as a public health crisis. 
                          It is a complex chronic disease caused by multiple reasons for excess body fat, which can sometimes lead to health problems. 
                          It is important to emphasise that body fat itself is not a disease. 
                          However, when excess body fat is excessive, it can have an impact on bodily functions. 
                          These changes develop gradually and may worsen further over time, leading to adverse health effects."),
                        
                        p("WHO classifies obesity into different types based on the severity of obesity. 
                          The following are the different obesity classes according to body mass index (BMI): ",
                        span("Underweight, ","Normal weight, ","Overweight, ","Obesity I, ", "Obesity II, "," and Obesity III.", style = "color:blue")),
                     
                        p("However, the application of BMI may be less accurate for certain populations, 
                          such as children, pregnant women, the elderly, or athletes, 
                          because it does not take into account the physiological characteristics and varying body fat content of these populations. 
                          In addition, the BMI formula does not take into account the effects of age and gender on body weight. 
                          With age, muscle mass may decrease and fat mass may increase, which may affect the interpretation of BMI."),
                        
                        
                        p("Therefore, the system takes these factors into account. What's more, when you seek care from the System, 
                          it wants to know your whole health story. 
                          Not only will it ask about your previous health, 
                          it will also find out about your current diet and exercise patterns and whether you have tried any weight loss programmes in the past. 
                          It may ask about the health history of your blood family. It will also ask about your mode of transport. 
                          It will combine all your answers to predict your level of obesity." ),
                        
                        
                        p("Firstly, please click the button below, then follow the questions asked by the system page to select your answers, 
                          and finally, after you have answered all the questions, 
                          click 'click me!' to complete the obesity level assessment. 
                          Based on your answers, the system will determine your obesity level and give you the appropriate health advice. Enjoy it!"),
                        br(),
                        
                        actionButton("evaluationPage", label = "Evaluate it now!", 
                                     style = "color: #fff; background-color: #4A83C1; border-color: #4A83C1; border-radius: 5px;")),
                        
                        
                        column(5,
                          div(style = "float: right; margin-top: 150px; margin-right:5px", 
                          tags$img(
                          src = "https://specialtycareus.com/wp-content/uploads/Specialtycare-medical-blog-bg-obesity-paradox-bmi.jpg",
                          width = 499,
                          height = 280,
                        )))
                        )
                        )
                       


input_box <- shinydashboard::box(title = "Patient characteristics",
                 status = "warning",
                 header = TRUE, 
                 solidHeader = TRUE,
                 width = 13,
                 h4(strong("Instructions:")),
                 p("Please select the following characteristics and click 'Click me!'"),
                 br(),
                 
                 fluidRow(
                   column(
                     6,
                     sliderInput("age", "what is your age?:", min = 14, max = 100, value = 14),
                     sliderInput("height", "what is your Height (m):", min = 1.40, max = 2.10, value = 1.40),
                     sliderInput("weight", "what is your Weight (kg):", min = 30, max = 200, value = 30),
                     selectInput("betw_meals", "Do you eat any food between meals?", choices = c("No" = 1, "Sometimes" = 2, "Frequently" = 3, "Always" = 4)),
                     selectInput("num_meals", "How many main meals do you have daily?", choices = c("1-2 meals" = 1, "3 meals" = 2, "More than 3 meals" = 3, "No answer" = 4)),
                     selectInput("num_exercise", "How often do you have physical activity?", choices = c("Never" = 1, "1-2 days" = 2, "2-4 days" = 3, "4-5 days" = 4)),
                     
                    
                   ),
                   column(
                     6,
                     #offset = 1,
                     selectInput("gender", "What is your gender?:", choices = c("Female" = 1, "Male" = 2)),
                     selectInput("smoke", "Do you smoke?:", choices = c("No" = 1,"Yes" = 2)),
                     selectInput("fam_history", "Has a family member suffered or suffers from overweight?", choices = c("No" = 1,"Yes" = 2)),
                     selectInput("eat_highcal", "Do you eat high caloric food frequently?", choices = c("No" = 1,"Yes" = 2)),
                     selectInput("monitor_cal", "Do you monitor the calories you eat daily?", choices = c("No" = 1,"Yes" = 2)),
                     selectInput("transportation", "Which transportation do you usually use?:", choices = c("Bike" = 1, "Motorbike" = 2, "Automobile" = 3, "Public Transportation" = 4, "Walking" = 5)),
                     
                     actionButton("predictButton", "Click me!",
                                  style = "color: #fff; background-color: #4A83C1; border-color: #4A83C1; border-radius: 5px;")
                 
    )))
    
    
display_results <- shinydashboard::box(title = "Obesity level prediction",
                       solidHeader = TRUE, 
                       align =  "center",
                       status = "warning",
                       width = 12,
                       uiOutput("obesity_prediction"))

    
    
body <- dashboardBody(tabItems(
  tabItem(tabName = "introduction",info_introduction),
  tabItem(tabName = "predictors", input_box),
  tabItem(tabName = "results", display_results)),
  
  

  
)

dashboardPage(
    header,
    menu_sidebar,
    body
  )

