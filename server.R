
library(shiny)
library(shinydashboard)
library(tidymodels)
library(tidyverse)
library(randomForest)


model <- readRDS("C:/Users/17839/Desktop/Thesis_work/obesity_model.rds")

function(input, output,session){
  
  observeEvent(input$predictButton, {
    
    input_df <- data.frame(
                          age = input$age,
                          height = input$height,
                          weight = input$weight,
                          gender = as.integer(input$gender),
                          fam_history = as.integer(input$fam_history),
                          eat_highcal = as.integer(input$eat_highcal),
                          num_meals = as.integer(input$num_meals),
                          betw_meals= as.integer(input$betw_meals),
                          smoke = as.integer(input$smoke),
                          monitor_cal = as.integer(input$monitor_cal),
                          num_exercise = as.integer(input$num_exercise),
                          transportation = as.integer(input$transportation))

  
  prediction_prob <- predict(model, new_data = input_df, type = "prob")
  prediction_class <- predict(model, new_data = input_df, type = "class")
 
  print(input_df)
  print(prediction_prob)
  
  prediction_prob <- predict(model, input_df, type = "prob") %>% 
    gather() %>%   #  wide  -> long type
    arrange(desc(value)) %>%   # sort
    slice_max(value,n = 1) %>%   # max value
    select(value)  # select that value


  
  
  output$obesity_prediction <- renderUI({
    
    if (prediction_class$.pred_class == "Underweight") {
        advice <- div(
          p(strong("You are currently in",span("Underweight" ,style = "color:red"), ", consider supplementing your diet with a balanced mix of nutrients and energy. We also advocate small, frequent and slow meals.")),
          
          p("Healthy Eating Habits:"),
          HTML("<ul style='text-align: left;'><li>In terms of diet we recommend that you supplement your diet with adequate animal protein, vegetable protein, starchy vegetables, dairy products such as: poultry (especially lean meats), milk, eggs, legumes, nuts, rice, noodles and potatoes.</li>
                <li>You may also want to consider using high-protein or high-energy supplements, protein drinks or shakes to supplement nutrients that may be lacking in your daily diet.</li>
               <li>You can increase the number of times you eat each day by splitting your meals into multiple meals, including healthy snacks, to ensure that you continue to provide your body with energy and nutrients.</li></ul>"),
          
          p("Regular Physical Activity:"),
          HTML("<ul style='text-align: left;'><li>We recommend moderate cardio and strength training to help boost appetite, increase muscle mass and overall health.</li></ul>"),
          
          p("Lifestyles:"),
          HTML("<ul style='text-align: left;'><li>You should get plenty of sleep and relaxation, which will help your body recover and digest and absorb.</li>
        <li>We recommend that you reduce your dependence on tobacco and alcohol. Tobacco and excessive alcohol consumption can have a negative impact on your health.</li></ul>"),
        )
           
         picture <- div(
           style = "margin-top: 100px;",
           img(src = "https://domf5oio6qrcr.cloudfront.net/medialibrary/10082/healthy-eating.jpg",width = 383,height = 255)) 
         
                             
        
    } else if(prediction_class$.pred_class == "Normalweight"){
      advice <- div(
        p(strong("Perfect! You are in a", span("Good physical condition",style = "color:green"), ". Please continue to live a healthy lifestyle.")),
  
        p("Healthy Eating Habits:"),
        HTML("<ul style='text-align: left;'><li>Please continue to maintain a healthy diet with adequate intake of vegetables, fruits, whole grains and proteins.</li>
        <li>Control consumption of sugar and processed foods. </li></ul>"),
        
        p("Regular Physical Activity:"),
        HTML("<ul style='text-align: left;'><li>Please continue to maintain your proper weight and health by performing at least 150 minutes of moderate-intensity cardio or 75 minutes of vigorous-intensity cardio each week. It is also beneficial to do some strength training each week.</li></ul>"),
    
        
        p("Lifestyles:"),
        HTML("<ul style='text-align: left;'><li>Please continue to get an adequate amount of sleep, which is usually recommended at 7-9 hours per night for adults.</li>
        <li>Please continue to keep avoiding smoking and limiting the amount of alcohol you consume, ideally not exceeding the weekly alcohol consumption recommendations.</li></ul>"),
      )
      
      picture <- div(
        style = "margin-top: 50px;",
        img(src = "https://www.dermascope.com/media/k2/items/cache/0408a2293faf36ad52407dd4fbf08a03_XL.jpg",width = 387,height = 258)) 
      

    } else if (prediction_class$.pred_class == "Overweight"){
      
      advice <- div(
        p(strong("You are currently in",span("Overweight" ,style = "color:red"), ", and lifestyle changes is important to address this issue")),
        
        p("Dietary changes:"),
        HTML("<ul style='text-align: left;'><li>Use a balanced diet that focuses on portion control, natural foods, and nutrient-dense choices.</li>
                <li>Reduce intake of processed foods, sugary drinks, and high-fat snacks.</li>
                <li>we also recommend that you try different dietary patterns, such as Mediterranean diet, DASH diet, and plant-based diet and meal timing, such as Intermittent Fasting and Alternate-Day Fasting.</li></ul>"),
        p("Regular Physical Activity:"),
        HTML("<ul style='text-align: left;'><li>Increase physical activity through aerobic exercise, resistance exercise, or combine them.</li>
              <li>Engaging in 150 minutes moderate-intensity exercise per week helps prevent weight gain, and dediacting at least 250 minutes per week to such activities is linked with significant weight loss clinically.</li></ul>"),
        p("Behavior Change:"),
        HTML("<ul style='text-align: left;'><li>Consider behavioral therapy or counseling to address emotional eating and develop sustainable habits.</li>
              <li>Pay more attention to self-monitoring, set realistic and achievable weight loss goals.</li></ul>"),
        
      )
      
      picture <- div(
        style = "margin-top: 50px;",
        img(src = "https://i.etsystatic.com/19837560/r/il/7a91f7/3238749288/il_570xN.3238749288_e3w2.jpg",width = 387,height = 236)) 
      
    } else if (prediction_class$.pred_class == "Obesity_I"){
      advice <- div(
        p(strong("You are currently in",span("Obesity Level I" ,style = "color:red"), ". Lifestyle changes may not be effective interventions, considering medication as another viable option.")),
       
        p("Dietary Changes:"),
        HTML("<ul style='text-align: left;'><li>Focus on consuming a well-balanced diet that includes adequate protein, high-fiber foods and controlled sugar intake.</li>
              <li>Reduce intake of processed foods, sugary drinks, and high-fat snacks.</li>
              <li>we also recommend that you try different dietary patterns, such as Mediterranean diet, DASH diet, and plant-based diet and meal timing, such as Intermittent Fasting and Alternate-Day Fasting.</li></ul>"),
        p("Regular Physical Activity:"),
        HTML("<ul style='text-align: left;'><li>Increase physical activity through aerobic exercise, resistance exercise, or combine them.</li>
              <li>Engaging in 150 minutes moderate-intensity exercise per week helps prevent weight gain, and dediacting at least 250 minutes per week to such activities is linked with significant weight loss clinically.</li></ul>"),
        p("Behavior Change:"),
        HTML("<ul style='text-align: left;'><li>Consider behavioral therapy or counseling to address emotional eating and develop sustainable habits.</li>
              <li>Pay more attention to self-monitoring, set realistic and achievable weight loss goals.</li></ul>"),
        p("Medication:"),
        HTML("<ul style='text-align: left;'><li>Seeking medical advice, consider using anti-obesity medications such as Orlistat, phentermine/topramax, and naltrexone/acetone under the supervision of a physician.</li></ul>"),
      )
      
      picture <- div(
        style = "margin-top: 50px;",
        img(src = "https://www.verywellhealth.com/thmb/RoOXYaFSwXpNa_qSkQTsXjMhw0s=/1500x0/filters:no_upscale():max_bytes(150000):strip_icc()/VWH-GettyImages-56658752-6faa867865ed400f925770decc9844fd.jpg",width = 390,height = 265)) 
      
    } else if (prediction_class$.pred_class == "Obesity_II"){
      advice <- div(
        p(strong("You are currently in",span("Obesity Level II" ,style = "color:red"), ". Lifestyle changes may not be effective interventions, considering medication as an alternative option.")),
       
        p("Dietary Changes:"),
        HTML("<ul style='text-align: left;'><li>Focus on consuming a well-balanced diet that includes adequate protein, high-fiber foods and controlled sugar intake.</li>
              <li>Reduce intake of processed foods, sugary drinks, and high-fat snacks.</li>
              <li>we also recommend that you try different dietary patterns, such as Mediterranean diet, DASH diet, and plant-based diet and meal timing, such as Intermittent Fasting and Alternate-Day Fasting.</li></ul>"),
        p("Regular Physical Activity:"),
        HTML("<ul style='text-align: left;'><li>Increase physical activity through aerobic exercise, resistance exercise, or combine them.</li>
              <li>Engaging in 150 minutes moderate-intensity exercise per week helps prevent weight gain, and dediacting at least 250 minutes per week to such activities is linked with significant weight loss clinically.</li></ul>"),
        p("Behavior Change:"),
        HTML("<ul style='text-align: left;'><li>Consider behavioral therapy or counseling to address emotional eating and develop sustainable habits.</li>
              <li>Pay more attention to self-monitoring, set realistic and achievable weight loss goals.</li></ul>"),
        p("Medication:"),
        HTML("<ul style='text-align: left;'><li>Seeking medical advice, consider using anti-obesity medications such as Orlistat, phentermine/topramax, and naltrexone/acetone under the supervision of a physician.</li></ul>"),
      )
      
      picture <- div(
        style = "margin-top: 50px;",
        img(src = "https://www.statnews.com/wp-content/uploads/2016/05/AntiObesityPill_AP110172404-1024x576.jpg",width = 389,height = 218)) 
      
    } else {
      advice <- div(
        p(strong("You are currently in",span("Obesity Level III" ,style = "color:red"), ". Lifestyle changes and medication may not be as effective interventions for you in this obesity level, considering bariatric surgeryas as an alternative option")),
        
        p("Dietary Changes:"),
        HTML("<ul style='text-align: left;'><li>Focus on consuming a well-balanced diet that includes adequate protein, high-fiber foods and controlled sugar intake.</li>
              <li>Reduce intake of processed foods, sugary drinks, and high-fat snacks.</li>
              <li>we also recommend that you try different dietary patterns, such as Mediterranean diet, DASH diet, and plant-based diet and meal timing, such as Intermittent Fasting and Alternate-Day Fasting.</li></ul>"),
        p("Regular Physical Activity:"),
        HTML("<ul style='text-align: left;'><li>Increase physical activity through aerobic exercise, resistance exercise, or combine them.</li>
              <li>Engaging in 150 minutes moderate-intensity exercise per week helps prevent weight gain, and dediacting at least 250 minutes per week to such activities is linked with significant weight loss clinically.</li></ul>"),
        p("Behavior Change:"),
        HTML("<ul style='text-align: left;'><li>Consider behavioral therapy or counseling to address emotional eating and develop sustainable habits.</li>
              <li>Pay more attention to self-monitoring, set realistic and achievable weight loss goals.</li></ul>"),
        p("Gastrointestinal surgery:"),
        HTML("<ul style='text-align: left;'><li>Consult with your doctor to discuss the possibility of bariatric surgery.</li>
          <li>Evaluate different surgical options, such as gastrointestinal bypass (BPD), gastrectomy (SG), or Roux-en-Y gastric bypass (RYGB), based on individual health status and preferences.</li></ul>"),
      )
      
      picture <- div(
        style = "margin-top: 50px;",
        img(src = "https://medicine.wustl.edu/wp-content/uploads/Intestines.jpg",width = 378,height = 252)) 
      
    }
    
    
    prediction_box <- valueBox("obesity_value", 
                   value = paste0(round(100 * prediction_prob$value, 1), "%"),
                   subtitle = tags$h4(paste0("Your obesity level is: ", prediction_class$.pred_class), style = "text-align: center; "),
                   icon = icon("user-plus"),
                   color = "yellow"
                   )
                   
   
    
    suggestion_box <- shinydashboard::box(
      title = tags$h3("Suggestion: ", style = "text-align: left; margin-top: 0px; padding-bottom: 10px; border-bottom: 2px solid #ccc;"),
      status = "info",
      advice,
      width = 8, 
      solidHeader = TRUE, 
    )
    
    
    picture_box <- shinydashboard::box(
      picture,
      width = 4, 
      solidHeader = TRUE, 

    )
    
    tagList(prediction_box, suggestion_box, picture_box )
    
    
  })
  
    updateTabItems(session, "tabs", "results")
  
  })
  
  observeEvent(input$evaluationPage,{
    updateTabItems(session, "tabs", "predictors")
  })
  
  
}
  