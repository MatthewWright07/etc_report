
# packages
library(shinydashboard)
library(tidyverse)
library(shiny)
library(gtable)
library(gt)  # Added gt library
library(pagedown)
library(webshot)
library(htmlwidgets)
library(readr)
library(rsconnect)
library(shinyscreenshot)

rsconnect::setAccountInfo(name='matthew-wright07',
                          token='4BD027099DF9AA7AE30C519C5B493E5F',
                          secret='vhXiCSyVl1xjcyZmEX6Tm4alhnHv26HEsQ2Sp5Bb')


url <- "https://raw.githubusercontent.com/MatthewWright07/etc_report/main/data.csv"
data <- read_csv(url)


data <- unique(data)

report_16s <- data %>%
  filter(group != "DS") %>% 
  select(c(id, 
           group, 
           age_at_test, 
           time_point, 
           Maturity_offset, 
           Jump_height = cmj_height, 
           Jump_speed = cmj_rsi,
           Bounciness = rj_rsi, 
           Strength = max_force,
           Relative_strength = rel_max_force, 
           l_ham,
           r_ham,
           x10m_accel = x10m,
           x20m_speed = x20m,
           Fitness = ift,)) %>%
  filter(group %in% c("u14s", "u16s"))

mean_data_16s <- report_16s %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) 

table_16s <- report_16s %>%
  filter(time_point %in% c("August 2024", "April 2025")) 


mean_data_16s <- mean16s %>%
  group_by(id) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) 



#mean_data<- na.omit(mean_data)

mean_u16s_values <- mean_data_16s %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

sd_u16s_values <- mean_data_16s %>%
  summarize(across(where(is.numeric), sd, na.rm = TRUE))

# Calculate z-scores
z_scores <- report_16s %>%
  mutate(across(where(is.numeric), ~ (.-mean_u16s_values[[cur_column()]])/sd_u16s_values[[cur_column()]])) %>%
  mutate(Speed = x20m_speed*-1,
         Accel = x10m_accel*-1) %>% select(-c(x10m_accel, x20m_speed, age_at_test))

t_scores <- z_scores %>%
  mutate(across(where(is.numeric), ~ . * 10 + 50)) 



# pivot long

t_score_long <-pivot_longer(t_scores, cols = c(4:14), names_to = "Test", values_to = "Rating") 

score_long <- report_16s  %>%
  select(-c("age_at_test")) %>%
  rename(Speed = x20m_speed,
         Accel = x10m_accel) %>%
  pivot_longer(., cols = c(4:14), names_to = "Test", values_to = "Score") 

t_score_long <- left_join(t_score_long, score_long, 
                          by = c( "id", "group",  "time_point", "Test"))

t_score_long <-t_score_long %>%
  mutate(Descriptor = case_when(
    Rating < 45 ~ "Area to improve",
    Rating  >= 45 & Rating  < 55 ~ "Average",
    Rating >= 55 & Rating < 65 ~ "Above average",
    Rating >= 65 & Rating < 75 ~ "Area of strength",
    Rating  >= 75 ~ "Super strength",
    TRUE ~ NA_character_  # Default case if none of the above conditions are met
  ))


report_table <- t_scores %>%
  mutate(
    Hamstring_strength = rowMeans(select(., l_ham, r_ham), na.rm = TRUE)
  )  %>%
  select(id, 
         group, 
         time_point, 
         Jump_height,
         Jump_speed,
         Bounciness,
         Strength,
         Relative_strength, 
         Hamstring_strength,
         Accel,
         Fitness) %>% 
  pivot_longer(., cols = c(4:11), names_to = "Test", values_to = "Rating") 


score_table <- report_16s %>%
  select(-c("age_at_test")) %>%
  rename(Speed = x20m_speed,
         Accel = x10m_accel) %>%
  mutate(
    Hamstring_strength = rowMeans(select(., l_ham, r_ham), na.rm = TRUE)
  )  %>%
  select(id, 
         group, 
         time_point, 
         Jump_height,
         Jump_speed,
         Bounciness,
         Strength,
         Relative_strength, 
         Hamstring_strength,
         Accel,
         Fitness) %>% 
  pivot_longer(., cols = c(4:11), names_to = "Test", values_to = "Score") 

report_table <- left_join(report_table, score_table, 
          by = c( "id", "group",  "time_point", "Test"))

report_table <- report_table %>%
  mutate(Descriptor = case_when(
    Rating < 45 ~ "Area to improve",
    Rating  >= 45 & Rating  < 55 ~ "Average",
    Rating >= 55 & Rating < 65 ~ "Above average",
    Rating >= 65 & Rating < 75 ~ "Area of strength",
    Rating  >= 75 ~ "Super strength",
    TRUE ~ NA_character_  # Default case if none of the above conditions are met
  ))


report_table <- report_table%>% 
  filter(time_point %in% c("April 2025", "August 2024")) %>% 
  pivot_wider(., names_from = time_point, values_from = c(Rating, Score, Descriptor) ) %>% 
  select(-c(`Rating_August 2024`, `Descriptor_August 2024`)) %>% 
  rename(`August 2024`=  `Score_August 2024`,
         `April 2025`=  `Score_April 2025`,
         `FIFA Rating`=  `Rating_April 2025`,
         `Descriptor`=  `Descriptor_April 2025`,
  )


table_long <- report_table %>%
  mutate(Change = round(`April 2025` - `August 2024`, 2)) %>%
  filter(!is.na(`April 2025`))%>%
  select(id, Test, `August 2024`, `April 2025`, Change,`FIFA Rating`, `Descriptor`)


descriptions <- data.frame(
  Test = c("Jump_height", "Jump_speed", "Bounciness", "Strength", "Relative_strength", "Hamstring_strength", "Fitness", "Speed", "Accel"),
  Description = c("How hight you jumped in cm, indicates leg power", "Both how high and how fast you jumped (m/s)", "Your ability to jump repeatedly (m/s)", "Isometric mid-thigh pull strength (N)", "Strength relative to body weight on the mid-thigh pull (N/m)", "Hamstring strength (average of left & right leg) (N)", "Speed at the final completed stage of the 30:15 test (km/hr)", "Your time to sprint 20 m (s)","Your time to sprint 10 m (s)")
)


table_long <- table_long %>%
  left_join(descriptions, by = "Test")

thresholds <- list(
  "Maturity_offset" = c(-2.5, 2.5),
  "Jump_height" = c(-2.1, 2.1),
  "Jump_speed" = c(-0.05, 0.05),
  "Bounciness" = c(-0.11, 0.11),
  "Strength" = c(-80, 80),
  "Relative_strength" = c(-2.0, 2.0),
  "Hamstring_strength" = c(-20, 20),
  "Accel" = c(-0.07, 0.07),
  "Speed" = c(-0.05, 0.05),
  "Fitness" = c(-0.5, 0.5)
  # Add more tests and their thresholds as needed
)


# Function to apply thresholds
apply_thresholds <- function(df, thresholds) {
  df %>%
    rowwise() %>%
    mutate('from August' = case_when(
      Change > thresholds[[Test]][2] ~ "Improving",
      Change < thresholds[[Test]][1] ~ "Reducing",
      TRUE ~ "Staying the same"
    )) %>%
    ungroup()
}

# Preprocess the data to include color information
table_long <- apply_thresholds(table_long, thresholds)


table_long <- table_long %>%
  mutate(
    `August 2024` = signif(`August 2024`, 3),
    `April 2025` = signif(`April 2025`, 3),
    Change = signif(Change, 3),
    `FIFA Rating` = signif(`FIFA Rating`, 2)
  )

TSA <- table_long %>% select(c(id, Test,  `FIFA Rating`)) %>% rename( Rating = `FIFA Rating`)


TSA_mean <- TSA %>%
  group_by(id) %>%
  filter(Rating == mean(Rating, na.rm = TRUE)) %>%
  slice(1) # In case there are ties, this will select the first occurrence

TSA_max <- TSA %>%
  group_by(id) %>%
  filter(Rating == max(Rating, na.rm = TRUE)) %>%
  slice(1) # In case there are ties, this will select the first occurrence



TSA_min <- TSA %>%
  group_by(id) %>%
  filter( Rating == min(Rating, na.rm = TRUE)) %>%
  slice(1) # In case there are ties, this will select the first occurrence





ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "ETC Testing Report"),
  dashboardSidebar(
    selectInput("selected_id", "Select ID:", choices = unique(report_16s$id))
   
  ),
  dashboardBody(
    actionButton("go", "Screenshot Report"),
    fluidRow(
      infoBoxOutput("tasBox"),
      infoBoxOutput("superStrengthBox"),
      infoBoxOutput("areaForImprovementBox")
    ),
    fluidRow(
      box(title = "Text Box", width = 6, htmlOutput("text")),
      box(title = "Results Plot", width = 6, plotOutput("plot"))
     
    ),
    fluidRow(
      box(title = "Results Table", width = 12, gt::gt_output("table"))
    )
  )
)


# Define the create_gt_table function
create_gt_table <- function(df) {
  df %>%
    select(Test, Description, `August 2024`, `April 2025`, `FIFA Rating`, Descriptor, Change, `from August`) %>%
    gt() %>%
    fmt_missing(columns = everything(), missing_text = "")
  
}

server <- function(input, output) {
  filtered_data <- reactive({
    t_score_long %>%
      filter(id == input$selected_id & time_point == "April 2025" & Test != "Maturity_offset")
  })
  
  filtered_table <- reactive({
    table_long %>%
      filter(id == input$selected_id)
  })
  
  filtered_TSA <- reactive({
    TSA_mean %>%
      filter(id == input$selected_id)
  })
  
  filtered_TSA_max <- reactive({
    TSA_max %>%
      filter(id == input$selected_id)
  })
  
  filtered_TSA_min <- reactive({
    TSA_min %>%
      filter(id == input$selected_id)
  })
  
  output$plot <- renderPlot({
    indi_t <- filtered_data()
    TAS <- round(mean(indi_t$Rating), 0)
    
    indi_t %>%
      ggplot() +
      aes(x = Test, y = Rating, fill = Test) +
      ylim(-20, 100) +
      geom_col(position = "dodge2") +
      scale_fill_viridis_d(option = "cividis", direction = 1) +
      coord_polar(theta = "x", clip = "off") +
      theme_minimal() +
      theme(axis.text.y = element_blank(), 
            axis.title = element_blank(), 
            legend.position = "bottom",
            axis.text.x = element_text(size = 12, face = "bold")) +
      annotate("text", x = 0, y = -18, label = TAS, size = 6, color = "black") +
      theme(legend.position = "none")
  })
  
  output$table <- gt::render_gt({
    create_gt_table(filtered_table())
  })
  
  output$text <- renderUI({
    HTML(
      "<p style='font-size:16px;'>Physical fitness is important to help you perform your skill through the whole game & enable you to train harder to become a better football player:</p>
    <ul style='font-size:16px;'>
    <p style='font-size:16px;'>When football skill is similar, strong, fast, powerful players are likely to perform better in a game. The best female football teams have players who consistently achieve higher fitness tests scores than other teams.</p>
    <br style='margin-bottom: 0.5em;'>
    <p style='font-size:16px;'>This report shows you where you are against your peers and your improvement over the season. These scores are an only an estimate of your actual strength or fitness and they can be influenced by how you felt on the day, so bear this in mind.</p>
    <br style='margin-bottom: 0.5em;'>
    <p style='font-size:16px;'>Your “FIFA ranking” is out of 100, but scores between 45 & 55 are around the average for U14s and U16s players. Scores above 60 are good, and anything above 65 or 70 is excellent. This is also a good way to show the areas of fitness you need to improve a little more on and where your super strengths are.</p>
         <br style='margin-bottom: 0.5em;'>
    <p style='font-size:16px;'> A description of most tests can be found in the table but note r_ham and l_ham refer to left and right leg hamstring muscle strength.</p>"
      
    )
  })
  
  
  
  
  
  output$tasBox <- renderInfoBox({
    indi_t <- filtered_TSA()
    TSA <-  indi_t %>% select(Rating)
    
    infoBox(
      "Total Score of Athleticism (TSA)", paste("Rating:", TSA_mean$Rating), 
      icon = icon("dumbbell"),
      color = "blue"
    )
  })
  
  output$superStrengthBox <- renderInfoBox({
    indi_t <- filtered_TSA_max()
    super_strength <- indi_t %>% select(Test, Rating)
    
    infoBox(
      "Super Strength", paste("Test:", super_strength$Test, "Rating:", super_strength$Rating),
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$areaForImprovementBox <- renderInfoBox({
    indi_t <- filtered_TSA_min()
    area_for_improvement <- indi_t %>% select(Test, Rating)
    
    infoBox(
      "Area for Improvement", paste("Test:", area_for_improvement$Test, "Rating:", area_for_improvement$Rating),
      icon = icon("stairs"),
      color = "red"
    )
    
    
  })
  
  
  
  
  observeEvent(input$go, {
    screenshot()
  })
  
  
}




shinyApp(ui = ui, server = server)


