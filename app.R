library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

data_gender <- data.frame(
  Gender = c("Female", "Male"),
  Responses = c(80, 25),
  Percentage = c(76.2, 23.8)
)
inference_gender <- "The majority of the respondents (76.2%) are Female, while 23.8% are Male, indicating a higher representation of female consumers in the survey."

data_age <- data.frame(
  Age_Group = c("8 – 24 years", "25 – 35 years", "36 – 50 years", "Above 50 years"),
  Responses = c(73, 18, 7, 7),
  Percentage = c(69.5, 17.1, 6.7, 6.7)
)
inference_age <- "The largest age group among respondents is 8-24 years (69.5%), followed by 25-35 years (17.1%). The older age groups (36-50 and Above 50) have a smaller representation at 6.7% each."

data_education <- data.frame(
  Qualification = c("10th/ 12th Equivalent", "Degree/ Diploma", "Master’s Degree", "Doctorate Degree", "Others"),
  Responses = c(21, 64, 12, 5, 3),
  Percentage = c(20.0, 61.0, 11.4, 4.8, 2.9)
)
inference_education <- "A significant majority of respondents (61%) hold a Degree/Diploma. 20% have 10th/12th equivalent qualifications, while Master's and Doctorate degrees account for smaller percentages (11.4% and 4.8% respectively)."

data_occupation <- data.frame(
  Occupation = c("Government Employee", "Private Employee", "Business", "Student", "Others"),
  Responses = c(6, 27, 3, 67, 2),
  Percentage = c(5.7, 25.7, 2.9, 63.8, 1.9)
)
inference_occupation <- "Students constitute the largest occupational group among respondents (63.8%). Private employees make up 25.7%, with other categories having much smaller representation."

data_income <- data.frame(
  Income_Group = c("Less than 10000", "10000 – 30000", "30001 - 50000", "50001 - 70000", "More than 70000"),
  Responses = c(61, 23, 8, 7, 6),
  Percentage = c(58.1, 21.9, 7.6, 6.7, 5.7)
)
inference_income <- "A substantial portion of respondents (58.1%) have a monthly income of less than ₹10,000, which aligns with the high percentage of students in the survey. The income groups decrease significantly as the income level rises."

data_buy_branded <- data.frame(
  Opinion = c("Yes", "No"),
  Responses = c(93, 12),
  Percentage = c(88.6, 11.4)
)
inference_buy_branded <- "A strong majority of consumers (88.6%) prefer to buy branded FMCG products, indicating a significant influence of brands in the Ernakulam district."

data_frequency_branded <- data.frame(
  Frequency = c("Always", "Often", "Rarely", "Never"),
  Responses = c(28, 52, 24, 1),
  Percentage = c(26.7, 49.5, 22.9, 1.0)
)
inference_frequency_branded <- "Most respondents (49.5%) 'Often' buy branded FMCG products, while 26.7% 'Always' do. Only a small fraction (1%) 'Never' purchases branded products."

data_influencing_factor_purchase <- data.frame(
  Factor = c("Quality", "Price", "Offers", "Availability", "Packaging", "Loyalty", "Confidence/ Trust in branded products", "Advertisement", "Influenced by others", "All of the above", "Others"),
  Responses = c(44, 9, 3, 2, 6, 1, 8, 2, 1, 27, 2),
  Percentage = c(41.9, 8.6, 2.9, 1.9, 5.7, 1.0, 7.6, 1.9, 1.0, 25.7, 1.9)
)
inference_influencing_factor_purchase <- "Quality is the most influencing factor (41.9%) for purchasing branded FMCG products, followed by 'All of the above' factors (25.7%). Confidence/Trust (7.6%) and Packaging (5.7%) also play a role, while Loyalty and being influenced by others have minimal impact (1% each)."

data_fmcg_categories_raw <- data.frame(
  Category = rep(c("Home care", "Personal care", "Food and Beverages", "Alcohol and Cigarettes", "OTC-Over the Counter Medicines"), each = 4),
  Frequency = rep(c("Always", "Often", "Rarely", "Never"), 5),
  Responses = c(28, 56, 18, 3, 33, 48, 24, 0, 43, 42, 17, 3, 7, 14, 17, 67, 18, 27, 40, 20),
  Percentage_Category = c(21.7, 30.0, 15.5, 3.2, 25.6, 25.7, 20.7, 0, 33.3, 22.4, 14.6, 3.2, 5.4, 7.5, 14.6, 72.0, 14.0, 14.4, 34.5, 21.5)
)
data_fmcg_categories <- data_fmcg_categories_raw %>%
  group_by(Category) %>%
  mutate(Percentage_within_Category = Responses / sum(Responses) * 100) %>%
  ungroup() %>%
  mutate(Frequency = factor(Frequency, levels = c("Always", "Often", "Rarely", "Never")))
inference_fmcg_categories <- "Food and Beverages are 'Always' purchased by the highest percentage of consumers (33.3%). Home Care and Personal Care products are 'Often' purchased by 30% and 25.7% of respondents respectively. A significant 72% 'Never' purchase branded products in the Alcohol and Cigarettes category. OTC Medicines are 'Rarely' purchased by 34.5% of consumers."

data_brand_switching_6months <- data.frame(
  Opinion = c("Yes", "No", "Neutral"),
  Responses = c(45, 17, 43),
  Percentage = c(42.9, 16.2, 41.0)
)
inference_brand_switching_6months <- "Almost equal percentages of respondents have 'Yes' (42.9%) or 'Neutral' (41%) opinions on brand switching in FMCG over the past 6 months. This suggests a significant portion of consumers are open to or have recently switched brands, indicating a lack of strong brand loyalty."

data_frequency_switching <- data.frame(
  Frequency = c("Always", "Often", "Rarely", "Never"),
  Responses = c(18, 43, 40, 4),
  Percentage = c(17.1, 41.0, 38.1, 3.8)
)
inference_frequency_switching <- "While 41% 'Often' switch FMCG brands and 38.1% 'Rarely' switch, only a small percentage (3.8%) 'Never' switches, reinforcing the finding that brand loyalty is not overwhelmingly high among respondents."

data_influencing_factor_switching <- data.frame(
  Factor = c("Advertising", "Sales Promotion", "Peer Influence", "Price Difference", "Others"),
  Responses = c(33, 2, 12, 34, 24),
  Percentage = c(31.4, 1.9, 11.4, 32.4, 22.9)
)
inference_influencing_factor_switching <- "Price Difference (32.4%) and Advertising (31.4%) are the two most significant factors influencing brand switching. 'Others' also account for a substantial portion (22.9%), while Sales Promotion and Peer Influence have less impact."

data_packaging_influence <- data.frame(
  Level = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"),
  Responses = c(34, 48, 20, 3, 0),
  Percentage = c(32.4, 45.7, 19.0, 2.9, 0.0)
)
inference_packaging_influence <- "A large majority of respondents either 'Agree' (45.7%) or 'Strongly agree' (32.4%) that FMCG product packaging influences their purchase decision, highlighting its importance as a visual cue."

data_influencing_factors_packaging <- data.frame(
  Factor = c("Colour", "Convenience", "Protection"),
  Responses = c(15, 41, 49),
  Percentage = c(14.3, 39.0, 46.7)
)
inference_influencing_factors_packaging <- "Protection (46.7%) and Convenience (39%) are the primary factors influencing consumers in FMCG packaging, while Colour (14.3%) is considered less important."

data_visual_attraction_packaging <- data.frame(
  Factor = c("Design", "Colour", "Images", "All of the above"),
  Responses = c(17, 4, 4, 80),
  Percentage = c(16.2, 3.8, 3.8, 76.2)
)
inference_visual_attraction_packaging <- "For visual attraction in FMCG packaging, an overwhelming majority (76.2%) are attracted by 'All of the above' factors (Design, Colour, Images). Design alone attracts 16.2%, while Colour and Images individually have minimal impact (3.8% each)."

data_comparing_by_packaging <- data.frame(
  Level = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"),
  Responses = c(26, 49, 24, 6, 0),
  Percentage = c(24.8, 46.7, 22.8, 5.7, 0.0)
)
inference_comparing_by_packaging <- "Most respondents 'Agree' (46.7%) or 'Strongly agree' (24.8%) that they compare different products based on packaging when buying, indicating packaging's role as a key comparison factor."

data_info_in_packaging <- data.frame(
  Level = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"),
  Responses = c(29, 54, 18, 4, 0),
  Percentage = c(27.6, 51.4, 17.1, 3.9, 0.0)
)
inference_info_in_packaging <- "A large majority (51.4% 'Agree', 27.6% 'Strongly agree') believe that FMCG packaging provides all necessary product information."

data_important_info_packaging <- data.frame(
  Info_Type = c("Price", "Manufacturing and expiry date", "Contents", "Usage", "All of the above"),
  Responses = c(7, 11, 5, 3, 79),
  Percentage = c(6.7, 10.5, 4.8, 2.8, 75.2)
)
inference_important_info_packaging <- "The vast majority of respondents (75.2%) believe that 'All of the above' information (Price, Mfg/Expiry Date, Contents, Usage) should be included in FMCG packaging. Manufacturing and expiry date (10.5%) is the next most important individual factor."

data_preference_eco_friendly <- data.frame(
  Level = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"),
  Responses = c(63, 35, 7, 0, 0),
  Percentage = c(60.0, 33.3, 6.7, 0.0, 0.0)
)
inference_preference_eco_friendly <- "An overwhelming majority of consumers either 'Strongly agree' (60%) or 'Agree' (33.3%) to preferring eco-friendly packaging, demonstrating a high demand for sustainable options."

data_found_eco_friendly <- data.frame(
  Level = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"),
  Responses = c(20, 34, 42, 9, 0),
  Percentage = c(19.0, 32.4, 40.0, 8.6, 0.0)
)
inference_found_eco_friendly <- "While 32.4% 'Agree' and 19% 'Strongly agree' that eco-friendly packaging is found in FMCG products, a significant 40% 'Neither agree nor disagree', suggesting that while it exists, its prevalence or visibility might not be universally perceived."

data_reasons_eco_friendly <- data.frame(
  Reason = c("Nature friendly", "To eliminate toxic material", "To recycle", "All of the above"),
  Responses = c(28, 5, 4, 68),
  Percentage = c(26.7, 4.8, 3.7, 64.8)
)
inference_reasons_eco_friendly <- "The primary reason for preferring eco-friendly packaging is 'All of the above' (64.8%), encompassing nature-friendliness, elimination of toxic materials, and recyclability. Nature friendly alone is also a significant reason (26.7%)."

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap');
      body {
        font-family: 'Inter', sans-serif;
        background-color: #e0f7fa;
        color: #263238;
      }
      .title-panel {
        background: linear-gradient(135deg, #42a5f5 0%, #1976d2 100%);
        color: white;
        padding: 30px;
        border-radius: 12px;
        margin-bottom: 30px;
        text-align: center;
        box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2);
      }
      .panel-title {
        font-size: 2.5em;
        font-weight: 700;
        margin-bottom: 10px;
      }
      .subtitle {
        font-size: 1.2em;
        font-weight: 400;
        opacity: 0.9;
      }
      .well {
        background-color: #ffffff;
        border: none;
        border-radius: 12px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
        padding: 25px;
      }
      .nav-tabs {
        margin-bottom: 25px;
        border-bottom: 3px solid #b3e5fc;
        display: flex;
        justify-content: center;
      }
      .nav-tabs > li {
        margin-bottom: -3px;
      }
      .nav-tabs > li > a {
        border-radius: 10px 10px 0 0;
        margin-right: 5px;
        color: #42a5f5;
        font-weight: 600;
        padding: 12px 25px;
        transition: all 0.3s ease;
        border: 1px solid #e0e0e0;
        border-bottom: none;
      }
      .nav-tabs > li > a:hover {
        background-color: #e3f2fd;
        color: #1565c0;
        border-color: #b3e5fc;
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        color: white;
        background: linear-gradient(90deg, #2196f3 0%, #1976d2 100%);
        border-color: #1976d2;
        border-bottom-color: transparent;
        box-shadow: 0 -4px 8px rgba(0, 0, 0, 0.1);
      }
      .tab-content {
        padding: 30px;
        background-color: #ffffff;
        border-radius: 12px;
        border: none;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
      }
      .plot-container {
        margin-top: 20px;
        padding: 20px;
        background-color: #f5f5f5;
        border-radius: 10px;
        box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.06);
      }
      .inference-text {
        margin-top: 20px;
        padding: 15px;
        background-color: #e1f5fe;
        border-left: 6px solid #0288d1;
        border-radius: 8px;
        color: #01579b;
        font-size: 1.05em;
        line-height: 1.5;
      }
      h4 {
        color: #0d47a1;
        font-weight: 700;
        margin-top: 30px;
        margin-bottom: 20px;
        text-align: center;
      }
      .form-group.shiny-input-container {
        margin-bottom: 25px;
      }
      .form-control {
        border-radius: 8px;
        border: 1px solid #b0bec5;
        padding: 10px 15px;
      }
      .control-label {
        font-weight: 600;
        color: #455a64;
        margin-bottom: 8px;
      }
    "))
  ),
  
  div(class = "title-panel",
      h1("FMCG Consumer Behavior: Data Analysis"),
      h4("A Study of Branding and Packaging Influence in Bengaluru")
  ),
  
  fluidRow(
    column(12,
           tabsetPanel(
             tabPanel("Demographics",
                      h4("Classification of Demographics", class="section-heading"),
                      selectInput("demographic_var", "Select Demographic Variable to Display:",
                                  choices = c("Gender", "Age", "Educational Qualification", "Occupation", "Monthly Income")),
                      fluidRow(
                        column(8, div(class = "plot-container", plotOutput("demographicPlot"))),
                        column(4, div(class = "inference-text", textOutput("demographicInference")))
                      )
             ),
             tabPanel("Branding & Purchase",
                      h4("Branding and Purchase Behavior", class="section-heading"),
                      selectInput("branding_purchase_var", "Select Branding/Purchase Aspect to Display:",
                                  choices = c("Purchase of Branded FMCG Products", "Frequency of Purchase (Branded FMCG)", "Most Influencing Factor for Purchase")),
                      fluidRow(
                        column(8, div(class = "plot-container", plotOutput("brandingPurchasePlot"))),
                        column(4, div(class = "inference-text", textOutput("brandingPurchaseInference")))
                      )
             ),
             tabPanel("FMCG Categories Usage",
                      h4("FMCG Categories: Frequency of Usage", class="section-heading"),
                      fluidRow(
                        column(8, div(class = "plot-container", plotOutput("fmcgCategoriesPlot"))),
                        column(4, div(class = "inference-text", textOutput("fmcgCategoriesInference")))
                      )
             ),
             tabPanel("Brand Switching",
                      h4("Brand Switching Behavior", class="section-heading"),
                      selectInput("brand_switching_var", "Select Brand Switching Aspect to Display:",
                                  choices = c("Brand Switching in Past 6 Months", "Frequency of Switching Brands", "Most Influencing Factor for Switching")),
                      fluidRow(
                        column(8, div(class = "plot-container", plotOutput("brandSwitchingPlot"))),
                        column(4, div(class = "inference-text", textOutput("brandSwitchingInference")))
                      )
             ),
             tabPanel("Packaging Influence",
                      h4("Influence of Packaging on Purchase Decision", class="section-heading"),
                      selectInput("packaging_influence_var", "Select Packaging Aspect to Display:",
                                  choices = c("Packaging Influence on Purchase Decision", "Influencing Factors in Packaging", "Visual Attraction in Packaging", "Comparing Products by Packaging", "Packaging Providing Information", "Important Information to Include in Packaging")),
                      fluidRow(
                        column(8, div(class = "plot-container", plotOutput("packagingInfluencePlot"))),
                        column(4, div(class = "inference-text", textOutput("packagingInfluenceInference")))
                      )
             ),
             tabPanel("Eco-Friendly Packaging",
                      h4("Eco-Friendly Packaging Preferences", class="section-heading"),
                      selectInput("eco_friendly_var", "Select Eco-Friendly Packaging Aspect to Display:",
                                  choices = c("Preference for Eco-Friendly Packaging", "Eco-Friendly Packaging Found in FMCG", "Reasons for Preferring Eco-Friendly Packaging")),
                      fluidRow(
                        column(8, div(class = "plot-container", plotOutput("ecoFriendlyPlot"))),
                        column(4, div(class = "inference-text", textOutput("ecoFriendlyInference")))
                      )
             )
           )
    )
  )
)

server <- function(input, output) {
  
  output$demographicPlot <- renderPlot({
    req(input$demographic_var)
    data <- switch(input$demographic_var,
                   "Gender" = data_gender,
                   "Age" = data_age,
                   "Educational Qualification" = data_education,
                   "Occupation" = data_occupation,
                   "Monthly Income" = data_income)
    
    x_var <- switch(input$demographic_var,
                    "Gender" = "Gender",
                    "Age" = "Age_Group",
                    "Educational Qualification" = "Qualification",
                    "Occupation" = "Occupation",
                    "Monthly Income" = "Income_Group")
    
    ggplot(data, aes_string(x = x_var, y = "Percentage", fill = x_var)) +
      geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.8) +
      geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 4, color = "#263238") +
      labs(title = paste(input$demographic_var, "of Respondents"), y = "Percentage (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#37474F"),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 10, color = "#546E7A"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, color = "#546E7A"),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      scale_fill_brewer(palette = "Set1") # More vibrant palette: Set1
  })
  
  output$demographicInference <- renderText({
    req(input$demographic_var)
    switch(input$demographic_var,
           "Gender" = inference_gender,
           "Age" = inference_age,
           "Educational Qualification" = inference_education,
           "Occupation" = inference_occupation,
           "Monthly Income" = inference_income)
  })
  
  output$brandingPurchasePlot <- renderPlot({
    req(input$branding_purchase_var)
    data <- switch(input$branding_purchase_var,
                   "Purchase of Branded FMCG Products" = data_buy_branded,
                   "Frequency of Purchase (Branded FMCG)" = data_frequency_branded,
                   "Most Influencing Factor for Purchase" = data_influencing_factor_purchase)
    
    x_var <- switch(input$branding_purchase_var,
                    "Purchase of Branded FMCG Products" = "Opinion",
                    "Frequency of Purchase (Branded FMCG)" = "Frequency",
                    "Most Influencing Factor for Purchase" = "Factor")
    
    if (input$branding_purchase_var == "Frequency of Purchase (Branded FMCG)") {
      data$Frequency <- factor(data$Frequency, levels = c("Always", "Often", "Rarely", "Never"))
    } else if (input$branding_purchase_var == "Most Influencing Factor for Purchase") {
      data$Factor <- factor(data$Factor, levels = data$Factor[order(-data$Percentage)])
    }
    
    ggplot(data, aes_string(x = x_var, y = "Percentage", fill = x_var)) +
      geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.8) +
      geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 4, color = "#263238") +
      labs(title = paste(input$branding_purchase_var), y = "Percentage (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#37474F"),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 10, color = "#546E7A"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, color = "#546E7A"),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      scale_fill_brewer(palette = "Dark2") 
  })
  
  output$brandingPurchaseInference <- renderText({
    req(input$branding_purchase_var)
    switch(input$branding_purchase_var,
           "Purchase of Branded FMCG Products" = inference_buy_branded,
           "Frequency of Purchase (Branded FMCG)" = inference_frequency_branded,
           "Most Influencing Factor for Purchase" = inference_influencing_factor_purchase)
  })
  
  output$fmcgCategoriesPlot <- renderPlot({
    ggplot(data_fmcg_categories, aes(x = Category, y = Percentage_within_Category, fill = Frequency)) +
      geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.8) +
      labs(title = "FMCG Categories: Frequency of Usage", y = "Percentage Within Category (%)", fill = "Frequency") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#37474F"),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 10, color = "#546E7A"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, color = "#546E7A"),
        legend.position = "right",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      scale_fill_manual(values = c("Always" = "#00C853", "Often" = "#2979FF", "Rarely" = "#FFD600", "Never" = "#D50000")) # Even more vibrant custom colors
  })
  
  output$fmcgCategoriesInference <- renderText({
    inference_fmcg_categories
  })
  
  output$brandSwitchingPlot <- renderPlot({
    req(input$brand_switching_var)
    data <- switch(input$brand_switching_var,
                   "Brand Switching in Past 6 Months" = data_brand_switching_6months,
                   "Frequency of Switching Brands" = data_frequency_switching,
                   "Most Influencing Factor for Switching" = data_influencing_factor_switching)
    
    x_var <- switch(input$brand_switching_var,
                    "Brand Switching in Past 6 Months" = "Opinion",
                    "Frequency of Switching Brands" = "Frequency",
                    "Most Influencing Factor for Switching" = "Factor")
    
    if (input$brand_switching_var == "Frequency of Switching Brands") {
      data$Frequency <- factor(data$Frequency, levels = c("Always", "Often", "Rarely", "Never"))
    } else if (input$brand_switching_var == "Most Influencing Factor for Switching") {
      data$Factor <- factor(data$Factor, levels = data$Factor[order(-data$Percentage)])
    }
    
    ggplot(data, aes_string(x = x_var, y = "Percentage", fill = x_var)) +
      geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.8) +
      geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 4, color = "#263238") +
      labs(title = paste(input$brand_switching_var), y = "Percentage (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#37474F"),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 10, color = "#546E7A"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, color = "#546E7A"),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      scale_fill_brewer(palette = "Spectral") # More dramatic palette: Spectral
  })
  
  output$brandSwitchingInference <- renderText({
    req(input$brand_switching_var)
    switch(input$brand_switching_var,
           "Brand Switching in Past 6 Months" = inference_brand_switching_6months,
           "Frequency of Switching Brands" = inference_frequency_switching,
           "Most Influencing Factor for Switching" = inference_influencing_factor_switching)
  })
  
  output$packagingInfluencePlot <- renderPlot({
    req(input$packaging_influence_var)
    data <- switch(input$packaging_influence_var,
                   "Packaging Influence on Purchase Decision" = data_packaging_influence,
                   "Influencing Factors in Packaging" = data_influencing_factors_packaging,
                   "Visual Attraction in Packaging" = data_visual_attraction_packaging,
                   "Comparing Products by Packaging" = data_comparing_by_packaging,
                   "Packaging Providing Information" = data_info_in_packaging,
                   "Important Information to Include in Packaging" = data_important_info_packaging)
    
    x_var <- switch(input$packaging_influence_var,
                    "Packaging Influence on Purchase Decision" = "Level",
                    "Influencing Factors in Packaging" = "Factor",
                    "Visual Attraction in Packaging" = "Factor",
                    "Comparing Products by Packaging" = "Level",
                    "Packaging Providing Information" = "Level",
                    "Important Information to Include in Packaging" = "Info_Type")
    
    if (x_var == "Level") {
      data[[x_var]] <- factor(data[[x_var]], levels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))
    } else if (input$packaging_influence_var == "Important Information to Include in Packaging") {
      data$Info_Type <- factor(data$Info_Type, levels = data$Info_Type[order(-data$Percentage)])
    }
    
    ggplot(data, aes_string(x = x_var, y = "Percentage", fill = x_var)) +
      geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.8) +
      geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 4, color = "#263238") +
      labs(title = paste(input$packaging_influence_var), y = "Percentage (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#37474F"),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 10, color = "#546E7A"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, color = "#546E7A"),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      scale_fill_brewer(palette = "RdBu") # Contrasting palette: RdBu
  })
  
  output$packagingInfluenceInference <- renderText({
    req(input$packaging_influence_var)
    switch(input$packaging_influence_var,
           "Packaging Influence on Purchase Decision" = inference_packaging_influence,
           "Influencing Factors in Packaging" = inference_influencing_factors_packaging,
           "Visual Attraction in Packaging" = inference_visual_attraction_packaging,
           "Comparing Products by Packaging" = inference_comparing_by_packaging,
           "Packaging Providing Information" = inference_info_in_packaging,
           "Important Information to Include in Packaging" = inference_important_info_packaging)
  })
  
  output$ecoFriendlyPlot <- renderPlot({
    req(input$eco_friendly_var)
    data <- switch(input$eco_friendly_var,
                   "Preference for Eco-Friendly Packaging" = data_preference_eco_friendly,
                   "Eco-Friendly Packaging Found in FMCG" = data_found_eco_friendly,
                   "Reasons for Preferring Eco-Friendly Packaging" = data_reasons_eco_friendly)
    
    x_var <- switch(input$eco_friendly_var,
                    "Preference for Eco-Friendly Packaging" = "Level",
                    "Eco-Friendly Packaging Found in FMCG" = "Level",
                    "Reasons for Preferring Eco-Friendly Packaging" = "Reason")
    
    if (x_var == "Level") {
      data[[x_var]] <- factor(data[[x_var]], levels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))
    } else if (input$eco_friendly_var == "Reasons for Preferring Eco-Friendly Packaging") {
      data$Reason <- factor(data$Reason, levels = data$Reason[order(-data$Percentage)])
    }
    
    ggplot(data, aes_string(x = x_var, y = "Percentage", fill = x_var)) +
      geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.8) +
      geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 4, color = "#263238") +
      labs(title = paste(input$eco_friendly_var), y = "Percentage (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#37474F"),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 10, color = "#546E7A"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, color = "#546E7A"),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      scale_fill_brewer(palette = "Greens") # Stronger green palette for eco-friendly
  })
  
  output$ecoFriendlyInference <- renderText({
    req(input$eco_friendly_var)
    switch(input$eco_friendly_var,
           "Preference for Eco-Friendly Packaging" = inference_preference_eco_friendly,
           "Eco-Friendly Packaging Found in FMCG" = inference_found_eco_friendly,
           "Reasons for Preferring Eco-Friendly Packaging" = inference_reasons_eco_friendly)
  })
  
}

shinyApp(ui = ui, server = server)
