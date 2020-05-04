# nutrient tracking for the scale-havers
# by devuroasts and mike gusev
# shiny app to expose insufficient nutrients
#loading libraries
library(dplyr)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(readxl)


options('scipen' = 999)
# data is got from : USDA National Nutrient Database for Standard Reference
ingredients <- read_excel("Ingredients.xlsx", 
                          skip = 1)
# data cleaning
ingredients <- ingredients[which(ingredients$`Nutrient value` != 0), c(1:5)]
names(ingredients) <- c("UUID", "name", "nutrientCode", "nutrientName", 'valuePer100g')
ingredients <- ingredients[which(gsub("[^0-9]", "", ingredients$nutrientName) == ''), ]
# redundant nutrients removed
ingredients <- ingredients[which(!ingredients$nutrientName %in%  c('Folate, DFE',
                                                                   'Folate, food', 
                                                                   "Vitamin E, added")), ]
# # read and merge branded and non-branded foods
# branded_foods <- read.csv("food_branded.csv")
# branded_foods <- branded_foods %>% select("fdc_id", "description") %>% rename('name' = 'description')
# 
# branded_foods_nutrients <- read.csv("food_nutrient_branded.csv")
# 
# branded_foods_joined <- branded_foods %>% left_join(branded_foods_nutrients, by = 'fdc_id')
# 
# 
# foods <- read.csv("food.csv")
# foods <- foods %>% select("fdc_id", "description") %>% rename('name' = 'description')
# 
# foods_nutrients <- read.csv("food_nutrient.csv")
# foods_joined <- foods %>% left_join(foods_nutrients, by = 'fdc_id') %>% distinct(name, nutrient_id,
#                                                                                  .keep_all = T)
# 
# foods_all <- bind_rows(foods_joined, branded_foods_joined)
# 
# 
# # read in nutrients and join
# nutrientNames <- read.csv("nutrient.csv")
# names(nutrientNames) <- c("nutrient_id", 'nutrient_name','nutrient_unit', 'rm', 'rm2')
# nutrientNames <- nutrientNames %>% select("nutrient_id", 'nutrient_name','nutrient_unit')
# foods_all <- foods_all %>% left_join(nutrientNames, by = 'nutrient_id')
# 
# foods_all <- foods_all %>% select("name", 'amount', 'nutrient_name','nutrient_unit')
# 
# #removing nonsense
# foods_all <- foods_all[-which(grepl("^\\d{1,}", foods_all$nutrient_name)), ]
# foods_all$nutrient_name <- as.character(foods_all$nutrient_name)
# names(foods_all) <- c('name', 'valuePer100g', 'nutrientName', 'nutrientUnit')
# 
# # standardizing measurements
# foods_all[which(foods_all$nutrientUnit %in% c('MG', 'MG_ATE')), 'valuePer100g'] <- 
#   foods_all[which(foods_all$nutrientUnit %in% c('MG', 'MG_ATE')), 'valuePer100g'] / 1000
# 
# foods_all[which(foods_all$nutrientUnit %in%  c('MG', 'MG_ATE')), 'nutrientUnit'] <- 'G'
# 
# 
# foods_all[which(foods_all$nutrientUnit == 'UG'), 'valuePer100g'] <- 
#   foods_all[which(foods_all$nutrientUnit == 'UG'), 'valuePer100g'] /  1000000
# 
# foods_all[which(foods_all$nutrientUnit == 'UG'), 'nutrientUnit'] <- 'G'
# 
# 
# foods_all[which(foods_all$nutrientUnit == 'kJ'), 'valuePer100g'] <- 
#   foods_all[which(foods_all$nutrientUnit == 'kJ'), 'valuePer100g'] /  4.184
# 
# foods_all[which(foods_all$nutrientUnit == 'kJ'), 'nutrientUnit'] <- 'KCAL'
# 
# #IU to grams
# 
# foods_all[which(foods_all$nutrientUnit == 'IU' & grepl('vitamin A', foods_all$nutrientName, ignore.case = T)),
#           'valuePer100g'] <- 
#   foods_all[which(foods_all$nutrientUnit == 'IU' & grepl('vitamin A', foods_all$nutrientName, ignore.case = T)),
#             'valuePer100g'] * .3 / 1000000
# 
# foods_all[which(foods_all$nutrientUnit == 'IU' & grepl('vitamin A', foods_all$nutrientName, ignore.case = T)),
#           'nutrientUnit'] <- 'G'
# 
# 
# foods_all[which(foods_all$nutrientUnit == 'IU' & grepl('vitamin E', foods_all$nutrientName, ignore.case = T)),
#           'valuePer100g'] <- 
#   foods_all[which(foods_all$nutrientUnit == 'IU' & grepl('vitamin E', foods_all$nutrientName, ignore.case = T)),
#             'valuePer100g'] * .9 / 1000
# 
# foods_all[which(foods_all$nutrientUnit == 'IU' & grepl('vitamin E', foods_all$nutrientName, ignore.case = T)),
#           'nutrientUnit'] <- 'G'
# 
# 
# foods_all[which(foods_all$nutrientUnit == 'IU' & grepl('vitamin D', foods_all$nutrientName, ignore.case = T)),
#           'valuePer100g'] <- 
#   foods_all[which(foods_all$nutrientUnit == 'IU' & grepl('vitamin D', foods_all$nutrientName, ignore.case = T)),
#             'valuePer100g'] * .025 / 1000000
# 
# foods_all[which(foods_all$nutrientUnit == 'IU' & grepl('vitamin D', foods_all$nutrientName, ignore.case = T)),
#           'nutrientUnit'] <- 'G'
# 
# foods_all$nutrientUnit <- as.character(foods_all$nutrientUnit)
# 
# foods_all <- foods_all[-which(is.na(foods_all$valuePer100g) | foods_all$valuePer100g == 0), ]
# 
# ingredients <- foods_all
# 
# rm(branded_foods, foods_joined, branded_foods_joined, branded_foods_nutrients, foods, foods_nutrients, nutrientNames, foods_all)
# 
# #making names easier to read / taking out very infrequent nutrients
# ingredients <-  ingredients[-which(ingredients$nutrientName %in% c("Specific Gravity", 'Choline, free',
#                                                                   'Choline, from glycerophosphocholine',
#                                                                   "Choline, from phosphotidyl choline",
#                                                                   'Choline, from shingomyelin','Carbohydrate, by summation',
#                                                                   'Carbohydrate, other', 'Total sugar alcohols')), ]
# 
# 
# nutrientFreq <- data.frame(table(ingredients$nutrientName))
# ingredients <- ingredients[-which(ingredients$nutrientName %in% c(as.character(nutrientFreq[which(nutrientFreq$Freq <= 30), ]$Var1))),  ]
# 
# ingredients$nutrientName <-  sub(",.+", "", ingredients$nutrientName)
# ingredients$nutrientName <-  sub("\\(.+", "", ingredients$nutrientName)
# ingredients$nutrientName <- trimws(ingredients$nutrientName)
# ingredients$nutrientName <- gsub("-","", ingredients$nutrientName)
# ingredients <- ingredients %>% distinct()
# 
# #data too big
# ingredients <- ingredients[sample(nrow(ingredients),10000, replace = F), ]
# ingredients <- ingredients[order( ingredients$name, decreasing = F), ]
#ingredients value is g per 100g

# reading in fda guidelines on nutrients
nationalInstituteHealthDV <- read.csv("NationalInstituteHealthDV.csv",
                                      encoding="855", 
                                      header=FALSE, skip = 1,
                                      stringsAsFactors=FALSE)[c(1,3)]
names(nationalInstituteHealthDV) <- c('Nutrient', "FDA Recommendation (g)")


ui <- shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Inconsolata');
      
    * {
        font-family: 'Inconsolata';
        font-weight: 500;
        line-height: 1.1;
      }
      body {
        background-color: #d3d3d3;
      }
       
      footer{
        position: fixed;
        left: 8%;
        bottom: 2%;
        width:100%;
        height: 80px;
        color: 'E6E6E6';
        z-index: 1000;
      }
      @media (max-width: 600px) {
      footer {
        display: none;
        width: 0%;
        height: 0%;
      }
}
      
  )"
    ))
  ),
  
  # App title ----
  titlePanel("Nutrient Tracking For the Scale-Havers"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = 'left',
                
                # Sidebar panel for inputs ----
                
                sidebarPanel(
                  uiOutput("ingredientName"),
                  uiOutput("ingredientQuantity"),
                  #submit button
                  actionButton("submit", label = "Add Ingredient"),
                  # percent of day for daily nutrition comparison 
                  # percent of day for daily nutrition comparison 
                  radioButtons('percentOfDay', "Amount of Food Intake \n Represented \n
                                         In Ingredient List", inline = T, 
                               choices = c("Meal", "Day", "Week")),
                  #clear all ingredients
                  actionButton("clear", label = "Clear All Ingredients")
                )
                ,
                # tabs for displaying outputs ----
                mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel(h3("Ingredient List"), 
                                       DT::dataTableOutput("dataframe")),
                              tabPanel(h3("Total Nutrition"),
                                       DT::dataTableOutput("nutritionDataframe"))
                              #,
                              #tabPanel(h3("Graphs"),
                              #             plotOutput("spider"))
                  ),
                  HTML('<footer>
                       <strong>Devuroasts // Cornflower</strong> 
                    </footer>'))
  )
) 
)


server <- shinyServer(function(input, output) {
  
  # reactive ingredient, quantity pairs
  ingredientPairs <- reactiveValues(ingredientName = unique(ingredients$name),
                                    ingredientQuantity = rep(0, length(unique(ingredients$name))))
  
  # on submit, update list of ingredient / quan
  observeEvent(input$submit, {
    ingredientPairs$ingredientQuantity[which(ingredientPairs$ingredientName ==
                                               input$ingredient) ] <- input$ingredientQuantity
  })
  
  #clear all ingredientQuantities on clear
  observeEvent(input$clear, {
    ingredientPairs$ingredientQuantity <- rep(0, length(unique(ingredients$name)))
  })
  
  # reactive inputs defined here
  output$ingredientName <- renderUI({
    
    selectizeInput(inputId = 'ingredient',
                   label = 'Ingredients', 
                   choices = ingredientPairs$ingredientName,
                   selected = NULL,
                   multiple = FALSE, 
                   width = '100%', 
                   size = NULL)
  })
  
  output$ingredientQuantity <- renderUI({
    req(input$ingredient)
    n <- ingredientPairs$ingredientQuantity[which(ingredientPairs$ingredientName == 
                                                    input$ingredient)]
    numericInput("ingredientQuantity","Grams:",min = 0, max = 1000, step = .5, value = n)
  })
  
  
  # show list of ingredients chosen
  ingredientData <- reactive({
    req(input$ingredientQuantity)
    df <- data.frame(Ingredients = ingredientPairs$ingredientName,
                     Grams = ingredientPairs$ingredientQuantity)
    df <- df[-which(df$Grams == 0), ]
  })
  
  # standardize sig figs and no sci notation
  options(digits = 2)
  options(scipen = 999)
  
  # show nutrient data from chosen ingredient data
  nutrientData <- reactive({
    req(ingredientData())
    nutrientDF <- ingredientData()
    df <- ingredients[which(tolower(ingredients$name) %in% tolower(ingredientData()$Ingredients)), ]
    names(df) <- gsub("name", "Ingredients", names(df), ignore.case = F)
    df <- merge(df, nutrientDF, by = 'Ingredients', all.x = T, all.y = T)
    totalNutrient <- data.frame(tapply(df$valuePer100g * (df$Grams/100), df$nutrientName, FUN=sum), stringsAsFactors = F)
    totalNutrient$Nutrient <- row.names(totalNutrient)
    totalNutrient <- totalNutrient[c(2,1)]
    row.names(totalNutrient) <- NULL
    names(totalNutrient) <- c("Nutrient", "Amount(g, or kcal for Energy)")
    df <- totalNutrient
    
    mealScaler <- switch(input$percentOfDay, Meal = 100/3, Day = 100, Week = 700)
    
    df %>% 
      mutate(`Projected Daily Amount` = `Amount(g, or kcal for Energy)` * (100/mealScaler)) %>%
      left_join(nationalInstituteHealthDV, by = 'Nutrient') %>%
      arrange(factor(ifelse(grepl("carb|protein|energy|fat", df$Nutrient, ignore.case = T), 0, 1)))
    
  })
  
  nutrientDataTable <- reactive({
    datatable(nutrientData(),options = list(paging=FALSE, searching=FALSE, processing=FALSE))
  })
  
  
  output$dataframe <- DT::renderDataTable( ingredientData(), options = 
                                             list(paging = F, searching = F, ordering=F, 
                                                  language = list(
                                                    zeroRecords = "Added ingredients will show up here")))
  
  output$nutritionDataframe <- DT::renderDataTable( nutrientDataTable(),  
                                                    options = list(rowCallback = I('
            function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                      // Bold and green cells for conditions
                                      if (parseFloat(aData[2]) >= parseFloat(aData[3]))
                                      $("td:eq(0)", nRow).css("background-color", "#C9F0B5");
                                      if (parseFloat(aData[2]) < parseFloat(aData[3]))
                                      $("td:eq(0)", nRow).css("background-color", "#B5B7F0");
  }'),
                                                                   paging = F, searching = F, ordering=F))
  
}
)

#run app
shinyApp(ui, server)

