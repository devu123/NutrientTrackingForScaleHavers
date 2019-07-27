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

#ingredients value is g per 100g

# cleaning up rest of nutrient names
ingredients$nutrientName <- gsub(", RAE||, DFE", "", ingredients$nutrientName)
ingredients$nutrientName <- gsub("Folate, total", "Folate", ingredients$nutrientName)
ingredients$nutrientName <- gsub("Choline, total", "Choline", ingredients$nutrientName)
ingredients$nutrientName <- gsub("Vitamin E \\(alpha-tocopherol\\)", "Vitamin E",
                                 trimws(ingredients$nutrientName))

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
                             sliderTextInput('percentOfDay', "Percent Of Daily Food Intake \n Represented \n
                                         In Ingredient List",
                                         selected = 33, choices = round(c(
                                           seq(1,99, by = 1),
                                           seq(100,700, by = 100)), 
                                           0),
                                         grid = TRUE),
                             #clear all ingredients
                             actionButton("clear", label = "Clear All Ingredients")
                )
                ,
                # tabs for displaying outputs ----
                mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel(h3("Ingredient List"), 
                                       dataTableOutput("dataframe")),
                              tabPanel(h3("Total Nutrition"),
                                       dataTableOutput("nutritionDataframe"))
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
    selectInput(inputId = 'ingredient',
                label = 'Ingredients', 
                choices = ingredientPairs$ingredientName,
                selected = NULL,
                multiple = FALSE,
                selectize = TRUE, 
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
    df$`Projected Daily Amount` <- format(df['Amount(g, or kcal for Energy)'] * (100/input$percentOfDay), nsmall = 2)
    df <- df %>% full_join(nationalInstituteHealthDV, by = 'Nutrient')  
    arrange(df, factor(ifelse(grepl("carb|protein|energy|fat", df$Nutrient, ignore.case = T), 0, 1)))
    
    })

  output$dataframe <- renderDataTable( ingredientData(), options = 
                                         list(paging = F, searching = F, ordering=F, 
                                              language = list(
                                                zeroRecords = "Added ingredients will show up here")))

  output$nutritionDataframe <- renderDataTable( nutrientData(),  
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
