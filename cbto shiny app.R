# CBT-O Shiny app

library(shiny)
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(plotly)
library(randomNames)

# CBT data from Beth
##cbtdata <- read_excel("beths pano data_BF.xlsx")
##cbtdata <- cbtdata %>% rename(icdlevel = 'icd level')

# Fake data
cbtdata <- data.frame(reviews = sample(3:20,100, replace=TRUE), 
                      rcts = sample(3:50,100, replace=TRUE), 
                      icdlevel = sample(c("Primary (physical)", "Secondary (psychological)", "Tertiary"), 
                                        size =100, replace=TRUE), 
                      participants = sample(100:600,100, replace=TRUE), 
                      icd = unique(randomNames(n=100))) #random names represent ICD condition in real data

modelTypes <- unique(cbtdata$icdlevel)

# Define UI for application 
ui <- fluidPage(
          
          # Give title
          titlePanel("CBT-O data"), 
          
          # Sidebar layout with a input and output definitions
          sidebarLayout(
                    
                    # Inputs
                    sidebarPanel(
                              
                              # Select variable for dropdown menu
                              selectInput(inputId = "icdlevel", 
                                           label = "ICD level:",
                                           choices = modelTypes, 
                                           selected = "Primary (physical)") 
                    ),
                    # Outputs
                    mainPanel(
                              plotlyOutput(outputId = "scatterplot")
                    )
          )
)


# Define server function required to create the scatterplot
server <- function(input, output) {
          
          reactdata <- reactive({
                    cbtdata %>% filter(icdlevel == input$icdlevel)
          })
          
          # Create the scatterplot object the plotOutput function is expecting - here I'm using plotly not static ggplot
          output$scatterplot <- renderPlotly({
                    a <- ggplot(data = reactdata(), 
                                aes(x = reactdata()$reviews, 
                                    y = reactdata()$rcts,
                                    size = reactdata()$participants,
                                    color = reactdata()$icd
                                )) +
                              geom_jitter() +
                              labs(x="No. of reviews", y="No. of RCTs") +
                              xlim(0,25) +
                              ylim(0,80) + theme(legend.position = "bottom") 
                    
                    ggplotly(a, width = 700, height = 600) %>%  
                              layout(showlegend = FALSE)
          })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)



