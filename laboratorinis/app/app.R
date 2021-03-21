library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Odontologines praktikos veikla"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "imones_pav", label = "Imones pavadinimas", choices = NULL, selected = NULL)
    ),
    mainPanel(tabsetPanel(
      tabPanel("grafikas", plotOutput("plot")),
      tabPanel("lentele", tableOutput("table"))
    )
    )
  )
)
server <- function(input, output, session) {
  data <- read_csv("https://raw.githubusercontent.com/redvens/KTU-duomenu-vizualizacija/main/laboratorinis/data/lab_sodra.csv") %>% filter(ecoActCode == "862300")
  updateSelectizeInput(session, "imones_pav", choices = data$name, server = TRUE)
  
  output$table <- renderTable(
    data %>%
      filter(name == input$imones_pav) , digits = 0
  )
  
  output$plot <- renderPlot(
    data %>%
      filter(name == input$imones_pav) %>%
      ggplot(aes(x = month, y = avgWage)) +
      geom_line(size =1)+
      geom_point() +theme_classic()+
      labs(title = "Vidutinis atlyginimas")      
  )
}
shinyApp(ui, server)
