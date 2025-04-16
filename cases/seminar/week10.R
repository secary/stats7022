pacman::p_load(shiny, ggplot2, dplyr)

library(shiny)

data(mpg, package = "ggplot2")
min_displ <- min(mpg$displ)
max_displ <- max(mpg$displ)

ui <- fluidPage(
  titlePanel("MPG data"),
  sidebarLayout(
    sidebarPanel(
      # ADD INPUT 
      sliderInput(
        "range",
        label = "Range of displacement",
        min = min_displ,
        max = max_displ,
        value = c(min_displ, max_displ)
      ),
      
      checkboxGroupInput("checkGroup", label = h3("Drivetrain"), 
                         choices = list("Four-wheel Drive" = '4',
                                        "Front-wheel Drive" = "f", 
                                        "Rear-wheel Drive" = "r"),
                         selected = c('4', 'r', 'v')),
      
      textInput("text", label = h3("Enter other name"), value = "Max"),
      
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))

    
    ),
    
    mainPanel(
      titlePanel("A Plot"),
      plotOutput("scatter"),
      
      titlePanel("A table"),
      tableOutput("tab"),
      
      titlePanel("Some Writing"),
      textOutput("txt")
    )
  )
)



server <- function(input, output, session){
  output$scatter <- renderPlot({
    mpg %>% 
      ggplot(aes(displ, cty)) +
      geom_point() +
      theme_bw() + xlim(input$range[1], input$range[2])
  })
  
  output$tab <- renderTable({
      mpg %>% 
        group_by(drv) %>% 
        summarise(mean = mean(cty))
      })
  
  output$txt <- renderText({
    paste("Hello Max")
  })
    
    
}

shinyApp(ui, server)