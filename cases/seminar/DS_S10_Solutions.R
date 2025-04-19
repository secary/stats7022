library(shiny)
library(tidyverse)

data(mpg, package = "ggplot2")
min_displ <- min(mpg$displ)
max_displ <- max(mpg$displ)

ui <- fluidPage(
  titlePanel("MPG data"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "range",
        label = "Range of displacement",
        min = min_displ,
        max = max_displ,
        value = c(min_displ, max_displ)
      ),
      checkboxGroupInput(
        "drive",
        label = "Drivetrain",
        choices = list("Four-wheel drive" = "4",
                       "Front-wheel drive" = "f",
                       "Rear-wheel drive" = "r"),
        selected = c("4","f","r")
      ),
      textInput(
        "name",
        label = "Enter other name", value = "")
    ),
    mainPanel(
      h1("A plot"),
      plotOutput("scatter"),
      h1("A table"),
      tableOutput("summary"),
      h1("Some writing"),
      h3(textOutput("writing"))
    )
  )
)

server <- function(input, output, session) {
  output$scatter <- renderPlot({
    mpg %>%
      ggplot(aes(displ, cty)) +
      geom_point() +
      theme_bw() +
      xlim(input$range[1], input$range[2])
  })
  output$summary <- renderTable({
    mpg %>%
      filter(drv %in% input$drive) %>%
      group_by(drv) %>%
      summarise(
        mean = mean(cty)
      )
  })
  output$writing <- renderText({
    glue::glue("Hello {input$name}")
  })
}

shinyApp(ui, server)