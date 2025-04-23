library(shiny)
library(tidyverse)

data(mpg, package = "ggplot2")
min_displ <- min(mpg$displ)
max_displ <- max(mpg$displ)
min_cty <- min(mpg$cty)
max_cty <- max(mpg$cty)

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
        label = "Drive",
        choices = list("Four-wheel drive" = "4",
                       "Front-wheel drive" = "f",
                       "Rear-wheel drive" = "r"),
        selected = c("4", "f", "r")
      ),
      radioButtons("colour",
                   label = "Theme",
                   choices = list("Viridis" = "viridis",
                                  "Plasma" = "plasma",
                                  "Mako" = "mako"),
      selected = NULL)
    ),
    mainPanel(
      h1("A plot"),
      plotOutput("scatter")
    )
  )
)

server <- function(input, output, session) {
  output$scatter <- renderPlot({
    mpg %>%
      filter(drv %in% input$drive) %>%
      ggplot(aes(x = displ, y = cty, color = drv, shape = drv)) +
      geom_point(cex = 3) +
      xlim(input$range[1], input$range[2]) +
      ylim(min_cty,max_cty) +
      viridis::scale_colour_viridis(option = input$colour,
                                    begin = 0,
                                    end = 3/4,
                                    discrete = TRUE)
  })
}

shinyApp(ui, server)