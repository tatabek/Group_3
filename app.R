library(shiny)

ui <- navbarPage("Production Planning",
                 tabPanel("Learning Curve", learning_curve_ui("learning_curve")),
                 tabPanel("Moving Average", moving_average_ui("moving_average")),
                 tabPanel("Forecasting", forecasting_ui("forecasting")),
                 tabPanel("Aggregate Planning", aggregate_planning_ui("aggregate_planning"))
)

server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  moving_average_server("moving_average")
  forecasting_server("forecasting")
  aggregate_planning_server("aggregate_planning")
}

shinyApp(ui, server)