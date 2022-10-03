# Proportional Hazards with Frailty
# Constant hazard is 2 or 1 with gamma unobserved heterogeneity
# G. Rodríguez / 5 March 2017

library(shiny)
library(ggplot2)
library(dplyr)

# set up data

t = seq(.00, 2.30, .02)
n = length(t)
d <- group_by(data.frame(
  group = c(rep("low",n), rep("high",n)),
  t = rep(t, 2),
  h = c(rep(1, n), rep(2, n))
), group)
d <- mutate(d, H = cumsum(h)*.02, p = h)

# shiny ui

ui <- fluidPage(

   titlePanel("Proportional Hazards with Frailty"),

      mainPanel(
        plotOutput("hazards"),
        sliderInput("var","variance of frailty:", 0, 0.5, value=0, step=.01)
   )
)

# shiny server

server <- function(input, output) {

   output$hazards <- renderPlot({
       mutate(d, p = h/(1 + input$var * H)) %>%
       ggplot(aes(t, h, color=group)) + geom_line() + geom_line(aes(t, p), size=1.2) +
        scale_y_continuous(limits=c(0,2.2))
   })
}

shinyApp(ui = ui, server = server)


