# Unobserved Heterogeneity
# A mortality crossover and unobserved frailty
# G. Rodríguez / 2 March 2017 rev 5 Nov 2022

library(ggplot2)
library(dplyr)

usw <- read.table("https://grodri.github.io/datasets/usWhiteBlackMales2002.dat")
names(usw) <- c("age", "lxw", "lxb")

us <- data.frame(
  race = c(rep("white",nrow(usw)),rep("black",nrow(usw))),
  age = rep(usw$age + 0.5, 2),
  lx = c(usw$lxw, usw$lxb)) %>% group_by(race)
  
us <- mutate(us, 
  H=-log(lx/100000), 
  h = lead(H) - H, 
  Hm = (H + lead(H))/2,
  h0 = h) %>% filter(!is.na(h))           


ui <- shinyUI(fluidPage(
  titlePanel("Mortality Crossover"),
  mainPanel(
    plotOutput("hazards"),
    sliderInput("var","variance of frailty:", min=0, max=1.2, step=0.1, value= 0)
  )
))

server <- shinyServer(function(input, output) {
  output$hazards <- renderPlot({
    sigma2 <- input$var
    mutate(us, h0 = h * exp(sigma2 * Hm)) %>%
    ggplot(aes(age, log(h0), color=race)) + geom_line() +
      scale_y_continuous(limits=c(-10, 5))
  })
})

shinyApp(ui = ui, server = server)
