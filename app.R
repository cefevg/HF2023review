#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rcompanion)
library(ggplot2)

# Define UI for application that draws a power analysis
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      sliderInput("neg.mean", "DMSO surface mean:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.01),
      
      # Input: Decimal interval with step value ----
      sliderInput("neg.cv", "DMSO surface CV:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.01),
      
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      sliderInput("neg.reps", "DMSO replicates:",
                  min = 3, max = 100,
                  value = 3, step = 1),
      
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      sliderInput("comp.reps", "Compound replicates:",
                  min = 3, max = 50,
                  value = 3, step = 1),
      
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      sliderInput("alpha", "Alpha (false positives rate):",
                  min = 0, max = 100,
                  value = 5, step = 1),
    
      submitButton("Submit")
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      plotOutput('scatterplot'),
      
      
      # Output: Table summarizing the values entered ----
      plotOutput('powerplot')
      
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  
  variables <- reactive({
    growth.red <- 1 - input$comp.mean
  
  dmso.std <- input$neg.mean*input$neg.cv
  
  #comp.std <- input$comp.mean*input$comp.cv
  
  })
  
  # Power as a function of efficacy
  
  simulations <- function(g1.mean, g1.std, g1.reps, g2.reps) {
    
  Eff.tested <- seq(from = 0.50, to = 0.99, by = 0.01)
  
  results <- rep(0, length(Eff.tested))
  
  for (eff.sim in Eff.tested) {
    
    growth.red <- 1 - eff.sim
    
    signif <- rep(0, 1000)
    
    for (i in 1:1000) {
      
      controls <- rnorm(g1.reps, g1.mean, g1.std)
      
      molec <- rnorm(g2.reps, growth.red*g1.mean, g1.std)
      
      molec <- unlist(lapply(molec, function (x) 100*c(mean(controls)-x)/mean(controls)))
      
      signif[i] <- t.test(molec, mu = 50, alternative = "greater")$p.value
      
    }
    
    results[match(round(eff.sim,3), round(Eff.tested, 3))] <- sum(signif<0.1)/10
    
  }
  
  return(results)
  
  }
  
  # Show the values in an HTML table ----
  #output$scatterplot <- renderPlot({
    
   # ggplot(data.frame(Surface = c(input$neg.mean, input$comp.mean), Std = c(input$neg.cv, input$comp.cv), Treatment = as.factor(c("Control", "Compound"))),
    #       aes(x = Treatment, y = Surface, col = Treatment)) +
     # geom_line() +
      #geom_errorbar(aes(ymin = Surface-Std, ymax = Surface +Std)) +
      #theme_bw()
    
  #})
  
  # Show the values in an HTML table ----
  output$powerplot <- renderPlot({
    
      ggplot(data.frame(Efficacy = seq(from = 0.50, to = 0.99, by = 0.01), Power = simulations(input$neg.mean,input$neg.cv, input$neg.reps, input$comp.reps)),
             aes(x = Efficacy, y = Power)) +
      geom_line() +
      scale_x_continuous(breaks = seq(from = 0.50, to = 0.99, by = 0.01)) +
      geom_hline(yintercept = 80, col = "red") +
      theme_bw() +
      ggtitle("Power as a function of target Efficacy")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
