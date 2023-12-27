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
ui <- navbarPage("Experimental wondering",
                 
                 # Descriptive stats tab title
                 
                 tabPanel("Power analysis",
                          
                          # Descriptive stats title in the tab
                          
                          titlePanel("Power analysis"),
                        
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      numericInput("neg.mean", "DMSO surface mean:",
                   min = 0, max = 0.5, value = 0.5, step = 0.01),
      
      # Input: Decimal interval with step value ----
      numericInput("neg.cv", "DMSO surface CV:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.01),
      
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      numericInput("neg.reps", "DMSO replicates:",
                  min = 3, max = 100,
                  value = 3, step = 1),
      
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      numericInput("comp.reps", "Compound replicates:",
                  min = 3, max = 50,
                  value = 3, step = 1),
      
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      numericInput("alpha", "Alpha (false positives rate):",
                  min = 0, max = 100,
                  value = 5, step = 1),
    
      submitButton("Submit")
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      plotOutput('powerplot')
      
    )
  )
),

# CV power analysis

tabPanel("CV - power analysis",
         
         # Predictive stats title in the tab
         
         titlePanel("How much variation is too much variation for you?"),
         
         # Sidebar layout with input and output definitions ----
         sidebarLayout(
           
           # Sidebar to demonstrate various slider options ----
           sidebarPanel(
             
             # Input: Simple integer interval ----
             numericInput("target.eff", "Target efficacy:",
                          min = 51, max = 100, value = 55, step = 1),
             
             # Input: Animation with custom interval (in ms) ----
             # to control speed, plus looping
             numericInput("dmso.reps", "DMSO replicates:",
                          min = 3, max = 100,
                          value = 3, step = 1),
             
             # Input: Animation with custom interval (in ms) ----
             # to control speed, plus looping
             numericInput("target.reps", "Compound replicates:",
                          min = 3, max = 50,
                          value = 3, step = 1),
             
             # Input: Animation with custom interval (in ms) ----
             # to control speed, plus looping
             numericInput("alpha.sec", "Alpha (false positives rate):",
                          min = 0, max = 100,
                          value = 5, step = 1),
             
             submitButton("Submit")
             
           ),
           # Main panel for displaying outputs ----
           mainPanel(
             
             
             # Output: Table summarizing the values entered ----
             plotOutput('cvpowerplot')
             
           )
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
  
  simulations <- function(g1.mean, g1.std, g1.reps, g2.reps, alphalevel) {
    
  Eff.tested <- seq(from = 0.50, to = 0.99, by = 0.01)
  
  results <- rep(0, length(Eff.tested))
  
  for (eff.sim in Eff.tested) {
    
    growth.red <- 1 - eff.sim
    
    signif <- rep(0, 1000)
    
    for (i in 1:1000) {
      
      controls <- rnorm(g1.reps, g1.mean, g1.std)
      
      molec <- rnorm(g2.reps, growth.red*g1.mean, g1.std)
      
      molec <- unlist(lapply(molec, function (x) 100*c(mean(controls)-x)/mean(controls)))
      
      signif[i] <- t.test(molec, mu = 50, alternative = "greater", conf.level = 1 - alphalevel/100)$p.value
      
    }
    
    results[match(round(eff.sim,3), round(Eff.tested, 3))] <- sum(signif<0.1)/10
    
  }
  
  return(results)
  
  }
  
  # Power as a function of CV
  
  cvsimulations <- function(target.eff, target.reps, dmso.reps, alphalevel2) {
    
    control.mean <- 0.5
    
    growth.red <- 1 - target.eff/100
    
    target.sd <- 1
    
    results <- rep(0, 50)
    
    cv.tested <- seq(from = 0.01, to = 0.5, by = 0.01)
    
    for (cv.sim in cv.tested) {
      
      cv <- cv.sim
      
      std <- control.mean*cv
      
      signif <- rep(0, 1000)
      
      for (i in 1:1000) {
        
        controls <- rnorm(dmso.reps, control.mean, std)
        
        molec <- rnorm(target.reps, growth.red*control.mean, std)
        
        molec <- unlist(lapply(molec, function (x) 100*c(mean(controls)-x)/mean(controls)))
        
        signif[i] <- t.test(molec, mu = 50, alternative = "greater", conf.level = 1 - alphalevel2/100)$p.value
        
      }
      
      results[match(round(cv.sim,3), round(cv.tested, 3))] <- sum(signif<0.1)/10
      
    }
    
    res.rep <- data.frame(CV = cv.tested, Power = results)
    
    return(res.rep)
    
  }
  
  # Show the values in an HTML table ----
  output$powerplot <- renderPlot({
    
      ggplot(data.frame(Efficacy = seq(from = 0.50, to = 0.99, by = 0.01), Power = simulations(input$neg.mean,input$neg.cv, input$neg.reps, input$comp.reps, input$alpha)),
             aes(x = Efficacy, y = Power)) +
      geom_line() +
      scale_x_continuous(breaks = seq(from = 0.50, to = 0.99, by = 0.01)) +
      geom_hline(yintercept = 80, col = "red") +
      theme_bw() +
      ggtitle("Power as a function of target Efficacy")
    
  })
  
  # Show the values in an HTML table ----
  output$cvpowerplot <- renderPlot({
    
      ggplot(cvsimulations(input$target.eff, input$target.reps, input$dmso.reps, input$alpha.sec), aes(x = CV, y = Power)) +
      geom_line() +
      scale_x_continuous(breaks = seq(from = 0.01, to = 0.5, by = 0.01)) +
      geom_hline(yintercept = 80, col = "red") +
      theme_bw() +
      ggtitle("Power as a function of DMSO CV")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
