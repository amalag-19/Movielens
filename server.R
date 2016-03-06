
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  #Requiring packages to plot
  source("diag.R")
  require(grid)
  #Choosing the parameter ID
  output$parameter <- renderUI({radioButtons(inputId = 'param',label = "Choose the parameter type",choices = list("delta_ic" = "deli","a_i" = "ai","b_j" = "bj","sig2_eps" = "s2e","a" = "av","Theta" = "Th","b" = "bv","Delta" = "Del","Sigma" = "Sig"),selected = "ai")})
  
  ## Parameters for del_i
  output$deli1 <- renderUI({numericInput('deli1', 'User ID (1-943)', value=1,min=1,max=943,step=1)})
  output$deli2 <- renderUI({numericInput('deli2', 'C (1,2)', value=1,min=1,max=2,step=1)})
  
  ## Parameters for a_i
  output$ai1 <- renderUI({numericInput('ai1', 'User ID (1-943)', value=1,min=1,max=943,step=1)})
  output$ai2 <- renderUI({numericInput('ai2', 'T (1-4)', value=1,min=1,max=4,step=1)})
  
  ## Parameters for b_j
  output$bj1 <- renderUI({numericInput('bj1', 'Movie ID (1-1682)', value=1,min=1,max=1682,step=1)})
  output$bj2 <- renderUI({numericInput('bj2', 'T (1-4)', value=1,min=1,max=4,step=1)})
  
  ## Parameters for a
  output$a1 <- renderUI({numericInput('a1', 'T (1-4)', value=1,min=1,max=4,step=1)})
  
  ## Parameters for Theta
  output$Th1 <- renderUI({numericInput('Th1', 'T (1-4)', value=1,min=1,max=4,step=1)})
  output$Th2 <- renderUI({numericInput('Th2', 'p (1-22)', value=1,min=1,max=22,step=1)})
 
  ## Parameters for b
  output$b1 <- renderUI({numericInput('b1', 'T (1-4)', value=1,min=1,max=4,step=1)})
  
  ## Parameters for Delta
  output$Del1 <- renderUI({numericInput('Del1', 'T (1-4)', value=1,min=1,max=4,step=1)})
  output$Del2 <- renderUI({numericInput('Del2', 'q (1-19)', value=1,min=1,max=19,step=1)})
  
  ## Parameters for Sigma
  output$Sig1 <- renderUI({numericInput('Sig1', 'T (1-4)', value=1,min=1,max=4,step=1)})
  output$Sig2 <- renderUI({numericInput('Sig2', 'T (1-4)', value=1,min=1,max=4,step=1)})
  
  ## Generating Plots
  output$Plots <- renderPlot({
    input$go
    a_i.chain<-list()
    # isolate(if (input$param=="uij"){
    #   isolate(a_i.chain[[1]]<-as.vector(T4.c5000[[1]][[input$ai1]][input$ai2]))
    #   isolate(plot12<-rmean.plotter(a_i.chain))
    # })
    isolate({if (input$param=="deli"){
      isolate(a_i.chain[[1]]<-as.vector(T4.c5000[[2]][input$deli1,input$deli2,]))
      isolate(plot12<-rmean.plotter(a_i.chain))
    }})
    isolate({if (input$param=="ai"){
      isolate(a_i.chain[[1]]<-as.vector(T4.c5000[[3]][input$ai1,input$ai2,]))
      isolate(plot12<-rmean.plotter(a_i.chain))
    }})
    isolate({if (input$param=="bj"){
      isolate(a_i.chain[[1]]<-as.vector(T4.c5000[[4]][input$bj1,input$bj2,]))
      isolate(plot12<-rmean.plotter(a_i.chain))
    }})
    isolate({if (input$param=="s2e"){
      isolate(a_i.chain[[1]]<-as.vector(T4.c5000[[5]]))
      isolate(plot12<-rmean.plotter(a_i.chain))
    }})
    isolate({if (input$param=="av"){
      isolate(a_i.chain[[1]]<-as.vector(T4.c5000[[6]][input$a1,]))
      isolate(plot12<-rmean.plotter(a_i.chain))
    }})
    isolate({if (input$param=="Th"){
      isolate(a_i.chain[[1]]<-as.vector(T4.c5000[[7]][input$Th1,input$Th2,]))
      isolate(plot12<-rmean.plotter(a_i.chain))
    }})
    isolate({if (input$param=="bv"){
      isolate(a_i.chain[[1]]<-as.vector(T4.c5000[[8]][input$b1,]))
      isolate(plot12<-rmean.plotter(a_i.chain))
    }})
    isolate({if (input$param=="Del"){
      isolate(a_i.chain[[1]]<-as.vector(T4.c5000[[9]][input$Del1,input$Del2,]))
      isolate(plot12<-rmean.plotter(a_i.chain))
    }})
    isolate({if (input$param=="Sig"){
      isolate(a_i.chain[[1]]<-as.vector(T4.c5000[[10]][input$Sig1,input$Sig2,]))
      isolate(plot12<-rmean.plotter(a_i.chain))
    }})
    isolate(plot34<-margdens.plotter(a_i.chain))
    isolate(print(multiplot(plot12[[1]],plot12[[2]],plot34[[1]],plot34[[2]],cols = 2)))
    
    })
})
