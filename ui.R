
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  fluidPage(#Creating a fluid page to include all terms.
    navbarPage(#Command used to creat a page with tabs to navegate.
      title="Diagnostic plots for Movielens",theme = "bootstrap.css",#Defining the general name and the theme been used by this app.
      tabPanel(title = "Read Me",#Creating first tab with theoretical explanation
        sidebarPanel(#Creating the side bar with the buttons to choose what you wish to see
                     radioButtons(inputId = 'theopanel',label = "Menu",
                                  choices = list("Read Me"="read"),selected = "read",inline = FALSE),
                     tags$head( tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
                                tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config'))
          
        ),
        mainPanel(#Creating the main panel with the information selected using the buttons. Using conditional panel.
          includeHTML("introduction.html")#Including HTML with the introduction.
        )
      ),
      tabPanel(title = "Plotter",#Creating tab to plot
               sidebarPanel(#Creating side bar panel with the information of what you wish to plot
                 uiOutput("parameter"),#Choosing type of parameter
                 
                 # conditionalPanel(condition = "(input.param == 'uij')",#If discreate choosing the distribution and the parameters
                 #                  uiOutput("uij1"),
                 #                  uiOutput("uij2")),
                 conditionalPanel(condition = "(input.param == 'deli')",#If discreate choosing the distribution and the parameters
                                  uiOutput("deli1"),
                                  uiOutput("deli2")),
                 conditionalPanel(condition = "(input.param == 'ai')",#If discreate choosing the distribution and the parameters
                                  uiOutput("ai1"),
                                  uiOutput("ai2")),
                 conditionalPanel(condition = "(input.param == 'bj')",#If discreate choosing the distribution and the parameters
                                  uiOutput("bj1"),
                                  uiOutput("bj2")),
                 conditionalPanel(condition = "(input.param == 'av')",#If discreate choosing the distribution and the parameters
                                  uiOutput("a1")),
                 conditionalPanel(condition = "(input.param == 'Th')",#If discreate choosing the distribution and the parameters
                                  uiOutput("Th1"),
                                  uiOutput("Th2")),
                 conditionalPanel(condition = "(input.param == 'bv')",#If discreate choosing the distribution and the parameters
                                  uiOutput("b1")), 
                 conditionalPanel(condition = "(input.param == 'Del')",#If discreate choosing the distribution and the parameters
                                  uiOutput("Del1"),
                                  uiOutput("Del2")),
                 conditionalPanel(condition = "(input.param == 'Sig')",#If discreate choosing the distribution and the parameters
                                  uiOutput("Sig1"),
                                  uiOutput("Sig2")),
                 actionButton(inputId = "go",label = "Plot") #Button to plot
               ),
               mainPanel(
                 plotOutput("Plots")#Output, plot
               )
      )
    )
  )
)
