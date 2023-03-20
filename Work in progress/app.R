
# Load R packages
library(shiny)
library(shinythemes)


  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
      theme = "cerulean", 
      "Démineur",
      tabPanel("Selectionnez les parametres ",
               sidebarPanel(
                 tags$h3("Input:"),
                 textInput("txt3", "Choisissez la difficulté", ""),
                 textInput("txt1", "nombre de lignes:", ""),
                 textInput("txt2", "nombre de colonnes :", ""),
                 textInput("txt3", "nombre de Bombes :", ""),
                 
               ), # sidebarPanel
               mainPanel(
                            h1("Demineur1"),
                            
                            h4("Choix du joueur"),
                            verbatimTextOutput("txtout"),

               ) # mainPanel
               
      ), 
      tabPanel("JEU", "This panel is intentionally left blank")
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    output$txtout <- renderText({
      paste( input$txt1, input$txt2,input$txt3, sep = " " )
    })
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
