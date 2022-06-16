
library(shiny)
library(deSolve)
library(shinythemes)
library(ggplot2)
library(gridExtra)
# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),
                  # Application title
                  tags$h2("Energía limpia en México"),
                  p("Politicas que incentiven mayor participación de fuentes renovables en la generación de energía asi como la disminución de emisiones de CO2, creado por:",
                    tags$a(href="https://github.com/AlexaJeressi/Generacion_energia", "Alexa Martinez")),
                  hr(),
                  
    #titlePanel("Energía Limpia en México"),

    sidebarLayout(
        sidebarPanel(
          h3("Inversion"),
          span(tags$i(h6("Incentivar la inversión en energías limpias")), style="color:#EB6638"),
            sliderInput("incremento.publico",
                        "Subsidio inversion publica:",
                        min = 0,
                        max = 5,
                        value = 0.1,
                        step = 0.50,
                        animate = TRUE),
          sliderInput("incremento.privado",
                      "Subsidio inversion privada:",
                      min = 0,
                      max = 1,
                      value = 0.1,
                      step = 0.01),
            br(),
          h3("Impuestos"),
          span(tags$i(h6("Aplicar impuestos a quienes produzcan energía fósil")), style="color:#EB6638"),
          sliderInput("impuesto.publico",
                      "Impuesto al sector publico por la producción de cada GW instalado :",
                      min = 0,
                      max = 1,
                      value = 0.1,
                      step = 0.01),
            
            sliderInput("impuesto.privado",
                        "Impuesto al sector privado por la producción de cada GW instalado:",
                        min = 0,
                        max = 1,
                        value = 0.1,
                        step = 0.01),
          h3("Subsidio fósil"),
          span(tags$i(h6("Aumento de la inversión en el sector fósil público")), style="color:#EB6638"),
          sliderInput("incremento.fosil.publico",
                      "Subsidio a la inversión:",
                      min = 0,
                      max = 5,
                      value = 0.1,
                      step = 0.50,
                      animate = TRUE),
         
            
            
           
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type="tabs",
                      tabPanel("Energía y emisiones",
                               plotOutput("Energia")),
                      tabPanel("Datos",
                               (dataTableOutput("Tabla"))))
            
        )
    )
))
