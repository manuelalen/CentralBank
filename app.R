#
# Author: Manuel Alén Sánchez
# Date: 28-01-2022
# Version: 1.0
#

library(shiny)
library(dplyr)
library(ggplot2)

server <- function(input, output) {
    output$plot <- renderPlot({
        dinero <- read.csv("centralBank.csv", header = TRUE,sep = ",")
        data <- reactive({
            req(input$sel_Asunto)
            df <- dinero %>% filter(Gasto %in% input$sel_Asunto)%>% group_by(Mes) %>% summarise(Dinero = sum(Dinero))  
        })
        
        #plot
        output$plot <- renderPlot({
            g <- ggplot(data(), aes(x =Mes, y = Dinero, fill = Dinero,color=Dinero))
            g + geom_bar(stat = "identity", position = "dodge")
        })


    })
    
}
ui <- fluidPage(
    h1("Banco Central de España"),
    selectInput(inputId = "sel_Asunto",
                label = "Destino del Gasto",
                list("Salarios","Pensiones","Educación","Sanidad","Infraestructura","Transporte","Alimentación",
                     "Vivienda","Medio Ambiente")),
    plotOutput("plot")
)

# Run the application 
shinyApp(ui = ui, server = server)
