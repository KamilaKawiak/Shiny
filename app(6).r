

library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)


ui <- dashboardPage(
    dashboardHeader(title = "Analiza wariancji"),
    dashboardSidebar(
        # dodanie menu bocznego
        sidebarMenu(
            menuItem("Statystyki", tabName = "Statystyki", icon = icon("Statystyki"),badgeLabel = "Statystyki", badgeColor = "olive"),
            menuItem("Wykresy", icon = icon("Wykresy"), tabName = "Wykresy",
                     badgeLabel = "Wykresy", badgeColor = "teal"),
            menuItem("Mean, var i boxplot", icon = icon("Opis"), tabName = "Opis", badgeLabel = "Opis", badgeColor = "yellow"),
            menuItem("Testy", icon = icon("Testy"), tabName = "Testy",
                     badgeLabel = "Testy", badgeColor = "lime"),
            menuItem("Weryfikacja", icon = icon("Weryfikacja"), tabName = "Weryfikacja",
                     badgeLabel = "Weryfikacja", badgeColor = "blue")


            
        ), 
        fluidPage(
            theme = shinytheme("cyborg")),
        selectInput(
            'vector', h3("Wybierz wektor"),
            choices = c(5), 
            multiple=FALSE, selected=5
        ),
        sliderInput("slider", "Liczba obserwacji:", 1, 180, 100)
    ),
    
    dashboardBody(
        fluidRow(
            tabItems(
                tabItem(tabName = "Statystyki",
                        h2("Statystyki opisowe"),
                        box(
                            tableOutput("op"),
                            h3(verbatimTextOutput('print1'))
                        )
                ),
                tabItem(tabName = "Wykresy",
                        h2("Histogram i boxplot"),
                        box(plotOutput("plot1", width = 620),plotOutput("plot2", width = 620))
                ),
    
                tabItem(tabName = "Opis",
                        h2("Srednia, wariancja i boxplot"),
                        h3(verbatimTextOutput('print3')),
                        h4(verbatimTextOutput('print4')),
                        plotOutput("plot3")
                        
                ),

                tabItem(tabName = "Testy",
                        h2("Testy analizy wariancji:"),
                        box(h3(verbatimTextOutput('print2')),
                            h4(verbatimTextOutput('print5')),
                            h5(verbatimTextOutput('print6')),
                            h6(verbatimTextOutput('print7')))
                        
                        ),
                tabItem(tabName = "Weryfikacja",
                        h2("Weryfikacja zalozen"),
                        h3(verbatimTextOutput('print8')),
                        h4(verbatimTextOutput('print9')),
                        h5(verbatimTextOutput('print10'))
                        )
                )

            

            )
            
            
            
        )
    
)

server <- function(input, output) {
     s <- 
     read.csv("https://stat.gov.pl/download/gfx/portalinformacyjny/pl/defaultaktualnosci/6285/3/1/1/prawomocnie_skazane_osoby_dorosle.csv", 
           row.names=NULL, sep=";", header=TRUE, colClasses=c("character","character","character","factor","numeric"))
     output$op <-renderTable({ 
         data.frame(cbind(c("Liczba obserwacji","Minimum","Srednia","Odchylenie standardowe",
                            "Mediana","Medianowe odchylenie bezwzgledne","Maksimum"),
                          c(length(s[seq_len(input$slider),as.numeric(input$vector)]),
                            min(s[seq_len(input$slider),as.numeric(input$vector)]),
                            mean(s[seq_len(input$slider),as.numeric(input$vector)]),
                            sd(s[seq_len(input$slider),as.numeric(input$vector)]),
                            median(s[seq_len(input$slider),as.numeric(input$vector)]),
                            mad(s[seq_len(input$slider),as.numeric(input$vector)]),
                            max(s[seq_len(input$slider),as.numeric(input$vector)])
                          )
         )
         )
     })
     
     output$print1 <- renderPrint({
         print("Kwantyle")
         quantile(s[seq_len(input$slider),as.numeric(input$vector)])
     })
     
     output$plot1 <- renderPlot({
         data <- s[seq_len(input$slider),as.numeric(input$vector)]
         hist(data, col=rainbow(15), border = "darkblue",probability = T)
         lines(density(data),col="black",lty="dotdash",lwd=3)
         grid()
     })
    
     output$plot2 <- renderPlot({
         data <- s[seq_len(input$slider),as.numeric(input$vector)]
         boxplot(data,col="maroon", border="darkblue",horizontal = T)
     })
     
     output$print2 <- renderPrint({
         oneway.test(s$liczba_skazanych ~ s$rok)
     })
     
     output$plot3 <- renderPlot({
         boxplot(s$liczba_skazanych ~ s$rok,col="maroon", border="darkblue",pch = 15)
     })
     
     output$print3 <- renderPrint({
         print("Srednia")
         tapply(s$liczba_skazanych, s$rok, mean)
     })
     
     output$print4 <- renderPrint({
         print("Wariancja")
         tapply(s$liczba_skazanych, s$rok, var)
     })
     
     output$print5 <- renderPrint({
         oneway.test(s$liczba_skazanych ~ s$rok, var.equal = T)
     })
     
     output$print6 <- renderPrint({
         aov.out <- aov(s$liczba_skazanych ~ s$rok, data = s)
         summary(aov.out)
     })
     
     output$print7 <- renderPrint({
         aov.out <- aov(s$liczba_skazanych ~ s$rok, data = s)
         TukeyHSD(aov.out)
     })
     
     output$print8 <- renderPrint({
         bartlett.test(s$liczba_skazanych ~ s$rok)
         
     })
     
     output$print9 <- renderPrint({
         kruskal.test(s$liczba_skazanych ~ s$rok)
         
     })
     
     output$print10 <- renderPrint({
         tapply(s$liczba_skazanych, s$rok, shapiro.test)
     })
     

     
}

shinyApp(ui, server)


