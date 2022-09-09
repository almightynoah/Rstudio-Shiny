#install.packages("shiny")
#install.packages("agridat")

library(shiny)
library(ggplot2)
library(dplyr)
library(agridat)

Barley <- as.data.frame(beaven.barley)

ui <- 
  fluidPage(
    titlePanel("Data Project Nano-One"),
    sidebarLayout(
      position = "right",
      sidebarPanel(h3("Inputs for histogram"), 
                   selectInput("gen", "1. Pilih jenis", choices = c("Vx100" = "a","XM200L" = "b","Des55" = "c","AQ01" = "d","LX198" = "e"), selected = "a"),
                   br(),
                   selectInput("col", "2. Pilih warna histogram", choices = c("biru" = "blue","hijau"="green","merah"="red","abu-abu"="gray"), selected = "grey"),
                   br(),
                   sliderInput("bin", "3. Pilih nomor tempat histogram", min=1, max=25, value= c(10)),
                   br(),
                   textInput("text", "4. Masukkan text untuk ditampilkan", "")),
      mainPanel(
        plotOutput("myhist"),
        tableOutput("mytable"),
        textOutput("mytext"),
        tags$div(style="color:black",
                 tags$p("Visit us at:"),
                 tags$a(href = "https://ourcodingclub.github.io", "Coding Club")
        )
      )
    )
  )



server <- function(input, output) {
  output$myhist <- renderPlot(ggplot(Barley, aes(x = yield)) + 
                                geom_histogram(bins = input$bin, fill = input$col, group=input$gen, 
                                               data=Barley[Barley$gen == input$gen,],
                                               colour = "black"))
  
  output$mytext <- renderText(input$text)
  
  output$mytable <- renderTable(Barley %>%
                                  filter(gen == input$gen) %>%
                                  summarise("Mean" = mean(yield), 
                                            "Median" = median(yield),
                                            "STDEV" = sd(yield), 
                                            "Min" = min(yield),
                                            "Max" = max(yield)))
}
shinyApp(ui = ui, server = server)



