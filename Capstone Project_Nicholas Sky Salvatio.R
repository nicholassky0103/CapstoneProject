library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggcorrplot)

library(readr)
df <- read_csv("C:/Users/Nicholas Sky/Downloads/Produk Domestik Regional Bruto.csv")
View(df)

df$Tahun <- as.factor(df$Tahun)

ui <- dashboardPage(
  dashboardHeader(title = 'Capstone Project'),
  dashboardSidebar(
    sidebarMenu(
      selectInput(inputId = 'Provinsi', label = 'Provinsi:',
                  choices = c(unique(df$Provinsi)), selected = 99, size = 13, selectize = FALSE),
      menuItem("About Me", icon = icon("linkedin"), href = "https://www.linkedin.com/in/nicholas-sky-salvatio-1957091b6"))),
  dashboardBody(
    fluidRow(
      box(
        title = 'Tabel Pertumbuhan Ekonomi Regional',
        status = 'primary',
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 12,
        tableOutput('table1')
      ),
      box(
        title = 'PDB Harga Berlaku (Biru) dan Harga Konstan (Merah)',
        status = 'primary',
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput('plot1', height = '300px')
      ),
      box(
        title = 'Pendapatan per Kapita Harga Berlaku (Kuning) dan Harga Konstan (Hijau)',
        status = 'primary',
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput('plot2', height = '300px')
      ),
      box(
        title = 'Laju Pertumbuhan PDB (Biru) dan Pendapatan per Kapita (Merah)',
        status = 'primary',
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput('plot3', height = '300px')
      ),
      box(
        title = 'Korelasi antar Variabel',
        status = 'primary',
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput('plot4', height = '300px')
      )
    )
  )
)

server <- function(input, output, session){
  data1 <- reactive({
    df %>% filter(Provinsi == input$Provinsi)
  })
  data2 <- reactive({
    data1() %>% select(-Provinsi, -Tahun, -Laju_HK, -Laju_Pendapatan_HK)
  })
  observe({
    updateSelectInput(session, 'Provinsi', 'Provinsi:',
                      choices = c(unique(df$Provinsi)))
  })
  output$table1 <- renderTable(data1())
  output$plot1 <- renderPlot(
    ggplot(data1(), aes(x = Tahun)) +
      geom_bar(aes(y=PDB_Harga_Berlaku), stat="identity", position ="identity", alpha=.3, fill='lightblue', color='lightblue4') +
      geom_bar(aes(y=PDB_Harga_Konstan), stat="identity", position ="identity", alpha=.8, fill='pink', color='red') + xlab('') +
      ylab('') + geom_text(aes(y = PDB_Harga_Berlaku, label = PDB_Harga_Berlaku), vjust=1.6, color="blue", size=3.5) + 
      geom_text(aes(y = PDB_Harga_Konstan, label = PDB_Harga_Konstan), vjust=1.6, color="red", size=3.5))
  output$plot2 <- renderPlot(
    ggplot(data1(), aes(x = Tahun)) +
      geom_bar(aes(y=Pendapatan_HB), stat="identity", position ="identity", alpha=.3, fill='yellow', color='yellow4') +
      geom_bar(aes(y=Pendapatan_HK), stat="identity", position ="identity", alpha=.8, fill='green', color='green4') + xlab('') +
      ylab('') + geom_text(aes(y = Pendapatan_HB, label = Pendapatan_HB), vjust=1.6, color="orange", size=3.5) + 
      geom_text(aes(y = Pendapatan_HK, label = Pendapatan_HK), vjust=1.6, color="green4", size=3.5))
  output$plot3 <- renderPlot(
    ggplot(data1(), aes(x = Tahun, group = 1)) +
      geom_line(aes(y = Laju_HK), color = 'blue', size = 1) +
      geom_line(aes(y = Laju_Pendapatan_HK), color = 'red', size = 1) + xlab('') + ylab(''))
  output$plot4 <- renderPlot(
    ggcorrplot(round(cor(data2()),1), lab = TRUE, type = 'lower')
  )
}

shinyApp(ui, server)
