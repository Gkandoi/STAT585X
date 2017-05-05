library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(lubridate)

# Load datasets

HSA <- unique(read.table(gzfile("../data/goa_human.gaf.gz"),skip = 34, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")[,c(3,5,7,9,14)])
MMU <- unique(read.table(gzfile("../data/gene_association.mgi.gz"),skip = 47, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")[,c(3,5,7,9,14)])
ATH <- unique(read.table(gzfile("../data/gene_association.tair.gz"),skip = 24, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")[,c(3,5,7,9,14)])
ZFN <- unique(read.table(gzfile("../data/gene_association.zfin.gz"),skip = 28, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")[,c(3,5,7,9,14)])
HSA$V14 <- as.character(ymd(HSA$V14))
MMU$V14 <- as.character(ymd(MMU$V14))
ATH$V14 <- as.character(ymd(ATH$V14))
ZFN$V14 <- as.character(ymd(ZFN$V14))


# Define UI for application
ui <- fluidPage(    
# Application title
  titlePanel("GOexplore: Gene Ontology Explorer!"),
# Sidebar with a radio button to select the Organism
  sidebarPanel(
    radioButtons("radio", label = h3("Choose organism"),
                 choices = list("Human" = 1, "Mouse" = 2, "Athaliana" = 3, "ZebraFish" = 4), 
                 selected = 4),
    conditionalPanel(
      condition = "input.conditionedPanels == 'Table'" ),
    conditionalPanel(
      condition = "input.conditionedPanels == 'Bar Plot of Genes'", uiOutput("data")),
    conditionalPanel(
      condition = "input.conditionedPanels == 'Bar Plot of Go terms'",uiOutput("data2")),
    conditionalPanel(
      condition = "input.conditionedPanels == 'Annotation Added'", uiOutput("data3"))
  ),
  
# Define the tab panels
  mainPanel(
    tabsetPanel(
      tabPanel("Table", dataTableOutput("view")),
      tabPanel("Plot", plotlyOutput("aspect")),
      tabPanel("Bar Plot of Genes",plotlyOutput("bars", height = "600px")),
      tabPanel("Bar Plot of Go terms",plotlyOutput("bars2", height = "600px")),
      tabPanel("Annotation Added", plotlyOutput("bars3", height = "600px")),
      id = "conditionedPanels"
    )
  )
)

# Define server logic
server <- function(input, output) {
  datasetInput <- reactive({
    if (input$radio == 1) {
      HSA
    } else if (input$radio == 2) {
      MMU
    } else if (input$radio == 3) {
      ATH
    } else if (input$radio == 4) {
      ZFN
    }
    
  })

# Show the first "10" observations and give user an option to view more based on the DataTable.
  output$view <- renderDataTable(datasetInput(), options = list(pageLength = 10))

# Plot the number of annotations for every evidence code, colored by the aspect of annotations.
  output$aspect <- renderPlotly({
    gg <-  ggplot(data = datasetInput(), aes(x = V7, fill = V9)) +
      geom_bar(position = "dodge")
    ggplotly(gg)
  })

# Extract the list of genes and provide to the user.
  output$data <- renderUI({
    data <- datasetInput()
    selectInput("genes",label="Select Gene",choices=sort(unique(data$V3)), size = 5, selectize=F,selected=data$V3[1])
  })
  
# Plot the number of annotations for every gene based on the GO Term and colored by the aspect code.
  output$bars <- renderPlotly({
    gg <-  datasetInput() %>% filter(V3==input$genes) %>% ggplot(aes(x=V5, fill = V9)) +
      geom_bar(position = "dodge")
    ggplotly(gg)
  })

# Extract the list of GO terms and provide to the user.
  output$data2 <- renderUI({
    data2 <- datasetInput()
    selectInput("goterms",label="Select Go term",choices=sort(unique(data2$V5)), size = 5, selectize=F,selected=data2$V5[1])
  })
  
# Plot the number of annotations for every GO term based on the aspect of annotations and colored by the evidence code.  
  output$bars2 <- renderPlotly({
    gg <-  datasetInput() %>% filter(V5==input$goterms) %>% ggplot(aes(x=V9, fill = V7)) +
      geom_bar(position = "dodge")
    ggplotly(gg)
  })

# Extract the list of Dates and provide to the user.
  output$data3 <- renderUI({
    data3 <- datasetInput()
    dateInput("DatePlot", "Select Date", min=min(data3$V14), max=max(data3$V14), value=ymd(20021014))
  })
  
# Plot the number of annotations added on a specific date based on the GOTerms, colored by the evidence code and faceted by aspect.
  output$bars3 <- renderPlotly({
    gg <-  datasetInput() %>% filter(V14==input$DatePlot) %>% ggplot(aes(x=V5, fill = V7)) + facet_wrap(~V9, ncol = 1) +
      geom_bar(position = "dodge")
    ggplotly(gg)
  })
  
  
}

# Bind ui and server together
shinyApp(ui, server)
