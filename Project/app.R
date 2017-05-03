library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(lubridate)
# Load datasets

HSA <- unique(read.table("./data/goa_human.gaf",skip = 34, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")[,c(3,5,7,9,14)])
MMU <- unique(read.table("./data/gene_association.mgi",skip = 47, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")[,c(3,5,7,9,14)])
HSA$V14 <- as.character(ymd(HSA$V14))
MMU$V14 <- as.character(ymd(MMU$V14))

# Define UI for application
ui <- fluidPage(    
# Application title
  titlePanel("GOexplore: Gene Ontology Explorer!"),
# Sidebar with a radio button to select the Organism
  sidebarPanel(
    radioButtons("radio", label = h3("Choose organism"),
                 choices = list("Human" = 1, "Mouse" = 2), 
                 selected = 1),
    conditionalPanel(
      condition = "input.conditionedPanels == 'Table'", numericInput(inputId = "n","# of Annotations", value = 10)),
    conditionalPanel(
      condition = "input.conditionedPanels == 'Bar Plot of Genes'", uiOutput("data")),
    conditionalPanel(
      condition = "input.conditionedPanels == 'Bar Plot of Go terms'",uiOutput("data2")),
    conditionalPanel(
      condition = "input.conditionedPanels == 'Growth'", uiOutput("data3"))
  ),
  
# Define the tab panels
  mainPanel(
    tabsetPanel(
      tabPanel("Table", tableOutput("view")),
      tabPanel("Plot", plotlyOutput("aspect")),
      tabPanel("Bar Plot of Genes",plotlyOutput("bars")),
      tabPanel("Bar Plot of Go terms",plotlyOutput("bars2")),
      tabPanel("Growth", plotlyOutput("bars3")),
      id = "conditionedPanels"
    )
  )
)

# Define server logic
# Download the current annotations from the GO server.
server <- function(input, output) {
  datasetInput <- reactive({
    if (input$radio == 1) {
      HSA
    }  else {
      MMU
    }
  })

# Show the first "10" observations and give user an option to view more than 10.
  output$view <- renderTable({
    head(datasetInput(), n = input$n)
  })
# Plot the number of annotations for every evidence code, colored by the aspect of annotations.
  output$aspect <- renderPlotly({
    gg <-  ggplot(data = datasetInput(), aes(x = V7, fill = V9)) +
      geom_bar(position = "dodge")
    ggplotly(gg)
  })

# Extract the list of genes and provide to the user a drop-down menu.
  output$data <- renderUI({
    data <- datasetInput()
    selectInput("genes",label="gene",choices=sort(unique(data$V3)), size = 5, selectize=F,selected=data$V3[1])
  })
  
# Plot the number of annotations for every gene based on the aspect of annotations and colored by the evidence code.
  output$bars <- renderPlotly({
    gg <-  datasetInput() %>% filter(V3==input$genes) %>% ggplot(aes(x=V9, fill = V7))+
      geom_bar(position = "dodge")
    ggplotly(gg)
  })

# Extract the list of GO terms and provide to the user a drop-down menu.
  output$data2 <- renderUI({
    data2 <- datasetInput()
    selectInput("goterms",label="go terms",choices=sort(unique(data2$V5)), size = 5, selectize=F,selected=data2$V5[1])
  })
  
# Plot the number of annotations for every GO term based on the aspect of annotations and colored by the evidence code.  
  output$bars2 <- renderPlotly({
    gg <-  datasetInput() %>% filter(V5==input$goterms) %>% ggplot(aes(x=V9, fill = V7)) +
      geom_bar(position = "dodge")
    ggplotly(gg)
  })

# Extract the list of genes and provide to the user a drop-down menu.
  output$data3 <- renderUI({
    data3 <- datasetInput()
    dateInput("DatePlot", "Date Plot", min=min(data3$V14), max=max(data3$V14), value=median(data3$V14))
  })
  
# Plot the number of annotations for every gene based on the aspect of annotations and colored by the evidence code.
  output$bars3 <- renderPlotly({
    gg <-  datasetInput() %>% filter(V14==input$DatePlot) %>% ggplot(aes(x=V9, fill = V7)) +
      geom_bar(position = "dodge")
    ggplotly(gg)
  })
  
  
}

# Bind ui and server together
shinyApp(ui, server)
