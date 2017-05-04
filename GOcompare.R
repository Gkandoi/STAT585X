library(shiny)
library(ggplot2)
library(plotly)
library(readr)

# Load datasets
HSA <- unique(read.table(gzfile("./Project/data/goa_human.gaf.gz"),skip = 34, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")[,c(3,5,7,9,14)])
MMU <- unique(read.table(gzfile("./Project/data/gene_association.mgi.gz"),skip = 47, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")[,c(3,5,7,9,14)])
colnames(HSA) <- c("Gene", "GOTerms", "Evidence", "Aspect", "Date")
colnames(MMU) <- c("Gene", "GOTerms", "Evidence", "Aspect", "Date")
HSA$Gene <- toupper(HSA$Gene)
MMU$Gene <- toupper(MMU$Gene)
All_Genes <- as.character(unique(toupper(rbind(HSA,MMU)[,1])))


ui <- fluidPage(    
  # Application title
  titlePanel("GOcompare: Gene Ontology Comparison!"),
  # Sidebar with a menu to select the Gene
  sidebarPanel(
    selectInput("genes",label="Select Gene",choices=sort(unique(All_Genes)), size = 5, selectize=F,selected="ABCA1")
  ),
  
  # Define the tab panels
  mainPanel(
    plotlyOutput("bars"),
    plotlyOutput("bars2"))
)

# Define server logic
server <- function(input, output) {
  
  
  # Plot the number of annotations for every gene based on the GO Term of annotation, colored by the evidence code and faceted by the Aspect.
  output$bars <- renderPlotly({
    gg <-  HSA %>% filter(Gene==input$genes) %>% ggplot(aes(x=GOTerms, fill = Evidence)) + facet_wrap(~Aspect, nrow = 1) +
      labs(title = "Humans") +
      geom_bar(position = "dodge")
    ggplotly(gg)
  })
  
  output$bars2 <- renderPlotly({
    gg <-  MMU %>% filter(Gene==input$genes) %>% ggplot(aes(x=GOTerms, fill = Evidence)) + facet_wrap(~Aspect, nrow = 1) +
      labs(title = "Mouse") +
      geom_bar(position = "dodge")
    ggplotly(gg)
  })
  
  
}

# Bind ui and server together
shinyApp(ui, server)