# load RSEgene object first
text_read <- "Welcome columns are genes
and rows are the sample names , the first column is sample ID and there is additional column
called class."

text_welcome <- "welcome"

normalized_df <- data.frame()
read_df <- data.frame()
splited_df_list <- list(train_x = data.frame(), train_y = vector(),
                        test_x = data.frame(), test_y = vector())
model_list <- list()
selected_genes <- vector()
selected_genes_df <- data.frame()

library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme ="gallery.css",
  tags$head(tags$link(rel = "stylesheet", type = "text/css",href = "gallery.css")),

  titlePanel(htmlOutput("current_process")),

  navbarPage("My_Application",id = "My_Application",
             tabPanel("Load data",
                      span(text_read),
                      textInput("id", "writeID"),
                      HTML("Choose: <br> (1) if columns are genes and rows are the sample names , the first column is sample ID and there is additional column
                           called class <br> (2) if bla bla bla format"),
                      radioButtons("format_with", label = "format_with", choices = c(1,2), selected = 1),
                      actionButton(inputId = "read_button", label = "Read Data"))

             ,

             tabPanel("Normalisation",
                      radioButtons("normalize_with", label = "normalize_with", choices = c("deseq2","voom"), selected = "deseq2"),
                      actionButton(inputId = "normalize_button",label = "Normalize Data")),

             tabPanel("Split data",
                      actionButton(inputId = "split_button", label = "OK")),

             tabPanel("Compare models"),

             tabPanel("Visualize selected genes"),

             tabPanel("SVM")

))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#' server function for the shiny app
server <- function(input, output, session) {
  #-------------------------------------------------------------------------------
  variables = reactiveValues(current_task = text_welcome)
  #-------------------------------------------------------------------------------
  # Buttons
  observeEvent(input$read_button,{
    rse_gene <<- load_data(input$id)
    class1 <<- get_class(rse_gene)
    updateTabsetPanel(session,"My_Application", selected = "Normalisation")

  })
  observeEvent(input$normalize_button,{
    #normalized_df <<- normaliv(read_df, input$normalize_with)
    v_all <<- RNASeqclassifier::normalize_voom(counts)
    updateTabsetPanel(session,"My_Application", selected = "Split data")
  })

  observeEvent(input$split_button,{
    l <<- RNASeqclassifier::split_counts(v_all$E, class1)
    updateTabsetPanel(session,"My_Application", selected = "Compare models")
  })
  #---------------------------------------------------------------------------
  output$current_process <- renderText(paste0(":~$",
                                              variables$current_task))
}

#' Run the shiny application
#' @param ui object
#' @param server function
start_shiny <- function(){
  shinyApp(ui = ui, server = server)
}

