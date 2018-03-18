# load RSEgene object first

normalized_df <- data.frame()
read_df <- data.frame()
splited_df_list <- list(train_x = data.frame(), train_y = vector(),
                        test_x = data.frame(), test_y = vector())
model_list <- list()
selected_genes <- vector()
selected_genes_df <- data.frame()

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme ="gallery.css",
  tags$head(tags$link(rel = "stylesheet", type = "text/css",href = "gallery.css")),

  titlePanel(htmlOutput("current_process")),

  navbarPage("My_Application",id = "My_Application",
             tabPanel("Upload_data",
                      span(text_read),
                      fileInput("file1", "Choose CSV File", multiple = TRUE,accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                      HTML("Choose: <br> (1) if columns are genes and rows are the sample names , the first column is sample ID and there is additional column
                           called class <br> (2) if bla bla bla format"),
                      radioButtons("format_with", label = "format_with", choices = c(1,2), selected = 1),
                      actionButton(inputId = "read_button", label = "Read Data")),

             tabPanel("Normalisation",
                      radioButtons("normalize_with", label = "normalize_with", choices = c("deseq2","voom"), selected = "deseq2"),
                      actionButton(inputId = "normalize_button",label = "Normalize Data")),

             tabPanel("Split data",
                      radioButtons("split_with",label = "Split data into train and test ?",choices = c("yes","no"),selected = "yes"),
                      actionButton(inputId = "split_button", label = "OK")),

             tabPanel("Compare models",
                      fluidRow(
                        column(8,
                               selectInput("run_with",label = "select important genes with",choices = c("RF_up_sampling","RF_smote_sampling","glm_up_sampling","glm_smote_sampling")),
                               actionButton(inputId = "add_model_button", label = "Add model"),
                               actionButton(inputId = "remove_model_button", label = "Remove model"),
                               actionButton(inputId = "use_models_button", label = "use selected models"),
                               numericInput(inputId = "num_genes", label = "Number of genes to select",value = 1)),

                        column(12,plotOutput("roc")))),

             tabPanel("Visualize selected genes",
                      actionButton(inputId = "svm_button", label = "Train SVM on the selected genes"),
                      radioButtons("barplot_with",label = "update_plot_scale_each_gene ?",choices = c("yes","no"),selected = "yes"),
                      plotOutput("barplot")),

             tabPanel("SVM",
                      verbatimTextOutput("confusion_matrix")),



             tabPanel("Settings",id = "Settings",
                      selectInput("config_variable", "choose variable ?",choices = colnames(config_df)),
                      textInput("config_value", "value"),
                      actionButton("config_update",label = "update"))))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
server <- function(input, output, session) {
  #-------------------------------------------------------------------------------
  variables = reactiveValues(current_task = text_welcome, config_df2 = NULL)
  #-------------------------------------------------------------------------------
  # Buttons
  observeEvent(input$read_button,{
    read_df <<- readv(input$file1$datapath,input$format_with)
    updateTabsetPanel(session,"My_Application", selected = "Normalisation")

  })
  observeEvent(input$normalize_button,{
    #normalized_df <<- normaliv(read_df, input$normalize_with)
    normalized_df <<- normalized_df2
    updateTabsetPanel(session,"My_Application", selected = "Split data")
  })

  observeEvent(input$split_button,{
    splited_df_list <<- splitv(normalized_df, input$split_with)
    updateTabsetPanel(session,"My_Application", selected = "Compare models")
  })

  observeEvent(input$add_model_button,{
    model_list[[length(model_list)+1]] <<- runv(splited_df_list,input$run_with)
    output$roc <- renderPlot(plot_multipe_rocs(model_list))
  })

  observeEvent(input$remove_model_button, {

    for (i in 1 : length(model_list)){
      if (model_list[[i]]$modelInfo$label == input$run_with){
        model_list <<- model_list[-i]
      }
    }
    output$roc <- renderPlot(plot_multipe_rocs(model_list))

  })

  observeEvent(input$use_models_button, {
    selected_genes <<- selectv(model_list,train_x =  splited_df_list[[1]],
                               train_y = splited_df_list[[2]],num_genes = input$num_genes)
    selected_genes_df <<- generate_selected_genes_df(selected_geness = selected_genes,
                                                     splited_df_list,
                                                     with = input$split_with)
    updateTabsetPanel(session,"My_Application", selected = "Visualize selected genes")

  })

  observeEvent(input$svm_button, {
    updateTabsetPanel(session,"My_Application", selected = "SVM")
    svmm <- svm(selected_genes_df)
    output$confusion_matrix <- renderPrint(svmm)
  })


  output$barplot <- renderPlot({
    compare_selected_features_barplot(selected_genes_df, input$barplot_with)},
    height = 1000, width = 900)

  # settings button
  observeEvent(input$config_update,{
    variables$config_df2[,which(
      colnames(variables$config_df2) == input$config_variable)] <<- input$config_value

  })

  observeEvent(input$config_variable,{
    updateTextInput(session, "config_value", value =
                      paste(variables$config_df2[,which(colnames(variables$config_df2) == input$config_variable)]))
  })
  #---------------------------------------------------------------------------
  output$current_process <- renderText(paste0(":~$",
                                              variables$current_task))
}

# Run the application
start_shiny <- function() shinyApp(ui = ui, server = server)

