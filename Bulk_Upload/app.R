library(shiny)
library(dplyr)
library(DT)

error_store <<- data.frame(
  filename = c(''),
  file_type = c(''),
  comment = c(''), 
  verify_status = c(''), 
  error = c('')
)

# UI ####
ui <- fluidPage(
  
  ## Bulk Upload ####
  titlePanel("Bulk Upload & Validation"),
  ## Body ####
  mainPanel(
    splitLayout(
      fileInput('file', "Upload file", multiple = TRUE, accept = ".csv"), 
      shinyFeedback::useShinyFeedback(), 
      actionButton("verify", "Verify Files", icon = icon('square-check')), 
      actionButton("reload", "Reload Page", icon = icon('refresh'))
    ),
    splitLayout(downloadButton("file_template_download_1", "Download data_1 template file"),
                downloadButton("file_template_download_2", "Download data_2 template file")
    ),
    hr(),
    uiOutput("uploaded_data")
  )
)

# Server ####
server <- function(input, output, session) {
  observeEvent(input$reload, {
    error_store <<- data.frame(
      filename = c(''),
      file_type = c(''),
      comment = c(''), 
      verify_status = c(''), 
      error = c('')
    )
    session$reload()
  })
  
  ## Download Sample ####
  ### Data 1 template ####
  output$file_template_download_1 <- downloadHandler(
    filename = function() {
      paste("Sample data 1", ".csv", sep = "")
    },
    content = function(file) {
      temp = read.csv("data_1.csv", header = TRUE)
      write.csv(temp, file, row.names = FALSE)
    }
  )
  ### Data 2 template ####
  output$file_template_download_2 <- downloadHandler(
    filename = function() {
      paste("Sample data 2", ".csv", sep = "")
    },
    content = function(file) {
      temp = read.csv("data_2.csv", header = TRUE)
      write.csv(temp, file, row.names = FALSE)
    }
  )
  
  ## Bulk Upload ####
  ### Bulk Upload Modal Dialog ####
  observeEvent(input$file, {
    df_type <- c('NA', 'Data 1', 'Data 2')
    
    print(input$file)
    
    data <- input$file
    
    if (!is.null(input$file)) {
      files <- data$name
      
      filecount <- length(files)
      ui_mod <- c()
      for (i in 1:filecount) {
        ui_mod[[i]] <- fluidRow(column(
          3,
          tags$div(basename(files[i]), style = "body { word-wrap: break-word; }")
        ),
        column(
          3,
          selectInput(paste0("dataset", i), "Choose data set", choices = df_type)
        ),
        column(3, textInput(
          paste0("comments", i), "Comments:", ""
        )))
      }
      
      showModal(modalDialog(
        fluidPage(h3(strong(
          "Upload Files"
        ), align = "center"), hr(), ui_mod),
        size = 'l',
        fade = TRUE,
        footer = tagList(
          actionButton("submit_data", "Submit"),
          modalButton('Close')
        )
      ))
      
      data_details <<- data
    }
  })
  
  ### Store details ####
  observeEvent(input$submit_data, {
    removeModal()
    
    temp <- data.frame(
      filelink = c(''),
      filetype = c(''),
      filecomment = c(''),
      filename = c('')
    )
    px <- temp[-1,]
    filecount <- nrow(data_details)
    for(i in 1:filecount){
      temp$filelink <- data_details$datapath[i]
      temp$filetype <- input[[paste0("dataset", i)]]
      temp$filecomment <- input[[paste0("comments", i)]]
      temp$filename <- data_details$name[i]
      
      px <- px %>% bind_rows(temp)
    }
    
    write.csv(px, "Bulk_Upload.csv", row.names = FALSE)
  })
  
  ### Bulk upload verify ####
  observeEvent(input$verify, {
    df <- read.csv("Bulk_Upload.csv", header = TRUE)
    
    if("Data 1" %in% df$filetype){
      tryCatch(data_1_upload(), error = function(e) TRUE)
    } 
    
    if("Data 2" %in% df$filetype){
      tryCatch(data_2_upload(), error = function(e) TRUE)
    } 
    
    print("test3")
    check <- 'Failure' %in% error_store$verify_status
    if(check){
      shinyFeedback::hideFeedback("file")
      shinyFeedback::feedbackDanger("file", check, "Some uploads have error")
    } else {
      shinyFeedback::hideFeedback("file")
      shinyFeedback::feedbackSuccess("file", !check, "Successfully uploaded")
    }
    ### render data table #####
    output$uploaded_data <- renderUI({
      fluidRow(
        h3('Upload Status'),
        renderDT(error_store[-1,], filter = "top", rownames = FALSE)
      )
    })
  })
  
  
  
  ## Individual reactive ####
  ### Data 1 reactive ####
  data_1_upload <- reactive({
    df <- read.csv("Bulk_Upload.csv", header = TRUE)
    df <- df %>% filter(filetype == "Data 1")
    
    if(tools::file_ext(df$filelink[1]) != 'csv'){
      error_store_temp <- data.frame(
        filename = df$filename[1],
        file_type = df$filetype[1],
        comment = df$filecomment[1], 
        verify_status = 'Failure', 
        error = "Invalid Data 1 file format. Upload .csv file"
      )
      error_store <<- error_store %>% bind_rows(error_store_temp)
      req(tools::file_ext(df$filelink[1]) == 'csv')
    } 
    
    df_dat <- read.csv(df$filelink[1], header = TRUE)
    
    col_names <- c("A", "B", "C")
    col_check <- sum(1*(col_names == colnames(df_dat)))
    
    if(col_check != 3){
      error_store_temp <- data.frame(
        filename = df$filename[1],
        file_type = df$filetype[1],
        comment = df$filecomment[1], 
        verify_status = 'Failure', 
        error = "Invalid Data 1 file format. Please download the template."
      )
      error_store <<- error_store %>% bind_rows(error_store_temp)
      req(col_check == 3)
    } 
    
    error_store_temp <- data.frame(
      filename = df$filename[1],
      file_type = df$filetype[1],
      comment = df$filecomment[1], 
      verify_status = 'Success', 
      error = NA
    )
    error_store <<- error_store %>% bind_rows(error_store_temp)
    print("test1")
  })
  
  ### Data 2 reactive ####
  data_2_upload <- reactive({
    df <- read.csv("Bulk_Upload.csv", header = TRUE)
    df <- df %>% filter(filetype == "Data 2")
    
    if(tools::file_ext(df$filelink[1]) != 'csv'){
      error_store_temp <- data.frame(
        filename = df$filename[1],
        file_type = df$filetype[1],
        comment = df$filecomment[1], 
        verify_status = 'Failure', 
        error = "Invalid Data 2 file format. Upload .csv file"
      )
      error_store <<- error_store %>% bind_rows(error_store_temp)
      req(tools::file_ext(df$filelink[1]) == 'csv')
    } 
    
    df_dat <- read.csv(df$filelink[1], header = TRUE)
    
    col_names <- c("A", "D", "E")
    col_check <- sum(1*(col_names == colnames(df_dat)))
    
    if(col_check != 3){
      error_store_temp <- data.frame(
        filename = df$filename[1],
        file_type = df$filetype[1],
        comment = df$filecomment[1], 
        verify_status = 'Failure', 
        error = "Invalid Data 2 file format. Please download the template."
      )
      error_store <<- error_store %>% bind_rows(error_store_temp)
      req(col_check == 3)
    } 
    
    error_store_temp <- data.frame(
      filename = df$filename[1],
      file_type = df$filetype[1],
      comment = df$filecomment[1], 
      verify_status = 'Success', 
      error = NA
    )
    error_store <<- error_store %>% bind_rows(error_store_temp)
    print("test2")
  })
}

shinyApp(ui, server)