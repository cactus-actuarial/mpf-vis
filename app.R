# libraries
library(tibble)
library(ggplot2)
library(shiny)
library(readxl)
library(dplyr)
library(DT)

# settings
options(scipen = 999) # disable scientific notation
set.seed(123)

# functions
read_mpf <- function(datapath) {
    data <- read.table(file = datapath, sep = ",", skip = 1, header = TRUE)
    data$X.1 <- NULL
    return(data)
}

return_binsize <- function(vector) {
    span <- diff(range(vector))
    no_unique_values <- length(unique(vector))
    if (no_unique_values <= 100 && class(vector) == "integer") {
        1
    } else {
        if(class(vector) == "integer") {
            round(span/100, 0)
        } else {
            span/100
        }
    }
}

return_breaks <- function(vector) {
    if (class(vector) == "integer") {
        down <- round(min(vector), 0)
        up   <- round(max(vector), 0)
    } else {
        down <- min(vector)
        up   <- max(vector)
    }
    
    span <- up - down
    if (span <= 100  && class(vector) == "integer") {
        seq(from = down, to = up, by = 1)
    } else {
        if(class(vector) == "integer") {
            seq(from = down, to = up, by = round(span/20, 0))
        } else {
            seq(from = down, to = up, by = span/20)
        }
    }
}

is_long <- function(vector) {
    length(unique(vector)) > 1
}

### app
# ui
ui <- fluidPage(
    
    titlePanel("Model Point Files visualiser", windowTitle = "MPF visualiser"),
    
    sidebarLayout(
        sidebarPanel(
            wellPanel(
                h4("Load data"),
                radioButtons(inputId = "no_mpf", label = "Input type:", choices = c("Show 1 MPF" = 1, "Compare 2 MPFs" = 2), 
                             selected = 1, inline = TRUE),
                hr(),
                
                # 1 MPF
                # fileInput(inputId = "file_1", label = "1st MPF:", accept = ".rpt"),
                h4("1st MPF"),
                textInput(inputId = "path_1", label = "Path:"),
                selectInput(inputId = "product_1", label = "Product:", choices = c()),
                
                # 2 MPFs
                conditionalPanel(condition = "input.no_mpf == 2", h4("2nd MPF")),
                conditionalPanel(condition = "input.no_mpf == 2", textInput(inputId = "path_2", label = "Path:")),
                conditionalPanel(condition = "input.no_mpf == 2", selectInput(inputId = "product_2", label = "Product:", choices = c()))
            ),
            
            wellPanel(
                h4("Graph settings"),
                selectInput(inputId = "selected_var", label = "Choose variable:", choices = c()),
                radioButtons(inputId = "hist_type", label = "Histogram type:", choices = c("frequency" = 1, "density" = 2), 
                             selected = 1, inline = TRUE)
            )
        ),
        
        mainPanel(
            tabsetPanel(id = "tabs",
                tabPanel(title = "Plot", br(), 
                    
                    plotOutput(outputId = "plot"),
                    
                    hr(),
                    
                    # no of records in MPF
                    conditionalPanel(condition = "input.no_mpf == 1",
                                     htmlOutput(outputId = "no_rows_s") # single
                    ),
                    
                    conditionalPanel(condition = "input.no_mpf == 2",
                                     htmlOutput(outputId = "no_rows_d") # double
                    )
                ),
                
                tabPanel(title = "Data_1",
                    dataTableOutput(outputId = "data_1")
                ),
                tabPanel(title = "Data_2",
                    dataTableOutput(outputId = "data_2")
                )
            )
        )
    )
)

# server
server <- function(input, output, session) {

      # show/hide Data_2 tab
    observe({
      if(input$no_mpf == 1) {
        hideTab(inputId = "tabs", target = "Data_2")
      }
      if(input$no_mpf == 2) {
        showTab(inputId = "tabs", target = "Data_2")
      }
    })
    

    # get a list of rpt files
    prod_list_1 <- reactive({
      path_1 <- input$path_1
      list.files(path = path_1, pattern = "*.rpt", ignore.case = TRUE)
    })
    
    prod_list_2 <- reactive({
      path_2 <- input$path_2
      list.files(path = path_2, pattern = "*.rpt", ignore.case = TRUE)
    })
    
    # update product list choices
    observe({
      req(prod_list_1())
      updateSelectInput(session, "product_1", choices = prod_list_1())
    })
    
    observe({
      req(prod_list_2())
      updateSelectInput(session, "product_2", choices = prod_list_2())
    })
  
    # load data
    data_mpf <- reactive({
      
      # 1 MPF
      if (input$no_mpf == 1) {
        req(input$path_1)
        req(input$product_1)
        
        datapath_1 <- paste(input$path_1, "\\", input$product_1, sep = "")
        
        if (is.null(input$product_1))
          return(NULL)
        
        data <- read_mpf(datapath_1)
        data$MPF <- rep("1st_MPF", dim(data)[1]) # set indicator for the dataset 
        data
        
      # 2 MPFs
      } else if (input$no_mpf == 2) {
        req(input$path_1)
        req(input$path_2)
        req(input$product_1)
        req(input$product_2)
        
        datapath_1 <- paste(input$path_1, "\\", input$product_1, sep = "")
        datapath_2 <- paste(input$path_2, "\\", input$product_2, sep = "")
        
        if (is.null(input$product_1) || is.null(input$product_2))
          return(NULL)
        
        data_1 <- read_mpf(datapath_1)
        data_2 <- read_mpf(datapath_2)

        # set indicators for two datasets
        data_1$MPF <- rep("1st_MPF", dim(data_1)[1])
        data_2$MPF <- rep("2nd_MPF", dim(data_2)[1])
        
        # choose only common columns
        com_cols <- intersect(colnames(data_1), colnames(data_2))
        data <- rbind(subset(data_1, select = com_cols), subset(data_2, select = com_cols))
        data
      }
    })
    
    # update variables select list
    observe({
        req(data_mpf())
        data <- data_mpf()
        numeric_cols <- colnames(data)[which(sapply(data, is.numeric))]
        updateSelectInput(session, "selected_var", choices = numeric_cols)
    })
    
    # x-axis breaks
    binsize <- reactive({
        req(input$selected_var)
        return_binsize(data_mpf()[, input$selected_var])
    })
    
    # x-axis ticks
    breaks_range <- reactive({
        return_breaks(data_mpf()[, input$selected_var])
    })
    
    
    ### tab Plot

    ## main plot
    output$plot <- renderPlot({
        data_mpf <- data_mpf()
        if (is.null(data_mpf)) {}
        else {
          if (input$no_mpf == 1 & input$hist_type == 1) {  
              ggplot(data = data_mpf, aes(x = data_mpf[, input$selected_var], y = ..count..)) +
                geom_histogram(binwidth = binsize(), fill = "white", colour = "black") +
                xlab(input$selected_var) +
                scale_x_continuous(breaks = breaks_range())
          } else if (input$no_mpf == 1 & input$hist_type == 2) {
              ggplot(data = data_mpf, aes(x = data_mpf[, input$selected_var], y = ..density..)) +
                geom_histogram(binwidth = binsize(), fill = "white", colour = "black") +
                xlab(input$selected_var) +
                scale_x_continuous(breaks = breaks_range())
          } else if (input$no_mpf == 2 & input$hist_type == 1) {
              ggplot(data = data_mpf, aes(x = data_mpf[, input$selected_var], y = ..count.., fill = MPF)) +
                geom_histogram(position = "identity", binwidth = binsize(), colour = "black", alpha = 0.4) +
                xlab(input$selected_var) +
                scale_x_continuous(breaks = breaks_range())
          } else if (input$no_mpf == 2 & input$hist_type == 2) {
              ggplot(data = data_mpf, aes(x = data_mpf[, input$selected_var], y = ..density.., fill = MPF)) +
                geom_histogram(position = "identity", binwidth = binsize(), colour = "black", alpha = 0.4) +
                xlab(input$selected_var) +
                scale_x_continuous(breaks = breaks_range())
          }
        }
    })
    
    ## number of records info
    output$no_rows_s <- renderUI(
      if (is.null(data_mpf())) {}
      else {
        HTML("MPF contains ", nrow(data_mpf()), " records.")
      }
    )
    
    output$no_rows_d <- renderUI(
      if (is.null(data_mpf())) {}
      else {
        HTML("1st MPF contains ", nrow(subset(data_mpf(), MPF == "1st_MPF")), " records.",
             "<br>2nd MPF contains ", nrow(subset(data_mpf(), MPF == "2nd_MPF")), " records.")
      }
    )
    
    ### tab Data
    output$data_1 <- DT::renderDataTable(
      if (is.null(data_mpf())) {}
      else {
        subset(data_mpf(), MPF == "1st_MPF")
      }
    )
    output$data_2 <- DT::renderDataTable(
      if (is.null(data_mpf())) {}
      else {
        subset(data_mpf(), MPF == "2nd_MPF")
      }
    )
}

shinyApp(ui = ui, server = server)
