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

rbimodal <- function (n, cpct, mu1, mu2, sig1, sig2) {
    y0 <- rlnorm(n,mean=mu1, sd = sig1)
    y1 <- rlnorm(n,mean=mu2, sd = sig2)
    
    flag <- rbinom(n,size=1,prob=cpct)
    y <- y0*(1 - flag) + y1*flag 
}

# create demo data
n1 = 5000
n2 = 6000

demo_data <- as.data.frame(matrix(nrow = (n1 + n2), ncol = 3))
colnames(demo_data) <- c("MPF", "AGE_AT_ENTRY", "SUM_ASSURED")

demo_data$MPF <- c(rep("1st_MPF", n1), rep("2nd_MPF", n2))

demo_data$AGE_AT_ENTRY <- as.integer(
                          c(round(rbimodal(n1, 0.2, log(30), log(45), log(1.1), log(1.1)), 0),  
                            round(rbimodal(n2, 0.2, log(30), log(45), log(1.1), log(1.1)), 0)))

demo_data$SUM_ASSURED  <- c(round(rlnorm(n1, meanlog = log(100), sdlog = log(1.2)), digits = 1)*10^2,
                            round(rlnorm(n2, meanlog = log(100), sdlog = log(1.2)), digits = 1)*10^2)

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
                
                strong("Choose RPT file:"),
                
                checkboxInput(inputId = "sample_data", label = "Use sample data", value = TRUE),
                
                
                # own data, 1 MPF
                conditionalPanel(condition = "input.sample_data == 0",
                                 fileInput(inputId = "file_1", label = "1st MPF:", accept = ".rpt")
                ),
                
                # own data, 2 MPFs
                conditionalPanel(condition = "input.sample_data == 0 && input.no_mpf == 2",
                                 fileInput(inputId = "file_2", label = "2nd MPF:", accept = ".rpt")
                ),

                
                # sample data, 1 MPF
                conditionalPanel(condition = "input.sample_data == 1 && input.no_mpf == 1",
                                 htmlOutput(outputId = "path_s")
                ),
                
                # sample data, 2 MPFs
                conditionalPanel(condition = "input.sample_data == 1 && input.no_mpf == 2",
                                 htmlOutput(outputId = "path_d")
                )
            ),
            
            wellPanel(
                h4("Graph settings"),
                selectInput(inputId = "selected_var", label = "Choose variable:", 
                            choices = c("AGE_AT_ENTRY", "SUM_ASSURED")),
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
                                     htmlOutput(outputId = "top_text_s") # single
                    ),
                    
                    conditionalPanel(condition = "input.no_mpf == 2",
                                     htmlOutput(outputId = "top_text_d") # double
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
  
    ## left panel
    output$path_s <- renderUI(
        HTML("<b>1st MPF:</b> C:/Actuarial/YE2018/MPF/IPROD1.rpt")
    )

    output$path_d <- renderUI(
        HTML("<b>1st MPF:</b> C:/Actuarial/YE2018/MPF/IPROD1.rpt<br>
              <b>2nd MPF:</b> C:/Actuarial/YE2017/MPF/IPROD1.rpt")
    )
    
    # show/hide Data_2 tab
    observe({
      if(input$no_mpf == 1) {
        hideTab(inputId = "tabs", target = "Data_2")
      }
      if(input$no_mpf == 2) {
        showTab(inputId = "tabs", target = "Data_2")
      }
    })
    
    # load data
    data_mpf <- reactive({
        # sample data, 1 MPF
        if (input$no_mpf == 1 && input$sample_data == 1) {
            data <- subset(demo_data, MPF == "1st_MPF")
            data
        # sample data, 2 MPFs
        } else if (input$no_mpf == 2 && input$sample_data == 1) {
            data <- demo_data
            data
        # own data, 1 MPF
        } else if (input$no_mpf == 1 && input$sample_data == 0) {
            inFile <- input$file_1
            if (is.null(inFile))
              return(NULL)
            data <- read_mpf(inFile$datapath)
            data$MPF <- rep("1st_MPF", dim(data)[1])
            data
        # own data, 2 MPFs
        } else if (input$no_mpf == 2 && input$sample_data == 0) {
            inFile1 <- input$file_1
            inFile2 <- input$file_2
            if (is.null(inFile1) || is.null(inFile2))
              return(NULL)
            
            data_1 <- read_mpf(inFile1$datapath)
            data_2 <- read_mpf(inFile2$datapath)
  
            # set indicators for two datasets
            data_1$MPF <- rep("1st_MPF", dim(data_1)[1])
            data_2$MPF <- rep("2nd_MPF", dim(data_2)[1])
            
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
    
    ## information about number of records
    output$top_text_s <- renderUI(
      if (is.null(data_mpf())) {}
      else {
        HTML("MPF contains ", nrow(data_mpf()), " records.")
      }
    )
    
    output$top_text_d <- renderUI(
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
