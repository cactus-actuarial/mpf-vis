library(tibble)
library(ggplot2)
library(shiny)
library(readxl)
library(dplyr)
library(DT)

# settings
options(scipen = 999) # disable scientific notation
set.seed(123)

# create sample data
demo_data <- as.data.frame(matrix(nrow = 2*10^5, ncol = 3))
colnames(demo_data) <- c("Product", "AGE_AT_ENTRY", "SUM_ASSURED")
demo_data$Product <- c(rep("IPROD1", 10^5), rep("IPROD2", 10^5))
demo_data$AGE_AT_ENTRY <- c(round(rnorm(10^5, mean = 45, sd = 5), 0), round(rnorm(10^5, mean = 35, sd = 3), 0))
demo_data$SUM_ASSURED <- c(round(rnorm(10^5, mean = 5*10^2, sd = 10), 0)*10^3, round(rnorm(10^5, mean = 10, sd = 2), 0)*10^3)

data <- subset(demo_data, Product == "IPROD1")
head(data)
data[, "AGE_AT_ENTRY"]

# functions
read_mpf <- function(path, filename) {
    file <- paste(path, "\\", filename, ".rpt", sep = "")
    data <- read.table(file = file, sep = ",", skip = 1, header = TRUE)
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
    
    titlePanel("Model Point Files visualizer", windowTitle = "MPF visualizer"),

    sidebarLayout(
        sidebarPanel(
            wellPanel(
                radioButtons(inputId = "no_mpf", label = "Input type:", choices = c("Show 1 MPF" = 1, "Compare 2 MPFs" = 2), 
                             selected = 1, inline = TRUE),
                radioButtons(inputId = "hist_type", label = "Histogram type:", choices = c("frequency" = 1, "density" = 2), 
                             selected = 1, inline = TRUE)
            ),
            
            wellPanel(
                conditionalPanel(condition = "input.no_mpf == 1",
                    htmlOutput(outputId = "path")
                ),
                
                conditionalPanel(condition = "input.no_mpf == 2",
                    htmlOutput(outputId = "path_1_2")
                )
            ),            
            
            wellPanel(
                selectInput(inputId = "selected_product", label = "Choose product:",  
                            choices = c("IPROD1", "IPROD2")),
                selectInput(inputId = "selected_var",     label = "Choose variable:", 
                            choices = c("AGE_AT_ENTRY", "SUM_ASSURED"))
            )
            
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(title = "Plot", br(), 
                    
                    conditionalPanel(condition = "input.no_mpf == 1",
                        htmlOutput(outputId = "top_text_1")
                    ),
                    
                    conditionalPanel(condition = "input.no_mpf == 2",
                        htmlOutput(outputId = "top_text_2")
                    ),

                    hr(),
                    
                    conditionalPanel(condition = "input.no_mpf == 1 && input.hist_type == 1",
                        plotOutput(outputId = "plot_1_f")
                    ),
                    
                    conditionalPanel(condition = "input.no_mpf == 1 && input.hist_type == 2",
                        plotOutput(outputId = "plot_1_d")
                    ),
                    
                    conditionalPanel(condition = "input.no_mpf == 2 && input.hist_type == 1",
                        plotOutput(outputId = "plot_2_f")
                    ),
                    
                    conditionalPanel(condition = "input.no_mpf == 2 && input.hist_type == 2",
                        plotOutput(outputId = "plot_2_d")
                    ),
                    
                    hr(),
                    
                    conditionalPanel(condition = "input.no_mpf == 1",
                        verbatimTextOutput(outputId = "summ_text")
                    )
                    
                ),
                
                tabPanel(title = "Data",
                         br(),
                         DTOutput(outputId = "data")
                )
            )
        )
    )
)

# server
server <- function(input, output, session) {

    # load data
    data_mpf <- reactive({
        if (input$no_mpf == 1) {
            req(input$selected_product)
            sel_prod <- input$selected_product
            data <- subset(demo_data, Product == sel_prod)
            data
        } else if (input$no_mpf == 2) {
            data <- demo_data
            data
        }
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
    
    
    ## tab Plot
    # main plot
    output$plot_1_f <- renderPlot(
        ggplot(data = data_mpf(), aes(x = data_mpf()[, input$selected_var], y = ..count..)) +
            geom_histogram(binwidth = binsize(), fill = "white", colour = "black") +
            xlab(input$selected_var) +
            scale_x_continuous(breaks = breaks_range(), labels=function(x) format(round(x, 0), big.mark = " ", scientific = FALSE))
    )
    
    output$plot_1_d <- renderPlot(
        ggplot(data = data_mpf(), aes(x = data_mpf()[, input$selected_var], y = ..density..)) +
            geom_histogram(binwidth = binsize(), fill = "white", colour = "black") +
            xlab(input$selected_var) +
            scale_x_continuous(breaks = breaks_range(), labels=function(x) format(round(x, 0), big.mark = " ", scientific = FALSE))
    )
    
    output$plot_2_f <- renderPlot(
        ggplot(data = data_mpf(), aes(x = data_mpf()[, input$selected_var], y = ..count.., fill = ind)) +
            geom_histogram(position = "identity", binwidth = binsize(), colour = "black", alpha = 0.4) +
            xlab(input$selected_var) +
            scale_x_continuous(breaks = breaks_range(), labels=function(x) format(round(x, 0), big.mark = " ", scientific = FALSE))
    )
    
    output$plot_2_d <- renderPlot(
        ggplot(data = data_mpf(), aes(x = data_mpf()[, input$selected_var], y = ..density.., fill = ind)) +
            geom_histogram(position = "identity", binwidth = binsize(), colour = "black", alpha = 0.4) +
            xlab(input$selected_var) +
            scale_x_continuous(breaks = breaks_range(), labels=function(x) format(round(x, 0), big.mark = " ", scientific = FALSE))
    )
    
    output$top_text_1 <- renderUI(
      #HTML(paste0(description()), 
      HTML(paste0(         
             "<br>MPF contains ", nrow(data_mpf()), " records."))
    )
    
    output$top_text_2 <- renderUI(
        #HTML(paste0(description()),
        HTML(paste0( 
             "<br>1st MPF contains ", nrow(data_mpf()), " records.",
             "<br>2nd MPF contains ", nrow(data_mpf()), " records."))
    )
    
    output$summ_text <- renderPrint(
        summary(data_mpf()[, input$selected_var])
    )

    
    ## tab Data
    output$data <- DT::renderDT({data_mpf()})
}

shinyApp(ui = ui, server = server)