### Todo:
# - populate data tabs
# - allow own files
# - rsconnect!

# libraries
library(tibble)
library(ggplot2)
library(shiny)
library(readxl)
library(dplyr)
library(DT)

# settings
options(scipen = 999) # disable scientific notation
options(shiny.reactlog = TRUE)
set.seed(123)

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

rbimodal <- function (n, cpct, mu1, mu2, sig1, sig2) {
    y0 <- rlnorm(n,mean=mu1, sd = sig1)
    y1 <- rlnorm(n,mean=mu2, sd = sig2)
    
    flag <- rbinom(n,size=1,prob=cpct)
    y <- y0*(1 - flag) + y1*flag 
}

# create demo data
n1 = 12000
n2 = 11500
n3 = 5000
n4 = 6000

demo_data <- as.data.frame(matrix(nrow = (n1 + n2 + n3 + n4), ncol = 4))
colnames(demo_data) <- c("Year", "Product", "AGE_AT_ENTRY", "SUM_ASSURED")

demo_data$Year <- c(rep("2018", n1), rep("2017", n2), rep("2018", n3), rep("2017", n4))

demo_data$Product <- c(rep("IPROD1", n1), rep("IPROD1", n2), rep("IPROD2", n3), rep("IPROD2", n4))

demo_data$AGE_AT_ENTRY <- as.integer(
                          c(round(rnorm(n1-n2, mean = 40.5, sd = 5), 0),  round(rnorm(n2, mean = 40, sd = 5), 0), # 2018 IPROD1
                            round(rnorm(n2, mean = 40, sd = 5), 0), # 2017 IPROD1
                            round(rbimodal(n3, 0.2, log(30), log(45), log(1.1), log(1.1)), 0),  # 2018 IPROD2
                            round(rbimodal(n4, 0.2, log(30), log(45), log(1.1), log(1.1)), 0))) # 2017 IPROD2

demo_data$SUM_ASSURED  <- c(round(rnorm(n1, mean = 31, sd = 5), digits = 1)*10^3, # 2018 IPROD1
                            round(rnorm(n2, mean = 29, sd = 5), digits = 1)*10^3, # 2017 IPROD1
                            round(rlnorm(n3, meanlog = log(100), sdlog = log(1.2)), digits = 1)*10^2, # 2018 IPROD2
                            round(rlnorm(n4, meanlog = log(100), sdlog = log(1.2)), digits = 1)*10^2) # 2017 IPROD2

### app
# ui
ui <- fluidPage(
    
    titlePanel("Model Point Files visualizer", windowTitle = "MPF visualizer"),
    
    sidebarLayout(
        sidebarPanel(
            wellPanel(
                h4("Load data"),
                radioButtons(inputId = "no_mpf", label = "Input type:", choices = c("Show 1 MPF" = 1, "Compare 2 MPFs" = 2), 
                             selected = 1, inline = TRUE),
                conditionalPanel(condition = "input.no_mpf == 1",
                                 htmlOutput(outputId = "path_s")
                ),
                conditionalPanel(condition = "input.no_mpf == 2",
                                 htmlOutput(outputId = "path_d")
                )
            ),
            
            wellPanel(
                h4("Graph settings"),
                selectInput(inputId = "selected_var", label = "Choose variable:", 
                            choices = c("AGE_AT_ENTRY", "SUM_ASSURED")),
                radioButtons(inputId = "hist_type", label = "Histogram type:", choices = c("frequency" = 1, "density" = 2), 
                             selected = 1, inline = TRUE)
            ),            
            
            wellPanel(
                selectInput(inputId = "selected_product", label = "Choose product:",  
                            choices = c("IPROD1", "IPROD2"))
            )
            
        ),
        mainPanel(
            tabsetPanel(id = "tabs",
                tabPanel(title = "Plot", br(), 
                    
                    plotOutput(outputId = "plot"),
                    
                    hr(),
                    
                    conditionalPanel(condition = "input.no_mpf == 1",
                        htmlOutput(outputId = "summ_text_s"),
                        verbatimTextOutput(outputId = "summ_s")
                    ),
                    
                    conditionalPanel(condition = "input.no_mpf == 2",
                        htmlOutput(outputId = "summ_text_d_1"),
                        verbatimTextOutput(outputId = "summ_d_1"),
                        htmlOutput(outputId = "summ_text_d_2"),
                        verbatimTextOutput(outputId = "summ_d_2")
                    ),
                    
                    hr(),
                    
                    conditionalPanel(condition = "input.no_mpf == 1",
                                     htmlOutput(outputId = "top_text_s") # single
                    ),
                    
                    conditionalPanel(condition = "input.no_mpf == 2",
                                     htmlOutput(outputId = "top_text_d") # double
                    )
                ),
                
                tabPanel(title = "Data"),
                tabPanel(title = "Data_1"),
                tabPanel(title = "Data_2")
            )
        )
    )
)

# server
server <- function(input, output, session) {
  
    ## left panel
    output$path_s <- renderUI(
        HTML("<b>MPF path:</b> C:/Actuarial_Department/Model_YE2018/MPF/IPROD1.rpt")
    )

    output$path_d <- renderUI(
        HTML("<b>1st MPF path:</b> C:/Actuarial_Department/Model_YE2018/MPF/IPROD1.rpt<br>
              <b>2nd MPF path:</b> C:/Actuarial_Department/Model_YE2017/MPF/IPROD1.rpt")
    )
    
    #
    observe({
      if(input$no_mpf == 1) {
        showTab(inputId = "tabs", target = "Data")
        hideTab(inputId = "tabs", target = "Data_1")
        hideTab(inputId = "tabs", target = "Data_2")
      }
      if(input$no_mpf == 2) {
        hideTab(inputId = "tabs", target = "Data")
        showTab(inputId = "tabs", target = "Data_1")
        showTab(inputId = "tabs", target = "Data_2")
      }
    })
    
    # load data
    data_mpf <- reactive({
        if (input$no_mpf == 1) {
            req(input$selected_product)
            data <- subset(demo_data, Product == input$selected_product & Year == "2018")
            data
        } else if (input$no_mpf == 2) {
            req(input$selected_product)
            data <- subset(demo_data, Product == input$selected_product)
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
    # information about number of records
    output$top_text_s <- renderUI(
        HTML("<br>MPF contains ", nrow(data_mpf()), " records.")
    )
    
    output$top_text_d <- renderUI(
        HTML("<br>1st MPF contains ", nrow(subset(data_mpf(), Year == "2018")), " records.",
             "<br>2nd MPF contains ", nrow(subset(data_mpf(), Year == "2017")), " records.")
    )
    
    # main plot
    output$plot <- renderPlot({
        if (input$no_mpf == 1 & input$hist_type == 1) {  
            ggplot(data = data_mpf(), aes(x = data_mpf()[, input$selected_var], y = ..count..)) +
              geom_histogram(binwidth = binsize(), fill = "white", colour = "black") +
              xlab(input$selected_var) +
              scale_x_continuous(breaks = breaks_range(), labels=function(x) format(round(x, 0), big.mark = " ", scientific = FALSE))
        } else if (input$no_mpf == 1 & input$hist_type == 2) {
            ggplot(data = data_mpf(), aes(x = data_mpf()[, input$selected_var], y = ..density..)) +
              geom_histogram(binwidth = binsize(), fill = "white", colour = "black") +
              xlab(input$selected_var) +
              scale_x_continuous(breaks = breaks_range(), labels=function(x) format(round(x, 0), big.mark = " ", scientific = FALSE))
        } else if (input$no_mpf == 2 & input$hist_type == 1) {
            ggplot(data = data_mpf(), aes(x = data_mpf()[, input$selected_var], y = ..count.., fill = Year)) +
              geom_histogram(position = "identity", binwidth = binsize(), colour = "black", alpha = 0.4) +
              xlab(input$selected_var) +
              scale_x_continuous(breaks = breaks_range(), labels=function(x) format(round(x, 0), big.mark = " ", scientific = FALSE))
        } else if (input$no_mpf == 2 & input$hist_type == 2) {
            ggplot(data = data_mpf(), aes(x = data_mpf()[, input$selected_var], y = ..density.., fill = Year)) +
              geom_histogram(position = "identity", binwidth = binsize(), colour = "black", alpha = 0.4) +
              xlab(input$selected_var) +
              scale_x_continuous(breaks = breaks_range(), labels=function(x) format(round(x, 0), big.mark = " ", scientific = FALSE))
        }
    })
    
    # variables summary 
    output$summ_text_s <- renderUI(
        HTML("<p>Summary of ", input$selected_var, ":</p>")    
    )
    
    output$summ_s <- renderPrint(
        summary(data_mpf()[, input$selected_var])
    )
    
    output$summ_text_d_1 <- renderUI(
        HTML("<p>Summary of ", input$selected_var, ":</p>
              <p>1st MPF:")    
    )
    
    output$summ_d_1 <- renderPrint(
        summary(subset(data_mpf(), Year == 2018)[, input$selected_var])
    )
    
    output$summ_text_d_2 <- renderUI(
        HTML("<p>2nd MPF:</p>")    
    )
    
    output$summ_d_2 <- renderPrint(
        summary(subset(data_mpf(), Year = 2017)[, input$selected_var])
    )
}

shinyApp(ui = ui, server = server)