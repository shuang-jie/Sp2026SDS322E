library(shiny)
library(tidyverse)

# Load data
dat <- read_csv("https://github.com/rdpeng/stat322E_public/raw/main/data/pm25_data.csv.gz")

# Keep only numeric variables for plotting
num_vars <- names(dat %>% select(where(is.numeric)))

# ---------------- UI ----------------
ui <- fluidPage(
    
    titlePanel("Dataset Scatterplot Explorer"),
    
    sidebarLayout(
        sidebarPanel(
            
            h4("Variable Selection"),
            
            selectInput("var1", "X variable:",
                        choices = num_vars,
                        selected = num_vars[1]),
            
            selectInput("var2", "Y variable:",
                        choices = num_vars,
                        selected = num_vars[2]),
            
            hr(),
            
            h4("Filter"),
            
            # Dynamic slider (FIXED)
            uiOutput("xrange_ui"),
            
            hr(),
            
            h4("Plot Options"),
            
            checkboxInput("smooth", "Add smooth curve", TRUE),
            
            sliderInput("alpha", "Transparency:",
                        min = 0.1, max = 1, value = 0.6),
            
            sliderInput("size", "Point size:",
                        min = 1, max = 5, value = 2)
        ),
        
        mainPanel(
            plotOutput("scatterPlot"),
            br(),
            verbatimTextOutput("summary")
        )
    )
)

# ---------------- Server ----------------
server <- function(input, output, session) {
    
    # Dynamic slider based on selected X variable
    output$xrange_ui <- renderUI({
        req(input$var1)
        
        sliderInput("xrange",
                    "Filter X range:",
                    min = min(dat[[input$var1]], na.rm = TRUE),
                    max = max(dat[[input$var1]], na.rm = TRUE),
                    value = range(dat[[input$var1]], na.rm = TRUE))
    })
    
    # Reactive filtered data
    filtered_data <- reactive({
        req(input$var1, input$var2, input$xrange)
        
        df <- dat %>%
            filter(.data[[input$var1]] >= input$xrange[1],
                   .data[[input$var1]] <= input$xrange[2])
        
        df
    })
    
    # Plot
    output$scatterPlot <- renderPlot({
        
        df <- filtered_data()
        
        # Handle empty case
        if (nrow(df) == 0) {
            plot.new()
            text(0.5, 0.5, "No data in selected range")
            return()
        }
        
        p <- ggplot(df, aes(x = .data[[input$var1]],
                            y = .data[[input$var2]])) +
            geom_point(alpha = input$alpha,
                       size = input$size) +
            theme_bw() +
            labs(
                title = paste("Scatterplot of", input$var1, "vs", input$var2),
                x = input$var1,
                y = input$var2
            )
        
        if (input$smooth) {
            p <- p + geom_smooth(method = "loess", se = TRUE, color = "blue")
        }
        
        p
    })
    
    # Summary output
    output$summary <- renderPrint({
        summary(filtered_data())
    })
}

# Run app
shinyApp(ui = ui, server = server)

