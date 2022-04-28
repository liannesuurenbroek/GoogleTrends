library(shiny)
library(tidyverse)
library(gtrendsR)
# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Google Trends"),
    sidebarLayout(
        sidebarPanel(
            helpText("Choose a term for which you want to know the trends on google"),
            textInput("text", h3("Search term"), 
                      value = "R"),
            actionButton("search", "Search")
        ),
        mainPanel(
            textOutput("selected_term"),
            plotOutput("selected_plot")
        )
    )
)

geenmineen <- function(plottie){
    if(sum(plottie$hits == "<1")>0){
        plottie[plottie$hits == "<1",]$hits <- "0"
        return(plottie)}
    else{return(plottie)}
}

geennull <- function(zoekterm){
    if(typeof(gtrends(zoekterm)$interest_over_time)=="list"){
        paste("Hits for the search term", zoekterm)
    }
    else{
        paste("This search term could not be found")
    }
}

geennullplot <- function(plottie){
    if(typeof(plottie)=="list"){
        return(plottie)
    }
    else{
        template=gtrends("R")$interest_over_time
        template$hits=0
        return(template)
    }
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    tekstje <- eventReactive(input$search, {geennull(input$text)})
    output$selected_term <- renderText({
        tekstje()
    })
    plotje <- eventReactive(input$search,{geenmineen(geennullplot(gtrends(input$text)$interest_over_time))})
    output$selected_plot <- renderPlot({
        ggplot(plotje(), aes(x=date, y=as.numeric(hits))) +
            geom_line() +
            geom_smooth(span=.3) +
            scale_x_datetime(name = "Date") +
            coord_cartesian(ylim = c(0, 100)) +
            scale_y_continuous(name = "Relative popularity")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
