#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(magrittr)
library(data.table)
library(readr)



# eigener code:
coronaDT = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
setDT(coronaDT) # make sure that coronaD is a data table rather than a data frame
Dates = colnames(coronaDT[,!1:4])

# MELT DOES: TRANSFORM FROM WIDE INTO A LONG TABLE 

PlotDT = melt(coronaDT, id.vars = 1:4,
              measure.vars = Dates,
              variable.name = "Date",
              value.name = "Dead")

PlotDT$Date = PlotDT$Date %>%
    as.Date(format = "%m/%d/%y")

## steps to a plottable table with death per capita "zusammen"

PlotDTClean = PlotDT[is.na(`Province/State`)]


# total population data: 

totPop <- read_csv("API_SP.POP.TOTL_DS2_en_excel2csv_v2_2917451.csv")


# totPop <- read_csv("SupergroupShinyCoronaApp/API_SP.POP.TOTL_DS2_en_excel2csv_v2_2917451.csv")

#totPop <- read_csv("Mastermodule/TBS/Programming/R/Group project/API_SP.POP.TOTL_DS2_en_excel2csv_v2_2917451.csv")

setDT(totPop)

totPop1 = totPop[4:269,!2:64]
# View(totPop1)

# how to change column names?


# calculate death per capita (100k inhabitants):

# inner join:
zusammen = merge(PlotDTClean, totPop1, by.x = 'Country/Region', by.y = 'Data Source', all = FALSE)
zusammen



zusammen$deathPer100k <- (zusammen$Dead / zusammen$...65 )* 100000
zusammen

setnames(zusammen, "...65", "Population")

zusammen
    









# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # select countries input: 
            selectizeInput("selectcountries", "Select countries", unique(PlotDT$`Country/Region`),
                           multiple = TRUE, options = list(
                               'plugins' = list('remove_button'),
                               'create' = TRUE,
                               'persist' = FALSE,
                               placeholder = 'Select countries'
                               )
            ),
            
            # dont know what it does 
            textOutput("out")
            
            
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("normalXYGraph"),
            plotOutput("somethingelse")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # output$normalXYGraph <- renderPlot({
    #     ggplot(data = PlotDT[`Country/Region` == input$country & is.na(`Province/State`)], aes (x = Date, y= Dead) ) + geom_line() +   geom_line(aes(colour = `Country/Region`))
    #     # generate bins based on input$bins from ui.R
    #     
    # })
    
    output$normalXYGraph <- renderPlot({
        ggplot( data = PlotDT[`Country/Region` %in% (input$countries) & is.na(`Province/State`)],
                aes (x = Date, y= Dead, group= `Country/Region`)) + 
            geom_line() +   
            geom_line(aes(colour = `Country/Region`))
        
    })
    
    output$somethingelse <- renderPlot({
        ggplot( data = PlotDT[`Country/Region` %in% (input$countries) & is.na(`Province/State`)],
                aes (x = Date, y= Dead, group= `Country/Region`)) + 
            geom_line() +   
            geom_line(aes(colour = `Country/Region`))
        
    })
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
