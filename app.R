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

## steps to a plottable table with death per capita "PlotPerCapita_100k"

PlotDT_OnlyCountries = PlotDT[is.na(`Province/State`)]

# total population data: 

totPopRaw <- read_csv("API_SP.POP.TOTL_DS2_en_excel2csv_v2_2917451.csv")
setDT(totPopRaw)

totPopClean = totPopRaw[4:269,!2:64]

# calculate death per capita (100k inhabitants):
# inner join:
PlotPerCapitaDT_100k = merge(PlotDT_OnlyCountries, totPopClean, by.x = 'Country/Region', by.y = 'Data Source', all = FALSE)

# calculation:
PlotPerCapitaDT_100k$deathPer100k <- (PlotPerCapitaDT_100k$Dead / PlotPerCapitaDT_100k$...65 )* 100000

# rename column ...65:
setnames(PlotPerCapitaDT_100k, "...65", "Population in 2020")



# outsource plot function

plotCoronaDeaths = function( listOfCountries, dataInput) { # scale, minDate, maxDate, , columnName ?
    
    ggplot( data = dataInput[`Country/Region` %in% (listOfCountries) & is.na(`Province/State`)],
            aes (x = Date, y= Dead, group= `Country/Region`)) + 
        geom_line() +   
        geom_line(aes(colour = `Country/Region`))
    
}





# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # select countries input: 
            selectizeInput("countries", "Select countries", unique(PlotDT$`Country/Region`),
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
            plotOutput("plot_absoluteDeaths"),
            plotOutput("plot_deathsPer100k")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot_absoluteDeaths <- renderPlot({

        plotCoronaDeaths ( input$countries, PlotDT ) # input$scale? column nane?

    })
    
    output$plot_deathsPer100k <- renderPlot({

        plotCoronaDeaths ( input$countries, PlotPerCapitaDT_100k ) # input$scale?

    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
