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



# read indicator tables..


c1_flag <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c1_flag.csv")
setDT(c1_flag) # make sure that coronaD is a data table rather than a data frame



# 
# library(readxl)
# xlfile = "OxCGRT_timeseries_all.xlsx"
# sheets = excel_sheets(xlfile)
# 
# C1Flag = read_excel(xlfile, sheet = sheets[6]) # c1_flag
# setDT( C1Flag )




Dates_C1Flag = colnames(C1Flag[,!1:2])


MeltedC1Flag = melt(C1Flag, id.vars = 1:2,
                    measure.vars = Dates_C1Flag,
                    variable.name = "Date_C1Flag",
                    value.name = "C1effective")


View (MeltedC1Flag)




# eigener code:
coronaDT = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
setDT(coronaDT) # make sure that coronaD is a data table rather than a data frame
Dates = colnames(coronaDT[,!1:4])

# EU27
EU27 = c("Austria", "Belgium", "Bulgaria", "Croatia", 
         "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Germany", "Greece", 
         "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", 
         "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", 
         "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
EU27DT = coronaDT[`Country/Region` %in% EU27][is.na(`Province/State`)]

# coronaDT = rbind(EU27DT,cbind(data.table(`Country/Region`='EU27'), t(colSums(EU27DT[,!1:4]))),fill=TRUE)


# MELT DOES: TRANSFORM FROM WIDE INTO A LONG TABLE 

MeltedCoronaDT = melt(coronaDT, id.vars = 1:4,
                      measure.vars = Dates,
                      variable.name = "Date",
                      value.name = "Dead")

MeltedCoronaDT

MeltedCoronaDT$Date = MeltedCoronaDT$Date %>%
    as.Date(format = "%m/%d/%y")

MeltedCoronaDT

## steps to a plottable table with death per capita "PlotPerCapita_100k"

MeltedCoronaDT_OnlyCountries = MeltedCoronaDT[is.na(`Province/State`)]

# total population data: 

totPopRaw <- read_csv("API_SP.POP.TOTL_DS2_en_excel2csv_v2_2917451.csv")
setDT(totPopRaw)

totPopClean = totPopRaw[4:269,!2:64]

# rename column ...65:
setnames(totPopClean, "...65", "Population in 2020")
totPopClean
# add EU27 population:

DP_LIVE <- read_csv("DP_LIVE_01102021210956871.csv")
setDT(DP_LIVE)

Pop_EU27 = (DP_LIVE[ LOCATION == "EU27" & SUBJECT == "TOT" & MEASURE == "MLN_PER" & TIME == 2020, list (LOCATION, Value) ])

setnames( Pop_EU27, "LOCATION", "Data Source")
setnames( Pop_EU27, "Value", "Population in 2020" )

totPopClean = rbind( totPopClean, Pop_EU27 ) 
totPopClean[ `Data Source` == "EU27", `Population in 2020` := totPopClean[ `Data Source` == "EU27", `Population in 2020` ] * 1000000 ] 


# calculate death per capita (100k inhabitants):
# inner join:
PlotDT = merge(MeltedCoronaDT_OnlyCountries, totPopClean, by.x = 'Country/Region', by.y = 'Data Source', all = FALSE)

# calculation:
PlotDT$deathPer100k <- (PlotDT$Dead / PlotDT$`Population in 2020`)* 100000
View(PlotDT)

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
    
    
    # superhero theme & tabsets:
    navbarPage("Superdashboard",
               theme = shinythemes::shinytheme("superhero"),
               tabPanel("Home"),
               tabPanel("Home2"),
               tabPanel("Home3")
    ),
    
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
            textOutput("out"),
            
            # Date range input
            dateRangeInput(inputId = "date", label = "Date range",
                           start = min(PlotDT$Date),
                           end   = max(PlotDT$Date)),
            
            #Download option input
            downloadButton(outputId = "download_data", label = "Download"),
            
            
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
        
        plotCoronaDeaths ( input$countries, PlotDT ) # input$scale?
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

#Wiam testing 
#Taha Testing 

