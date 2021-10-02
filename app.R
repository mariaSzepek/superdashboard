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

library(dplyr)
library(magrittr)
library(scales)



# read indicator tables:

c1_flag <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c1_flag.csv")


# setDT(c1_flag) # make sure that coronaD is a data table rather than a data frame
setDT(c1_flag) 

# ueberrschriften von den spalten 
cols <- names(c1_flag)[4:length(colnames(c1_flag))] # cols <- names(c1_flag)[4:642]
# spalten von der tabelle in ordentliches format umbenennen
setnames(c1_flag, cols, format(as.Date(cols, '%d%B%Y'), "%Y-%m-%d"))
c1_flag

# neu formatierte spaltenueberschriften als datum benutzen
dates_c1Flag = names(c1_flag)[4:length(colnames(c1_flag))]
str(dates_c1Flag)


MeltedC1Flag = melt(c1_flag, id.vars = 2:3,
                    measure.vars = dates_c1Flag,
                    variable.name = "Date_FirstStartC1",
                    value.name = "C1effective")

#str(MeltedC1Flag$Date_FirstStartC1)

setnames(MeltedC1Flag, "country_name", "Country/Region")

MeltedC1Flag

# find start of c1 measure per country

# 


first_c1_case = MeltedC1Flag %>%
    # Group by because we want to operate by level of variable
    dplyr::group_by(`Country/Region`)%>%
    # Remove all rows of variable if they are 0 or NA aren't any rows with Deaths==0
    dplyr::filter( C1effective != 0 & !is.na(C1effective) ) %>% 
    # Keep the first row of each variable, after sorting by Date
    # This gives us the first non-zero row
    dplyr::arrange(Date_FirstStartC1) %>%
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    #Get first c1 day by country = 1st imposing of c1 
    dplyr::select(`Country/Region`, `Date_FirstStartC1`,`C1effective`)

# View(first_c1_case)



# read corona data set:

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



coronaDT = rbind(coronaDT, cbind(data.table(`Province/State`=NA,`Country/Region`='EU27',Lat=0,Long=0), t(colSums(EU27DT[,!1:4]))),fill=TRUE)
# coronaDT = rbind(EU27DT,cbind(data.table(`Country/Region`='EU27'), t(colSums(EU27DT[,!1:4]))),fill=TRUE)


# MELT DOES: TRANSFORM FROM WIDE INTO A LONG TABLE 

MeltedCoronaDT = melt(coronaDT, id.vars = 1:4,
                      measure.vars = Dates,
                      variable.name = "Date",
                      value.name = "Dead")

MeltedCoronaDT$Date = MeltedCoronaDT$Date %>%
    as.Date(format = "%m/%d/%y")


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

PlotDT
first_c1_case
# join PlotDT and first_c1_case by left outer join

# as.Date(MeltedC1Flag$Date_FirstStartC1, format = "%m/%d/%Y")
# str(MeltedC1Flag$Date_FirstStartC1)

str(PlotDT$Date)
str(PlotDT$Date)
# first_c1_case$Date_FirstStartC1 <- as.character( first_c1_case$Date_FirstStartC1 )
first_c1_case$Date_FirstStartC1 <- as.Date( first_c1_case$Date_FirstStartC1 )
first_c1_case$Date_FirstStartC1

PlotDT = merge( PlotDT, first_c1_case, by.x = c( "Country/Region", "Date" ), by.y = c( "Country/Region", "Date_FirstStartC1" ) , all.x=TRUE)


# View(PlotDT)




# outsource plot function


plotCoronaDeaths = function ( listOfCountries, dataInput, columnName, scale = "linear", y_label) {    
    
    #png(filename="plot.png", width=600, height=600)
    
    p = ggplot(data = dataInput [ is.na(`Province/State`) ],
               aes_string(x = "Date", y = columnName, group = "`Country/Region`")) +
        geom_line(color="grey") + # all the lines
        geom_line(data = dataInput [ `Country/Region` %in% (listOfCountries) & is.na(`Province/State`) ],
                  aes_string(color="`Country/Region`"), size = 2) + # selected countries only
        ylab(paste(y_label, " (absolute)"))
    
    if ( (dataInput[ `Country/Region` %in% (listOfCountries) & is.na(`Province/State`) & C1effective == 1, .N ]) > 0 ) { 
        
        p = p + geom_point(data= dataInput[ `Country/Region` %in% (listOfCountries) & is.na(`Province/State`) & C1effective == 1 ],
                           aes_string(x="Date", y = columnName, color = "\"C1Effective\""), color = "red", size=3)
        # + scale_color_manual(name="Effectiveness", labels=c("C1effective", "C6Effective"), values=("blue", "brown"))                    
    }
    
    if (scale == "logarithmic") {
        p = p +
            scale_y_log10(name = paste(y_label, " (logarithmic)"),
                          limits = c(1, NA))
        
    }
    
    p
    
    # ggplot( data = dataInput[`Country/Region` %in% (listOfCountries) & is.na(`Province/State`)],
    #         aes (x = Date, y= Dead, group= `Country/Region`)) + 
    #     geom_line() +   
    #     geom_line(aes(colour = `Country/Region`))
    
    # # highlight days of imposing c1 for the first time
    # dataInput %>% 
    #     ggplot(aes(x=Date,y=Dead)) + 
    #     geom_point(alpha=0.3) +
    #     geom_point(data=dataInput[ C1effective == 1 & ( `Country/Region` %in% (listOfCountries) & is.na(`Province/State`) ) ], 
    #                aes(x=Date,y=Dead), 
    #                color='red',
    #                size=3)
    
    
    # dataInput [ `Country/Region` %in% (listOfCountries) & is.na(`Province/State`)  ] %>% 
    #     ggplot(aes(x=Date,y=Dead)) + 
    #     geom_point(alpha=1.0, size=0.0) +
    #     geom_point(data= dataInput[ `Country/Region` %in% (listOfCountries) & is.na(`Province/State`) & C1effective == 1 ],
    #                aes(x=Date, y=Dead, color=C1effective),size=3)
    
    
    
}

# plotCoronaDeathsPer100k = function (listOfCountries,  dataInput ) {
#     
#     ggplot( data = dataInput[`Country/Region` %in% (listOfCountries) & is.na(`Province/State`)],
#             aes (x = Date, y= deathPer100k, group= `Country/Region`)) + 
#         geom_line() +   
#         geom_line(aes(colour = `Country/Region`))
#     
#     
#     
# }

# inutil : syntaxe

# plotCoronaDeaths = function( listOfCountries, dataInput, columnName =  "Dead") { # scale, minDate, maxDate, , columnName ?
#     
#     ggplot( data = dataInput[`Country/Region` %in% (listOfCountries) & is.na(`Province/State`)],
#             aes (x = Date, y= columnName, group= `Country/Region`)) + 
#         geom_line() +   
#         geom_line(aes(colour = `Country/Region`))
#     
# }

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    
    # superhero theme & tabsets:
    navbarPage("Superdashboard",
               theme = shinythemes::shinytheme("superhero"),
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
            
            
            # Scale input
            radioButtons(
                inputId = "scale", 
                label = "Scale:",
                choices = c("linear","logarithmic"),
                selected = "linear"
            ),
            
            
            
            
            #Download option input
            downloadButton(outputId = "download_data", label = "Download"),
            
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            # tabsets
            tabsetPanel(
                tabPanel("Plot AbsoluteDeaths", plotOutput("plot_absoluteDeaths")), 
                tabPanel("Plot Deaths per 100k ", plotOutput("plot_deathsPer100k")), 
                tabPanel("Table", textOutput("nothing"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$myScale = renderText(
        print(input$scale)
    )
    output$nothing = renderText(
        print("noting at all!!! bruder!")
    )
    
    
    
   
    
    
    
    output$plot_absoluteDeaths <- renderPlot({
        
        plotCoronaDeaths ( input$countries,  subset(PlotDT, Date >= input$date[1] & Date <= input$date[2]), "Dead", input$scale, "Deaths")# input$scale? column nane?
        
    }, width = "auto",
    height = 600)
    
    output$plot_deathsPer100k <- renderPlot({
        
        plotCoronaDeaths ( input$countries ,subset(PlotDT, Date >= input$date[1] & Date <= input$date[2]), "deathPer100k", input$scale, "Deaths per 100k population") # input$scale?
        
    })
    
    # plotCoronaDeaths ( input$countries, PlotDT, "deathPer100k" )
    
    
    #Define server logic to download a csv file containing only the selected data
    output$download_data <- downloadHandler(
        filename = "download_data.csv",
        content = function(file) {
            data <- subset(PlotDT, `Country/Region` %in% input$countries & Date >= input$date[1] & Date <= input$date[2])
            write.csv(data, file, row.names = FALSE)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

#Wiam testing 
#Taha Testing 

