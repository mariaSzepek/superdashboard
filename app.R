#
#
# Group: Maria Sofie Szepek, Fathiah, Taha Moutifri, Wiam Benabderrazak
#

library(shiny)
library(ggplot2)
library(magrittr)
library(data.table)
library(readr)

library(dplyr)
library(magrittr)
library(scales)



# read national measures indicator tables:

c1_flag <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c1_flag.csv")
c6_flag <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c6_flag.csv")


# setDT to make sure it is a data table
setDT(c1_flag) 
setDT (c6_flag)

# headings from the columns
cols <- names(c1_flag)[4:length(colnames(c1_flag))]
cols <- names(c6_flag)[4:length(colnames(c6_flag))]

# rename columns from table to needed format
setnames(c1_flag, cols, format(as.Date(cols, '%d%B%Y'), "%Y-%m-%d"))
setnames(c6_flag, cols, format(as.Date(cols, '%d%B%Y'), "%Y-%m-%d"))

# used newly formatted columns headings as date
dates_c1Flag = names(c1_flag)[4:length(colnames(c1_flag))]
dates_c6Flag = names(c6_flag)[4:length(colnames(c6_flag))]
str(dates_c1Flag)

# transform wide into long data tables
MeltedC1Flag = melt(c1_flag, id.vars = 2:3,
                    measure.vars = dates_c1Flag,
                    variable.name = "Date_FirstStartC1",
                    value.name = "C1effective")

MeltedC6Flag = melt(c6_flag, id.vars = 2:3,
                    measure.vars = dates_c6Flag,
                    variable.name = "Date_FirstStartC6",
                    value.name = "C6effective")

# rename column names
setnames(MeltedC1Flag, "country_name", "Country/Region")

setnames(MeltedC6Flag, "country_name", "Country/Region")


# find start of c1 measure per country
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
    # Get first c1 day by country = 1st imposing of c1 
    dplyr::select(`Country/Region`, `Date_FirstStartC1`,`C1effective`)

# analog to find start of c1 measure 
first_c6_case = MeltedC6Flag %>%
    # Group by because we want to operate by level of variable
    dplyr::group_by(`Country/Region`)%>%
    # Remove all rows of variable if they are 0 or NA aren't any rows with Deaths==0
    dplyr::filter( C6effective != 0 & !is.na(C6effective) ) %>% 
    # Keep the first row of each variable, after sorting by Date
    # This gives us the first non-zero row
    dplyr::arrange(Date_FirstStartC6) %>%
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    # Get first day by country = 1st imposing
    dplyr::select(`Country/Region`, `Date_FirstStartC6`,`C6effective`)

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

# create EU27 data table and bind to corona data 
EU27DT = coronaDT[`Country/Region` %in% EU27][is.na(`Province/State`)]


coronaDT = rbind(coronaDT, cbind(data.table(`Province/State`=NA,`Country/Region`='EU27',Lat=0,Long=0), t(colSums(EU27DT[,!1:4]))),fill=TRUE)

# MELT coronaDT into long and set Date column as DATE format 
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

# add EU27 population data:
DP_LIVE <- read_csv("DP_LIVE_01102021210956871.csv")
setDT(DP_LIVE)

Pop_EU27 = (DP_LIVE[ LOCATION == "EU27" & SUBJECT == "TOT" & MEASURE == "MLN_PER" & TIME == 2020, list (LOCATION, Value) ])

setnames( Pop_EU27, "LOCATION", "Data Source")
setnames( Pop_EU27, "Value", "Population in 2020" )

# combine population data from two different data sets
totPopClean = rbind( totPopClean, Pop_EU27 ) 
totPopClean[ `Data Source` == "EU27", `Population in 2020` := totPopClean[ `Data Source` == "EU27", `Population in 2020` ] * 1000000 ] 


# calculate death per capita (100k inhabitants):
# inner join:
PlotDT = merge(MeltedCoronaDT_OnlyCountries, totPopClean, by.x = 'Country/Region', by.y = 'Data Source', all = FALSE)

# calculation:
PlotDT$deathPer100k <- (PlotDT$Dead / PlotDT$`Population in 2020`)* 100000




# set Date format for dates columns 
first_c1_case$Date_FirstStartC1 <- as.Date( first_c1_case$Date_FirstStartC1 )
first_c6_case$Date_FirstStartC6 <- as.Date( first_c6_case$Date_FirstStartC6 )
first_c1_case$Date_FirstStartC1

# join PlotDT and first_c1_case resp. first_c6_case by left outer join
PlotDT = merge( PlotDT, first_c1_case, by.x = c( "Country/Region", "Date" ), by.y = c( "Country/Region", "Date_FirstStartC1" ) , all.x=TRUE)
PlotDT = merge( PlotDT, first_c6_case, by.x = c( "Country/Region", "Date" ), by.y = c( "Country/Region", "Date_FirstStartC6" ) , all.x=TRUE)



# outsource plot function
plotCoronaDeaths = function ( listOfCountries, dataInput, columnName, scale = "linear", y_label) {    
    
    # plot graphs as learned during lecture, use aes_string instead of aes to enable passing of column name
    p = ggplot(data = dataInput [ is.na(`Province/State`) ],
               aes_string(x = "Date", y = columnName, group = "`Country/Region`")) + 
    # plot all the possible graphs 
        geom_line(color="grey") + # all the lines
    # highlight graphs which's countries were chosen (color by country)
        geom_line(data = dataInput [ `Country/Region` %in% (listOfCountries) & is.na(`Province/State`) ],
                  aes_string(color="`Country/Region`"), size = 2) + # selected countries only
        ylab(paste(y_label, " (absolute)"))
   
    # plot national measure's first dates per country
    # if condition catches all the cases like e.g. ! ("EU27" %in% listOfCountries)
    if ( (dataInput[ `Country/Region` %in% (listOfCountries) & is.na(`Province/State`) & C1effective == 1, .N ]) > 0  ) { 
        
        p = p +  geom_point( data= dataInput[ `Country/Region` %in% (listOfCountries) & is.na(`Province/State`) & C1effective == 1 ],
                           aes_string(x="Date", y = columnName, color = "\"C1Effective\""), color = "red", size=3) 
            
         # + scale_color_manual(name="Effectiveness", labels=c("C1effective", "C6Effective"), values=C("blue", "brown"))
        
        if ( (dataInput[ `Country/Region` %in% (listOfCountries) & is.na(`Province/State`) & C6effective == 1, .N ]) > 0  ) {
            
            p = p + geom_point( data= dataInput[ `Country/Region` %in% (listOfCountries) & is.na(`Province/State`) & C6effective == 1 ],
                            aes_string(x="Date", y = columnName, color = "\"C6Effective\""), color = "blue", size=3)
            
        }
        
        
    } 
    
    # enable logarithmic plot
    if (scale == "logarithmic") {
        p = p +
            scale_y_log10(name = paste(y_label, " (logarithmic)"),
                          limits = c(1, NA))
        
    }
    
    p
    
    
}


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
                tabPanel("Plot Deaths per 100k ", plotOutput("plot_deathsPer100k"))
        
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$myScale = renderText(
        print(input$scale)
    )
    
    
    output$plot_absoluteDeaths <- renderPlot({
        # call plot function with column which contains absolute deaths
        plotCoronaDeaths ( input$countries,  subset(PlotDT, Date >= input$date[1] & Date <= input$date[2]), "Dead", input$scale, "Deaths")
        
    }, width = "auto",
    height = 600)
    
    output$plot_deathsPer100k <- renderPlot({
        # call for plot of deaths per capita
        plotCoronaDeaths ( input$countries ,subset(PlotDT, Date >= input$date[1] & Date <= input$date[2]), "deathPer100k", input$scale, "Deaths per 100k population") # input$scale?
        
    })
    
   
    
    
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



