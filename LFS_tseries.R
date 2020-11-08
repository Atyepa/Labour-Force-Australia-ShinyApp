library(tidyverse)
library(highcharter)
library(ggthemes)
library(DT)
library(rsdmx)
library(shinyWidgets)
library(lubridate)
library(zoo)
library(rsconnect)
library(writexl)
library(readxl)
library(scales)

#-----------------------------------------------------
#---- Load LF  data from ABS.Stat SDMX-XML API ----
#-----------------------------------------------------

#--- SDMX data query URL---
sdmx_dat <-  "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/LF/0.1+2+3+4+5+13+6+9+14+10+18.3+1+2.1599.10+20.M/all?startTime=1980-01&endTime=2025-12"
             
#--- SDMX DSD URL---
sdmx_dsd <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/LF"

#---Read in SDMX---
dat <- readSDMX(sdmx_dat, dsd = TRUE)
dsd <- readSDMX(sdmx_dsd)

#--- Associate data and dsd ---
dat <- setDSD(dat, dsd)

#---Make dataframe
dat <- as.data.frame(dat, labels = TRUE)

#--- Select only useful cols ---
LFSdat <- dat %>% 
  select(ITEM_label.en, SEX_ABS_label.en, TSEST_label.en, obsTime, obsValue) %>% 
  rename("LF_item" = ITEM_label.en, Series = TSEST_label.en, SEX = SEX_ABS_label.en,  "Mth" = obsTime, "Value" = obsValue) %>% 
  mutate(Mth = paste0(Mth,"-01")) %>% 
  mutate(Value = round(Value,2))

#--make units--
LFSdat <-  LFSdat %>% 
  mutate(units = case_when(
    LF_item %in% c("Unemployed persons ('000)",
                   "Employed persons ('000)",
                   "Employed - full time ('000)",
                   "Employed - part time ('000)",
                   "Labour Force ('000)",
                   "Not in the Labour Force ('000)",
                   "Civilian population ('000)") ~ "Persons (000s)",
    
    LF_item %in% c("Unemployment rate (%)",
                   "Participation rate (%)",
                "Employment to population ratio (%)") ~ "Percent", 
    
    LF_item == "Employed Persons - Monthly hours worked in all jobs ('000)" ~ "Monthly hours (000s)" )  )


#---Make Mth into date formats---
LFSdat <- LFSdat %>% 
  mutate(startmth = ymd(Mth))%>% 
  mutate(endmth = ceiling_date(startmth, "month")-1) %>% 
  mutate(date = endmth -1) %>% 
  mutate(date = floor_date(date, "month"))


#--- Start series from 40 years earlier---
dmax <- max(LFSdat$date)
dmin <- dmax- months(480)

LFSdatL <- LFSdat %>%
  filter(date >= dmin)

#--- Make other date objects ----
latest <- format(dmax,"%b %Y")
now <- format(today(),"%d %B %Y")

#---LF components ---
#---Make components list ---
LF_item <- LFSdatL %>% 
  group_by(LF_item) %>% 
  tally()

#---- Make categoricals factors & lists for UI---

#-- Series - 

series <- LFSdatL %>% 
  group_by(Series) %>% 
  tally()
  
Series <- as.list(series$Series)
names(Series) <- series$Series

#-- Units lookup--
u <- LFSdatL %>% 
  group_by(LF_item, units) %>% 
  tally() %>% 
  select(-3)


#--- ABS preferred colours---
abscol <- c("#336699", "#669966", "#99CC66", "#993366", "#CC9966", "#666666")

#----------------------------------------
#---- SHINY DASHBOARD----
#----------------------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)

#------------
#----UI----
#------------
ui <- fluidPage(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  headerPanel("Labour Force, Australia"),
  sidebarPanel(
    
    sliderInput("dateRange","Date range:",
                min = as.Date(dmin),
                max = as.Date(dmax),
                value=as.Date(c(dmin, dmax)),
                step = 3,
                timeFormat="%b %Y"),
  
    
    checkboxGroupInput("SEX", "Sex:", choices = c("Males", "Females", "Persons"), selected = c("Persons"), inline = TRUE), 
    
    checkboxGroupInput("SERIES", "Series:", choices = c(Series), selected = "Seasonally Adjusted", inline = TRUE),   
    
      
    tags$div( tags$hr()),
    tags$div(
      tags$h4(tags$strong("Select measure:"))),
    
        pickerInput ("LF_item", " ", choices= c("Unemployment rate (%)",
                                                 "Participation rate (%)",
                                                 "Employment to population ratio (%)",
                                                 "Unemployed persons ('000)",
                                                 "Employed persons ('000)",
                                                 "Employed - full time ('000)",
                                                 "Employed - part time ('000)",
                                                 "Labour Force ('000)",
                                                 "Not in the Labour Force ('000)",
                                                 "Civilian population ('000)",
                                                 "Employed Persons - Monthly hours worked in all jobs ('000)"),
                                         
                 selected = c("Unemployment rate (%)"), multiple = TRUE ),
    
    downloadButton("downloadTb", "Download selection:")), 
  
    mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Graph", highchartOutput("hcontainer",height = "720px")),
                tabPanel("Table", verbatimTextOutput("message"), DT::dataTableOutput("table"))
    ),
    
    
    # adding the div tag to the mainpanel
    tags$div(class="header", checked=NA,
             tags$p(paste0("Source:"),
                    tags$a(href="https://www.abs.gov.au/ausstats/abs@.nsf/mf/6202.0",
                           (paste0("Australian Bureau of Statistics,
                                   Labour Force, Australia, ", latest)))),
                            tags$p(paste0("Retrieved from"),
                                     tags$a(href="http://stat.data.abs.gov.au/",
                                            (paste0("ABS.Stat: ", now))))
    )))
             

#==========================================
# Server
#=========================================
server <- function(input, output) {  
  
  
  I <- reactive({
    list (LF_item = input$LF_item) })    
  
  
  Sx <- reactive({
    list (SEX = input$SEX) })    
  
  S <- reactive({
    list (SERIES = input$SERIES) })    
 
  U <- reactive({u %>% 
        filter(LF_item %in% I()$LF_item)
  })
  
 
    
  df <- reactive({ LFSdatL %>%
      group_by(LF_item, SEX, Series) %>%
      arrange(date) %>%
      mutate(nxt = if_else(date< max(date), lead(date), date))%>%
      filter(nxt >= input$dateRange[1]) %>%  
      filter(date <= input$dateRange[2]) %>%  
      mutate(Month = format(date, "%b %Y")) %>% 
      filter(LF_item %in% I()$LF_item) %>% 
      filter(SEX %in% Sx()$SEX) %>% 
      filter(Series %in% S()$SERIES)
  
  })
 
  U <- reactive({ LFSdatL %>%
      filter(LF_item %in% I()$LF_item) %>% 
      group_by(LF_item) %>%
      summarise(unit = max(units))
      
  })
  
  
  
  #--- Spread items into columns and combine into table for output tab ---
  
  tabc  <- reactive({ df() %>%
      group_by(LF_item, SEX, Series) %>%
      arrange(date) %>%
      select(date, LF_item, SEX, Series, Value) %>%
      spread(LF_item, Value) %>%
      mutate(Month = format(date, "%b %Y")) %>% 
      arrange(date) %>% 
      select(-date) %>% 
      select(Month, everything()) 
     
          })
  
  
  output$message <- renderText({ 
    
    if(is.null(I()$LF_item)){
        msg <- "No data items selected"}
      
    
    if(!is.null(I()$LF_item)){
      msg <- ""}
    
    msg
    
    }) 
  
  
  output$table = DT::renderDataTable({
    tabc()
    
  })
  
  
  output$hcontainer <- renderHighchart({
    
  
    {
      
      hc <- df() %>%
        hchart(.,
               type = "line",
               hcaes(x = date,
                     y = Value,
                     group = paste0(LF_item, ", ", SEX,", ", Series))) %>%
                 hc_xAxis(type = "datetime", crosshair = TRUE, dateTimeLabelFormats = list(day = '%b, %Y')) %>% 
                 hc_xAxis(title = list(text = "Month"), labels = list(enabled = TRUE)) %>%
                 hc_yAxis(title = list(text = paste0(U()$unit))) %>%
                 hc_add_theme(hc_theme_economist()) %>%
                 hc_title(text = paste0(input$LF_item)) %>%
                 hc_colors(abscol) %>%
                 hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    }
    #group = paste0(SEX,", ", Series)) %>%
    
    hc
    
    
    })
  
  # Downloadable xlsx --
  output$downloadTb <- downloadHandler(
    filename = function() { paste0(input$LF_item,", ", format(input$dateRange[1],"%b %Y"),  " to ",format(input$dateRange[2],"%b %Y"),".xlsx") },
    content = function(file) { write_xlsx(tabc(), path = file) }
  )
  
}

#========================================  
shinyApp(ui, server)
#========================================