library(testthat)
library(assertive, warn.conflicts = FALSE)
library(plyr)
library(shiny)
library(shinyjs)
library(shinyBS)
library(DBI)
library(readxl)
library(Rfast)
library(lubridate)
library(DT)
library(dplyr)
library(tidyverse)
library(xlsx)
library(shinythemes)
library(shinyWidgets)
library(shinyhelper)
library(shinyalert)
source("mainModel.R")
source("querryHelper.R")
source("CoefSpreader.R")
source("CoefCalculator.R")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
con <- dbConnect(RSQLite::SQLite(), "UniData.db")
dbExecute(con, "PRAGMA foreign_keys=ON")
#### UI ####

ui <- fluidPage(
  shinyjs::useShinyjs(),
  useShinyalert(),
  setBackgroundImage(
    ## src = ".jpg"
  ),
  theme = shinythemes::shinytheme("cerulean"),
  navbarPage("SODSS",
             ### HOME UI ####
             tabPanel("Home",
                      titlePanel("Main Model"),
                      sidebarLayout(
                        
                        sidebarPanel(
                          numericInput("homeWeekNo", "Please enter the week number :", value = 6, max = 53, min = 1),
                          numericInput("homeYearNo", "Please enter the year : ", value = 2018),
                          numericInput("OverallServiceLevel", "Please enter the overall service level", value = 0.75, min = 0, max = 1, step = 1/100),
                          numericInput("spec", 'Please enter the specification', value = 1, min = 0.1),
                          actionButton("fcFileButton", "Upload"),
                          shinyjs::hidden(p(id = "text1", "Processing...")),
                          tags$hr(style="border-color: black;"),
                          helper(shiny_tag = "Need help?",
                                 icon = "question-circle",
                                 colour = "red",
                                 type = "markdown",
                                 content = "home")
                        ),
                        
                        mainPanel(
                          textOutput("tr"),
                          tableOutput("afterSeyhan")

                        )
                      )         
             ),
             ### EDIT UI ####
             
             navbarMenu("Edit",
                        ## ADD SKU MANUAL UI####
                        tabPanel("Add SKU Manual",
                                 titlePanel("Add SKU Manual"),
                                 sidebarLayout
                                 (
                                   sidebarPanel
                                   (
                                     textInput("addSkuManCode","Please enter the SKU Code : ", placeholder = "SKU Code"),
                                     br(),
                                     selectInput("addSkuManType","Please enter the type of SKU : ",
                                                 list("DESSERT" = "DESSERT", "DRESSINGS" = "DRESSINGS", "SAVOURY" = "SAVOURY", "TEA" = "TEA")),
                                     br(),
                                     textInput("addSkuManMOQ", "Please enter the MOQ value of the SKU : ", placeholder = "Minimum order Quantity"),
                                     br(),
                                     radioButtons("addSkuManLs", "Please select the MOQ type : ", choices = list("MOQ" = "MOQ", "Free" = "Free")),
                                     br(),
                                     numericInput("addSkuManHoldCost", "Please enter the holding cost : ", value = 25),
                                     br(),
                                     numericInput("addSkuManSl", "Please enter the minimum desired service level : ", value = 75),
                                     br(),
                                     numericInput("addSkuWS", "Please enter the error value : ", value = 0),
                                     br(),
                                     actionButton("addSkuManButton" ,"ADD")
                                   ),
                                   mainPanel
                                   (
                                     textOutput("addSkuManText"),
                                     DT::dataTableOutput("addSkuManTable")
                                   )
                                   
                                 )
                        ),
                        ## ADD SKU FROM EXCEL UI####
                        tabPanel("Add SKU From EXCEL",
                                 titlePanel("Add SKU From EXCEL"),
                                 sidebarLayout
                                 (
                                   sidebarPanel
                                   (
                                     fileInput("inputSkuFile","Upload SKU Excel File"),
                                     actionButton("buttonSKUxlsxUpload", "Upload"),
                                     tags$hr(style="border-color: black;"),
                                     helper(shiny_tag = "Need help?",
                                            icon = "question-circle",
                                            colour = "red",
                                            type = "markdown",
                                            content = "addsku")
                                   ),
                                   mainPanel
                                   (
                                     DT::dataTableOutput("upSkuTable")
                                   )
                                   
                                 )
                        ),
                        ## DELETE BY SKU CODE UI ####
                        tabPanel("Remove by SKU Code",
                                 titlePanel("Remove by SKU Code"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     numericInput(inputId = "delBySkuCode", label = "Please enter the SKU Code :", value = NULL),
                                     radioButtons(inputId = "Type",label = "Select the wanted types: ",choices = list("Moq" = "moq", "Free" = "free"),selected = "moq"),
                                     actionButton("delBySkuButton","DO")
                                     
                                   ),
                                   
                                   mainPanel(
                                     
                                     tableOutput(outputId = "delBySkuTable")
                                   )
                                 )
                        ),
                        ## DELETE SKU BY EXCEL UI ####
                        tabPanel("Remove SKU via EXCEL",
                                 titlePanel("Remove SKU via EXCEL"),
                                 sidebarLayout
                                 (
                                   sidebarPanel
                                   (
                                     fileInput("deleteSkuFile","Upload SKU Excel File"),
                                     actionButton("buttonSKUxlsxDelete", "Upload"),
                                     tags$hr(style="border-color: black;"),
                                     helper(shiny_tag = "Need help?",
                                            icon = "question-circle",
                                            colour = "red",
                                            type = "markdown",
                                            content = "deletesku")
                                   ),
                                   mainPanel
                                   (
                                     DT::dataTableOutput("delSkuTable"),
                                     textOutput("message")
                                   )
                                   
                                 )
                        ),
                        ## DELETE BY DATE RANGE UI ####
                        tabPanel("Remove by Date",
                                 titlePanel("Remove by Date"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     splitLayout(
                                       selectInput("delByDateFromMonth","Enter Month",list(
                                         "1" = 1, "2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12)),
                                       selectInput("delByDateFromYear","Enter Year", c(
                                         "2018"=2018,"2019"=2019,"2020"=2020,"2021"=2021))
                                     ),
                                     br(),
                                     splitLayout(
                                       selectInput("delByDateToMonth","Enter Month",list(
                                         "1" = 1, "2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12)),
                                       selectInput("delByDateToYear","Enter Year", c(
                                         "2018"=2018,"2019"=2019,"2020"=2020,"2021"=2021))
                                     ),
                                     
                                     actionButton("delByDateButton","DELETE")
                                     
                                     
                                   ),
                                   
                                   mainPanel(
                                     textOutput(outputId = "delByDateWarn"),
                                     tableOutput(outputId = "delByDateSalesTable"),
                                     tableOutput(outputId = "delByDateFcTable")
                                   )
                                 )
                                 
                        )
             ),
             ### ADD SALES ####
             tabPanel("Add Sales",
                      titlePanel("Add Sales"),
                      sidebarLayout(
                        
                        sidebarPanel(
                          verticalLayout(fileInput("inputxlsxFile","Upload sales excel file..."),
                                         actionButton("buttonBilkent", "Upload")),
                          tags$hr(style="border-color: black;"),
                          helper(shiny_tag = "Need help?",
                                 icon = "question-circle",
                                 colour = "red",
                                 type = "markdown",
                                 content = "addsales")
                        ),
                        
                        
                        
                        mainPanel(
                          tableOutput("tableoutputBilkent"),
                          textOutput("textoutputBilkent"),
                        )
                      )         
                      
             ),
             ### ADD FORECAST ####
             tabPanel("Add Forecast",
                      titlePanel("Add Forecast"),
                      sidebarLayout(
                        
                        sidebarPanel(
                          fileInput("inputxlsxFile2", "Upload forecast excel file..."),
                          actionButton("buttonBilkent2", "Upload"),
                          tags$hr(style="border-color: black;"),
                          helper(shiny_tag = "Need help?",
                                 icon = "question-circle",
                                 colour = "red",
                                 type = "markdown",
                                 content = "addforecast")
                          
                        ),
                        
                        mainPanel(
                          tableOutput("tableoutputBilkent2"),
                          textOutput("textoutputBilkent2")
                        )
                      )
             ),
             ### UPDATE INVENTORY ####
             navbarMenu("Inventory",
                        tabPanel("Create Inventory",
                                 titlePanel("Create Inventory"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     textInput("invWeeklyCode","WeeklyCode", value = "01012018"),
                                     actionButton("invcreatebutton","Create"),
                                     tags$hr(style="border-color: black;"),
                                     helper(shiny_tag = "Need help?",
                                            icon = "question-circle",
                                            colour = "red",
                                            type = "markdown",
                                            content = "addforecast")
                                   ),
                                   mainPanel(
                                     tableOutput("createinventory")
                                   )
                                 )),
                        
                        
                        tabPanel("Update Inventory",
                                 titlePanel("Update Inventory"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("updateinvfile","Upload invetory excel file..."),
                                     actionButton("invupdatebutton", "Upload"),
                                     tags$hr(style="border-color: black;"),
                                     helper(shiny_tag = "Need help?",
                                            icon = "question-circle",
                                            colour = "red",
                                            type = "markdown",
                                            content = "addforecast")
                                   ),
                                   mainPanel(
                                     tableOutput("updateinventory")
                                   )
                                 )
                        )
                        
             ),
             
             ### DATE VIEW ####
             navbarMenu("View",
                        tabPanel("View by Week/Month/Year",
                                 titlePanel("View by Week/Month/Year"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     verticalLayout(
                                       splitLayout(numericInput("normalweekNo", "Week No:", value = 1, min = 1, max = 5),
                                                   numericInput("monthNo", "Month No", value = 1, min = 1, max = 12),
                                                   numericInput("yearNo", "Year No", value = 2018, min = 2018, max = 2035)),
                                       actionButton("viewButton", "View by Month/Year")
                                     )
                                     
                                   ),
                                   
                                   mainPanel(
                                     tableOutput("viewtable1")
                                     
                                   )
                                 )         
                        ),
                        tabPanel("View by Starting Day of Week",
                                 titlePanel("View by Starting Day of Week"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     verticalLayout( 
                                       dateInput("dateinput", "Starting Date of the Week: ", value = "2018-01-01", min = "2018-01-01", max = "2035-08-20",format = "yyyy/mm/dd"),
                                       actionButton("viewButton2", "View by Date")  
                                     )
                                   ),
                                   
                                   mainPanel(
                                     tableOutput("viewtable2")
                                   )
                                 )         
                        ),
                        tabPanel("View by WeeklyCode",
                                 titlePanel("View by WeeklyCode"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     verticalLayout(
                                       textInput("date3input", "WeeklyCode:", value = "01012018"),
                                       actionButton("viewButton3", "View by WeeklyCode")
                                     )
                                   ),
                                   
                                   mainPanel(
                                     tableOutput("viewtable3")
                                   )
                                 )         
                        ),
                        tabPanel("SKU View",
                                 titlePanel("SKU View"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     verticalLayout( 
                                       numericInput("viewskuinput", "SKU:", value = 20018418),
                                       actionButton("viewskubutton", "View by Sku")
                                     )
                                   ),
                                   
                                   mainPanel(
                                     tableOutput("viewskutable")
                                   )
                                 )         
                        )
                        
             ),
             ### ADD PREORDER ####
             tabPanel("Add Order",
                      titlePanel("Add Order"),
                      sidebarLayout(
                        
                        sidebarPanel(
                          fileInput("inputPreorderxlsx", "Upload forecast excel file..."),
                          actionButton("addPreOrderButton", "Upload"),
                          tags$hr(style="border-color: black;"),
                          helper(shiny_tag = "Need help?",
                                 icon = "question-circle",
                                 colour = "red",
                                 type = "markdown",
                                 content = "addpreorder")
                          
                        ),
                        
                        mainPanel(
                          tableOutput("preOrderTable"),
                          textOutput("preOrderText")
                        )
                      )
             ),
             ### ADJUSTMENTS ####
             navbarMenu("Adjust",
                        ## CALCULATE COEFFICIENTS ####
                        tabPanel("Calculate Coefficients",
                                 titlePanel("Calculate Coefficients"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     splitLayout(
                                       selectInput("calCoefFromMonth","Enter Month",list(
                                         "1" = 1, "2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12)),
                                       selectInput("calCoefFromYear","Enter Year", c(
                                         "2018"=2018,"2019"=2019,"2020"=2020,"2021"=2021))
                                     ),
                                     br(),
                                     splitLayout(
                                       selectInput("calCoefToMonth","Enter Month",list(
                                         "1" = 1, "2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12)),
                                       selectInput("calCoefToYear","Enter Year", c(
                                         "2018"=2018,"2019"=2019,"2020"=2020,"2021"=2021))
                                     ),
                                     actionButton("calCoefButton","CALCULATE")
                                   ),
                                   mainPanel(
                                     
                                     uiOutput("coefHeader1"),
                                     tags$div(class = "oldCoefTable", checked = NA,
                                              DT::dataTableOutput("oldCoef")),
                                     
                                     tags$br(),
                                     
                                     uiOutput("coefHeader2"),
                                     tags$div(class = "newCoefTable", checked = NA,
                                              DT::dataTableOutput("dataCoef"))
                                     
                                   )
                                 )),
                        ## CALCULATE SIGMA VALUES ####
                        tabPanel("Calculate Sigma Values",
                                 titlePanel("Calculate Sigma Values"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     splitLayout(
                                       numericInput("calSigFromMonth","Enter Month",value = 1, min = 1, max = 12),
                                       numericInput("calSigFromYear","Enter Year", value = 2018, min = 2018)
                                     ),
                                     br(),
                                     splitLayout(
                                       numericInput("calSigToMonth","Enter Month",value = 1, min = 1, max= 12),
                                       numericInput("calSigToYear","Enter Year", value = 2018, min =2018 )
                                     ),
                                     actionButton("calSigButton","CALCULATE")
                                   ),
                                   mainPanel(
                                     tableOutput("salesRow"),
                                     textOutput("fcRow")
                                     
                                   )
                                 )
                                 
                                 
                                 
                        ),
                        ## UPDATE HOLDING COST ####
                        tabPanel("Update Holding Cost",
                                 titlePanel("Update Holding Cost"),
                                 
                                 sidebarLayout(
                                   sidebarPanel(
                                     splitLayout(
                                       textInput("skucode", "Enter SKU Code",value = '20018418'), 
                                       textInput("holdingcost", "Holding Cost", value = "")),
                                     actionButton("buttonupdates", "Update")
                                     
                                   ),
                                   
                                   mainPanel(
                                     tableOutput("newholdingcostrow"),
                                     tableOutput("oldholdingcostrow")
                                   )
                                 )         
                                 
                        ),
                        ## UPDATE HOLDING COST via EXCEL ####
                        tabPanel("Update Holding Cost via EXCEL",
                                 titlePanel("Update Holding Cost via EXCEL"),
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     fileInput("inputHCFile","Upload holding cost excel file"),
                                     actionButton("buttonHCxlsx", "Update"),
                                     tags$hr(style="border-color: black;"),
                                     helper(shiny_tag = "Need help?",
                                            icon = "question-circle",
                                            colour = "red",
                                            type = "markdown",
                                            content = "updateholdingcost")
                                     
                                   ),
                                   
                                   mainPanel(
                                     DT::dataTableOutput("tableHCxlsx")
                                   )
                                 )         
                                 
                        ),
                        ## UPDATE INDIVIDUAL SERVICE ####
                        tabPanel("Update Individual Service",
                                 titlePanel("Update Individual Service"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     splitLayout(
                                       numericInput("indvSKU","Enter SKU Code",value = 20018418), 
                                       numericInput("newServiceLevel","Enter Service Level",value = 0.5,min = 0, max = 1, step = 0.01)
                                     ),
                                     actionButton("individualButton","UPDATE")
                                   ),
                                   mainPanel(
                                     tableOutput("oldIndv"),
                                     tableOutput("newIndv")
                                     
                                   )
                                 )
                        ),
                        ## UPDATE INDIVIDUAL SERVICE via EXCEL ####
                        tabPanel("Update Individual Service via EXCEL",
                                 titlePanel("Update Individual Service via EXCEL"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("inputSLFile","Upload individual service level excel file"),
                                     actionButton("individualButtonxlsx","Update"),
                                     tags$hr(style="border-color: black;"),
                                     helper(shiny_tag = "Need help?",
                                            icon = "question-circle",
                                            colour = "red",
                                            type = "markdown",
                                            content = "updateindividualservice")
                                   ),
                                   mainPanel(
                                     DT::dataTableOutput("indServXlsx")
                                   )
                                 )
                        ),
                        
                        ## UPDATE WEEKLY FORECAST ####
                        tabPanel("Update Weekly Forecast",
                                 titlePanel("Update Weekly Forecast"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("fileupdateforecast", "Upload forecast excel file..."),
                                     actionButton("updateforecastbutton","UPDATE"),
                                     tags$hr(style="border-color: black;"),
                                     helper(shiny_tag = "Need help?",
                                            icon = "question-circle",
                                            colour = "red",
                                            type = "markdown",
                                            content = "updateindividualservice")
                                   ),
                                   mainPanel(
                                     DT::dataTableOutput("updateforecastoutput")
                                   )
                                 )
                                 ),
                        tabPanel("Update Sales",
                                 titlePanel("Update Weekly Sales"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("fileupdatesales", "Upload sales excel file..."),
                                     actionButton("updatesalesbutton","UPDATE"),
                                     tags$hr(style="border-color: black;"),
                                     helper(shiny_tag = "Need help?",
                                            icon = "question-circle",
                                            colour = "red",
                                            type = "markdown",
                                            content = "updateindividualservice")
                                   ),
                                   mainPanel(
                                     DT::dataTableOutput("updatesalesoutput")
                                   )
                                 )
                        )
                        
                        
             )
  ))

                            
#### SERVER ####    
server <- function(input,output,session) {
  observe_helpers(session = shiny::getDefaultReactiveDomain(),
                  help_dir = "helpfiles", withMathJax = TRUE)
  interactive()
  
  ### SERVER HOME ####
  fcFileOutput <- observeEvent(input$fcFileButton,{
    
    shinyjs::disable("fcFileButton")
    shinyjs::show("text1")
    
    sku <- dbGetQuery(con, "SELECT * FROM SKU")
    
    #Sku set and sigma calculations
    skuSet <- sku$Code
    e3 <- sku$WeeklySigma*0.75
    ef3 <- sku$WeeklyMean
    ef12 <- ef3*2
    e12 <- e3*1.5
    spec <- input$spec
    SL <- input$OverallServiceLevel
    # Determining current week ID
    curWeek <- dbGetQuery(con, paste0("SELECT * FROM DATE WHERE WeekNo = ", input$homeWeekNo, " AND YearNo = ", input$homeYearNo))
    weekID = curWeek$ID[1]
    
    if(nrow(curWeek) == 2) # 1 if current week is split and not edge of the year
    {
      prewWeek = dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID - 1 ))
      next1Week = dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID + 2 ))
      next2Week = dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID + 3))
    }
    
    else
    {
      if(curWeek$SplitWeek == 1) # 1 if start of year with split week
      {
        curWeek <- rbind(curWeek,dbGetQuery(con,paste0(dbGetQuery(con,"SELECT * FROM DATE WHERE ID = ", WeekID - 1))))
        prewWeek = dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID - 2 ))
        next1Week = dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID + 1 ))
        next2Week = dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID + 2))
      }
      
      else if(curWeek$SplitWeek == 2) # 1 if end of year with split week
      {
        curWeek <- rbind(curWeek,dbGetQuery(con,paste0(dbGetQuery(con,"SELECT * FROM DATE WHERE ID = ", WeekID + 1))))
        prewWeek = dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID - 1 ))
        next1Week = dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID + 2 ))
        next2Week = dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID + 3))
      }
      else
      {
        prewWeek = dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID - 1 ))
        next1Week = dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID + 1 ))
        next2Week = dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID + 2))
        
        if(prewWeek$SplitWeek == 1)
        {
          prewWeek <- rbind(prewWeek,dbGetQuery(con,paste0("SELECT * FROM DATE WHERE ID = ", weekID - 2 )))
        }
        if(next1Week$SplitWeek == 2)
        {
          next1Week <- rbind(next1Week,next2Week)
          next2Week <- dbGetQuery(con, paste0("SELECT * FROM DATE WHERE ID = ", weekID + 3))
        }
        if(next2Week$SplitWeek == 2)
        {
          next2Week <- rbind(next2Week,(dbGetQuery(con,paste0("SELECT * FROM DATE WHERE ID = ",weekID + 3))))
        }
      }
    }
    
    #Inventory Data
    inv0 <- dbGetQuery(con,paste0("SELECT Value FROM INVENTORY WHERE WeeklyCode = ","'",curWeek[1,]$WeeklyCode,"'" ))
    
    if(nrow(curWeek) == 2)
    {
      inv0 <- inv0 + dbGetQuery(con,paste0("SELECT Value FROM INVENTORY WHERE WeeklyCode = ","'",curWeek[2,]$WeeklyCode,"'" ))
    }
    
    ## PreOrder Data
    preOrder <- dbGetQuery(con,paste0("SELECT Value FROM PREORDER WHERE ReceivedWeeklyCode = ","'",curWeek[1,]$WeeklyCode,"'" ))
    
    if(nrow(curWeek) == 2)
    {
      preOrder <- preOrder + dbGetQuery(con,paste0("SELECT Value FROM PREORDER WHERE ReceivedWeeklyCode = ","'",curWeek[2,]$WeeklyCode,"'" ))
    }
    
    # Forecast Data
    
    fc1 <- dbGetQuery(con,paste0("SELECT Value FROM FORECAST WHERE WeeklyCode = ","'",curWeek[1,]$WeeklyCode,"'"))
    fc2 <- dbGetQuery(con,paste0("SELECT Value FROM FORECAST WHERE WeeklyCode = ","'",next1Week[1,]$WeeklyCode,"'"))
    fc3 <- dbGetQuery(con,paste0("SELECT Value FROM FORECAST WHERE WeeklyCode = ","'",next2Week[1,]$WeeklyCode,"'"))
    
    
    if(nrow(curWeek) == 2)
    {
      fc1 <- fc1 + dbGetQuery(con,paste0("SELECT Value FROM FORECAST WHERE WeeklyCode = ","'",curWeek[2,]$WeeklyCode,"'"))
    }
    
    if(nrow(next1Week) == 2)
    {
      fc2 <- fc2 + dbGetQuery(con,paste0("SELECT Value FROM FORECAST WHERE WeeklyCode = ","'",next1Week[2,]$WeeklyCode,"'"))
    }
    
    if(nrow(next2Week) == 2)
    {
      fc3 <- fc3 + dbGetQuery(con,paste0("SELECT Value FROM FORECAST WHERE WeeklyCode = ","'",next2Week[2,]$WeeklyCode,"'"))
    }
    fc12 <- fc1 + fc2
    
    
    
    #Limit calculation
    varMOQ <- zeros(nrow(sku),1)
    limit <- zeros(nrow(sku),1)
    
    for (step in 1:nrow(sku))
    {
      varMOQ[step] <- ifelse(sku$LotSize[step] == "moq", sku$MOQ[step],sku$MOQ[step] * spec)
      limit[step] <- as.integer(ceiling(max((fc12[step,] + (4 * e12[step]) + fc3[step,] + (4 * e3[step]) - inv0[step,] - preOrder[step,]) - (sku$MOQ[step] * 1000),0) / (varMOQ[step] * 1000)) + 2)
    }
    
    seyhanIn <- as.data.frame(skuSet)
    seyhanIn[2] <- sku$MOQ * 1000
    seyhanIn[3] <- varMOQ * 1000
    seyhanIn[4] <- sku$HoldCost
    seyhanIn[5] <- fc12 + ef12
    seyhanIn[6] <- fc3 + ef3
    seyhanIn[7] <- e12
    seyhanIn[8] <- e3
    seyhanIn[9] <- inv0
    seyhanIn[10] <- preOrder
    seyhanIn[11] <- limit
    seyhanIn[12] <- sku$ServLevel
    
    # print(nrow(seyhanIn[1]),nrow(seyhanIn[2]),nrow(seyhanIn[3]),nrow(seyhanIn[4]),nrow(seyhanIn[5]),nrow(seyhanIn[6]),nrow(seyhanIn[7]),nrow(seyhanIn[8]),nrow(seyhanIn[9]),nrow(seyhanIn[10]),nrow(seyhanIn[11]),nrow(SL),nrow(seyhanIn[12]))
    seyhanOut <- mainModel(seyhanIn[1],seyhanIn[2],seyhanIn[3],seyhanIn[4],seyhanIn[5],seyhanIn[6],seyhanIn[7],seyhanIn[8],seyhanIn[9],seyhanIn[10],seyhanIn[11],SL,seyhanIn[12])
    
    orderWrite <- as.data.frame(skuSet)
    orderWrite[2] <- curWeek[1,]$WeeklyCode
    orderWrite[3] <- seyhanOut$Order[,2]
    names(orderWrite)[1] <- "Code"
    names(orderWrite)[2] <- "CreatedWeeklyCode"
    names(orderWrite)[3] <- "Value"
    dbWriteTable(con,"ORDER",orderWrite,append = TRUE)
    write.xlsx(orderWrite,"Order.xlsx",row.names=FALSE)
    if (nrow(curWeek) == 2){
      orderWrite2 = as.data.frame(skuSet)
      orderWrite2[2] = curWeek[2,]$WeeklyCode
      orderWrite2[3] = 0
      names(orderWrite2)[1] <- "Code"
      names(orderWrite2)[2] <- "CreatedWeeklyCode"
      names(orderWrite2)[3] <- "Value"
      dbWriteTable(con,"ORDER",orderWrite2,append = TRUE)
    }
    
    output$tr <- renderText({input$homeWeekNo})
    output$afterSeyhan <- renderTable({
      orderWrite 
    })
    shinyalert("Done!", "Model run finished..", type = "success")
    shinyjs::enable("fcFileButton")
    shinyjs::hide("text1")
  })
  
  ### SERVER EDIT ####
  ## SERVER ADD SKU ####
  addSkuManQuery <- observeEvent(input$addSkuManButton,{
    addSkuManDf <- as.data.frame(input$addSkuManCode)
    addSkuManDf[2] <- input$addSkuManType
    addSkuManDf[3] <- input$addSkuManMOQ
    addSkuManDf[4] <- input$addSkuManLs
    addSkuManDf[5] <- input$addSkuManHoldCost
    addSkuManDf[6] <- input$addSkuManSl/100
    addSkuManDf[7] <- input$addSkuWS
    names(addSkuManDf)[1] <- "Code"
    names(addSkuManDf)[2] <- "Type"
    names(addSkuManDf)[3] <- "MOQ"
    names(addSkuManDf)[4] <- "LotSize"
    names(addSkuManDf)[5] <- "HoldCost"
    names(addSkuManDf)[6] <- "ServLevel"
    names(addSkuManDf)[7] <- "WeeklySigma"
    # addSkuManQuery <- dbExecute(con,paste0("INSERT INTO SKU (Code, Type, MOQ, LotSize, HoldCost, ServLevel, WeeklySigma) Values (",
    #                                         input$addSkuManCode,",",input$addSkuManType, "," , input$addSkuManMOQ , "," ,
    #                                         input$addSkuManLs, ",",input$addSkuManHoldCost,",", input$addSkuManSl, ",", input$addSkuWS))
    dbWriteTable(con,"SKU",addSkuManDf,append = TRUE)
    re <- reactive(
      output$addSkuManTable <- DT::renderDataTable({dbGetQuery(con,"SELECT * FROM SKU")}))#dbGetQuery(con,"SELECT * FROM SKU")
    re()
  })
  
  
  ## SERVER ADD SKU BY EXCEL ####
  addSkuXlsxQuery <- observeEvent(input$buttonSKUxlsxUpload,{
    
    req(input$inputSkuFile)
    inFile <- input$inputSkuFile
    
    skuXlsx <- read_excel(inFile$datapath, 1)
    
    dbWriteTable(con,"SKU",skuXlsx,append = TRUE)
    reAddSku <- reactive(
      output$upSkuTable <- DT::renderDataTable({skuXlsx}))#dbGetQuery(con,"SELECT * FROM SKU")
    reAddSku()
  })
  ## SERVER DELETE SKU BY SKU CODE #####
  delBySkuQuery <- observeEvent(input$delBySkuButton, {
    delBySkuQuery <- dbExecute(con, paste0("DELETE FROM SKU WHERE Code = ", input$delBySkuCode))
    
    output$delBySkuTable <- renderTable({
      delBySkuTableQuery <- dbGetQuery(con,paste0("SELECT * FROM SKU WHERE LotSize = ","'",as.character(input$Type),"'"))
    })
  })
  
  
  output$delBySkuTable <- renderTable({
    delBySkuTableQuery <- dbGetQuery(con,paste0("SELECT * FROM SKU WHERE LotSize = ","'",as.character(input$Type),"'"))
  })
  ## SERVER DELETE SKU BY EXCEL ####
  delByXlsxQuery <- observeEvent(input$buttonSKUxlsxDelete, {
    req(input$deleteSkuFile)
    inFile <- input$deleteSkuFile
    
    skuDeleteXlsx <- read_excel(inFile$datapath, 1)
    
    for (i in 1:nrow(skuDeleteXlsx)){
      dbExecute(con,paste0("DELETE FROM SKU WHERE Code = ","'",skuDeleteXlsx[i,1],"'"))
    }
    reDelSku <- reactive(
      #output$skuDeleteXlsx <- DT::renderDataTable({tmp})#dbGetQuery(con,"SELECT * FROM SKU")
      output$message <- renderText({("The SKU list succesfully deleted from database.")}))    
    reDelSku()
  })
  ## SERVER DELETE SKU BY DATE RANGE #####
  delByDatePrewQuery <- observeEvent(input$delByDateButton,{
    delByDatePrewQuery <- querryHelper("SELECT","SALES", input$delByDateFromMonth, input$delByDateFromYear, input$delByDateToMonth, input$delByDateToYear,con)
    
    output$delByDateWarn <- renderText({
      print("You are about the delete SKUs that codes shown below. Are you sure?")
    })
    
    output$delByDateSalesTable <- renderTable({
      delByDatePrewQuery
    })
  })
  
  
  
  ### SALES UPLOADING ####
  bilkentService <- observeEvent(input$buttonBilkent , {
    
    req(input$inputxlsxFile)
    inFile <- input$inputxlsxFile
    
    dd <- read_excel(inFile$datapath, 1)
    
    for (i in 1:nrow(dd)) {
      
      code          <- dd[i,1]
      weeklycode    <- dd[i,2]
      value         <- dd[i,3]
      
      
      q1 <- dbGetQuery(con, paste("SELECT MonthlyCode, NormalWeekNo, SplitType 
                         FROM DATE
                         WHERE WeeklyCode = ","'",as.character(weeklycode),"'",
                                  sep = ""))
      
      q2 <- dbGetQuery(con, paste("SELECT Type
                                    FROM SKU
                                    WHERE Code = ",as.character(code),
                                  sep = ""))
      
      dd[i,4] <- q1[1,1] 
      dd[i,5] <- q1[1,2]                  
      dd[i,6] <- q1[1,3]
      dd[i,7] <- q2[1,1]
      
      
      if (dd[i,7] == "DRESSINGS") {
        dd[i,8] <- paste("D",as.character(dd[i,6]), sep = "")  
      } else {
        dd[i,8] <- paste("N",as.character(dd[i,6]), sep = "") 
      }
      
      
    }
    
    
    names(dd) <- c("Code","WeeklyCode","Value","MonthlyCode","WeekNo","SplitType","Type","CoefType")
    
    new_d <- data.frame(dd[1],dd[7],dd[2],dd[5],dd[8],dd[4],dd[3])

    dbWriteTable(con, "SALES", new_d, append = TRUE)
    
    output$tableoutputBilkent <- renderTable({
      new_d
    })
  })
  
  ### FORECAST UPLOADING ####
  bilkent2servis <- observeEvent(input$buttonBilkent2 ,{
    
    req(input$inputxlsxFile2)
    inFile <- input$inputxlsxFile2
    
    d <- read_excel(inFile$datapath, 1)
    
    for (i in 1:nrow(d)) {
      
      monthno <- d[i,2]
      year <- d[i,3]
      code <- d[i,1]
      
      q1 <- dbGetQuery(con, paste("SELECT WeeklyCode, MonthlyCode 
                         FROM DATE
                         WHERE YearNo = ",as.character(year),
                                  " AND MonthNo = ", as.character(monthno),
                                  sep = ""))
      
      q2 <- dbGetQuery(con, paste("SELECT Type
                                    FROM SKU
                                    WHERE Code = ",as.character(code),
                                  sep = ""))
      
      q3 <- dbGetQuery(con, paste("SELECT SplitType 
                         FROM DATE
                         WHERE YearNo = ",as.character(year),
                                  " AND MonthNo = ", as.character(monthno),
                                  sep = ""))
      
      if (q2 == "DRESSINGS") {
        typevar <- paste("D",as.character(q3[1,1]), sep = "")  
      } else {
        typevar <- paste("N",as.character(q3[1,1]), sep = "") 
      }
      
      
      if (q3[1,1] == 0) {
        q4 <- dbGetQuery(con, paste("SELECT firstWeek, secondWeek, thirdWeek, fourthWeek 
                         FROM COEFS
                         WHERE CoefType = ","'",as.character(typevar),"'",
                                    sep = ""))
        weekvalues <- t(data.frame(1,2,3,4))
      } else {
        q4 <- dbGetQuery(con, paste("SELECT firstWeek, secondWeek, thirdWeek, fourthWeek, fifthWeek
                         FROM COEFS
                         WHERE CoefType = ","'",as.character(typevar),"'",
                                    sep = ""))
        weekvalues <- t(data.frame(1,2,3,4,5))
      }
      
      
      
      
      for (j in 1:ncol(q4)) {
        weekvalues[j,1] <- as.numeric(d[i,4])*as.numeric(q4[1,j])
      }
      
      q1[3] <- d[i,1]
      q1[4] <- "NO"
      q1[5] <- weekvalues
      q1[6] <- q2
      
      if (i == 1) {
        d2 <- q1
      } else {
        d2 <- rbind(d2,q1)
      }
      
      
      
      
    }
    names(d2)[4] <- "Flag"
    names(d2)[5] <- "Value" 
    d2 <- data.frame(d2[3],d2[6],d2[1],d2[5],d2[4],d2[2])
    
    dbWriteTable(con, "FORECAST", d2, append = TRUE)
    
    timee <- as.character(Sys.time())
    substr(timee,14,14) <- "-"
    substr(timee,17,17) <- "-"
    write.xlsx(data.frame(d2[1],d2[3],d2[4]),paste0("Forecast Created at ",as.character(timee),".xlsx"),row.names=FALSE)
    
    output$tableoutputBilkent2 <- renderTable({d2})
  })
  
  ### INVENTORY ####
  createinventory <- observeEvent(input$invcreatebutton,{
    
    curweek <- input$invWeeklyCode

    q1 <- dbGetQuery(con, paste("SELECT ID,SplitWeek
                         FROM DATE
                         WHERE WeeklyCode = ",
                                "'",as.character(curweek),"'",
                                sep = ""))
    
    if (q1[1,2] == 2) {
      
      curweek2 <- dbGetQuery(con, paste("SELECT WeeklyCode
                         FROM DATE
                         WHERE ID = ",
                                        "'",as.character(q1[1,1]+1),"'",
                                        sep = ""))
      
      lastweek <- dbGetQuery(con, paste("SELECT WeeklyCode
                         FROM DATE
                         WHERE ID = ",
                                        "'",as.character(q1[1,1]-1),"'",
                                        sep = ""))
      
      sales <- dbGetQuery(con, paste("SELECT Code, Value
                         FROM SALES
                         WHERE WeeklyCode = ",
                                     "'",as.character(lastweek),"'",
                                     "ORDER BY Code ASC",
                                     sep = ""))
      
      preorder <- dbGetQuery(con, paste("SELECT Code, Value
                         FROM PREORDER
                         WHERE ReceivedWeeklyCode = ",
                                        "'",as.character(lastweek),"'",
                                        "ORDER BY Code ASC",
                                        sep = ""))
      
      inventory0 <- dbGetQuery(con, paste("SELECT Code, Value
                         FROM INVENTORY
                         WHERE WeeklyCode = ",
                                          "'",as.character(lastweek),"'",
                                          "ORDER BY Code ASC",
                                          sep = ""))
      
      # if ( nrow(sales) != nrow(preorder) || nrow(sales) != nrow(inventory0) || nrow(preorder) != nrow(inventory0) ) {
      #   stopApp( returnValue = "ALLAH SIFA VERSIN")
      # }
      
      seyhaninlaneti <- data.frame(Code = inventory0[1], WeeklyCode = curweek, Value = (inventory0[2] + preorder[2] - sales[2]))
    
      inventory1 <- rbind(seyhaninlaneti,data.frame(Code = inventory0[1],WeeklyCode = curweek2,Value = 0))
      
      names(inventory1) <- c("Code","WeeklyCode","Value")
      
      dbWriteTable(con, "INVENTORY", inventory1, append = TRUE)
      
      write.xlsx(seyhaninlaneti,paste0("Inventory for ",curweek,".xlsx"),row.names=FALSE)
      
      output$createinventory <- renderTable({seyhaninlaneti})
    }
    else if (q1[1,2] == 0) {
      q2 <- dbGetQuery(con, paste("SELECT WeeklyCode,SplitWeek
                         FROM DATE
                         WHERE ID = ",
                                  "'",as.character(q1[1,1]-1),"'",
                                  sep = ""))
      
      if (q2[1,2] == 0) {
        lastweek <- q2[1,1]
        
        sales <- dbGetQuery(con, paste("SELECT Code, Value
                         FROM SALES
                         WHERE WeeklyCode = ",
                                       "'",as.character(lastweek),"'",
                                       "ORDER BY Code ASC",
                                       sep = ""))
        
        preorder <- dbGetQuery(con, paste("SELECT Code, Value
                         FROM PREORDER
                         WHERE ReceivedWeeklyCode = ",
                                          "'",as.character(lastweek),"'",
                                          "ORDER BY Code ASC",
                                          sep = ""))
        
        inventory0 <- dbGetQuery(con, paste("SELECT Code, Value
                         FROM INVENTORY
                         WHERE WeeklyCode = ",
                                            "'",as.character(lastweek),"'",
                                            "ORDER BY Code ASC",
                                            sep = ""))
        
        # if ( nrow(sales) != nrow(preorder) || nrow(sales) != nrow(inventory0) || nrow(preorder) != nrow(inventory0) ) {
        #   stopApp( returnValue = "ALLAH SIFA VERSIN")
        # }
        
        inventory1 <- data.frame(inventory0[1],curweek,inventory0[2] + preorder[2] - sales[2])
        names(inventory1) <- c("Code","WeeklyCode","Value")
        dbWriteTable(con, "INVENTORY", inventory1, append = TRUE)
        
        write.xlsx(inventory1,paste0("Inventory for ",curweek,".xlsx"),row.names=FALSE)
        
        output$createinventory <- renderTable({inventory1})
        
      }
      else if (q2[1,2] == 1) {
        lastweek2 <- q2[1,1]
        
        lastweek <- dbGetQuery(con, paste("SELECT WeeklyCode
                                          FROM DATE
                                          WHERE ID = ",
                                          "'",as.character(q1[1,1]-2),"'",
                                          sep = ""))
        
        sales <- dbGetQuery(con, paste("SELECT Code, Value
                         FROM SALES
                         WHERE WeeklyCode = ",
                                       "'",as.character(lastweek),"'",
                                       "ORDER BY Code ASC",
                                       sep = ""))
        
        preorder <- dbGetQuery(con, paste("SELECT Code, Value
                         FROM PREORDER
                         WHERE ReceivedWeeklyCode = ",
                                          "'",as.character(lastweek),"'",
                                          "ORDER BY Code ASC",
                                          sep = ""))
        
        inventory0 <- dbGetQuery(con, paste("SELECT Code, Value
                         FROM INVENTORY
                         WHERE WeeklyCode = ",
                                            "'",as.character(lastweek),"'",
                                            "ORDER BY Code ASC",
                                            sep = ""))
        
        # if ( nrow(sales) != nrow(preorder) || nrow(sales) != nrow(inventory0) || nrow(preorder) != nrow(inventory0) ) {
        #   stopApp( returnValue = "ALLAH SIFA VERSIN")
        # }
        
        inventory1 <- data.frame(inventory0[1],curweek,inventory0[2] + preorder[2] - sales[2])
        names(inventory1) <- c("Code","WeeklyCode","Value")
        dbWriteTable(con, "INVENTORY", inventory1, append = TRUE)
        
        write.xlsx(inventory1,paste0("Inventory for ",curweek,".xlsx"),row.names=FALSE)
        
        output$createinventory <- renderTable({inventory1})
      }
    }
    
  })
  
  updateinventory <- observeEvent(input$invupdatebutton, {
    
    req(input$updateinvfile)
    inFile <- input$updateinvfile
    excl <- read_excel(inFile$datapath, 1)
    
    for (i in 1:nrow(excl)) {
      
      dbExecute(con, paste("UPDATE INVENTORY 
               SET Value = ","'",as.character(excl[i,3]),"'",
                           "WHERE Code = ","'",as.character(excl[i,1]),"'",
                           "AND WeeklyCode = ","'",as.character(excl[i,2]),"'",
                           sep = ""))  
    }
    
    out <- dbSendQuery(con, paste0("SELECT * FROM INVENTORY WHERE WeeklyCode = ","'",as.character(excl[1,2]),"'"))
    output$updateinventory <- renderTable({excl})
  })
  
  ### DATE VIEWING ####
  
  dateviewing <- observeEvent(input$viewButton,{
    month <- input$monthNo
    year <- input$yearNo
    normalweek <- input$normalweekNo
    
    d_view <- dbGetQuery(con, paste0("SELECT MonthlyCode, WeeklyCode, WeekNo AS CumulativeWeekNo, SplitDay 
                            FROM DATE WHERE 
                            MonthNo = ",as.character(month), 
                                     " AND YearNo = ",as.character(year),
                                     " AND NormalWeekNo = ",as.character(normalweek))) 
    
    output$viewtable1 <- renderTable({data.frame(d_view)})
  })
  
  dateviewing2 <- observeEvent(input$viewButton2,{
    date <- input$dateinput
    
    d_view <- dbGetQuery(con, paste0("SELECT MonthlyCode, WeeklyCode, WeekNo AS CumulativeWeekNo, SplitDay 
                            FROM DATE WHERE 
                            WeekStartDate = ","'",as.character(date),"'")) 
    
    output$viewtable2 <- renderTable({data.frame(d_view)})
  })
  
  dateviewing3 <- observeEvent(input$viewButton3,{
    
    wcode <- input$date3input
    
    d_view <- dbGetQuery(con, paste0("SELECT MonthlyCode, WeeklyCode, WeekNo AS CumulativeWeekNo, SplitDay 
                            FROM DATE WHERE 
                            WeeklyCode = ","'",as.character(wcode),"'"))
    
    output$viewtable3 <- renderTable({data.frame(d_view)})
  })
  
  ### SKU VIEWING ####
  skuviewing <- observeEvent(input$viewskubutton, {
    
    skuno <- input$viewskuinput
    
    d_view <- dbGetQuery(con, paste0("SELECT Code, Type, MOQ, LotSize, HoldCost, ServLevel, WeeklySigma
                            FROM SKU WHERE 
                            Code = ","'",as.character(skuno),"'")) 
    
    output$viewskutable <- renderTable({data.frame(d_view)})
  })
  
  ### ORDER UPLOADING ####
  addOrder <- observeEvent(input$addPreOrderButton ,{
    req(input$inputPreorderxlsx)
    inFile <- input$inputPreorderxlsx
    
    prOrData <- read_excel(inFile$datapath, 1)
    lastWeekCode <- prOrData[2,2]
    lastWeekInfo <- dbGetQuery(con,paste0("SELECT ID,SplitWeek FROM DATE WHERE WeeklyCode = ","'",lastWeekCode,"'"))
    
    
    if(lastWeekInfo[2] == 0)
    {
      currWeekInfo <- dbGetQuery(con,paste0("SELECT ID , SplitWeek, WeeklyCode FROM DATE WHERE ID = ", lastWeekInfo[1]+1))
     
       if (currWeekInfo[2] == 0) {
         currWeekCode <- currWeekInfo[3]
       }
       else if (currWeekInfo[2] == 2) {
         currWeekCode <- currWeekInfo[3]
         currWeekCode2 <- dbGetQuery(con,paste0("SELECT WeeklyCode FROM DATE WHERE ID = ", lastWeekInfo[1]+2))
       }
      
    }
    else if(lastWeekInfo[2] == 2)
    {
      currWeekInfo <- dbGetQuery(con,paste0("SELECT ID , SplitWeek, WeeklyCode FROM DATE WHERE ID = ", lastWeekInfo[1]+2))
      currWeekCode <- dbGetQuery(con,paste0("SELECT WeeklyCode FROM DATE WHERE ID = ", lastWeekInfo[1]+2))
    }
    
    outTable <- as.data.frame(prOrData[,1])
    outTable[2] <- currWeekCode
    outTable[3] <- as.data.frame(prOrData[,3])
    
    names(outTable)[1] = "Code"
    names(outTable)[2] = "ReceivedWeeklyCode"
    names(outTable)[3] = "Value"
    
    if (currWeekInfo[2] == 2) {
      outTable0 <- as.data.frame(prOrData[,1])
      outTable0[2] <- currWeekCode2
      outTable0[3] <- 0
      names(outTable0)[1] = "Code"
      names(outTable0)[2] = "ReceivedWeeklyCode"
      names(outTable0)[3] = "Value"
      outTable <- rbind(outTable,outTable0)
    }
    
    
    dbWriteTable(con,"PREORDER",outTable, append = TRUE)
    output$preOrderTable <- renderTable({outTable})
  })
  ### SERVER ADJUSTMENTS ####
  ## SERVER CALCULATE COEFFICIENTS ####
  calCoefQuery <- observeEvent(input$calCoefButton,{
    ##Aynı tarihleri girerse hata veriyor uyarı mesajı ayarla!!
    calCoefQuery <- querryHelper("SELECT", "SALES", input$calCoefFromMonth, input$calCoefFromYear, input$calCoefToMonth, input$calCoefToYear,con)
    
    splitted <- split(calCoefQuery, calCoefQuery$CoefType)
    for (i in 1:(lengths(splitted)[1] - 1))
    {
      x <- (splitted[[i]]$CoefType[1])
      
      if (grepl(x , 'D0')) {D0 <- splitted[[i]]}
      if (grepl(x , 'D1')) {D1 <- splitted[[i]]}
      if (grepl(x , 'D2')) {D2 <- splitted[[i]]}
      if (grepl(x , 'N0')) {N0 <- splitted[[i]]}
      if (grepl(x , 'N1')) {N1 <- splitted[[i]]}
      if (grepl(x , 'N2')) {N2 <- splitted[[i]]}
    }
    coefTable <- data.frame(CoefType = as.character(), firstWeek = as.numeric(),secondWeek = as.numeric(),thirdWeek = as.numeric(),fourthWeek = as.numeric(),fifthWeek = as.numeric(),stringsAsFactors = FALSE)
    
    coefTable[1,] <- CoefCalculator(CoefSpreader(D0),max(D0$WeekNo),"D0")
    coefTable[2,] <- CoefCalculator(CoefSpreader(D1),max(D1$WeekNo),"D1")
    coefTable[3,] <- CoefCalculator(CoefSpreader(D2),max(D2$WeekNo),"D2")
    coefTable[4,] <- CoefCalculator(CoefSpreader(N0),max(N0$WeekNo),"N0")
    coefTable[5,] <- CoefCalculator(CoefSpreader(N1),max(N1$WeekNo),"N1")
    coefTable[6,] <- CoefCalculator(CoefSpreader(N2),max(N2$WeekNo),"N2")
    
    
    oldcooeff <- as.data.frame(dbGetQuery(con,"SELECT * FROM COEFS"))
    
    for (i in 1:nrow(coefTable)) {
      dbGetQuery(con, paste("UPDATE COEFS 
               SET firstWeek = ", "'",as.character(coefTable[i,2]),"'",
                            "WHERE CoefType = ","'",as.character(coefTable[i,1]),"'", sep = ""))
      
      dbGetQuery(con, paste("UPDATE COEFS 
               SET secondWeek = ","'",as.character(coefTable[i,3]),"'",
                            "WHERE CoefType = ","'",as.character(coefTable[i,1]),"'", sep = ""))
      
      dbGetQuery(con, paste("UPDATE COEFS 
               SET thirdWeek = ","'",as.character(coefTable[i,4]),"'",
                            "WHERE CoefType = ","'",as.character(coefTable[i,1]),"'", sep = ""))
      
      dbGetQuery(con, paste("UPDATE COEFS 
               SET fourthWeek = ","'",as.character(coefTable[i,5]),"'",
                            "WHERE CoefType = ","'",as.character(coefTable[i,1]),"'", sep = ""))
      
      dbGetQuery(con, paste("UPDATE COEFS 
               SET fifthWeek = ","'",as.character(coefTable[i,6]),"'",
                            "WHERE CoefType = ","'",as.character(coefTable[i,1]),"'", sep = ""))
      
    }
    
    
    output$coefHeader1 <- renderUI({tags$h3("Old Coefficient Values")})
    output$coefHeader2 <- renderUI({tags$h3("New Coefficient Values")})
    output$oldCoef  <- DT::renderDataTable({oldcooeff}) 
    output$dataCoef <- DT::renderDataTable({coefTable})
    #output$dataCoef <- DT::renderDataTable({CoefSpreader(N1)})
  })
  ## SERVER SIGMA VALUES ####
  calSigQuery <- observeEvent(input$calSigButton,{
    ##Aynı tarihleri girerse hata veriyor uyarı mesajı ayarla!!
    calSigSales <- querryHelper("SELECT", "SALES", input$calSigFromMonth, input$calSigFromYear, input$calSigToMonth, input$calSigToYear,con)
    calSigFc <- querryHelper("SELECT", "FORECAST", input$calSigFromMonth, input$calSigFromYear, input$calSigToMonth, input$calSigToYear,con)
    salesRow <- calSigSales %>%
      group_by(Code) %>%
      count()
    fcRow <- calSigFc %>%
      group_by(Code) %>%
      count()
    
    corrFlag = FALSE
    if (nrow(salesRow) == nrow(fcRow)){ # Controlling sales and forecast data are compatible with each other.
      corrFlag = TRUE
      for (i in 1:nrow(salesRow))
      {
        if (salesRow[i,1] != fcRow[i,1] || salesRow[i,2] != fcRow[i,2])                       ## Update
        {
          corrFlag = FALSE              ## Update
        }
      }
    }
    
    if (corrFlag == FALSE) {            ## Update
      stop("This is an error message")  ## Update
    }
    
    orderedSales <- calSigSales %>% 
      mutate(weekNo = as.integer(substr(WeeklyCode,1,2)),monthNo = as.integer(substr(WeeklyCode,3,4)), yearNo = as.integer(substr(WeeklyCode,7,8))) %>% 
      arrange(Code,yearNo,monthNo,weekNo)
    
    orderedFc <- calSigFc %>% 
      mutate(weekNo = as.integer(substr(WeeklyCode,1,2)),monthNo = as.integer(substr(WeeklyCode,3,4)), yearNo = as.integer(substr(WeeklyCode,7,8))) %>% 
      arrange(Code,yearNo,monthNo,weekNo)
    
    Error = orderedSales[1]
    Error$SalesWeeklyCode = orderedSales$WeeklyCode
    Error$SalesValue = orderedSales$Value
    Error$FcCode = orderedFc$Code
    Error$FcWeeklyCode = orderedFc$WeeklyCode
    Error$FcValue = orderedFc$Value
    
    
    unq_sku <- unique(Error[1])
    
    for (i in 1:nrow(unq_sku)) {
      
      seyhaninlaneti <- Error[Error[1] == unq_sku[i,1],]
      
      seyhaninlaneti <- seyhaninlaneti %>% 
        mutate(error = SalesValue - FcValue)
      
      meanseyhan <- mean(seyhaninlaneti$error)
      stdseyhan <- sd(seyhaninlaneti$error)
      
      quantileseyhan <- qnorm(p = c(0.001, 0.999), mean = meanseyhan, sd = stdseyhan)
      
      seyhaninlaneti <- seyhaninlaneti[(quantileseyhan[1] < seyhaninlaneti[7] & seyhaninlaneti[7] < quantileseyhan[2]),]
      
      meanseyhan <- min(mean(seyhaninlaneti$error),0)
      stdseyhan <- sd(seyhaninlaneti$error)
      
      dbExecute(con, paste("UPDATE SKU 
               SET  WeeklySigma = ","'",as.character(stdseyhan),"'",
             " , WeeklyMean = ","'",as.character(meanseyhan),"'",        
             " WHERE Code = ","'",as.character(unq_sku[i,1]),"'", sep = ""))
      
    }
    
    out_render <- dbGetQuery(con, paste0("SELECT Code, WeeklySigma FROM SKU "))
    
    output$salesRow <- renderTable({out_render})
  })
  
  ## SERVER UPDATE HOLDING COST ####
  updates <- observeEvent(input$buttonupdates,{
    
    hold <- input$holdingcost
    sku <- input$skucode
    
    old <- dbGetQuery(con, paste("SELECT HoldCost 
                         FROM SKU
                         WHERE Code = ","'",as.character(sku),"'",
                                 sep = ""))
    
    dbGetQuery(con, paste("UPDATE SKU 
               SET HoldCost = ","'",as.character(hold),"'",
                          "WHERE Code = ","'",as.character(sku),"'", sep = ""))
    
    update_result <- data.frame(sku,old,hold)
    names(update_result) <- c("SKU Code","Old Holding Cost", "Updated Holding Cost")
    output$newholdingcostrow <- renderTable(({update_result
    }))
  } )
  ## SERVER UPDATE HOLDING COST via EXCEL ####
  updateHCxlsx <- observeEvent(input$buttonHCxlsx,{
    
    req(input$inputHCFile)
    inFile <- input$inputHCFile
    
    updateHC <- read_excel(inFile$datapath, 1)
    tot <- dbGetQuery(con, paste("SELECT HoldCost 
                         FROM SKU
                         WHERE Code = ","'",as.character(updateHC[1,1]),"'",
                                 sep = ""))
    dbExecute(con, paste("UPDATE SKU 
               SET HoldCost = ","'",as.character(updateHC[1,2]),"'",
                         "WHERE Code = ","'",as.character(updateHC[1,1]),"'", sep = ""))
    for (i in 2:nrow(updateHC)){
      old <- dbGetQuery(con, paste("SELECT HoldCost 
                         FROM SKU
                         WHERE Code = ","'",as.character(updateHC[i,1]),"'",
                                   sep = ""))
      
      dbExecute(con, paste("UPDATE SKU 
               SET HoldCost = ","'",as.character(updateHC[i,2]),"'",
                           "WHERE Code = ","'",as.character(updateHC[i,1]),"'", sep = ""))
      tot <- rbind(tot,old) 
    }
    
    
    update_result_xlsx <- data.frame(updateHC[,1],tot,updateHC[,2])
    names(update_result_xlsx) <- c("SKU Code","Old Holding Cost", "Updated Holding Cost")
    output$tableHCxlsx <- DT::renderDataTable(({update_result_xlsx}))
  })
  ## SERVER INDIVIDUAL SERVICE LEVELS ####
  
  calIndvQuery <- observeEvent(input$individualButton,{
    
    skucode <-  input$indvSKU 
    newlevel <- input$newServiceLevel
    
    
    old <- dbGetQuery(con, paste("SELECT Code, ServLevel
                         FROM SKU
                         WHERE Code = ","'",as.character(skucode),"'",
                                 sep = ""))
    
    dbExecute(con, paste("UPDATE SKU 
               SET ServLevel = ","'",as.character(newlevel),"'",
                         "WHERE Code = ","'",as.character(skucode),"'", sep = ""))
    
    new <- dbGetQuery(con, paste("SELECT Code, ServLevel
                         FROM SKU
                         WHERE Code = ","'",as.character(skucode),"'",
                                 sep = ""))
    
    output$oldIndv <- renderTable({old})
    output$newIndv <- renderTable({new})
    
  })
  
  
  
  ## SERVER INDIVIDUAL SERVICE LEVELS via EXCEL ####
  calIndvXlsxQuery <- observeEvent(input$individualButtonxlsx,{
    
    req(input$inputSLFile)
    inFile <- input$inputSLFile
    
    updateSL <- read_excel(inFile$datapath, 1)
    
    tot2 <- dbGetQuery(con, paste("SELECT ServLevel
                         FROM SKU
                         WHERE Code = ","'",as.character(updateSL[1,1]),"'",
                                  sep = ""))
    
    dbExecute(con, paste("UPDATE SKU 
               SET ServLevel = ","'",as.character(updateSL[1,2]),"'",
                         "WHERE Code = ","'",as.character(updateSL[1,1]),"'", sep = ""))
    
    for (i in 2:nrow(updateSL)){
      old2 <- dbGetQuery(con, paste("SELECT  ServLevel
                         FROM SKU
                         WHERE Code = ","'",as.character(updateSL[i,1]),"'",
                                    sep = ""))
      
      dbExecute(con, paste("UPDATE SKU 
               SET ServLevel = ","'",as.character(updateSL[i,2]),"'",
                           "WHERE Code = ","'",as.character(updateSL[i,1]),"'", sep = ""))
      tot2 <- rbind(tot2,old2)
    }
    update_SL_xlsx <- data.frame(updateSL[,1],tot2,updateSL[,2])
    names(update_SL_xlsx) <- c("SKU Code","Old Service Levels", "Updated Service Levels")
    output$indServXlsx <- DT::renderDataTable(({update_SL_xlsx}))
    
    
  })
  ## SERVER UPDATE WEEKLY FORECAST ####
  updateforecast <- observeEvent(input$updateforecastbutton,{
    
    req(input$fileupdateforecast)
    inFile <- input$fileupdateforecast
    dd <- read_excel(inFile$datapath, 1)
    
    for (i in 1:nrow(dd)) {
      
      dbExecute(con, paste0(
                "UPDATE FORECAST 
                SET Value = ",as.character(dd[i,3]), 
                " WHERE Code = ","'",as.character(dd[i,1]),"'",
                " AND WeeklyCode = ","'",as.character(dd[i,2]),"'")
                )
    }
    
    output$updateforecastoutput <- DT::renderDataTable(({dd}))
  })
  
  ## SERVER UPDATE SALES ####
  updatesales <- observeEvent(input$updatesalesbutton,{
    
    req(input$fileupdatesales)
    inFile <- input$fileupdatesales
    dd <- read_excel(inFile$datapath, 1)
    
    for (i in 1:nrow(dd)) {
      
      dbExecute(con, paste0(
        "UPDATE SALES 
                SET Value = ",as.character(dd[i,3]), 
        " WHERE Code = ","'",as.character(dd[i,1]),"'",
        " AND WeeklyCode = ","'",as.character(dd[i,2]),"'")
      )
    }
    
    output$updatesalesoutput <- DT::renderDataTable(({dd}))
  })
  
  ### DISCONNECTING ####
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}
shinyApp(ui = ui,server = server)
