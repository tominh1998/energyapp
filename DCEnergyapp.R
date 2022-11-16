library(shiny)
library(readr)
library(tidyverse)
library(dplyr)
energy <- read_rds("C:/Users/toanh/OneDrive/DATA-613/HW/hw06-tominh1998/dc_energy/data/energy_year.rds")
energy %>%
  mutate(
    Report_Year = as.factor(Report_Year),
    Type_SS = as.factor(Type_SS),
    Type_EPA = as.factor(Type_EPA),
    Metered_Energy = as.factor(Metered_Energy),
    Metered_Water = as.factor(Metered_Water),
    Era = case_when(Built<1900~'Pre-1900',
                    Built<1951~'Early-Mid 20th',
                    Built<2000~'Late 20th',
                    Built<2011~'Aughts',
                    TRUE~'Teens and later'),
    Era = as.factor(Era)
  ) -> energy
ui <- fluidPage(theme = bslib::bs_theme(version = 5, bootswatch = "default"),
                titlePanel("Analyzing Building Energy Performance Data"),
                tabsetPanel(tabPanel("Introduction",
                                     titlePanel("This app supports exploratory data analysis of Building Energy Performance in Washington, DC."),

                                     tags$p("The data used in this app are a subset of a dataset used by the city of Washington, DC Department of Energy & Environment (DOEE). It is data on public and private building energy performance disclosed under the Building Energy Performance Standards (BEPS) Program."),
                                    
                                     tags$p("The data set for the app was downloaded in February 2022 from"),
                                   
                                     tags$a("DC Open Data on Building Energy Performance", href = "https://opendata.dc.gov/datasets/DCGIS::building-energy-performance/about"),
                                    
                                     tags$p("The data set was filtered to remove data on non-compliant reports and columns not of interest. The data was further reduced by eliminating records where there appeared to be large discrepancies between the square footage of the building recorded for tax purposes and as reported by the owners in their reports. These discrepancies created extreme values that could hinder the analysis. Finally, the variables were renamed to be more human readable. The meta-data definitions can be found at:"),
                                   
                                     tags$a("Building Energy Performance Metadata", href = "https://www.arcgis.com/sharing/rest/content/items/10f4f09fc5684d9988ae83ae4cca8b70/info/metadata/metadata.xml?format=default&output=html"),
                                    
                                     tags$p("This data is meant to be in accordance with the US Government Energy Star Program. For help in interpreting the variables suggest:"),
                                   
                                     tags$a("EPA Energy Star Program", href = "EPA Energy Star Program")),
                              tabPanel("Univariate",
                                       sidebarLayout(
                                         sidebarPanel(
                                           varSelectInput("var1", "Variable?", data = energy),
                                           checkboxGroupInput("reportyears", "Which Report Years?",
                                                              choices = c("2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"),selected = "2020"),
                                           checkboxInput("flip","Flip Coordinates on Factors?"),
                                           checkboxInput("log", "Log Transform?"),
                                           sliderInput("num1", "Number of Bins?", value = 40, min = 1, max = 100),
                                           numericInput("null", "Null Value", value = 0, min = 0),
                                           tableOutput("ttest")
                                         ),
                                         mainPanel(
                                           plotOutput("plot1")
                                       )
                                       )
                                       ),
                              tabPanel("Bivariate",
                                       sidebarLayout(
                                         sidebarPanel(
                                           varSelectInput("var2", "X Variable?", data = energy, selected = "Source_EUI"),
                                           checkboxInput("log2", "Log Transform?"),
                                           varSelectInput("var3", "Y Variable?", data = energy, selected = "Site_EUI"),
                                           checkboxInput("log3", "Log Transform?"),
                                           checkboxInput("fit", "Fit OLS model?"),
                                           checkboxGroupInput("reportyears2", "Which Report Years?",
                                                              choices = c("2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"), selected = "2020")),
                                         mainPanel(
                                           plotOutput("plot2")))),
                              tabPanel("Spreadsheet",
                                       checkboxInput("numfac", "Numeric or Factor Only?"),
                                       dataTableOutput("datasheet")
                                       )
                            )
                )
            
server <- function(input, output, session) {
  output$energy <- renderTable({
    head(energy)
  })
  df<-reactive({
    validate(
      need(input$reportyears != "","Please choose a year"))
    energy %>%
      filter(Report_Year==input$reportyears)%>%
      filter(input$var1!="NA")})
  output$ttest <- renderTable({
    validate(
      need(class(energy[[input$var1]])!="factor","Variable is not numeric"))
    if (!!input$log == TRUE) {
      validate(
        need(df()[[input$var1]]>0,"Variable has one or more values <= 0 so can't be logged.")
      )
      ttest1<- t.test(log(df()[[input$var1]]), mu = input$null,conf.level=0.95)
     data.frame("P-value"=ttest1$p.value,
                "Estimate"=ttest1$estimate,
                "95% Lower"=ttest1$conf.int[[1]],
                "95% Upper"=ttest1$conf.int[[2]], check.names = FALSE)}
    else {
      ttest2<-t.test(df()[[input$var1]], mu = input$null,conf.level=0.95)
      data.frame("P-value"=ttest2$p.value,
                 "Estimate"=ttest2$estimate,
                 "95% Lower"=ttest2$conf.int[[1]],
                 "95% Upper"=ttest2$conf.int[[2]], check.names = FALSE)
    }
  })
  output$plot1 <- renderPlot({
      validate(
        need(class(df()[[input$var1]])=="numeric"|class(df()[[input$var1]])=="factor",'Please Choose Numeric or Factor Variable'))
      if (!!input$log == TRUE) {
        validate(
          need(class(df()[[input$var1]])!="factor","Can't Log Transform X"),
          need(df()[[input$var1]]>0,"Variable has one or more values <= 0 so can't be logged.")
        )
        ggplot(df(),aes(x = !!input$var1)) +
          facet_wrap(df()$Report_Year) +
          scale_x_continuous(trans='log10') +
          xlab(paste0("Log(",input$var1,")")) ->plotout1
      }
      else {
        ggplot(df(),aes(x = !!input$var1)) + facet_wrap(df()$Report_Year) ->plotout1
      }
    if (class(df()[[input$var1]])=="numeric") {
      plotout1 +
        geom_histogram(bins = input$num1)
    }
    else {
      if (!!input$flip == TRUE){
        plotout1 +
          geom_bar() +
          coord_flip()
      }
      else{
          plotout1 +
            geom_bar() 
      }
      } 
  })
  df2<-reactive({
      validate(
        need(input$reportyears2 != "","Please choose a year"))
      energy %>%
        filter(Report_Year==input$reportyears2)%>%
        group_by(Report_Year) %>%
        filter(input$var2!="NA")%>%
        filter(input$var3!="NA")})
  output$plot2 <- renderPlot({
      validate(
        need(class(df2()[[input$var2]])=="numeric"|class(df2()[[input$var2]])=="factor",'Please Choose Numeric or Factor for X Variable'),
        need(class(df2()[[input$var3]])=="numeric"|class(df2()[[input$var3]])=="factor",'Please Choose Numeric or Factor for Y Variable'))
      if (!!input$log2 == TRUE && !!input$log3 == TRUE) {
        validate(
          need(class(df2()[[input$var2]])!="factor","Can't Log Transform X"),
          need(class(df2()[[input$var3]])!="factor","Can't Log Transform Y"),
          need(df2()[[input$var2]]>0,"Variable X has one or more values <= 0 so can't be logged."),
          need(df2()[[input$var3]]>0,"Variable Y has one or more values <= 0 so can't be logged.")
        )
        ggplot(df2(),aes(x = !!input$var2,y= !!input$var3)) +
          scale_x_continuous(trans='log10') +
          scale_y_continuous(trans='log10') +
          xlab(paste0("Log(",input$var2,")")) +
          ylab(paste0("Log(",input$var3,")")) ->plotout2
      }
      else {
        if (!!input$log2 == TRUE && !!input$log3 == FALSE){
          validate(
            need(class(df2()[[input$var2]])!="factor","Can't Log Transform X"),
            need(df2()[[input$var2]]>0,"Variable X has one or more values <= 0 so can't be logged.")
          )
          ggplot(df2(),aes(x = !!input$var2,y= !!input$var3)) +
            scale_x_continuous(trans='log10') +
            xlab(paste0("Log(",input$var2,")"))  ->plotout2
        }
        else {
          if (!!input$log2 == FALSE && !!input$log3 == TRUE) {
            validate(
              need(class(df2()[[input$var3]])!="factor","Can't Log Transform Y"),
              need(df2()[[input$var3]]>0,"Variable Y has one or more values <= 0 so can't be logged.")
            )
            ggplot(df2(),aes(x = !!input$var2,y= !!input$var3)) +
              scale_y_continuous(trans='log10') +
              ylab(paste0("Log(",input$var3,")"))  ->plotout2
          }
          else {
            ggplot(df2(),aes(x = !!input$var2,y= !!input$var3))  -> plotout2
          }
        }
      }
      if (class(df2()[[input$var2]])=="numeric" && class(df2()[[input$var3]])=="numeric") {
          plotout2 +
          geom_point(aes(color = df2()$Report_Year)) +
          guides(color=guide_legend(title="Year")) -> plotout3
        if(!!input$fit == TRUE) {
          plotout3 +
            geom_smooth(method = lm)
        }
        else {
          plotout3
        }
      }
      else {
        if (class(df2()[[input$var2]])=="numeric" && class(df2()[[input$var3]])=="factor"||class(df2()[[input$var2]])=="factor" && class(df2()[[input$var3]])=="numeric"){
          plotout2 +
            geom_boxplot() 
        }
        else{
          plotout2 +
            geom_jitter() 
        }
      }
    })
  test <- function(data){
    renderDataTable(data,options=list(pageLength = 20))
  }
  output$datasheet <- renderDataTable(
    energy,
    options = list(pageLength = 20)
    )
}
shinyApp(ui, server)
