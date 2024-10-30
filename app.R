####################################################################
##                                                                ##
##                  World Development Indicator                   ##
##                          Version 1.3                           ##
##                     by: firman triatmojo                       ##
##                                                                ##
####################################################################

#Confidential, distribution is not allowed without my permission#

library(shiny)
library(shinydashboard)
library(dygraphs)
library(dplyr)
library(shinyBS)
library(tidyr)
library(highcharter)
library(googleVis)
library(data.table)
library(xts)
library(DT)
library(ggplot2)

countries <- read.csv(file = "./countries.csv", header = T)
indicator <- read.csv(file = "./indicator.csv", header = T)
income <- read.csv(file = "./income_cat.csv", header = T)
region <- read.csv(file = "./reg_name.csv", header = T)
indicator_name <- read.csv(file = "./indicator_name.csv", header = T)

#Define User Interface
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "World Development Indicator", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Countries", tabName = "timeseries", icon = icon("map-pin")),
      menuItem("Region Group", tabName = "myreg", icon = icon("map-o")),
      menuItem("Income Group", tabName = "inc", icon = icon("dollar")),
      menuItem("Map View", tabName = "map", icon = icon("location-arrow")),
      menuItem("Motion Chart", tabName = "motionchart", icon = icon("circle")),
      menuItem("Correlation", tabName = "corr", icon = icon("th-large")),
      menuItem("Model", tabName = "model", icon = icon("tasks")),
      menuItem("Historical Data set", tabName = "dat", icon = icon("database")),
      menuItem("About", tabName = "abt", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "timeseries",
              fluidPage(
                column(12, 
                       box(title = "Input", status = "info", solidHeader = T, width = 12, collapsible = T,
                           selectizeInput("country", "Select country:", countries, multiple=T, selected = c("Japan", "India", "China")),
                           selectizeInput("indicat", "Select indicator:", indicator, selected = "Forest rents (% of GDP)")
                       ),
                       tabBox(title = tagList(shiny::icon("bar-chart"), "Charts"),
                              width = 12, #status = "info", #height = 500,
                              tabPanel("WDI Changes Over Years",
                                       fluidPage(
                                         column(12, br(), dygraphOutput("ts")),
                                         column(12, br(), offset = 0, actionButton("lagi", "more"), align = "right"),
                                         column(12, bsModal("modallagi", "Showing All Value", "lagi", size = "large",
                                                            dataTableOutput("tsall")))
                                       )),
                              tabPanel("Boxplot",
                                       fluidPage(
                                         column(12, br(), highchartOutput("bxpl")),
                                         column(12, br(), offset = 0, actionButton("lagix", "more"), align = "right"),
                                         column(12, bsModal("modallagix", "Showing All Value", "lagix", size = "large",
                                                            dataTableOutput("tsallx")))
                                       ))
                       ))
              )),
      
      tabItem(tabName = "map",
              fluidPage(
                column(12,
                       box(title = "Input", status = "success", solidHeader = T, width = 12, collapsible = T,
                           selectizeInput("IndiCategory", "Select indicator:", indicator, selected = "CO2 intensity (kg per kg of oil equivalent energy use)"),
                           sliderInput("year", "Year:", min = 1960, max =2015, value =2002, animate = T)
                       ))
              ),
              fluidPage(
                column(12,
                       box(title = "Map View", status = "success", solidHeader = T,width = 12,#collapsible = T,
                           collapsible = F,
                           column(12, htmlOutput("geoChart", width = "95%"))
                           
                       ))
              ),
              
              fluidPage(
                column(12, 
                       box(width = 12, status= "success", collapsible = T,
                           column(12, plotOutput("topin")),
                           column(12, offset = 0, actionButton("more", "more"), align = "right"),
                           column(12, bsModal("modalExample", "Showing All Countries", "more", size = "large",
                                              dataTableOutput("topall")))
                           ))
              )
      ),
      
      tabItem(tabName = "motionchart",
              fluidPage(
                column(12,
                       box(title = "Input", status = "danger", solidHeader = T, width = 12, collapsible = T,
                           selectizeInput("varx", "Select X variable:", indicator, selected = "CO2 intensity (kg per kg of oil equivalent energy use)"),
                           selectizeInput("vary", "Select Y variable:", indicator, selected = "Total natural resources rents (% of GDP)"),
                           selectizeInput("size", "Select size variable:", indicator, selected = "CO2 emissions (kt)"),
                           sliderInput("year1", "Year",
                                       min = 1960, max =2015,
                                       value =2000, animate = T)
                           
                       ))
              ),
              fluidPage(
                column(12,
                       box(title = "Bubble Chart", status = "danger", solidHeader = T, 
                           width = 12, height = 700, collapsible = F,
                           htmlOutput("bubble")
                       ))
              )
      ),
      
      tabItem(tabName = "corr",
              fluidPage(
                column(12,
                       box(title = "Input", width = 12, solidHeader = T, collapsible = T, status = "warning",
                           selectizeInput("corcou", "Select country:", countries, selected = "Indonesia")
                           ))
              ),
              fluidPage(
                column(12,
                       tabBox(title = tagList(shiny::icon("gear"), "Correlation"), width = 12, height = 600,
                              tabPanel("Correlation Heatmap",
                                       fluidPage(
                                         column(12, plotOutput("gcorr"))
                                       )),
                              tabPanel("Correlation Data",
                                       fluidPage(
                                         column(12, dataTableOutput("tcorr"))
                                       ))
                           ))
              )
      ),
      
      tabItem(tabName = "model",
              fluidPage(
                column(12,
                       box(title = "Input", status = "warning", solidHeader = T, width = 12, collapsible = T,
                           selectizeInput("neg", "Select country:", countries, selected = "Indonesia"),
                           selectizeInput("dv", "Select dependent variable:", indicator, selected = "CO2 intensity (kg per kg of oil equivalent energy use)"),
                           selectizeInput("iv", "Select independent variable:", indicator, selected = "Forest rents (% of GDP)")
                       ))
              ),
              
              fluidPage(
                column(12,
                       tabBox(title = tagList(shiny::icon("gear"), "Model"), width = 12,
                              tabPanel("Summary ", icon = icon("chevron-down"),
                                       fluidPage(
                                         column(12, br(), verbatimTextOutput("summary"))
                                       )),
                              tabPanel("Histogram ", icon = icon("chevron-down"),
                                       fluidPage(
                                         column(6, highchartOutput("distPlot_dv")),
                                         column(6, highchartOutput("distPlot_iv"))
                                       )),
                              tabPanel("Scatter Plot ", icon = icon("chevron-down"),
                                       fluidPage(
                                         column(12, offset = 1, plotOutput("scatt", width = "80%"))
                                       )),
                              tabPanel("Model ", icon = icon("chevron-down"),
                                       fluidPage(
                                         column(12, verbatimTextOutput("model"))
                                       )),
                              tabPanel("Residuals ", icon = icon("chevron-down"),
                                       fluidPage(
                                         column(12, offset = 1,
                                                plotOutput("residuals_hist", width = "80%"),
                                                plotOutput("residuals_scatter", width = "80%"),
                                                plotOutput("residuals_qqline", width = "80%")
                                                )
                                       ))
                       ))
              )
      ),
      
      tabItem(tabName = "dat",
              fluidPage(
               column(12,
                      box(title = "Data Set of WDI", status = "info", solidHeader = T, width = 12, collapsible = F,
                          dataTableOutput("hist")
                          ))
             )
      ),
      
      tabItem(tabName = "abt",
              fluidPage(
                column(12,
                       box(
                         title = "About World Development Indicators", width = 12, solidHeader = T, collapsible = T, status = "primary",
                         h5("World Development Indicators (WDI) is the primary World Bank collection of development indicators, 
                            compiled from officially-recognized international sources. It presents the most current and accurate 
                            global development data available, and includes national, regional and global estimates. 
                            This statistical reference includes over 800 indicators covering more than 150 economies. 
                            The annual publication is released in April of each year. The online database is updated 
                            three times a year."), br(),
                         h5("The World Bank’s Open Data site provides access to the WDI database free of charge to all users. 
                            A selection of WDI data is featured at data.worldbank.org. Users can browse the data by Country, 
                            Indicators, Topics, and Data Catalog. WDI is the first source listed in the catalog and can be accessed directly via Data Bank.
                            The Economic and Social Data Service (ESDS) provides the macro-economic datasets free of charge for registered members of UK higher and further education institutions.")
                       ),
                       box(
                         title = "Metadata", solidHeader = T, collapsible = T, status = "primary", width = 12,
                         column(6, selectizeInput("metacou", "Country:", countries, multiple=F), br(),
                                verbatimTextOutput("couver"),
                                verbatimTextOutput("couver2"),
                                verbatimTextOutput("couver3"),
                                verbatimTextOutput("couver4")),
                         column(6, selectizeInput("metaind", "Indicator code:", indicator_name, multiple=F), br(),
                                verbatimTextOutput("indver"),
                                verbatimTextOutput("indver2"),
                                verbatimTextOutput("indver3"))
                       ),
                       box(
                         title = "Copyrights",
                         status = "primary",
                         solidHeader = T,
                         collapsible = F,
                         width = 12, #height = 350,
                         footer = h5('Distribution is not allowed. Contact me at', 
                                     a('f.triatmojo@gmail.com', 
                                       href = 'mailto:?Subject='), 
                                     ' for more informations.'
                                     ),
                         h5('Designed and developed by:'), 
                         br(), br(),
                         textOutput("frm"),
                         tags$head(tags$style("#frm{color: red; font-size: 25px; font-style: italic;}"
                                              )),
                         h5('- Data Science Enthusiast'),
                         br(), br(), br()
                       ))
              )
      ),
      
      tabItem(tabName = "inc",
        fluidPage(
          column(12, 
                 box(title = "Input", status = "info", solidHeader = T, width = 12, collapsible = T,
                     selectizeInput("income", "Select income groups:", c("All", income), multiple=F),
                     selectizeInput("inccat", "Select indicator:", indicator, selected = "Adjusted savings: energy depletion (% of GNI)")
                 ),
                 tabBox(title = tagList(shiny::icon("bar-chart"), "Charts"),
                        width = 12, #status = "info", #height = 500,
                        tabPanel("WDI Changes Over Years",
                                 fluidPage(
                                   column(12, br(), dygraphOutput("tsinc")),
                                   column(12, br(), offset = 0, actionButton("lagiinc", "more"), align = "right"),
                                   column(12, bsModal("modalinc", "Showing All Value", "lagiinc", size = "small",
                                                      dataTableOutput("tsallinc")))
                                 )),
                        tabPanel("Boxplot",
                                 fluidPage(
                                   column(12, br(), highchartOutput("bxplinc")),
                                   column(12, br(), offset = 0, actionButton("lagixinc", "more"), align = "right"),
                                   column(12, bsModal("modallagixinc", "Showing All Value", "lagixinc", size = "small",
                                                      dataTableOutput("tsallxinc")))
                                 ))
                 ))
        )
      ),
      
      tabItem(tabName = "myreg",
              fluidPage(
                column(12, 
                       box(title = "Input", status = "info", solidHeader = T, width = 12, collapsible = T,
                           selectizeInput("regi", "Select region:", c("All", region), multiple=F),
                           selectizeInput("regcat", "Select indicator:", indicator, selected = "Adjusted savings: energy depletion (% of GNI)")
                       ),
                       tabBox(title = tagList(shiny::icon("bar-chart"), "Charts"),
                              width = 12, #status = "info", #height = 500,
                              tabPanel("WDI Changes Over Years",
                                       fluidPage(
                                         column(12, br(), dygraphOutput("tsreg")),
                                         column(12, br(), offset = 0, actionButton("lagireg", "more"), align = "right"),
                                         column(12, bsModal("modalreg", "Showing All Value", "lagireg", size = "small",
                                                            dataTableOutput("tsallreg")))
                                       )),
                              tabPanel("Boxplot",
                                       fluidPage(
                                         column(12, br(), highchartOutput("bxplreg")),
                                         column(12, br(), offset = 0, actionButton("lagixreg", "more"), align = "right"),
                                         column(12, bsModal("modallagixreg", "Showing All Value", "lagixreg", size = "small",
                                                            dataTableOutput("tsallxreg")))
                                       ))
                       ))
              )
      )
    )
  ))

#Define server logic
server <- function(input, output) {
  #============================================================load the data
  mydata <- reactive({
    fread("./mydata.csv")
  })
  region <- reactive({
    fread("./region.csv")
  })
  income <- reactive({
    fread("./income.csv")
  })
  meta_country <- reactive({
    fread("./meta_country.csv")
  })
  meta_indicator <- reactive({
    fread("./meta_indicator.csv")
  })
  indicator_name <- reactive({
    fread("./meta_indicator.csv")
  })
  
  #============================================================1: TIMESERIES
  #1st plot timeseries
  output$ts <- renderDygraph({
    if(length(input$country) == 1) {
      y = mydata() %>% filter(`Country Name` %in% input$country, `Indicator Name` %in% input$indicat)
      y = t(y)
      y = y[5:nrow(y),]
      y=as.numeric(y)
      year=1960:2015
      yc=cbind(y,year)
      
      yc = as.data.frame(yc)
      yc$year = as.Date(as.character(yc$year), "%Y")
      colnames(yc)=c(input$country,"year")
      ts=xts(yc[,1],order.by =yc[,"year"])
      colnames(ts) <- input$country
      rm(y,year,yc)
      dygraph(ts, main = input$indicat)%>% dyRangeSelector()%>%
        #dyOptions(colors = RColorBrewer::brewer.pal(1, "Set2")) %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE)
    }
    else {
      y = mydata() %>% filter(`Country Name` %in% input$country, `Indicator Name` %in% input$indicat)
      y <- y[,-c(2,3,4)]
      y = t(y)
      ts = y[2:nrow(y),]
      colnames(ts) <- y[1,]
      
      year=1960:2015
      yc=cbind(ts,year)
      
      yc = as.data.frame(yc)
      yc$year = as.Date(as.character(yc$year), "%Y")
      ts=xts(yc,order.by =yc[,"year"])
      rm(y,year,yc)
      dygraph(ts, main = input$indicat)%>% dyRangeSelector()%>%
        #dyOptions(colors = RColorBrewer::brewer.pal(1, "Set2")) %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE,
                    highlightSeriesOpts = list(strokeWidth = 3)
                    )
    }
  })
  #1st plot boxplot
  output$bxpl <- renderHighchart({
    data2 <- mydata()[,-c(1,2,3,4)]
    data2 <- data.frame(sapply(data2, function(x) as.numeric(as.character(x))))
    data1 <- mydata()[,c(1,3)]
    data <- as.data.frame(cbind(data1, data2))
    rm(data1, data2)
    #filter
    y <- data %>% filter(`Country Name` %in% input$country, `Indicator Name` %in% input$indicat)
    y1 <- y[,-2]
    #now
    zz <- gather(y1, "year", "indicator", 2:57) %>% select(-year)
    hcboxplot(x = zz[,2], var = zz[,1], outliers = T) %>% 
      hc_chart(type = "column") %>%
      hc_title(text = input$indicat)
  })
  #1st cont
  output$tsall <- renderDataTable({
    y = mydata() %>% filter(`Country Name` %in% input$country, `Indicator Name` %in% input$indicat)
    y <- y[,-c(2,3,4)]
    datatable(y, caption = input$indicat,
              options = list(deferRender = T, paging = T, searching = T, scroller = T, scrollX = T, scrollY = T))
  })
  output$tsallx <- renderDataTable({
    y = mydata() %>% filter(`Country Name` %in% input$country, `Indicator Name` %in% input$indicat)
    y <- y[,-c(2,3,4)]
    datatable(y, caption = input$indicat,
              options = list(deferRender = T, paging = T, searching = T, scroller = T, scrollX = T, scrollY = T))
  })
  
  #============================================================2: MAP VIEW
  #2nd geo plot
  output$geoChart <- renderGvis({
    data <- mydata() %>% filter(`Indicator Name` %in% input$IndiCategory)
    data <- data[,c("Country Name", input$year)]
    colnames(data) <- c("countries", "indicator")
    gvisGeoChart(data, locationvar='countries', colorvar="indicator"
                 ,options=list(displayMode="Markers",
                               colorAxis="{colors:['purple', 'red', 'orange', 'grey']}",
                               backgroundColor="lightblue",width=1245, height=700)) #1195
    
  })
  #top10 cate
  output$topin <- renderPlot({
    top <- mydata() %>% filter(`Indicator Name` %in% input$IndiCategory)
    top <- top[,c("Country Name", input$year)]
    colnames(top) <- c("countries", "indicator")
    top <- top %>% arrange(desc(indicator)) %>% slice(1:10)
    top$indicator <- as.numeric(top$indicator)
    #plot
    library(ggplot2)
    ggplot2::ggplot(top, aes(reorder(countries, indicator), indicator, label = indicator)) +
      geom_bar(stat = "identity", fill = "#00cd00") +
      geom_text(size = 3) +
      ggtitle(paste0("Top 10 Indicator of ", input$InciCategory)) +
      xlab("Countries") + ylab(paste("Indicator Value in ", input$year, sep = "")) +
      scale_alpha_discrete(range = c(0.35, 0.9)) +
      theme_minimal() +
      coord_flip() + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  })
  #all val
  output$topall <- renderDataTable({
    top <- mydata() %>% filter(`Indicator Name` %in% input$IndiCategory)
    top <- top[,c("Country Name", input$year)]
    colnames(top) <- c("Countries", "Indicator")
    top <- top %>% arrange(desc(Indicator)) #%>% slice(1:10)
    top$Indicator <- as.numeric(top$Indicator)
    #table
    datatable(top, caption = input$IndiCategory,
              options = list(deferRender = T, paging = T, searching = T)) %>%
      formatStyle(
        'Indicator',
        background = styleColorBar(top$Indicator, '#00cd00'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  #============================================================3: MOTION CHART
  #3rd plot bubble
  output$bubble <- renderGvis({
    #df1
    bub1 <- mydata() %>% filter(`Indicator Name` %in% input$varx)
    bub1 <- bub1[, c("Country Name", input$year1)]
    colnames(bub1) <- c("Countries", input$year1)
    #df2
    bub2 <- mydata() %>% filter(`Indicator Name` %in% input$vary)
    bub2 <- bub2[, c("Country Name", input$year1)]
    colnames(bub2) <- c("Countries", input$year1)
    #df3
    bub3 <- mydata() %>% filter(`Indicator Name` %in% input$size)
    bub3 <- bub3[, c("Country Name", input$year1)]
    colnames(bub3) <- c("Countries", input$year1)
    #df4
    region <- region()
    colnames(region) <- c("Countries", "Region")
    #merge
    mymerge <- function(x, y){
      df <- merge(x, y, by= "Countries", all.x= TRUE)
      return(df)
    }
    bub <- Reduce(mymerge, list(bub1, bub2, region, bub3))
    colnames(bub) <- c("Countries", "X", "Y", "Region", "Size")
    #fix
    rm(bub1,bub2,bub3,region)
    #gvis
    gvisBubbleChart(bub, idvar = "Countries", xvar = "X", yvar = "Y",
                    colorvar = "Region", sizevar = "Size", 
                    options = list(width = 1258, height = 600, title = "Scatter plot for Two different Indicators"))
  })
  
  #============================================================4: CORRELATION
  #4th correlation matrix
  output$gcorr <- renderPlot({
    y <- mydata()[,-c(2,3)] %>% filter(`Country Name` %in% input$corcou)
    yx <- y[,-c(1,2)]
    yx <- data.frame(sapply(yx, function(x) as.numeric(as.character(x))))
    yx <- t(yx)
    #yx <- as.numeric(yx)
    colnames(yx) <- y[,2]
    cormat <- round(cor(yx), 2)
    cormat <- cormat[1:10, 1:10]
    rm(y,yx)
    
    get_lower_tri<-function(cormat){
      cormat[upper.tri(cormat)] <- NA
      return(cormat)
    }
    get_upper_tri <- function(cormat){
      cormat[lower.tri(cormat)]<- NA
      return(cormat)
    }
    upper_tri <- get_upper_tri(cormat)
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
    reorder_cormat <- function(cormat){
      dd <- as.dist((1-cormat)/2)
      hc <- hclust(dd)
      cormat <-cormat[hc$order, hc$order]
    }
    cormat <- reorder_cormat(cormat)
    upper_tri <- get_upper_tri(cormat)
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
    #plot
    library(ggplot2)
    ggheatmap <- ggplot2::ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ # minimal theme
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 10, hjust = 1))+
      coord_fixed() +
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = 8, barheight = 1,
                                   title.position = "top", title.hjust = 0.5))
    print(ggheatmap)
  }, height = 500)
  #4th cont
  output$tcorr <- renderDataTable({
    y <- mydata()[,-c(2,3)] %>% filter(`Country Name` %in% input$corcou)
    yx <- y[,-c(1,2)]
    yx <- data.frame(sapply(yx, function(x) as.numeric(as.character(x))))
    yx <- t(yx)
    #yx <- as.numeric(yx)
    colnames(yx) <- y[,2]
    cormat <- round(cor(yx), 2)
    rm(y,yx)
    datatable(cormat, caption = paste0("Correlation Matrix of WDI in ", input$corcou),
              options = list(deferRender = T, paging = F, searching = T,
                             scroller = T, scrollX = T, scrollY = 400))
  })
  
  #============================================================5: MODEL
  #5th Summary
  output$summary <- renderPrint({
    y <- mydata()[,-c(2,4)] %>% filter(`Country Name` %in% input$neg)
    yx <- y[,-c(1,2)]
    yx <- data.frame(sapply(yx, function(x) as.numeric(as.character(x))))
    yx <- t(yx)
    colnames(yx) <- y[,2]
    rm(y)
    summary(yx)
  })
  #5th Histogram
  output$distPlot_dv <- renderHighchart({
    y <- mydata()[,-c(2,4)] %>% filter(`Country Name` %in% input$neg)
    yx <- y[,-c(1,2)]
    yx <- data.frame(sapply(yx, function(x) as.numeric(as.character(x))))
    yx <- t(yx)
    colnames(yx) <- y[,2]
    x <- as.data.frame(yx[,input$dv])
    rm(yx, y)
    hchart(x[,1], name = input$dv)
  })
  output$distPlot_iv <- renderHighchart({
    y <- mydata()[,-c(2,4)] %>% filter(`Country Name` %in% input$neg)
    yx <- y[,-c(1,2)]
    yx <- data.frame(sapply(yx, function(x) as.numeric(as.character(x))))
    yx <- t(yx)
    colnames(yx) <- y[,2]
    x <- as.data.frame(yx[,input$iv])
    rm(yx, y)
    hchart(x[,1], color = "#B71C1C", name = input$iv)
  })
  #5th Scatter Plot
  output$scatt <- renderPlot({
    y <- mydata()[,-c(2,4)] %>% filter(`Country Name` %in% input$neg)
    yx <- y[,-c(1,2)]
    yx <- data.frame(sapply(yx, function(x) as.numeric(as.character(x))))
    yx <- t(yx)
    colnames(yx) <- y[,2]
    #plot
    plot(yx[,input$iv], yx[,input$dv],
         xlab = input$iv, ylab = input$dv,
         main = "Scatter Plot of Independent and Dependent Variables", pch = 16, 
         col = "black", cex = 1)
    abline(lm(yx[,input$dv]~yx[,input$iv]), col="grey", lwd = 2)
  })
  #5th Model
  model <- reactive({
    y <- mydata()[,-c(2,4)] %>% filter(`Country Name` %in% input$neg)
    yx <- y[,-c(1,2)]
    yx <- data.frame(sapply(yx, function(x) as.numeric(as.character(x))))
    yx <- t(yx)
    colnames(yx) <- y[,2]
    model <- lm(yx[,input$dv]~yx[,input$iv])
  })
  output$model <- renderPrint({
    summary(model())
  })
  #5th Residuals
  output$residuals_hist <- renderPlot({
    hist(model()$residuals, main = paste(input$dv, '~', input$iv), xlab = 'Residuals') 
  })
  output$residuals_scatter <- renderPlot({
    y <- mydata()[,-c(2,4)] %>% filter(`Country Name` %in% input$neg)
    yx <- y[,-c(1,2)]
    yx <- data.frame(sapply(yx, function(x) as.numeric(as.character(x))))
    yx <- t(yx)
    colnames(yx) <- y[,2]
    rm(y)
    plot(model()$residuals ~ yx[,input$iv], xlab = input$iv, ylab = 'Residuals')
    abline(h = 0, lty = 3) 
  })
  output$residuals_qqline <- renderPlot({
    qqnorm(model()$residuals)
    qqline(model()$residuals) 
  })
  
  #============================================================6: DATA SET
  #data
  output$hist <- renderDataTable({
    dataku <- mydata()[,-c(2,4)]
    datatable(dataku, caption = "World Development Indicator Data Set", filter = "top",
              options = list(deferRender = T, paging = T, searching = T,
                             scroller = T, scrollX = T, scrollY = F,
                             autoWidth = TRUE, pageLength = 5))
  })
  
  #============================================================7: INCOME GROUPS
  #ts
  output$tsinc <- renderDygraph({
    if(input$income != "All") {
      y = merge(mydata(), income(), by = "Country Name", all.x = T)
      y = y %>% filter(`Income Group` %in% input$income, `Indicator Name` %in% input$inccat)
      yin <- y[,5:60]
      yin <- data.frame(sapply(yin, function(x) as.numeric(as.character(x))))
      #sum
      sumin <- as.data.frame(colSums(yin))
      #add year
      year=1960:2015
      yc=cbind(sumin,year)
      
      yc = as.data.frame(yc)
      yc$year = as.Date(as.character(yc$year), "%Y")
      colnames(yc)=c("indicator","year")
      ts=xts(yc[,1],order.by =yc[,"year"])
      colnames(ts) <- input$income
      rm(y,year,yc,sumin,yin)
      dygraph(ts, main = input$inccat)%>% dyRangeSelector()%>%
        #dyOptions(colors = RColorBrewer::brewer.pal(1, "Set2")) %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE)
    }
    else {
      y = merge(mydata(), income(), by = "Country Name", all.x = T)
      y = y %>% filter(`Indicator Name` %in% input$inccat)
      yin <- y[,5:60]
      yin <- data.frame(sapply(yin, function(x) as.numeric(as.character(x))))
      yall <- as.data.frame(cbind(y[,61], yin))
      colnames(yall) <- c("income", as.character(1960:2015))
      
      #low
      ylow <- yall %>% filter(income %in% "Low income") %>% select(-income)
      ylowsum <- as.data.frame(colSums(ylow))
      #upper middle
      yupm <- yall %>% filter(income %in% "Upper middle income") %>% select(-income)
      yupmsum <- as.data.frame(colSums(yupm))
      #high income non
      yhin <- yall %>% filter(income %in% "High income: nonOECD") %>% select(-income)
      yhinsum <- as.data.frame(colSums(yhin))
      #low middle
      ylom <- yall %>% filter(income %in% "Lower middle income") %>% select(-income)
      ylomsum <- as.data.frame(colSums(ylom))
      #high inc mid
      yhio <- yall %>% filter(income %in% "High income: OECD") %>% select(-income)
      yhiosum <- as.data.frame(colSums(yhio))
      #NA 
      yNA <- yall %>% filter(income %in% NA) %>% select(-income)
      yNAsum <- as.data.frame(colSums(yNA))
      
      #combine
      #add year
      year=1960:2015
      da <- cbind(ylowsum,yupmsum,yhinsum,ylomsum,yhiosum,yNAsum,year)
      colnames(da) <- c("Low income", "Upper middle income", "High income: nonOECD", "Lower middle income", "High income: OECD", "Not Available", "year")
      
      da = as.data.frame(da)
      da$year = as.Date(as.character(da$year), "%Y")
      ts=xts(da[,1:6],order.by =da[,"year"])
      #colnames(ts) <- input$income
      rm(y,year,da,sumin,yin,yall,yhin,yhinsum,ylom,ylomsum,ylow,ylowsum,yNA,yNAsum,yupm,yupmsum,yhio,yhiosum)
      dygraph(ts, main = input$inccat)%>% dyRangeSelector()%>%
        #dyOptions(colors = RColorBrewer::brewer.pal(1, "Set2")) %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE)
    }
  })
  #ts data
  output$tsallinc <- renderDataTable({
    if(input$income != "All") {
      y = merge(mydata(), income(), by = "Country Name", all.x = T)
      y = y %>% filter(`Income Group` %in% input$income, `Indicator Name` %in% input$inccat)
      yin <- y[,5:60]
      yin <- data.frame(sapply(yin, function(x) as.numeric(as.character(x))))
      #sum
      sumin <- as.data.frame(colSums(yin))
      #add year
      year=1960:2015
      yc=cbind(sumin,year)
      yc = as.data.frame(yc)
      colnames(yc) <- c(input$inccat, "Year")
      rownames(yc) <- as.character(1960:2015)
      datatable(yc, caption = input$income, #filter = "top",
                options = list(deferRender = T, paging = F, searching = T,
                               scroller = T, scrollX = F, scrollY = 350,
                               autoWidth = TRUE))
    }
    else {
      y = merge(mydata(), income(), by = "Country Name", all.x = T)
      y = y %>% filter(`Indicator Name` %in% input$inccat)
      yin <- y[,5:60]
      yin <- data.frame(sapply(yin, function(x) as.numeric(as.character(x))))
      yall <- as.data.frame(cbind(y[,61], yin))
      colnames(yall) <- c("income", as.character(1960:2015))
      
      #low
      ylow <- yall %>% filter(income %in% "Low income") %>% select(-income)
      ylowsum <- as.data.frame(colSums(ylow))
      #upper middle
      yupm <- yall %>% filter(income %in% "Upper middle income") %>% select(-income)
      yupmsum <- as.data.frame(colSums(yupm))
      #high income non
      yhin <- yall %>% filter(income %in% "High income: nonOECD") %>% select(-income)
      yhinsum <- as.data.frame(colSums(yhin))
      #low middle
      ylom <- yall %>% filter(income %in% "Lower middle income") %>% select(-income)
      ylomsum <- as.data.frame(colSums(ylom))
      #high inc mid
      yhio <- yall %>% filter(income %in% "High income: OECD") %>% select(-income)
      yhiosum <- as.data.frame(colSums(yhio))
      #NA 
      yNA <- yall %>% filter(income %in% NA) %>% select(-income)
      yNAsum <- as.data.frame(colSums(yNA))
      
      #combine
      da <- cbind(ylowsum,yupmsum,yhinsum,ylomsum,yhiosum,yNAsum)
      colnames(da) <- c("Low income", "Upper middle income", "High income: nonOECD", "Lower middle income", "High income: OECD", "Not Available")
      rownames(da) <- as.character(1960:2015)
      da = as.data.frame(da)
      datatable(da, caption = input$inccat, #filter = "top",
                options = list(deferRender = T, paging = F, searching = T,
                               scroller = T, scrollX = T, scrollY = 350,
                               autoWidth = TRUE))
    }
  })
  #bxplot
  output$bxplinc <- renderHighchart({
    y = merge(mydata(), income(), by = "Country Name", all.x = T)
    y = y %>% filter(`Indicator Name` %in% input$inccat)
    yin <- y[,5:60]
    yin <- data.frame(sapply(yin, function(x) as.numeric(as.character(x))))
    yall <- as.data.frame(cbind(y[,61], yin))
    colnames(yall) <- c("income", as.character(1960:2015))
    
    #low
    ylow <- yall %>% filter(income %in% "Low income") %>% select(-income)
    ylowsum <- as.data.frame(colSums(ylow))
    #upper middle
    yupm <- yall %>% filter(income %in% "Upper middle income") %>% select(-income)
    yupmsum <- as.data.frame(colSums(yupm))
    #high income non
    yhin <- yall %>% filter(income %in% "High income: nonOECD") %>% select(-income)
    yhinsum <- as.data.frame(colSums(yhin))
    #low middle
    ylom <- yall %>% filter(income %in% "Lower middle income") %>% select(-income)
    ylomsum <- as.data.frame(colSums(ylom))
    #high inc mid
    yhio <- yall %>% filter(income %in% "High income: OECD") %>% select(-income)
    yhiosum <- as.data.frame(colSums(yhio))
    #NA 
    yNA <- yall %>% filter(income %in% NA) %>% select(-income)
    yNAsum <- as.data.frame(colSums(yNA))
    
    #combine
    #add year
    year=1960:2015
    da <- cbind(ylowsum,yupmsum,yhinsum,ylomsum,yhiosum,yNAsum,year)
    colnames(da) <- c("Low income", "Upper middle income", "High income: nonOECD", 
                      "Lower middle income", "High income: OECD", "Not Available", "year")
    rm(y,year,yin,yall,yhin,yhinsum,ylom,ylomsum,ylow,ylowsum,yNA,yNAsum,yupm,yupmsum,yhio,yhiosum)
    #if
    if(input$income != "All"){
      dat <- da %>% select(input$income)
      dat <- as.data.frame(cbind(dat, input$income))
      hcboxplot(x = dat[,1], var = dat[,2], outliers = T) %>% 
        hc_chart(type = "column") %>%
        hc_title(text = input$inccat)
    }
    else {
      zz <- da %>% select(-year)
      #zz <- zz[,order(names(zz))]
      zz <- gather(zz, "income", "indicator", 1:6)
      hcboxplot(x = zz[,2], var = zz[,1], outliers = T) %>% 
        hc_chart(type = "column") %>%
        hc_title(text = input$inccat)
    }
  })
  #bxplot data
  output$tsallxinc <- renderDataTable({
    y = merge(mydata(), income(), by = "Country Name", all.x = T)
    y = y %>% filter(`Indicator Name` %in% input$inccat)
    yin <- y[,5:60]
    yin <- data.frame(sapply(yin, function(x) as.numeric(as.character(x))))
    yall <- as.data.frame(cbind(y[,61], yin))
    colnames(yall) <- c("income", as.character(1960:2015))
    
    #low
    ylow <- yall %>% filter(income %in% "Low income") %>% select(-income)
    ylowsum <- as.data.frame(colSums(ylow))
    #upper middle
    yupm <- yall %>% filter(income %in% "Upper middle income") %>% select(-income)
    yupmsum <- as.data.frame(colSums(yupm))
    #high income non
    yhin <- yall %>% filter(income %in% "High income: nonOECD") %>% select(-income)
    yhinsum <- as.data.frame(colSums(yhin))
    #low middle
    ylom <- yall %>% filter(income %in% "Lower middle income") %>% select(-income)
    ylomsum <- as.data.frame(colSums(ylom))
    #high inc mid
    yhio <- yall %>% filter(income %in% "High income: OECD") %>% select(-income)
    yhiosum <- as.data.frame(colSums(yhio))
    #NA 
    yNA <- yall %>% filter(income %in% NA) %>% select(-income)
    yNAsum <- as.data.frame(colSums(yNA))
    
    #combine
    da <- cbind(ylowsum,yupmsum,yhinsum,ylomsum,yhiosum,yNAsum)
    colnames(da) <- c("Low income", "Upper middle income", "High income: nonOECD", 
                      "Lower middle income", "High income: OECD", "Not Available")
    rownames(da) <- as.character(1960:2015)
    rm(y,year,yin,yall,yhin,yhinsum,ylom,ylomsum,ylow,ylowsum,yNA,yNAsum,yupm,yupmsum,yhio,yhiosum)
    datatable(da, caption = input$inccat, #filter = "top",
              options = list(deferRender = T, paging = F, searching = T,
                             scroller = T, scrollX = T, scrollY = 350,
                             autoWidth = TRUE))
  })
  
  #============================================================8: REGION GROUPS
  #region ts
  output$tsreg <- renderDygraph({
    if(input$regi != "All") {
      y = merge(mydata(), region(), by = "Country Name", all.x = T)
      y = y %>% filter(Region %in% input$regi, `Indicator Name` %in% input$regcat)
      yin <- y[,5:60]
      yin <- data.frame(sapply(yin, function(x) as.numeric(as.character(x))))
      #sum
      sumin <- as.data.frame(colSums(yin))
      #add year
      year=1960:2015
      yc=cbind(sumin,year)
      
      yc = as.data.frame(yc)
      yc$year = as.Date(as.character(yc$year), "%Y")
      colnames(yc)=c("indicator","year")
      ts=xts(yc[,1],order.by =yc[,"year"])
      colnames(ts) <- input$regi
      ts[is.na(ts)] <- 0
      rm(y,year,yc,sumin,yin)
      dygraph(ts, main = input$regcat)%>% dyRangeSelector()%>%
        #dyOptions(colors = RColorBrewer::brewer.pal(1, "Set2")) %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE)
    }
    else {
      y = merge(mydata(), region(), by = "Country Name", all.x = T)
      y = y %>% filter(`Indicator Name` %in% input$regcat)
      yin <- y[,5:60]
      yin <- data.frame(sapply(yin, function(x) as.numeric(as.character(x))))
      yall <- as.data.frame(cbind(y[,61], yin))
      colnames(yall) <- c("region", as.character(1960:2015))
      
      #low
      ylow <- yall %>% filter(region %in% "East Asia & Pacific") %>% select(-region)
      ylowsum <- as.data.frame(colSums(ylow))
      #upper middle
      yupm <- yall %>% filter(region %in% "Europe & Central Asia") %>% select(-region)
      yupmsum <- as.data.frame(colSums(yupm))
      #high region non
      yhin <- yall %>% filter(region %in% "Latin America & Caribbean") %>% select(-region)
      yhinsum <- as.data.frame(colSums(yhin))
      #low middle
      ylom <- yall %>% filter(region %in% "Middle East & North Africa") %>% select(-region)
      ylomsum <- as.data.frame(colSums(ylom))
      #high inc mid
      yhio <- yall %>% filter(region %in% "North America") %>% select(-region)
      yhiosum <- as.data.frame(colSums(yhio))
      #Southasia
      sar <- yall %>% filter(region %in% "South Asia") %>% select(-region)
      sarsum <- as.data.frame(colSums(sar))
      #Sub-Saharan Africa
      ssa <- yall %>% filter(region %in% "Sub-Saharan Africa") %>% select(-region)
      ssasum <- as.data.frame(colSums(ssa))
      #NA 
      yNA <- yall %>% filter(region %in% NA) %>% select(-region)
      yNAsum <- as.data.frame(colSums(yNA))
      
      #combine
      #add year
      year=1960:2015
      da <- cbind(ylowsum,yupmsum,yhinsum,ylomsum,yhiosum,sarsum,ssasum,yNAsum,year)
      colnames(da) <- c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean", 
                        "Middle East & North Africa", "North America", "South Asia","Sub-Saharan Africa",
                        "Not Available", "year")
      
      da = as.data.frame(da)
      da$year = as.Date(as.character(da$year), "%Y")
      ts=xts(da[,1:6],order.by =da[,"year"])
      #colnames(ts) <- input$income
      rm(y,year,da,sumin,yin,yall,yhin,yhinsum,ylom,ylomsum,ylow,ylowsum,yNA,yNAsum,yupm,yupmsum,yhio,yhiosum,sarsum,ssasum,ssa,sar)
      dygraph(ts, main = input$regcat)%>% dyRangeSelector()%>%
        #dyOptions(colors = RColorBrewer::brewer.pal(1, "Set2")) %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE)
    }
  })
  #region bxplot
  output$bxplreg <- renderHighchart({
    y = merge(mydata(), region(), by = "Country Name", all.x = T)
    y = y %>% filter(`Indicator Name` %in% input$regcat)
    yin <- y[,5:60]
    yin <- data.frame(sapply(yin, function(x) as.numeric(as.character(x))))
    yall <- as.data.frame(cbind(y[,61], yin))
    colnames(yall) <- c("region", as.character(1960:2015))
    
    #low
    ylow <- yall %>% filter(region %in% "East Asia & Pacific") %>% select(-region)
    ylowsum <- as.data.frame(colSums(ylow))
    #upper middle
    yupm <- yall %>% filter(region %in% "Europe & Central Asia") %>% select(-region)
    yupmsum <- as.data.frame(colSums(yupm))
    #high region non
    yhin <- yall %>% filter(region %in% "Latin America & Caribbean") %>% select(-region)
    yhinsum <- as.data.frame(colSums(yhin))
    #low middle
    ylom <- yall %>% filter(region %in% "Middle East & North Africa") %>% select(-region)
    ylomsum <- as.data.frame(colSums(ylom))
    #high inc mid
    yhio <- yall %>% filter(region %in% "North America") %>% select(-region)
    yhiosum <- as.data.frame(colSums(yhio))
    #Southasia
    sar <- yall %>% filter(region %in% "South Asia") %>% select(-region)
    sarsum <- as.data.frame(colSums(sar))
    #Sub-Saharan Africa
    ssa <- yall %>% filter(region %in% "Sub-Saharan Africa") %>% select(-region)
    ssasum <- as.data.frame(colSums(ssa))
    #NA 
    yNA <- yall %>% filter(region %in% NA) %>% select(-region)
    yNAsum <- as.data.frame(colSums(yNA))
    
    #combine
    #add year
    year=1960:2015
    da <- cbind(ylowsum,yupmsum,yhinsum,ylomsum,yhiosum,sarsum,ssasum,yNAsum,year)
    colnames(da) <- c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean", 
                      "Middle East & North Africa", "North America", "South Asia","Sub-Saharan Africa",
                      "Not Available", "year")
    rm(y,year,yin,yall,yhin,yhinsum,ylom,ylomsum,ylow,ylowsum,yNA,yNAsum,yupm,yupmsum,yhio,yhiosum,sarsum,ssasum,ssa,sar)
    #if
    if(input$income != "All"){
      dat <- da %>% select(input$regi)
      dat <- as.data.frame(cbind(dat, input$regi))
      hcboxplot(x = dat[,1], var = dat[,2], outliers = T) %>% 
        hc_chart(type = "column") %>%
        hc_title(text = input$regcat)
    }
    else {
      zz <- da %>% select(-year)
      #zz <- zz[,order(names(zz))]
      zz <- gather(zz, "region", "indicator", 1:8)
      hcboxplot(x = zz[,2], var = zz[,1], outliers = T) %>% 
        hc_chart(type = "column") %>%
        hc_title(text = input$inccat)
    }
  })
  
  #============================================================9: ABOUT
  #rendertext
  output$frm <- renderText({
    "Firmansyah Tri Atmojo"
  })
  #rendercountrymeta
  output$couver <- renderText({
    mc <- meta_country() %>% filter(`Country Name` %in% input$metacou)
    #mc <- as.list(mc)
    mc[,2]
  })
  output$couver2 <- renderText({
    mc <- meta_country() %>% filter(`Country Name` %in% input$metacou)
    #mc <- as.list(mc)
    mc[,3]
  })
  output$couver3 <- renderText({
    mc <- meta_country() %>% filter(`Country Name` %in% input$metacou)
    #mc <- as.list(mc)
    mc[,4]
  })
  output$couver4 <- renderText({
    mc <- meta_country() %>% filter(`Country Name` %in% input$metacou)
    #mc <- as.list(mc)
    mc[,5]
  })
  #renderindicatormeta
  output$indver <- renderText({
    mc <- indicator_name() %>% filter(`Indicator Code` %in% input$metaind)
    #mc <- as.list(mc)
    mc[,2]
  })
  output$indver2 <- renderText({
    mc <- indicator_name() %>% filter(`Indicator Code` %in% input$metaind)
    #mc <- as.list(mc)
    mc[,3]
  })
  output$indver3 <- renderText({
    mc <- indicator_name() %>% filter(`Indicator Code` %in% input$metaind)
    #mc <- as.list(mc)
    mc[,4]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

