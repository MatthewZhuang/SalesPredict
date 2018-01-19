library(TTR)
library(shiny)
library(dygraphs)
library(xts)
library(ggplot2)
library(ggthemes)
library(zoo)

shinyUI(navbarPage("Sales Analysis", id = "nav",

    #six tab related to six module, change the sequence of code to alter the page display

	tabPanel("salesTrend",
        fluidPage(
        titlePanel("Overall Sales Trend"),
  
        sidebarLayout(
            sidebarPanel(
      
            helpText("In order to demonstrate the state of the business, this chart shows the overall sales, overlapped with a linear regression line illustrating the trend."),
    
            dateRangeInput("dates",
                   h3("Date range"),
                   start = "2015-05-10", 
                   end = "2016-03-26",
                   min="2015-01-10",
                   max="2016-03-26"
            )
        ),
  
        mainPanel(
            dygraphOutput("salesTrendplot")
        )
        )
    )
	),
	
	tabPanel("skuDistribution",
        fluidPage(
            titlePanel("SKU Sales Distribution"),
            sidebarLayout(
                sidebarPanel(
                    helpText("Sales distribution among all SKUs"),

                    sliderInput("slider",h3("Range of total sales"),
                        min = -30,
                        max = 950818,
                        value=c(50000,100000))
                    ),

                    mainPanel(
                        verbatimTextOutput("skuDistributionsummary"),
                        plotOutput("skuDistributionplot")
                    )
                )
            )
      ),

    tabPanel("predict",fluidPage(
        titlePanel("Sales Prediction"),

        sidebarLayout(
            sidebarPanel(
                helpText("The prediction of sales using exponential smoothing method"),
                selectInput("predictsku", h3("Choose a sku"),
                    choices = c("AC202B", "CA2014RB"))
                ),

                mainPanel(
                    dygraphOutput("predictdygraph")
                )
            )
        )
    ),

    tabPanel("promotion",fluidPage(
        titlePanel("Prediction with the Promotion Impact "),

        sidebarLayout(
            sidebarPanel(
                helpText("This chart shows the sales prediction while running an extra promotion"),
                selectInput("sku", h3("Choose a sku"),
                    choices = c("AC202B", "AC201")
                ),

                dateInput("date",label = h3("Promotion start time"),
                    value = "2015-12-10",
                    max="2016-3-10",
                    min="2015-1-06"),

                numericInput("num",label = h3("Promotion days"),
                    value = 3,
                    max=5,
                    min=1),

                numericInput("benefit",label = h3("Promotion benefit"),
                    value = "",
                    max=20000,
                    min=0)

        ),

        mainPanel(
            dygraphOutput("promotionplot")
        )
    )
    )
    ),

    tabPanel("inventory",fluidPage(
        titlePanel("Inventory "),

        sidebarLayout(
            sidebarPanel("SKU inventory overlapped with sales data"),

            mainPanel(
                dygraphOutput("inventoryplot1"),
                br(),
                br(),
                dygraphOutput("inventoryplot2")
            )
        )
    )
    ),

    tabPanel("inventoryImprove",fluidPage(
        titlePanel("Inventory Improvement"),

        sidebarLayout(
            sidebarPanel(

            helpText("The inventory management can be improved if the order quantity is set in accordance with the future sales. This chart illustrates the optimized inventory by taking this ordering strategy"),

            selectInput("improvesku", h3("Choose a sku"),
                choices = c("AC202B", "AC201"))
            ),

            mainPanel(
                dygraphOutput("inventoryImproveplot")
            )
        )
    )
    )
))
