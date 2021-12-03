
library(shiny)
library(ggplot2)
library(plotly)
library(curl)
library(jsonlite)

source("global.R")
# Define UI for application that draws a histogram
ui <- fluidPage(

  fluidRow(
    # Application title
    column(3, 
           # Sidebar 
           wellPanel(
             h4("ETH Dollar Cost Averaging (DCA) Simulator"),
             h6("Select dates, # of Buys & # Days between Buys"),
             hr(),
             dateInput(inputId = "date1", label = "Initial Date", 
                       min = "2015-08-07", max = "2021-10-01", 
                       value = "2021-01-01"), 
             dateInput(inputId = "date2", label = "Final Date", 
                       min = "2016-08-07", max = Sys.Date(), 
                       value = Sys.Date()), 
             numericInput(inputId = "bankroll",
                          label = "Amount Available",
                          min = 1000,
                          max =  1000000, 
                          value = 10000, step = 1000),
             numericInput(inputId = "dca_time",
                          label = "Number of DCA Buys (Default: 10)",
                          min = 5,
                          max =  100, 
                          value = 10, step = 1),
             numericInput(inputId = "dca_unit",
                          label = "Days Between Buys (Default: 1 Day)",
                          min = 1,
                          max =  30, 
                          value = 1, 
                          step = 1),
             actionButton(inputId = "begin", label = "Run Analysis")
           ),hr(),
           tableOutput("dca_table"), 
           hr(),
           HTML("If you'd like to support better hosting with RConnect, 
                donations can be sent to ENS: charliedao.eth on mainnet or 
                charliemarketplace.eth on Polygon, FTM, or Avalanche to fund 
                the collective's access to the servers.")
    ),
    column(9,
           fluidRow(
             column(4,plotlyOutput("dca_gains"),
                    verbatimTextOutput("dca_spread")),
             column(8, tradingview())
           ), hr(), 
           fluidRow(
             column(1, ""),
             column(5, plotlyOutput("dca_comparison_plot")),
             column(5, plotlyOutput("date_review")),
             column(1, "")
           )
    )
  )
)
  
# Define server logic required to draw a histogram
server <- function(input, output) {

  eth_filtered <- eventReactive(input$begin, { 
    eth[eth$date >= input$date1 & eth$date <= input$date2, ]
  })
  
  strategy <- eventReactive(input$begin, { 
    
    strategy <- list()
    test_dca <- function(){
      bankroll <- input$bankroll
      dca_time <- input$dca_time
      dca_unit <- input$dca_unit
      # any date picked must not require future DCA'ing.
      date_of_test <- sample(eth_filtered()$date[1:( nrow(eth_filtered()) - dca_time*dca_unit )], 
                             size = 1)
      price_on_date <- eth_filtered()[eth_filtered()$date == date_of_test, "price"] 
      amount_eth_all_in <- bankroll/price_on_date
      amount_eth_dca <- 0
      
      for(i in 0:dca_time){ 
        bankroll_fraction <- bankroll/dca_time
        temp_price <- eth_filtered()[eth_filtered()$date == date_of_test + dca_unit*i, "price"] 
        amount_eth_dca <- amount_eth_dca + bankroll_fraction/temp_price
        
      }
      
      return(
        list( 
          "start_date" = date_of_test,
          "all_in" = amount_eth_all_in,
          "DCA" = amount_eth_dca
        )
      )
    }
    
    set.seed(4)
    # 10,000 used for simulation but technically fixed number of possible start dates.
    withProgress(expr = {
      
      for(i in 1:10000){ 
        strategy[[i]] <- test_dca()  
        incProgress(amount = 1)
        
      }
    }, min = 0, max = 10000, value = 0, message = "Running Simulation")
    
    strategy_review <- do.call(what = rbind, args = lapply(strategy, as.data.frame))
    strategy_review$DCA_win <- strategy_review$DCA >=  strategy_review$all_in 
    
    strategy_review$dca_margin <- { 
      (strategy_review$DCA - strategy_review$all_in) / strategy_review$all_in
    }
    
    review_strat <<- strategy_review
    strategy_review
  })
  
  output$date_review <- renderPlotly({ 
    unique_review_strat <- unique(strategy())
    
    g <- ggplot(unique_review_strat, aes(x = start_date, y = as.numeric(DCA_win))) + 
      geom_line() + 
      theme_classic() + 
      xlab("Start Date Assessed") + 
      ylab("Winning Strategy") + 
      theme(title = element_text(size = rel(1.1)),
            axis.text = element_text(size = rel(1.1))) + 
      scale_y_discrete(limits = c(0,1), labels = c("ALL-IN", "DCA"))
    
      layout(p =ggplotly(g),
             title = list(text = paste0('Winning Strategy by Start Date',
                                        '<br>',
                                        '<sup>',
                                        'DCA: Spend 1/Nth bankroll per period','</sup>')))
    
    })
  
  output$dca_comparison_plot <- renderPlotly({ 
    
   g <- ggplot(strategy(), aes(x = all_in, y = DCA)) + 
      geom_point() + 
      geom_abline(slope = 1) + 
      theme_classic() + 
      xlab("ALL IN: Spend bankroll on start date") + 
      ylab("DCA: Spend 1/Nth bankroll per period") + 
      theme(title = element_text(size = rel(1.1)),
            axis.text = element_text(size = rel(1.1)))
   
      layout(p = ggplotly(g), title = list(text = paste0(
        'DCA versus All-In Purchases of ETH',
        '<br>',
        '<sup>',
        'Black Line Shows Equivalent Amount of ETH Accumulated','</sup>')))
    
    
    })
  
  output$dca_table <- renderTable({ 
    winner_percent <- data.frame(
      "Strategy" = c("DCA", "All-In"),
      "Win Percentage" = c(0,0), check.names = FALSE
    )
    dca_win_percent = sum(strategy()$DCA > strategy()$all_in)/10000
    
    winner_percent[1, "Win Percentage"] <- dca_win_percent
    winner_percent[2, "Win Percentage"] <- 1 - dca_win_percent
    
    winner_percent
    })
  
  output$dca_spread <- renderPrint({ 
    summary(strategy()$dca_margin)
    })
  
  output$dca_gains <- renderPlotly({ 
    strat <- strategy()
    
  g <- ggplot(strat, aes(x = dca_margin)) + 
      geom_histogram(aes(y = ..density..), 
                     fill = "lightgray",
                     binwidth = 0.01) + 
      geom_vline(xintercept = 0, color = "blue", linetype = "dashed") +
      geom_density(aes(y = ..density..)) +
      theme_classic() + 
      theme(title = element_text(size = rel(1.1)),
            axis.text = element_text(size = rel(1.1))) + 
    xlab("% Gain") + 
    ylab("Density")
    
    layout(p = ggplotly(g), title = list(text = 
      'Marginal Gain from DCA over All-In'))
  
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
