library(shiny)

# CONSTANTS
START_YEAR = 2016
END_YEAR = 2050
START_MARKET_SIZE = 1.98e9
PLOT_YRS=c(2018,2028)
paragraph_text = "*CGAR = Compound annual growth rate."
BG_LIGHT = "#d5f0fb"

# ROW 1
row_1 <-fluidRow(
  column(6,
         
         h3("Frankl market share inputs",align="center"),
         plotOutput("market_plot"),
         
         #h3("Market inputs",align="center"),
         sliderInput('cagr', 'CAGR* %', 
                     min=10, max=40, value=32, 
                     step=1),
         sliderInput('addressable', 'Addressable market %', 
                     min=30, max=100, value=80, 
                     step=5),
         sliderInput('discount_rate', 'Discount Rate %', 
                     min=10, max=40, value=30, 
                     step=1),
         
         p(paragraph_text)
  ),
  
  column(6,
         h3("Frankl valuation outputs",align="center"),
         plotOutput("value_plot"),
         plotOutput("ratio_plot")
  )
)

# ROW 1
row_2 <- fluidRow(
  
  column(4,
         
         h3("Market share inputs",align="center"),
         plotOutput("saturation_plot", height=300),
         sliderInput('sat_L', 'Market saturation %', 
                     min=5, max=50, value=20, 
                     step=5),
         sliderInput('sat_xo', 'Linear growth year', 
                     min=2020, max=2025, value=2024, 
                     step=1)
  ),
  
  column(4,
         
         h3("Velocity inputs",align="center"),
         plotOutput("velocity_plot", height=300),
         sliderInput('vel_L', 'Final velocity', 
                     min=2, max=18, value=10, 
                     step=1),
         sliderInput('vel_xo', 'Linear growth year', 
                     min=2020, max=2025, value=2024, 
                     step=1)
  ),
  
  
  column(4,
         
         h3("Token release",align="center"),
         plotOutput("tokens_plot", height=300),
         sliderInput('ico1', '% issued at first ICO', 
                     min=10, max=55, value=40, 
                     step=1)
  )
  
)

row_3 <- fluidRow(
  tableOutput("table_all"),
  tableOutput("table_values")
)

# Define UI for Valuation APP
ui <- fluidPage(theme="bootstrap.css",
  # App title ---- As Image
  headerPanel(img(src = "frankl-type-white.png", 
      height = 140, 
      width = 400
      )),
  row_1,
  row_2,
  row_3
)

# DATA SETUP
year = seq(START_YEAR, END_YEAR,1)
year_n = year-START_YEAR

# Define server logic to execute plots, and run the engine
server <- function(input, output) {
  
  # Get functions ---
  # These store the main calcs so the output plots can be simplified
  
  get_saturation <- reactive({
    sat_k = 0.6
    saturation = input$sat_L/(1+exp(-sat_k*(year-input$sat_xo))) # Set saturation based on input$variable
    })
  
  get_velocity <- reactive({
    vel_k = 0.3
    velocity = input$vel_L/(1+exp(-vel_k*(year-input$vel_xo)))
  })
  
  get_market <- reactive({
    # Market size > PQ
    market_total = START_MARKET_SIZE*(1+input$cagr/100)^year_n
    market_addressable = market_total * input$addressable/100
    market_share = market_addressable * get_saturation()/100 #PQ
    # Output
    df = data.frame(row.names = year, Total = market_total, Addressable = market_addressable, Frankl = market_share)
  })
  
  get_tokens <- reactive({
    
    # ICO 
    frankl_minted = 6*10e3*10e6
    
    ico_v = year_n*0
    ico_1 = 0.55
    ico_v[3] = input$ico1/100
    ico_2 = 0.15 + ico_1 - input$ico1/100
    ico_v[4] = ico_2
    
    # Foundation
    foundation_share = 0.15
    foundation_years = 10 
    foundation_v = year_n*0
    foundation_v[3:(2+foundation_years)] = foundation_share / foundation_years
    
    # Founding Team
    founder_share = 0.15 
    founder_years = 6 
    founder_v = year_n*0
    founder_v[3:(2+founder_years)] = founder_share / founder_years
    
    share_issued = cumsum(ico_v + founder_v + foundation_v) # as %

    # Hodl
    hodl_base = 0.6
    hodl_delta = 0.01
    hodl = hodl_base - ((year_n-2)*hodl_delta) # as %

    # Tokens
    tokens_issued = share_issued * frankl_minted
    tokens_hodl = share_issued * hodl * frankl_minted
    tokens_used = share_issued * (1-hodl) * frankl_minted
    
    # Output a dataframe
    df = data.frame(row.names = year, Hodl = tokens_hodl, Used = tokens_used)

  })
  
  get_values <- reactive({
    # Requires a 10 year rolling discounted value. [13:24] represents years 2028-2039
    # Requires a utility value based on value = PQ/Vn
    # Output as a 2 column DF in CENTS US
    value_utility = get_market()[,3]/get_tokens()[,2]/get_velocity()
    value_discount = value_utility[13:24]/(1+input$discount_rate/100)^10 #can we code this so the years are variable?
    print((1+input$discount_rate/100)^10)
    # Marry up years 2018-2029, utility from 2018-2029, and discounts calculated from 2028-2039
    df = data.frame (Year = year[3:14], Utility = 100 * value_utility[3:14], Discount = 100 * value_discount)
    })
  
  get_ratios <- reactive({
    # calculate a ultility value / market ratio
    ratios = data.frame(Year = year[3:14], Ratio = get_values()[,3]/get_values()[,2])
  })
  
  # Generate a plot of Market saturation
  output$saturation_plot <- renderPlot({
    par(bg = BG_LIGHT)
    plot(data.frame(year,get_saturation())[3:14,],
         main = "Frankl market share over time",
         type = c("l"), 
         ylim = c(0,50),
         ylab = "Saturation %"
         )
  })
  
  # Generate a plot of Velocity
  output$velocity_plot <- renderPlot({
    par(bg = BG_LIGHT)
    plot(data.frame(year,get_velocity())[3:14,],
         main = "Frankl token velocity",
         type = c("l"), 
         ylim = c(0,20),
         ylab = "Velocity"
         )
  })
  
  # Generate a plot of tokens
  output$tokens_plot <- renderPlot({
    df = as.matrix(get_tokens()[3:14,])
    par(bg = BG_LIGHT)
    barplot(t(df)/1e9,
            main = "Frankl tokens in circulation",
            ylab = "Tokens issued (Billion)",
            ylim=c(0,700)
         )
  })
  
  # Generate a plot of market
  # Need to Sort labels for columns w/out names
  output$market_plot <- renderPlot({
    df =  t(as.matrix(get_market()[3:14,]))
    par(bg = BG_LIGHT)
    barplot(df/10e9,
            main = "Market breakdown", 
            ylab = "$US Billion", # Hack the relable
            legend = colnames(get_market()), 
            beside = TRUE,
            args.legend = list(x = "topleft", bty = "n")
            )
  })
  
  # Generate a plot of value
  # Add Legend
  output$value_plot <- renderPlot({
    df = get_values()
    par(bg = BG_LIGHT)
    plot(df[c(1,2)], 
         main = "Frankl Token Value",
         type = c("l"),
         ylab = "Value US¢",
         col = "red"
         )
    
    lines(df[c(1,3)], 
          col = "blue"
          )
  })
  
  # Generate a plot of ratio
  output$ratio_plot <- renderPlot({
    df = get_ratios()
    par(bg = BG_LIGHT)
    plot(df,
         main = "Ratio of discounted market value to utility",
         type = c("l"),
         ylab = "Market Value / Utility Value Ratio"
    )

  })
  
  output$table_all <- renderTable(data.frame(Year = year, 
                                             Saturation = get_saturation(), 
                                             Velocity = get_velocity(), 
                                             get_market(),
                                             get_tokens()
                                             ))
  output$table_values <- renderTable(data.frame(get_values(),
                                                get_ratios()
  ), digits = 6)
  
}

# Call the main function
shinyApp(ui, server)

