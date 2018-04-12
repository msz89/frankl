library(shiny)

# CONSTANTS
START_YEAR = 2016
END_YEAR = 2050
START_MARKET_SIZE = 3.5e9
PLOT_YRS=c(2018,2028)

# Define UI for Valuation APP
ui <- fluidPage(
  
  # App title ----
  titlePanel("Frankl Token Valuation Model"),
  
  # Rows and Columns
  
  # ROW 1
  fluidRow(
    
    column(8,
           
           h3("Frankl market share",align="center"),
           plotOutput("market_plot"),

           #h3("Market inputs",align="center"),
           sliderInput('cagr', 'CAGR* %', 
                       min=10, max=40, value=32, 
                       step=1),
           sliderInput('addressable', 'Addressable market %', 
                       min=30, max=100, value=80, 
                       step=5),
           p("Some lorem ipsum about what this is and why it was chosen. Perhaps a link to the whitepaper"),
           br()
    ),
    
    column(4,
           h3("Frankl Token Value",align="center"),
           plotOutput("value_plot"),
           plotOutput("ratio_plot")
    )
  ),
  
  # ROW 2
  fluidRow(
    
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
)

# DATA SETUP
year = seq(START_YEAR, END_YEAR,1)
year_n = year-START_YEAR

# Define server logic to execute plots, and run the engine
server <- function(input, output) {
  
  # Generate a plot of Market saturation
  output$saturation_plot <- renderPlot({
    
    sat_k = 0.6
    saturation = input$sat_L/(1+exp(-sat_k*(year-input$sat_xo))) # Set saturation based on input$variable
    plot(data.frame(year,saturation)[2:12,], 
         type = c("l"), 
         ylim = c(0,50),
         ylab = "Saturation %"
         )# Plot this
    
  })
  
  # Generate a plot of Velocity
  output$velocity_plot <- renderPlot({
    
    vel_k = 0.3
    velocity = input$vel_L/(1+exp(-vel_k*(year-input$vel_xo)))
    
    # PLOT
    plot(data.frame(year,velocity)[2:12,], 
         type = c("l"), 
         ylim = c(0,20),
         ylab = "Velocity"
         )# Plot this
    
  })
  
  # Generate a plot of tokens
  output$tokens_plot <- renderPlot({
    
    # ICO 
    frankl_minted = 600*10e3*10e6
    #ico1 = 0.55 #var for modelling
    ico2 = 0.1 #build in 33% if ico1 doesnt happen
    
    ico_v = year_n*0
    ico_v[1] = input$ico1/100
    ico_v[2] = 0.15
    
    # Foundation
    foundation_share = 0.15
    foundation_years = 10 
    foundation_v = year_n*0
    foundation_v[1:foundation_years] = foundation_share / foundation_years
    
    # Founding Team
    founder_share = 0.15 
    founder_years = 6 
    founder_v = year_n*0
    founder_v[1:founder_years] = founder_share / founder_years
    
    share_issued = cumsum(ico_v + founder_v + foundation_v) # as %
    
    # Hodl
    hodl_base = 0.5
    hodl_delta = 0.01
    hodl = hodl_base - (year_n*hodl_delta)
    
    # Tokens
    tokens_issued = share_issued * frankl_minted
    tokens_hodl = share_issued * hodl * frankl_minted
    tokens_used = share_issued * (1-hodl) * frankl_minted
    
    # PLOT
    barplot(as.matrix(t(data.frame(tokens_issued/10e9)[1:10,])), 
            names=year[3:12],
            ylab = "Tokens issued (Billion)",
            ylim=c(0,6000)
         )
    
  })
  
  # Generate a plot of market
  output$market_plot <- renderPlot({
    
    # Market size > PQ
    market_total = START_MARKET_SIZE*(1+input$cagr/100)^year_n
    market_addressable = market_total * input$addressable/100
    saturation = input$sat_L/(1+exp(-sat_k*(year-input$sat_xo))) # Recalc saturation based on input$variable
    market_share = market_addressable * saturation/100 #PQ

    # PLOT
    df = data.frame(Total = market_total, Addressable = market_addressable, Share = market_share)
    barplot(t(as.matrix(df[3:13,]))/10e9,
            names=year[3:13],
            main="Market breakdown", 
            ylab = "USD$ Billion", # Hack the relable
            legend = colnames(df), 
            beside=TRUE,
            args.legend = list(x = "topleft", bty = "n")
            )
  })
  
  # Generate a plot of value
  # Fix this to calaculate in steps and recalc when everything is done.
  output$value_plot <- renderPlot({
    plot(data.frame(market_share/tokens_used)[3:13,], #get these two to flow through
         names=year[3:13],
         type = c("l")
    )
  })
  
  # Generate a plot of ratio
  # Fix this to calaculate in steps and recalc when everything is done.
  output$ratio_plot <- renderPlot({
    plot(data.frame(market_share/tokens_hodl)[3:13,], #get these two to flow through
         names=year[3:13],
         type = c("l")
    )
    
  })
}

shinyApp(ui, server)

