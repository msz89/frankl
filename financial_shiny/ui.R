
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(shiny)

perCheckoutLoss_str = 'Compound annual growth rate'
perLossFruitVeg_str = 'Addressable market'
perLossItemSwitching_str = 'Frankl Market Saturation at Y10'
numStores_str = '% of Tokens sold at ICO'
numCheckouts_str = 'Velocity'

# Define UI for slider demo application
fluidPage(
  
  #  Application title
  titlePanel("Frankl Valuation Model"),
  br(),
  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(
      # Total Loss
      sliderInput("totalLoss", paste0('Total Market Size Y0',":"),
                  min=6000000000, max=12000000000, value=8000000000, step=100000000, pre = "$"),
      
      # % Loss at Self Checkout
      sliderInput("perCheckoutLoss", paste0(perCheckoutLoss_str,":"),
                  min=0, max=40, value=32, step=1, post = "%"),
      
      # % Loss due to Fruit and Veggie
      sliderInput("perLossFruitVeg", paste0(perLossFruitVeg_str,":"),
                  min = 0, max = 100, value = 80, step=2, post = "%"),
      
      # % Loss due to "Item Switching"
      sliderInput("perLossItemSwitching", paste0(perLossItemSwitching_str,":"),
                  min = 0, max = 100, value = 20, step = 5,
                  post = "%", animate=TRUE),
      
      sliderInput("numStores", paste0(numStores_str,":"),
                  min = 0, max = 55, value = 20, step=1),
      
      sliderInput("numCheckouts", paste0(numCheckouts_str,":"),
                  min = 1, max = 20, value = 10, step=1)
      
    ),
    
    # Show a table summarizing the values entered
    mainPanel(
      tableOutput("values"),
      img(src="checkout.jpg", height = 400)
    )
  )
)