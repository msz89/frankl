
# This is the server logic for a Shiny web application.
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

# Define server logic for slider examples
function(input, output) {
  
  # Reactive expression to compose a data frame containing all of
  # the p.a values 
  # inputs/100 as percentages
  sliderValues <- reactive({
    total_loss = input$totalLoss
    selfCheckoutLoss = total_loss * (input$perCheckoutLoss/100)
    fruitAndVegLoss = selfCheckoutLoss * (input$perLossFruitVeg/100)
    itemSwitchLoss = fruitAndVegLoss * (input$perLossItemSwitching/100)
    perStoreLoss = itemSwitchLoss / input$numStores
    perCheckoutLoss = perStoreLoss /input$numCheckouts
    
    
    # Compose data frame
    data.frame(
      Name = c('Total Market size Y0',
               perCheckoutLoss_str, 
               perLossFruitVeg_str,
               perLossItemSwitching_str,
               numStores_str,
               numCheckouts_str),
      # Value column incl format
      'Value_p.a.' = as.character(c(paste0("$",signif(total_loss/10e8, digits=3), ' billion')
                             ,paste0("$",format(signif(selfCheckoutLoss/10e5, digits=3), big.mark=","), ' million')
                             ,paste0("$",format(signif(fruitAndVegLoss/10e5, digits=3), big.mark=","), ' million')
                             ,paste0("$",format(signif(itemSwitchLoss/10e5, digits=3), big.mark=","), ' million')
                             ,paste0("$",format(signif(perStoreLoss, digits=3), big.mark=","))
                             ,paste0("$",format(signif(perCheckoutLoss, digits=3), big.mark=","))
      )),
      # Assumption column
      Assumption = as.character(c('',
                              paste0(input$perCheckoutLoss,'%'), 
                             paste0(input$perLossFruitVeg,'%'),
                             paste0(input$perLossItemSwitching,'%'),
                             paste0(input$numStores,'%'),
                             input$numCheckouts)), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
}