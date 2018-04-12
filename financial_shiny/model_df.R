year = seq(2018,2050,1)
year_n = yr-2018

# Slider Values
market_2018 = 8*10e9
CAGR = 0.32
addressable = 0.8

# Generate vectors from other values

  # Saturation
  # Sliders
  sat_k = 0.55
  sat_L = 20 #tweak this for scenario
  sat_xo = 2024
  # Vector
  saturation = sat_L/(1+exp(-sat_k*(year-sat_xo)))

  # Velocity
  # Sliders
  vel_k = 0.3
  vel_L = 15
  vel_xo = 2023
  # Vector
  velocity = vel_L/(1+exp(-vel_k*(year-vel_xo)))
  
  # Number of tokens
  
  # ICO 
  frankl_minted = 600*10e3*10e6
  ico1 = 0.55 #var for modelling
  ico2 = 0.1 #build in 33% if ico1 doesnt happen
  
  ico_v = year_n*0
  ico_v[1] = ico1
  ico_v[2] = ico2
  
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
  

# Market size > PQ
market_total = market_2018*(1+CAGR)^year_n
market_addressable = market_2018 * addressable
market_share = market_addressable * saturation #PQ

# Monetary Base M = PQ/V
monetary_base = market_share / velocity

# Token Value M/n
value_utility = monetary_base / tokens_used
discount_rate = 0.30
# Note, this mus be corrected to a rolling 10 year discount
value_discounted = value_utility/(1+discount_rate)^year_n

# Ratios
ratio_market_to_utility = value_discounted / value_utility

df = data.frame(Val=value_discounted, value_utility )
#plot(df[1:10,], "Market")
matplot(year, df, type = c("l"),pch=1,col = 1:4)

# 3PM- 5PM

