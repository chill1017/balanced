# function to balance a portfolio
# returns asset prices and balancing simulation

library(tidyquant)
library(tidyverse)
library(progress)
library(reshape2)
require("quadprogpp")

# prop is the proportion of total value pA should be
# assumes asset is the direct output of tq_get(x="ASSET")

portfolio_balance <- function(asset, shares_to_start=1, prop=0.5, thresh){
  asset <- asset %>% select(date, price = "open")
  alpha <- -1/( 1 - (1/prop) )
  N <- length(asset$price)
  A <- rep(NA,N); A[[1]] <- shares_to_start; dA <- 0
  p1 <- asset$price[[1]]; dp <- 0
  B <- rep(NA,N)
  B[[1]] <- (p1/alpha)*A[[1]]
  TV <- rep(NA,N); TV[[1]] <- p1*A[[1]] + B[[1]]
  for(n in 2:N){
    pn <- asset$price[[n]]
    pn_1 <- asset$price[[n-1]]
    dp <- (asset$price[[n]] - asset$price[[n-1]])
    
    if( abs(dp)>thresh ){ 
      dA <- (dp/pn)*( A[[n-1]]/(alpha+1))
    } else{ 
      dA <- 0
      }
    
    A[[n]] <- A[[n-1]] - dA
    B[[n]] <- B[[n-1]] + pn*dA
    TV[[n]] <- pn*A[[n]] + B[[n]]
    # here is where the fee should be extracted
  }
  value <- asset; 
  value$price <- TV; 
  value <- value %>% rename(total_value = price) %>% mutate(base=B)
  
  # need to include base and actual asset data to output
  
  alldata <- left_join(asset, value)
  return(alldata)
}


plot_portfolio_sim <- function(data){
  output <- ggplot(
    data=data,
    aes(date)
  ) +
    geom_line(aes(y=price), color="black") +
    geom_line(aes(y=total_value), color="red") + 
    geom_line(aes(y=base), color="blue") +
    labs(
      title = "asset price and portfolio value",
      color = ""
    ) 
}


sim_and_plot <- function(asset, shares_to_start=1, prop=0.5, thresh=0.1){
  dump <- portfolio_balance(asset, shares_to_start, prop, thresh)
  graph <- plot_portfolio_sim(dump)
  plot(graph)
  
}
