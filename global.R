library(shiny)
library(shinydashboard)
library(shiny.semantic)
library(leaflet)
library(leaflet.extras2)
library(dplyr)

# Function to set ship type colors
ship_type_color <- function(df) {
  
  ship_type_color <- data.frame(
    ship_type = c("Cargo", "Tanker", "Tug", "Fishing", "Passenger", "Pleasure", "Navigation", "High Special" , "Unspecified"),
    type_color = c("white", "grey", "orange", "green", "red", "darkgrey", "yellow", "pink", "black")
  )
  
  rdf <- merge(df, ship_type_color, by="ship_type", all.x = TRUE)
  return(rdf)
  
}