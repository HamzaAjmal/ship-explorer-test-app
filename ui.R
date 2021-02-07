semanticPage(grid(
  grid_template = grid_template(
    default = list(
      areas = rbind(c("title", "ship_type", "ship_name"), c("map","map", "map")),
      cols_width = c("400px", "1fr"),
      rows_height = "100px", "400px","auto")
    ),
  area_styles = list(title = "margin: 40 px;", ship_type = "margin: 20px;", ship_name = "margin: 20px;"),
  
  title = h2(class = "ui_header", icon("ship"), div(class =  "content", "Ship Explorer")),
  ship_type = uiOutput("ship_type"),
  ship_name = uiOutput("ship_name"),
  map = leafletOutput("map", height = '800')
))