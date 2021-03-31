

# Library
library(networkD3)
library(dplyr)

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("ROE","ROE", "ROIC", "ROIC", "Non-operating return", "Non-operating return"), # use rep() and map
  target=c("ROIC","Non-operating return", "NOPM", "NOAT", "FLEV", "Spread"), 
  value=c(0.15, # ROIC
          0.05, # Non-operating return
          0.075, # NOPM
          2, # NOAT
          0.3, # Financial leverage
          0.1666667) # Spread
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyBasic1.html"))