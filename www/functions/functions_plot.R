# Function to plot a horizontal progress Bar
progressBarPlot <- function(total, val, col_val, col_background){
  
  p <- ggplot(data.frame(X_POSI = 1,IDENT  = c("val", "total-val"),VALUE  = c(val, total-val)),
              aes(x = X_POSI, y = VALUE))+
    geom_col(aes(fill = IDENT), width = 0.7,
             fill = c(col_val, col_background))+ 
    coord_flip()+
    theme(
      plot.margin = unit(c(0,0,0,0), "cm"),
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background = element_rect(fill = '#353c42', colour = '#353c42'),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_rect(fill = '#353c42', colour = '#353c42')
    )
  return(p)
}

# Function to generate an empty plot
mmEmptyPlotter <- function(title, text_col = "#bec5cb"){
  p <- plotly_empty(type = "scatter", 
                    mode = "markers") %>%
    config(displayModeBar = FALSE) %>%
    layout(title = list(
           text = title,
           yref = "paper",
           y = 0.5,
           font = list(color = text_col)),
           paper_bgcolor = '#272c30',
           plot_bgcolor = '#272c30')
  return(p)
}
