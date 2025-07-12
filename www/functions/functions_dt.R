# Default datatable
dataTableDefault <- function(df,
                             colnames = c(),
                             height = 380,
                             dom = "t",
                             filter = "top_left",
                             scrollX = TRUE,
                             selection = "single",
                             rownames = FALSE,
                             escape = TRUE,
                             pageLength = NA,
                             collapse_height = FALSE){
 
  if (length(colnames) == 0){
    colnames <- names(df)
  }
  if (grepl("f", dom, fixed = TRUE)){
    height <- height - 30
  }
  if (grepl("i", dom, fixed = TRUE)){
    height <- height - 30
  }
  if (is.na(pageLength)){
    pageLength <- nrow(df)
  }
  
  if(((pageLength * 36) + (pageLength-1) + 50) < height && collapse_height == TRUE){
    height <- (pageLength * 36 + (pageLength-1)) + 50
  }
  
  dt <- datatable(df,
                  options = list(dom = dom,
                                 filter = filter,
                                 pageLength = pageLength,
                                 scrollY = paste0(height-50,"px"),
                                 scrollX = scrollX),
                  selection = selection,
                  rownames = rownames,
                  colnames = colnames,
                  escape = escape,
                  callback = JS("$('.dataTables_scrollBody').css('border', '0px');"))
  return(dt)
}









