#------------------------------------
# Sidebar UI 
# -----------------------------------

output$columns <- renderUI({
  cols <- varnames()
  div(
    selectInput("columns", "Select columns to show:", choices  = as.list(cols), selected = names(cols)[1:min(c(4,min(length(cols))))], multiple = TRUE),
    tags$style(type='text/css', "#columns { height: 200px; padding-bottom: 35px;}")
  )
})

output$nrRows <- renderUI({
  if(is.null(input$datasets)) return()
  dat <- getdata()
  
  # number of observations to show in dataview
  nr <- nrow(dat)
  sliderInput("nrRows", "Rows to show (max 50):", min = 1, max = nr, value = min(15,nr), step = 1)
})

#------------------------------------
# Reactive's
# -----------------------------------

getdata_sel <- reactive({
  if(is.null(input$columns))
    return()
  
  dat <- getdata()
  
  if(!all(input$columns %in% colnames(dat)))
    return()
  
  dat <- dat[, input$columns, drop=F]
  if(any(chkMat(dat)))
    dat <- flatten(dat,keep.names=F)
  dat
})

#------------------------------------
# Main UI 
# -----------------------------------

output$dataviewer <- renderTable({
  if(is.null(input$datasets) || is.null(input$columns) || is.null(getdata_sel())) return()
  
  dat <- getdata_sel()
  
  if(!is.null(transform()))
    dat <- cbind(dat,transform())
  # Show only the selected columns and no more than 50 rows at a time
  nr <- min(input$nrRows,nrow(dat))
  dat[max(1,nr-50):nr, ]
})