#------------------------------------
# Main UI 
# -----------------------------------

output$summarize_data <- renderPrint({
  if(is.null(input$datasets) | is.null(input$columns) | is.null(getdata_sel())) 
     return(cat("Select variables\n"))
 
  cat("\nVariables summary:\n")
  psych::describe(getdata_sel())		
})