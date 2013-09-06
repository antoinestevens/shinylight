#------------------------------------
# functions used in UI 
# -----------------------------------

revFactorOrder <- function(x) {
  x <- as.factor(x)
  x <- factor(x, levels=rev(levels(x)))
  x
}

standardize_1sd <- function(x) {
  if(is.factor(x)) return(rescale(x))
  if(is.numeric(x)) return(scale(x))
  return(x)
}

centerVar <- function(x) {
  return(x - mean(x, na.rm = TRUE))
}

sq <<- function(x) x^2
inv <<- function(x) 1/x
st1 <<- standardize_1sd
cent <<- centerVar 
fct <<- as.factor
rfct <<- revFactorOrder
num <<- as.numeric
ch <<- as.character
d <<- as.Date
trans_options <- list("None" = "", "Log" = "log", "Square" = "sq", "Square-root" = "sqrt",
                      "Center" = "cent","Standardize" = "st1","Invert" = "inv",
                      "As factor" = "fct", "Rev factor order" = "rfct","cut" = "cut",
                      "As number" = "num", "As character" = "ch", "As date" = "d",
                      "Combine into one var" = "ci1v", "Uncombine" = "flatten")

#------------------------------------
# sidebar UI 
# -----------------------------------

output$ui_transform <- renderUI({
  # Inspired by Ian Fellow's transform ui in JGR/Deducer
  div(
    selectInput("tr_transfunction", "Transform columns", trans_options),
    conditionalPanel("input.tr_transfunction=='cut'",
       numericInput("ncut",label="Number of cuts:",value=2,min=1,step=1)
    ),
    conditionalPanel("input.tr_transfunction!='flatten'",
       textInput("tr_rename", "Rename (separate by ',')", '')     	
    ),
    HTML("<label>Copy-and-paste data from Excel</label>"),
    tags$textarea(id="tr_copyAndPaste", rows=3, cols=40, ""),
    tags$style(type='text/css', "#tr_copyAndPaste { width: 285px;}"),
    actionButton("addtrans", "Save new variables")
  )
})

#------------------------------------
# Reactive's 
# -----------------------------------

transform <- reactive({
	if(is.null(input$datasets) || is.null(input$tr_transfunction) || is.null(input$tr_copyAndPaste)) return()
	if(input$tr_transfunction == '' && input$tr_copyAndPaste == '') return()
  if(is.null(getdata_sel())) return()   
   
	dat <- getdata_sel()
	
	if(input$tr_transfunction != '') {
    if(!input$tr_transfunction %in% c("ci1v","flatten")){
			cn <- paste(input$tr_transfunction,colnames(dat), sep="_")
      if(input$tr_transfunction!='cut')
			  dat <- colwise(input$tr_transfunction)(dat)
      else
        dat <- colwise(input$tr_transfunction)(dat,input$ncut,include.lowest=T,dig.lab=2)
			colnames(dat) <- cn
    } else if(input$tr_transfunction == "flatten"){
      dat <- flatten(dat,keep.names=F)
    }
	}
		
  if(input$tr_copyAndPaste != '') {
		cpdat <- read.table(header=T, text=input$tr_copyAndPaste)
		cpname <- names(cpdat)
		if(cpname %in% colnames(dat)) names(cpdat) <- paste('cp',cpname,sep = '_')
		if(is.null(input$columns)) return(cpdat)
		dat <- cpdat
	}

	if(input$tr_rename != ''& input$tr_transfunction!="ci1v") {
		cvars <- input$tr_rename
		rcom <- unlist(strsplit(gsub(" ","",cvars), ","))
		names(dat)[names(dat)==input$columns] <- rcom
	}
  dat
})

observe({
	if(is.null(input$addtrans) || input$addtrans == 0) return()
	isolate({
		dat <- transform()
    if(input$tr_transfunction!="ci1v")
		  changedata(dat, colnames(dat))
    else
      changedata(dat, ifelse(input$tr_rename == '',"comb",input$tr_rename),to_mat=T)
	})
  updateSelectInput(session,"tr_transfunction",choices=trans_options,selected="None")
})