#------------------------------------
# Sidebar UI 
# -----------------------------------

output$ui_calval <- renderUI({
  v <- varnames_ismat()
  if(is.null(v))
    return()
  dat <- getdata()
  nrows <- nrow(dat)
  
  wellPanel(
    selectInput("calval_fun", "Sampling functions:",
                list("Kennard-Stone" = "kenStone","DUPLEX" = "duplex","SELECT" = "shenkWest", 
                     "Honigs" = "honigs", "Naes" = "naes","Random" = "ran","Stratified Random"="sran"),selected=NULL,multiple=F),    
    conditionalPanel(condition="input.calval_fun != 'ran'",
                     selectInput("calval_columns", "Select data:", choices  = as.list(v$cols), selected = v$sel, multiple = TRUE)
    ),
    conditionalPanel(condition="input.calval_fun != 'shenkWest' && input.calval_fun != 'duplex'",
                     sliderInput(inputId="k",label="Number of cal samples:",value=2,min=2,max=nrows,step=1)),
    conditionalPanel(condition="input.calval_fun == 'duplex'",
                     sliderInput(inputId="k_dup",label="Number of cal/val samples:",value=2,min=2,max=floor(nrows/2),step=1)),
    conditionalPanel(condition="input.calval_fun == 'shenkWest'",  
                     numericInput(inputId="calval_pc",label="Number of PC's/ Explained variance:",value=.99,min=0,step=0.001),
                     tags$style(type='text/css', "#calval_pc { width: 100px;}")               
    ),
    conditionalPanel(condition="input.calval_fun == 'kenStone' || input.calval_fun == 'duplex' || input.calval_fun == 'naes'",
                     checkboxInput(inputId="pcTRUE",label="Distance in PC space",FALSE),
                     conditionalPanel("input.pcTRUE",
                                      numericInput(inputId="calval_pc_ken",label="Number of PC's/ Explained variance:",value=.99,min=0,step=0.001),
                                      tags$style(type='text/css', "#calval_pc_ken { width: 100px;}")   
                     )
    ),
    conditionalPanel(condition="input.calval_fun == 'kenStone' || input.calval_fun == 'duplex'",
                     selectInput(inputId="calval_metric",label="Metric:",list("Euclidean" = "euclid","Mahalanobis" = "mahal"),selected="mahal",multiple=F)
    ),    
    conditionalPanel(condition="input.calval_fun == 'naes'",
                     selectInput(inputId="naes_method",label="Method:",list("closest to cluster samples" = 0,"farthest away fromt centre" = 1, "random" = 2),selected=0,multiple=F)),
    conditionalPanel(condition="input.calval_fun == 'honigs'",
                     selectInput(inputId="honigs_type",label="Input data type:",list("Absorbance" = "A","Continuum-removed reflectance" = "CR"),selected="A",multiple=F)),
    conditionalPanel(condition="input.calval_fun != 'honigs'&input.calval_fun != 'ran'&input.calval_fun!='sran'",
                     div(class='row',
                         div(class="span2 offset1", checkboxInput(inputId="calval_center",label="Center",value=TRUE)),
                         div(class="span2 offset1", checkboxInput(inputId="calval_scale",label="Scale",value=FALSE))
                     )
    ),
    conditionalPanel(condition="input.calval_fun == 'sran'",
                     numericInput(inputId="sran_ngroups",label="Nr breaks in the quantile (if numeric input) :",value=5,min=2,step=1),
                     tags$style(type='text/css', "#sran_ngroups { width: 100px; }")
    ),
    conditionalPanel(condition="input.calval_fun == 'shenkWest'",
                     numericInput(inputId="d_min",label="Minimum distance:",value=0.6,min=0,step=0.1),
                     tags$style(type='text/css', "#d_min { width: 100px; }"),               
                     checkboxInput(inputId="rm_out",label="Remove outlier",value=FALSE)),
    actionButton("action_calval",label="Submit & save as new variable"),
    textInput("cal_rename", "Variable name", 'train')
  )
})

output$ui_calval_viewer <- renderUI({
  if(is.null(input$datasets)) return()
  cols <- varnames_viewer()      
  sel <- cols[grepl("cal|val",cols)[1]]
  if(length(sel)) 
    sel <- NULL
  wellPanel(
    selectInput("calval_columns_viewer", "Select variable(s) to plot:", choices  = as.list(cols), selected = NULL, multiple = TRUE),
    selectInput("cal_columns_viewer", "Select train/test variable:", choices  = as.list(cols), selected = sel, multiple = FALSE),
    tags$hr(),
    actionButton("action_calPlot", "Make Plot")
  )
})

#------------------------------------
# Reactive's 
# -----------------------------------

getspec_cal <- reactive({
  if(is.null(input$datasets)|is.null(input$calval_columns)) 
    return()

  dat <- getdata()
  
  if(!is.null(input$calval_columns)) {
    if(!all(input$calval_columns %in% colnames(dat))) 
      return()
    dat <- flatten(dat[, input$calval_columns, drop = FALSE],keep.names=FALSE)    
  }  
  dat
})

calsel <- reactive({
  if(is.null(input$datasets)|is.null(input$calval_columns)) 
    return()
  if(is.null(input$action_calval)|input$action_calval==0) 
    return()   
  isolate({     
    dat <- as.matrix(getspec_cal())    
    if(input$calval_fun=="kenStone"){
      sel <- kenStone(dat,k=input$k,metric=input$calval_metric,pc=input$calval_pc_ken,.center=input$calval_center,.scale=input$calval_scale)    
    } else if (input$calval_fun=="duplex"){
      sel <- duplex(dat,k=input$k_dup,metric=input$calval_metric,pc=input$calval_pc_ken,.center=input$calval_center,.scale=input$calval_scale)
    } else if(input$calval_fun=="honigs"){
      sel <- honigs(dat,k=input$k,type=input$honigs_type)
    } else if (input$calval_fun=="shenkWest"){
      sel <- shenkWest(dat,d.min=input$d_min,pc=input$calval_pc,rm.outlier=input$rm_out,.center=input$calval_center,.scale=input$calval_scale)
    } else if (input$calval_fun=="naes"){
      sel <- naes(dat,k=input$k,method=input$naes_method,pc=input$calval_pc_ken,.center=input$calval_center,.scale=input$calval_scale)  
    } else if(input$calval_fun=="sran"){
      if(ncol(dat)>1)
        dat <- data.frame(x=rowMeans(dat,na.rm=T)) # if a spectral matrix, then we use the mean 
      #handle NA's in the response      
      natest <- is.na(dat[,1])      
      row.names(dat) <- 1:nrow(dat)
      if(any(natest)){
        dat <- dat[!natest,,drop=F]        
      }
      
      model <- as.numeric(row.names(dat)[createDataPartition(y=dat[,1],times=1,p=input$k/nrow(dat),groups=input$sran_ngroups,list=F)])
      sel <- list(model=model)
                    
    } else {
      sel <- list(model=sample(1:nrow(getdata()),size=input$k))
    }    
    sel  
  })      
  changedata(sel$pc, colnames(sel$pc)) # add pc scores if possible
  sel
})

getcal_col <- reactive({
  if(is.null(input$datasets)|is.null(input$calval_columns)) 
    return()
  if(is.null(input$action_calval)|input$action_calval==0) 
    return() 
  isolate({
    train <- rep("unknown",nrow(getdata()))
    sel <- calsel()    
    train[sel$test] <- "test"
    train[sel$model] <- "train"
    train <- factor(train,levels=c("unknown","test","train"))   
  }) 
  train
})

observe({
  if(is.null(input$datasets)|is.null(input$calval_columns)) 
    return()
  if(is.null(input$action_calval)|input$action_calval==0) 
    return() 
  if(is.null(getcal_col()))
    return()
  isolate({
      changedata(getcal_col(), input$cal_rename)
  })  
})

#------------------------------------
# Main UI 
# -----------------------------------

output$sel_print <- renderPrint({
  if(is.null(input$datasets)|is.null(input$calval_columns)|is.null(calsel())) 
    cat("Select variables, press 'Submit' and wait...")
  else
    cat(paste("Done!!"))
})

plotCal <- function(){
  dat <- getdata_viewer()
  par.set <- list(superpose.symbol = list(col = brewer.pal(9,"Set1"),pch=1:8))
  p <- (hexplom(dat[, input$calval_columns_viewer, drop = FALSE],par.settings=par.set,
                groups=dat[,input$cal_columns_viewer],
                auto.key=list(columns = 3),
                upper.panel= function(x,y,groups,subscripts,...){
                  panel.xyplot(x,y,groups=groups,subscripts=subscripts)},
                lower.panel=function(x,y,groups,subscripts,...){
                  panel.hexbinplot(x,y)
                  xcal <- x[subscripts][groups[subscripts]=="train"]
                  ycal <- y[subscripts][groups[subscripts]=="train"]
                  panel.xyplot(xcal,ycal,pch=3,col=brewer.pal(9,"Set1")[3])
                }      
  ))
  return(p)
}

output$calPlot <- renderPlot({  
  if(is.null(input$datasets)) 
    return()
  if(is.null(input$action_calPlot)) 
    return()
  isolate({
    if(is.null(input$calval_columns_viewer)|is.null(input$cal_columns_viewer))
      return()
    p <- plotCal()
    print(p)
  })
})

output$download_cal <- downloadHandler(
  filename = function(){
    paste(input$calName,input$saveAs_cal,sep=".")
  },
  # a function that actually opens a connection to a pdf/png and print the result to it
  content = function(f=NULL) {
    printSpectra(f,w=input$wcal,h=input$hcal,p=plotCal(),saveAs=input$saveAs_cal)    
  }
)