#------------------------------------
# Functions used in UI
# -----------------------------------

# Transformation fun
snv <- function (X) {
  X <- sweep(X, 1, rowMeans(X,na.rm=T), "-")
  X <- sweep(X, 1, apply(X, 1, sd,na.rm=T), "/")
}
SC <- function (x){
  x <- sweep(x, 2, apply(x, 2, sd), "/")# Scale
}
CEN <- function(x) {
  x <- sweep(x, 2, colMeans(x), "-")# Center
}
SCCEN <- function(x) {
  x <- sweep(sweep(x, 2, colMeans(x), "-"),2, apply(x, 2, sd), "/")# autoscale
} 
splcorr <- spliceCorrection
cr <- continuumRemoval
bs <- blockScale
bn <- blockNorm
gd <- gapDer
sg <- savitzkyGolay
bin <- binning
AA <- function(x) {
  x <- -log10(x) #absorbance
}
RR <- function(x){              
  x <-  1/10^x #reflectance
}
badBands_NA <- function(x,wav,bb){
  idx <- wav%in%bb
  if(any(idx))
    x[,idx] <- NA
  x
}

badBands_rm <- function(x,wav,bb){
  x[,!wav%in%bb]  
}

trans_spec_options <- list("None" = '', "Remove bad bands" = "badBands_rm","Replace bad bands with NA" = "badBands_NA",
                           "Conv to Abs" = "AA", "Conv to Refl" = "RR",
                           "SNV" = "snv", "Scale" = "SC","Center" = "CEN","auto-scale" = "SCCEN",
                           "Detrend" = "detrend", "Continuum removal" = "cr", 
                           "Gap derivative" = "gd", "Savitzky-Golay" = "sg", "Moving average" = "movav",
                           "Binning" = "bin", "Resample" = "resample", "Resample2" = "resample2",
                           "Block scale" = "bs", "Block norm" = "bn","Splice correction" = "splcorr")

#------------------------------------
# Sidebar UI
# -----------------------------------

output$spec_nrRows <- renderUI({
  if(is.null(input$datasets)) return()
  dat <- getdata()
  
  #  observations to show 
  nr <- 1:nrow(dat)  
  div(
    selectInput("spec_nrRows", "Obs. to show (max 50):", choices  = nr, selected = 1:min(5,nrow(dat)), multiple = TRUE),
    tags$style(type='text/css', "#spec_nrRows { height: 150px; padding-bottom: 35px;}")
  )
})

output$spec_columns <- renderUI({
  v <- varnames_ismat()
  if(is.null(v))
    return()
  div(
    selectInput("spec_columns", "Select spectral data:", choices  = as.list(v$cols), selected = v$sel, multiple = TRUE)  ,
    tags$style(type='text/css', "#spec_columns { height: 150px; padding-bottom: 35px;}")
  )
})

output$x_slider_spec <- renderUI({
  if(is.null(input$datasets)|is.null(NumCh())) return()    
  
  if(NumCh())  
    r <- range(as.numeric(as.character(col_spec())))
  else 
    return()
  
  sliderInput("x_slider_spec","X-axis range:",min=r[1],max=r[2],value=c(r[1],r[2]))
})

output$ui_labs <- renderUI({
  div(
    tags$hr(),
    textInput(inputId="spec_xlab","X-lab"),
    textInput(inputId="spec_ylab","Y-lab"),
    numericInput(inputId="spec_xticks","# X-ticks",value=10,min=2,max=100,step=1)
  )
})

output$ui_spec <- renderUI({
  div(
    uiOutput("x_slider_spec"),
    tags$hr(),
    selectInput("spec_fun", "Pre-treatment:", trans_spec_options),
    conditionalPanel(condition="input.spec_fun!=''",
     conditionalPanel(condition="input.spec_fun!='snv'|input.spec_fun!='AA'|input.spec_fun!='RR'",
          conditionalPanel(condition="input.spec_fun=='badBands_rm'|input.spec_fun=='badBands_NA'",
                           textInput("bb","Bad bands:"),
                           tags$style(type='text/css', "#bb { width: 180px;}"),
                           tags$br(),                         
                           actionButton("action_bb","Submit"),
                           tags$style(type='text/css', "#action_bb { margin-bottom: 9px;}")
          ),                       
          conditionalPanel(condition="input.spec_fun=='gd'",        
                           sliderInput("m_gd", 
                                       "Derivative order:", 
                                       min = 1, 
                                       max = 4, 
                                       value = 1),
                           sliderInput("s_gd", 
                                       "Segment size:", 
                                       min = 1, 
                                       max = 199, 
                                       value = 1),
                           sliderInput("w_gd", 
                                       "Window size:", 
                                       min = 3, 
                                       max = 199, 
                                       value = 3,
                                       step = 1),
                           numericInput("si_gd", "Sampling interval:", 1),
                           tags$style(type='text/css', "#si_gd { width: 70px;}")
          ),
          conditionalPanel(condition="input.spec_fun=='sg'",        
                           sliderInput("m", 
                                       "Derivative order:", 
                                       min = 0, 
                                       max = 4, 
                                       value = 0),
                           sliderInput("p", 
                                       "Polynomial order:", 
                                       min = 1, 
                                       max = 5, 
                                       value = 1),
                           sliderInput("w", 
                                       "Window size:", 
                                       min = 3, 
                                       max = 199, 
                                       value = 3,
                                       step=1),
                           numericInput("si", "Sampling interval:", 1),
                           tags$style(type='text/css', "#si { width: 70px;}")
          )
          ,
          conditionalPanel(condition="input.spec_fun=='splcorr'",        
                           textInput("splice", "Splice position:","c(1100,1830)"),
                           tags$style(type='text/css', "#splice { width: 180px;}"),
                           tags$br(),
                           actionButton("action_spl","Submit"),
                           tags$style(type='text/css', "#action_spl { margin-bottom: 9px;}")
          ),
          conditionalPanel(condition="input.spec_fun=='cr'",      
                           selectInput("type_cr", "Input type",choices=c("Reflectance","Absorbance"))
          )
          ,
          conditionalPanel(condition="input.spec_fun=='bs'",        
                           selectInput("type_bs", "Type:",choices=c("soft","hard"))
          ),
          conditionalPanel(condition="input.spec_fun=='bn'",        
                           numericInput("norm", "Target norm:", 1,min=0,step=0.01),
                           tags$style(type='text/css', "#norm { width: 70px;}")
                           
          ),
          conditionalPanel(condition="input.spec_fun=='bin'",        
                           numericInput("nbins", "Number of bins:",value=NA,min=1),
                           tags$style(type='text/css', "#nbins { width: 70px;}"),
                           numericInput("sbins", "Size of bins:",value=NA,min=1),
                           tags$style(type='text/css', "#sbins { width: 70px;}")          
          ),
          conditionalPanel(condition="input.spec_fun=='movav'",        
                           sliderInput("w_mov", 
                                       "Window size:", 
                                       min = 1, 
                                       max = 199, 
                                       value = 3)
          ),
          conditionalPanel(condition="input.spec_fun=='resample'",  
                           textInput("newwav_res", "New Wavelength vector:",""),
                           tags$style(type='text/css', "#newwav_res { width: 180px;}"),
                           tags$br(),
                           actionButton("action_res","Submit"),
                           tags$style(type='text/css', "#action_res { margin-bottom: 9px;}")
          ),
          conditionalPanel(condition="input.spec_fun=='resample2'",      
                           textInput("newwav_res2", "New Wavelength vector:",""),
                           tags$style(type='text/css', "#newwav_res2 { width: 180px;}"),
                           textInput("fwhm", "fwhm(s):"),
                           tags$style(type='text/css', "#fwhm { width: 180px;}"),
                           tags$br(),
                           actionButton("action_res2","Submit"),
                           tags$style(type='text/css', "#action_res2 { margin-bottom: 9px;}")
          )
     ),          
     actionButton("addtrans_spec", "Save as new variable(s)"),
     tags$style(type='text/css', "#addtrans_spec { margin-bottom: 10px;}"),
     textInput("rename_spec", "Prefix new variable(s) with:", '')    
    )
  )  
})

#------------------------------------
# Reactive's
# -----------------------------------

# Return the requested spectral matrix 
getspec <- reactive({
  if(is.null(input$datasets)|is.null(input$spec_columns)) 
    return()
  
  dat <- getdata()
  
  if(!all(input$spec_columns %in% colnames(dat))) 
    return()
  
  dat <- dat[, input$spec_columns,drop=F]
  
  if(length(input$spec_columns)==1)
    if(!chkMat(dat)[1])
      return()
  
  if(length(input$spec_columns)==1){
    CALL <- attr(dat[, input$spec_columns],"call") # store previous pre-processing call 
    wav_init <- attr(dat[, input$spec_columns],"wav_init") # store initial wavelengths 
  } else {
    CALL <- NULL  
    wav_init <- NULL
  }
  
  dat <- flatten(dat,keep.names=F)  
  attr(dat,"call") <- CALL    
  attr(dat,"wav_init") <- wav_init
  
  dat
})

# Return the requested spectral matrix only for vizualisation
getspec_viz <- reactive({
  if(is.null(input$datasets)|is.null(input$spec_columns)|is.null(getspec())) 
    return()
  
  getspec()[as.numeric(input$spec_nrRows),] 
})


col_spec <- reactive({
  if(is.null(input$datasets)|is.null(getspec())) 
    return()    
  colnames(getspec())
})

NumCh <- reactive({
  if(is.null(input$datasets)|is.null(col_spec()))
    return()    
  NumCheck(col_spec())
})

# Transform the spectral matrix
getspec_trans <- function(viz=F){
  if(is.null(getspec())|is.null(input$spec_fun)|is.null(NumCh())) 
    return()
  if(input$spec_fun=='')
    return()
  if(!NumCh())
    stop("Error, numeric colnames are required")
  
  wav <- as.numeric(col_spec())
  
  if(viz)
    q <- quote(getspec()[as.numeric(input$spec_nrRows),])
  else
    q <- quote(getspec())
  
  if (input$spec_fun %in%c("snv","AA","RR","SC","CEN","SCCEN")){
    CALL <- call(input$spec_fun,q)  
    dat <- eval(CALL)
  } else if (input$spec_fun == "detrend"){
    CALL <- call("detrend",q,wav)      
    dat <- eval(CALL)
  }  else if (grepl("badBands",input$spec_fun)){
    if(is.null(input$action_bb)|input$action_bb==0) return()          
    isolate({              
      bb <- eval(parse(text=input$bb))
      CALL <- call(input$spec_fun,q,wav=wav,bb=bb)
      dat <- eval(CALL)
    })
  } else if (input$spec_fun == "sg"){
    CALL <- call("savitzkyGolay",q,input$m,input$p,input$w,delta.wav=input$si)   
    dat <- eval(CALL)
  } else if (input$spec_fun == "gd"){
    CALL <- call("gapDer",q,input$m_gd,input$w_gd,input$s_gd,delta.wav=input$si_gd)
    dat <- eval(CALL)
  } else if (input$spec_fun == "splcorr"){
    if(is.null(input$action_spl)|input$action_spl==0) return()
    isolate({      
      try(splice <- eval(parse(text=input$splice)))      
      CALL <- call("spliceCorrection",q,wav,splice)
      dat <- eval(CALL)
    })
  } else if (input$spec_fun == "cr"){    
    type <- substr(input$type_cr,1,1)              
    CALL <- call("continuumRemoval",q,wav,type)    
    dat <- eval(CALL)
  } else if (input$spec_fun == "bs"){
    CALL <- call("blockScale",q,type=input$type_bs)
    dat <- eval(CALL)$Xscaled
  } else if (input$spec_fun == "bn"){
    CALL <- call("blockNorm",q,input$norm)
    dat <- eval(CALL)$Xscaled
  } else if (input$spec_fun == "bin"){
    if(is.na(input$nbins)&is.na(input$sbins)) return()
    if(!is.na(input$nbins)){
      CALL <- call("binning",q,bins=input$nbins)
      dat <- eval(CALL)
    } else {
      CALL <- call("binning",q,bin.size = input$sbins)
      dat <- eval(CALL)
    }
  } else if (input$spec_fun == "movav"){
    CALL <- call("movav",q,input$w_mov)
    dat <- eval(CALL)
  } else if (input$spec_fun == "resample"){
    if(is.null(input$action_res)|input$action_res == 0) return()
    isolate({      
      try(newwav_res <- eval(parse(text=input$newwav_res)))
      if(is(newwav_res,'try-error')) return()         
      CALL <- call("prospectr::resample",q,wav,newwav_res)
      dat <- eval(CALL)
    })
  } else if (input$spec_fun == "resample2"){
    if(is.null(input$action_res2)|input$action_res2 == 0) return()
    isolate({
      try(newwav_res2 <- eval(parse(text=input$newwav_res2)))
      try(fwhm <- eval(parse(text=input$fwhm))       )
      if(is(newwav_res2,'try-error')) return()   
      if(is(fwhm,'try-error')) fwhm <- NULL      
      CALL <- call("resample2",q,wav,newwav_res2,fwhm)
      dat <- eval(CALL)
    })
  }      
  if(is.null(attr(getspec(),"call")))
    attr(dat,"call") <- CALL
  else  
    attr(dat,"call") <- list(attr(getspec(),"call"),CALL)    
  if(is.null(attr(getspec(),"wav_init")))
    attr(dat,"wav_init") <- wav
  else
    attr(dat,"wav_init") <- attr(getspec(),"wav_init")
  
  dat
} 

observe({
  if(is.null(input$addtrans_spec) || input$addtrans_spec == 0) return()
  isolate({
    dat <- getspec_trans()    
    
    if(length(input$spec_columns)==1){
      if(input$rename_spec!='') 
        addColName <- paste(input$rename_spec,input$spec_columns,sep="_")
      else 
        addColName <- paste(input$spec_fun,input$spec_columns,sep="_")
    } else {
      if(input$rename_spec!='') 
        addColName <- input$rename_spec
      else 
        addColName <- input$spec_fun
    }    
    changedata(dat, addColName,to_mat=T)
  })  
  updateSelectInput(session,"spec_fun",choices=trans_spec_options,selected="None")
})

#------------------------------------
# Main UI
# -----------------------------------

plotS <- function(){
  if(is.null(NumCh())|is.null(input$x_slider_spec))
    return()
  if(!NumCh())
    stop("Error, numeric colnames are required for plot output")
  
  p <- plotSpectra(getspec_viz(),wav=as.numeric(col_spec()),group=as.factor(input$spec_nrRows),
                   wr=input$x_slider_spec,xlab=input$spec_xlab,
                   ylab=input$spec_ylab,brk=input$spec_xticks) + scale_colour_discrete("Sample ID") 
  return(p)
}

plotStr <- function(){
  if(is.null(getspec_trans()))
    return()
  
  dat <- as.data.frame(getspec_trans(viz=T))
  wav <- as.numeric(colnames(dat))
  p <- plotSpectra(dat,wav=wav,group=as.factor(input$spec_nrRows),
                   wr=input$x_slider_spec,xlab=input$spec_xlab,
                   ylab=input$spec_ylab,brk=input$spec_xticks) + scale_colour_discrete("Sample ID") 
  return(p)
}

# Generate a plot of the spectral data
output$SpecPlot <- renderPlot({
  if(is.null(plotS()))
    return()
  p <- plotS()
  print(p)
})

# Generate a plot of the transformed spectra
output$TransPlot <- renderPlot({ 
  if(is.null(plotStr()))
    return()  
  p <- plotStr()
  print(p)
})

output$download_spc <- downloadHandler(
  filename = function(){
    paste(input$spcName,input$saveAs_spc,sep=".")
  },
  # a function that actually opens a connection to a pdf/png and print the result to it
  content = function(f){
    printSpectra(f,w=input$wspc,h=input$hspc,p=plotS(),saveAs=input$saveAs_spc)
  }
)

output$download_spctr <- downloadHandler(
  filename = function(){
    paste(input$spcNametr,input$saveAs_spctr,sep=".")
  },
  # a function that actually opens a connection to a pdf/png and print the result to it
  content = function(f) {
    printSpectra(f,w=input$wspctr,h=input$hspctr,p=plotStr(),saveAs=input$saveAs_spctr)    
  }
)