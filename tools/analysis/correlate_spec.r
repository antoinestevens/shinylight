#------------------------------------
# Sidebar UI
# -----------------------------------

output$x_slider_cor <- renderUI({
  if(is.null(input$datasets)|is.null(NumCh_cor())) return()    
  
  if(NumCh_cor())  
    r <- range(sapply(col_cor(),as.numeric))
  else 
    return()
  
  sliderInput("x_slider_cor","X-axis range:",min=r[1],max=r[2],value=c(r[1],r[2]))   
})

output$cor_columns <- renderUI({
  v <- varnames_ismat()
  if(is.null(v))
    return()
  selectInput("cor_columns", "Select variable(s) to correlate with:", choices  = as.list(v$cols[grepl("numeric",names(v$cols))]), selected = NULL, multiple = TRUE) # only numeric
})

#------------------------------------
# Reactive's 
# -----------------------------------

getspec_cor <- reactive({
  if(is.null(input$datasets)|is.null(input$spec_columns)) 
    return()
  
  if(length(input$spec_columns)==1){
    dat <- list(getspec())  
  } else {
    dat <- getdata()
    if(all(chkMat(dat[,input$spec_columns,drop=F]))){
      dat <- foreach(i = 1:length(input$spec_columns))%do%flatten(dat[, input$spec_columns[i], drop = FALSE],keep.names=F)  
    } else {
      dat <- list(dat[, input$spec_columns, drop = FALSE])
    }
  } 
  dat
})

col_cor <- reactive({  
  if(is.null(getspec_cor())) return()  
  cols <- lapply(getspec_cor(),colnames)
  cols
})

NumCh_cor <- reactive({
  if(is.null(col_cor())) return()  
  nc <- all(sapply(col_cor(),NumCheck))
  nc
})

#------------------------------------
# Main UI
# -----------------------------------

output$no_cor_cols <- renderText("Select variables to correlate with")

plotC <- function(){
  if(is.null(NumCh())|is.null(input$x_slider_cor)|is.null(input$cor_columns))
    return()
  if(!NumCh_cor())
    stop("Error, numeric colnames are required")
  
  dat <- getspec_cor()  
  dat_ini <- getdata()
  correlateWith <- dat_ini[,input$cor_columns,drop=F]
  ismat <- all(chkMat(dat_ini[,input$spec_columns,drop=F]))    
  wav <- lapply(col_cor(),function(x)as.numeric(x))
  
  cors <- foreach(i = 1:length(dat),.combine=rbind)%do%{
    cors <- as.data.frame(cor(dat[[i]],correlateWith,use="pairwise.complete.obs"))
    if(ismat)
      cors$input <- input$spec_columns[i]
    else
      cors$input <- ""
    cors$wav <- wav[[i]]
    cors
  }
  
  cors <- cors[cors$wav>=min(input$x_slider_cor)&cors$wav<=max(input$x_slider_cor),]
  cors <- melt(cors,id.vars=c("input","wav"))  
  p <- ggplot(data=cors,aes(x=wav,y=value,colour=variable))   + theme_bw() 
  if(length(dat)>1)
    p <- p + geom_line(aes(linetype=input)) 
  else 
    p <- p + geom_line()
  p <- p + labs(x=input$spec_xlab,y="Correlation")
  brk  <- pretty(do.call(c,wav),n=input$spec_xticks) 
  p <- p + scale_x_continuous(breaks=brk)  
  return(p)
}

# Generate a plot of the correlations 
output$CorPlot <- renderPlot({
  p <- plotC()
  print(p)
})

output$download_cors <- downloadHandler(
  filename = function(){
    paste(input$corsName,input$saveAs_cors,sep=".")
  },
  # a function that actually opens a connection to a pdf/png and print the result to it
  content = function(f=NULL) {
    printSpectra(f,w=input$wcors,h=input$hcors,p=plotC(),saveAs=input$saveAs_cors)   
  }
) 