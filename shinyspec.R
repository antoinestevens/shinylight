#-------------------------------------
# functions used in UI's
#--------------------------------------
summary.DT <- function(x,y){
  list(rmse = sqrt(sum((x-y)^2,na.rm=T)/(length(x)-1)),
       rmsd = mean((x-y)^2)^.5,
       sdev = sd(x,na.rm=T),
       rpd =  sd(x,na.rm=T) /  sqrt(sum((x-y)^2,na.rm=T)/(length(x)-1)),
       rpiq = (quantile(x,.75,na.rm=T)-quantile(x,.25,na.rm=T))/sqrt(sum((x-y)^2,na.rm=T)/(length(x)-1)),
       r2  = cor(x,y,use="pairwise.complete.obs")^2,
       bias  = mean(x,na.rm=T)-mean(y,na.rm=T),
       SB = (mean(x,na.rm=T)-mean(y,na.rm=T))^2,
       NU = var(x,na.rm=T)*(1-lm(y~x)$coefficients[2])^2,
       LC = var(y,na.rm=T)*(1-cor(x,y,use="pairwise.complete.obs")^2),
       n = length(x))
}

chkMat <- function(x){
  if(!is.matrix(x))
    sapply(x,function(x)is.matrix(x)|is.data.frame(x))  
  else
    F
}

flatten <- function(x,keep.names=T){
  #'flatten' matrices or data.drame's within a data.frame
  ismat <- chkMat(x)
  if(any(ismat)){ 
    l <- list()
    for(i in 1:sum(ismat)){
      tmp <- x[,which(ismat)[i]]
      if(keep.names)
        colnames(tmp) <- paste(colnames(x[,which(ismat),drop=F])[i],colnames(tmp),sep=".")
      l[[i]] <- tmp      
    }
    x <- x[,!ismat,drop=FALSE]
    x <- data.frame(x,do.call("data.frame",c(l,check.names=F)),check.names=FALSE,row.names=NULL)
  } 
  x
}

NumCheck <- function(x) if (all(!is.na(suppressWarnings(as.numeric(as.character(x)))))) TRUE else FALSE

changedata <- function(addCol = NULL, addColName = "",to_mat=FALSE) {
	# function that changes data as needed
	if(is.null(addCol) || addColName == "") return()

  # We don't want to take a reactive dependency on anything
  isolate({
    if(!to_mat){
  	  values[[input$datasets]][,addColName] <- addCol
    } else {
      CALL <- attr(addCol,"call") # keep call attributes because it will be lost when converting to matrix
      wav_init <- attr(addCol,"wav_init")
      addCol <- as.matrix(addCol)
      attr(addCol,"call") <- CALL     
      attr(addCol,"wav_init") <- wav_init     
      values[[input$datasets]][[addColName]] <- addCol
    }
  }) 
}

getdata <- function(dataset = input$datasets) {
  values[[dataset]]
}  

getdata_viewer <- function() {
  dat <- flatten(values[[input$datasets]])
  dat
}

varnames <- function(){
  if(is.null(input$datasets)) return()
  
  dat <- getdata()
  cols <- colnames(dat)
  names(cols) <- paste(cols, " {", sapply(dat,class), "}", sep = "")
  cols
}

varnames_ismat <- function(){
  if(is.null(input$datasets)) 
    return()  
  cols <- varnames()      
  sel <- NULL
  ismat <- grepl("matrix|data\\.frame",names(cols))
  if(any(ismat)) 
    sel <- names(cols[ismat])[1]  
  list(sel=sel,cols=cols,ismat=ismat)
}

varnames_viewer <- function() {
  if(is.null(input$datasets)) return()
  
  dat <- getdata_viewer()
  cols <- colnames(dat)
  names(cols) <- paste(cols, " {", sapply(dat,class), "}", sep = "")
  cols
}
#' @title Fast (group) summary of spectral data
#' @description
#' Fast (group) summary of spectral data using the \code{data.table} package
#' @param spc numeric \code{data.frame} or \code{matrix} to process
#' @param indices \code{data.frame} of factor variables used to summarize data in groups
#' @param fun function to summarize spectra
#' @param ... additional args to fun
#' @note indices can be a \code{data.frame} with only one column
bySpectra <- function(spc,indices,fun=mean,...){
  require(data.table)
  tmp  <- data.table(indices,spc,check.names=F)  
  setkeyv(tmp,colnames(indices))
  return(as.data.frame(tmp[,lapply(.SD,fun,...),by=eval(colnames(indices))]))  
}

plotSpectra <- function(spc,wav=NULL,group=NULL,col=NULL,linetype=NULL,wr=NULL,brk=10,ylab,xlab,...){
  # Function to plot spectra, based on the ggplot2 package
  # spc = spectral matrix, with colnames = wavelengths
  # group = grouping variable, usually the id's of the sample
  # wr = wavelength range to plot
  # brk = breaks of the x-axis
  if(!is.null(wav)){
     colnames(spc) <- wav  
    if(length(wav)!=ncol(spc)) stop("length(wav) should be equal to ncol(spc)")    
  } 
  if(is.null(group)) group  <- as.character(1:nrow(spc))
  spc$group <- group
  spc$colour <- col
  spc$linetype <- linetype
  id.var  <- colnames(spc)[grep("group|colour|linetype",colnames(spc))]
  tmp <- melt(spc,id.var=id.var)    
  if(NumCheck(tmp$variable)){
    tmp$variable <- as.numeric(as.character(tmp$variable))
    if(!is.null(wr)) tmp <- tmp[tmp$variable>=min(wr)&tmp$variable<=max(wr),]
  }
  p <- ggplot(tmp,aes(variable,value,group=group)) + labs(x=xlab,y=ylab) 
  if(NumCheck(tmp$variable)){
    brk  <- pretty(tmp$variable,n=brk) 
    p <- p + scale_x_continuous(breaks=brk)
  }
  if(is.null(col)&is.null(linetype))   p <- p + geom_line(aes(colour=group)) 
  else if(!is.null(col)&is.null(linetype))   p <- p + geom_line(aes(colour=colour)) 
  else if(is.null(col)&!is.null(linetype))   p <- p + geom_line(aes(colour=group,linetype=linetype)) 
  else  p <- p + geom_line(aes(colour=colour,linetype=linetype)) 
  return(p + theme_bw())
}

printSpectra <- function(f,w,h,p,saveAs) {
  if(saveAs=="pdf"){
    pdf(file=f, onefile=T, width=w,height=h)
    p
    dev.off()
  } else {
    png(file=f,width=w,height=h,res=300,units="in")        
    p
    dev.off()
  }
}

plotAndSave_old <- function(p,fname,w,h,saveAs,b){
  #p = plot
  #b = button
  div(class="row",
    div(class="span1",''),
    div(class="span7",plotOutput(p)),
    div(class="span4", # creating a new column which spans 4
        div(class="row-fluid",
          div(class="span6",textInput(fname,"File name",value = "")),
          div(class="span3",selectInput(saveAs, "Ext.", choices = c('pdf','png'), selected = NULL, multiple = FALSE)),
          tags$style(type='text/css', paste0("#",saveAs," { width: 85px;}")),
          tags$style(type='text/css', paste0("#",fname," { width: 125px;}"))
        ),
        div(class="row-fluid",
          div(class="span6",numericInput(w,"width (in)",value = "8"),tags$style(type='text/css', paste0("#",w," { width: 70px;}"))),
          div(class="span6",numericInput(h,"height (in)",value = "8"),tags$style(type='text/css', paste0("#",h," { width: 70px;}")))
        ),
        downloadButton(b, 'Save plot'),
        tags$style(type='text/css', paste0("#",b," { vertical-align: top; height: 18.5px; width: 70px;}"))
    )
  )
}

# plot equation and r2 on ggplot2 
# (based on SO http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph)
lm_eqn <- function(mm) {  
    foreach(m = mm,.combine=c)%do%{
    l <- list(a = format(coef(m)[1], digits = 2),
              b = format(abs(coef(m)[2]), digits = 2),
              r2 = format(summary(m)$r.squared, digits = 3));
    
    if (coef(m)[2] >= 0)  {
      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
    } else {
      eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
    }
    
    as.character(as.expression(eq))
    }       
}