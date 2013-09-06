#------------------------------------
# Functions used in UI
# -----------------------------------

# Summary functions
meanp1sd <- function(x) mean(x) + sd(x)
meanm1sd <- function(x) mean(x) - sd(x)

#------------------------------------
# Sidebar UI
# -----------------------------------

output$ui_summarize_spec <- renderUI({
  v <- varnames_ismat()
  if(is.null(v))
    return()
   div(
    selectInput("summary_fun", "Summary functions:", list("None" = '', "mean" = "mean","median" ="median", "sd"="sd","min" = "min", "max"="max",
                                                          "mean + 1sd" = "meanp1sd" , "mean - 1sd" = "meanm1sd","quantile" = "quantile")
    ),          
    conditionalPanel(condition="input.summary_fun == 'quantile'",
                     sliderInput(inputId="q",label="Quantile",value=0.5,min=0,max=1,step=0.01)
    ),  
    selectInput("group_by", "Group by:", choices  = as.list(c('',v$cols[!v$ismat])), selected = '', multiple = TRUE)
  )
})

#------------------------------------
# Reactive's
# -----------------------------------

# Summarize the spectral matrix
getspec_summary <- reactive({
  if(is.null(input$datasets)|is.null(input$spec_columns)|is.null(input$summary_fun)|is.null(getspec())) 
    return()
  if(input$summary_fun == '')
    return()
    
  dat_summary <- getspec()
  
  if (input$summary_fun !="quantile"){
    if(input$group_by=='')
      dat_summary <- colwise(input$summary_fun,na.rm=T)(as.data.frame(dat_summary))
    else 
      dat_summary <- bySpectra(spc=dat_summary,indices=getdata()[, input$group_by,drop=F],eval(parse(text=input$summary_fun)),na.rm=T)      
  } else {
    if(input$group_by=='')
      dat_summary <- colwise(input$summary_fun,na.rm=T)(as.data.frame(dat_summary),probs=input$q)      
    else
      dat_summary <- bySpectra(spc=dat_summary,indices=getdata()[, input$group_by,drop=F],eval(parse(text=input$summary_fun),na.rm=T),input$q)        
  }      
  dat_summary
})  

NumChk_sum <- reactive({
  if(is.null(input$datasets) || is.null(input$spec_columns) || input$summary_fun == '' || is.null(getspec_summary())) return()
  
  cn <- colnames(getspec_summary())
  cn <- cn[!cn%in%input$group_by]
  nc <- NumCheck(cn)
})

#------------------------------------
# Main UI
# -----------------------------------

output$summary_table_spec <- renderTable({
  if(is.null(input$datasets) || (is.null(input$spec_columns))) return()
  dat <- getspec_summary()
})

output$downloadData_sum <- downloadHandler(
  filename = function() { paste(input$datasets[1],'.',input$saveAs_sum, sep='') },
  content = function(file) {
    
    ext <- input$saveAs_sum
    
    robj <- input$datasets[1]
    
    dat <- getspec_summary()
      
    assign(robj,dat)
    
    if(ext == 'rda' || ext == 'rdata') {
      save(list = robj, file = file)
    } else if(ext == 'csv') {
      write.csv(get(robj), file)
    }
  }
)

pSpec <- function(dat){
  if(input$group_by!=''){
    spc <- dat[,!colnames(dat)%in%input$group_by]
    p <- plotSpectra(spc,group=dat[,input$group_by],
                     brk = input$spec_xticks,ylab=input$spec_ylab,xlab=input$spec_xlab)
  } else {
    p <- plotSpectra(dat,brk=input$spec_xticks,ylab=input$spec_ylab,xlab=input$spec_xlab) + theme(legend.position="none")
  }
  print(p)
}

output$summary_plot_spec <- renderPlot({
  if(is.null(input$datasets) || is.null(input$spec_columns) || input$summary_fun == '' || is.null(getspec_summary())) return()
  if(is.null(NumChk_sum()))
    return()
  
  if(!NumChk_sum())
    return()
  dat <- getspec_summary()  
  pSpec(dat)
})

output$no_plot_sum <- renderText("All variables should have numeric colnames for plot output")
output$no_fun_sum <- renderText("Select a summary function")

output$ui_summary_plot_spec <- renderUI({
  if(is.null(input$datasets) || is.null(input$spec_columns) || input$summary_fun == '' || is.null(getspec_summary())) return()
  if(is.null(NumChk_sum()))
    return()
  
  if(!NumChk_sum()){
    textOutput("no_plot_sum")
  } else {
    plotAndSave("summary_plot_spec","spcName_sum","wspc_sum","hspc_sum","saveAs_spc_sum","download_spc_sum",800,400)    
  }
})

output$download_spc_sum <- downloadHandler(
  filename = function(){
    paste(input$spcName_sum,input$saveAs_spc_sum,sep=".")
  },
  # a function that actually opens a connection to a pdf/png and print the result to it
  content = function(f=NULL) {
      dat <- getspec_summary()  
    if(input$saveAs_spc_sum=="pdf"){
      pdf(file=f, onefile=T, width=input$wspc_sum,height=input$hspc_sum)
        pSpec(dat)
      dev.off()
    } else {
      png(file=f,width=input$wspc_sum,height=input$hspc_sum,res=300,units="in")
        pSpec(dat)
      dev.off()
    }
  }
)