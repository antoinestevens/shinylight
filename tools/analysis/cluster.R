#------------------------------------
# Sidebar UI
# -----------------------------------

output$ui_kmeansClustering <- renderUI({
  div(
    numericInput("km_seed", "Set random seed:", 1234, min = 0),
    sliderInput(inputId = "km_nrClus", label = "Number of clusters:", min=2,max=20,value=2),
    checkboxInput("clus_pcTRUE","Cluster on PC space?",TRUE),
    conditionalPanel("input.clus_pcTRUE",
                     div(class='row',
                         div(class="span2 offset1", checkboxInput(inputId="center_clus",label="Center",value=TRUE)),
                         div(class="span2 offset1", checkboxInput(inputId="scale_clus",label="Scale",value=TRUE))
                     ),
                     tags$style(type="text/css", '#center_clus {width: 50px; margin-right: 30px;}'),
                     tags$style(type="text/css", '#scale_clus {width: 50px;}'),
                     numericInput("clus_pc_Nr","Number of PC's/Explained variance:",value=.95,step=0.001),
                     tags$style(type='text/css', "#clus_pc_Nr { width: 80px;}")
    ),
    actionButton("action_clus", "Submit"),
    tags$style(type='text/css', "#action_clus { margin-bottom: 9px;}"),
    actionButton("km_saveclus", "Save cluster membership"),
    tags$style(type='text/css', "#km_saveclus { margin-bottom: 9px;}")
  )
})

#------------------------------------
# Reactive's
# -----------------------------------

kmeansClustering <- reactive({
  if(is.null(input$datasets)|is.null(input$explo_vars)) 
    return()
  if(is.null(input$action_clus)|input$action_clus==0)
    return()
  isolate({
    set.seed(input$km_seed)
    dat <- flatten(getdata()[,input$explo_vars,drop=F])
    
    if(input$clus_pcTRUE&ncol(dat)>1){
      pca <- prcomp(dat, center = input$center_clus, scale = input$scale_clus)
      pc <- input$clus_pc_Nr
      if (pc < 1) {
        pvar <- pca$sdev^2/sum(pca$sdev^2)
        pcsum <- cumsum(pvar) < pc
        if (any(pcsum)) 
          pc <- max(which(pcsum)) + 1
        else pc <- 1
      }             
      dat <- sweep(pca$x[, 1:pc, drop = F], 2,pca$sdev[1:pc], "/")
    }    
    list(km=kmeans(na.omit(object = data.frame(dat,check.names=F)), centers = input$km_nrClus, nstart = 10, iter.max = 500),
         dat=dat)    
  })
})

observe({
  if(is.null(input$km_saveclus) || input$km_saveclus == 0) return()
  isolate({
    clusmem <- kmeansClustering()$km$cluster
    changedata(as.factor(clusmem), paste("kclus",input$km_nrClus,sep=""))
  })
})

#------------------------------------
# Main UI
# -----------------------------------

output$kmeansClustering <- renderPrint({
  if(is.null(input$datasets))
    return(cat(""))  
  if(is.null(input$action_clus)| input$action_clus==0)
      return(cat("Select variable and press 'Submit'"))
  if(is.null(kmeansClustering()))
    return(cat(""))
  dat <-kmeansClustering()$km
  dat$cluster <- NULL
  print(dat)
})

output$kmeansPlot1 <- renderPlot({
  if(is.null(input$action_clus)| input$action_clus==0)
    return()
  tmp  <- data.table(cl=kmeansClustering()$km$cluster,getdata()[,input$explo_vars],check.names=F)  
  setkeyv(tmp,"cl")
  tmp <- (as.data.frame(tmp[,lapply(.SD,mean),by=cl]))  
  p <- plotSpectra(tmp[,-1],xlab=input$clus_xlab,ylab=input$clus_ylab,brk=input$clus_xticks) + scale_colour_discrete("Cluster #")
  print(p)
})

output$kmeansPlot2 <- renderPlot({
  if(is.null(input$action_clus)| input$action_clus==0)
    return()
  
  dat <- kmeansClustering()$dat
  if(ncol(dat)<=1)
    return()
  dat <- data.frame(dat[,1:min(10,ncol(dat))],check.names=F)
  dat$cc <- kmeansClustering()$km$cluster
  par.set <- list(superpose.symbol = list(col = brewer.pal(9,"Set1"),pch=1:9))  
  print(splom(~dat[,-ncol(dat)],data=dat,groups=cc,par.settings=par.set,auto.key=list(columns=4)))
})
