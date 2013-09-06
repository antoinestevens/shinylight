#------------------------------------
# Sidebar UI 
# -----------------------------------

output$explo_vars <- renderUI({
  v <- varnames_ismat()
  if(is.null(v))
    return()
  selectInput(inputId = "explo_vars", label = "Variables:", choices = as.list(v$cols), selected = v$sel, multiple = TRUE)
})

output$ui_pca <- renderUI({
  div(
    tags$br(),
    div(class='row',
        div(class="span2 offset1", checkboxInput(inputId="center_pca",label="Center",value=TRUE)),
        div(class="span2 offset1", checkboxInput(inputId="scale_pca",label="Scale",value=TRUE))
    ),
    tags$style(type="text/css", '#center_pca {width: 50px; margin-right: 30px;}'),
    tags$style(type="text/css", '#scale_pca {width: 50px;}'),
    actionButton("action_pca", "Submit"),
    tags$style(type='text/css', "#action_pca { margin-bottom: 5px;}"),
    conditionalPanel(condition="input.action_pca != 0",
                     uiOutput("pca_Nr"),
                     tags$style(type='text/css', "#pca_Nr { width: 85px;}"),
                     actionButton("pca_save", "Save PCA scores")
    )
  )
})

output$pca_Nr <- renderUI({
  if(is.null(input$explo_vars))
    return()
  dat <- flatten(getdata()[,input$explo_vars,drop=F])  
  numericInput(inputId = "pca_Nr", label = "Number of PC to save", min=1,max=ncol(dat),value=1,step=1)
  
})

output$ui_pcaPlot <- renderUI({
  if(is.null(input$datasets))
    return()
  dat <- flatten(getdata()[,input$explo_vars])  
  vars <- varnames()
  if(is.null(vars)) 
    return()
  div(
    conditionalPanel("input.tabpca == 'Loadings'",
      div(
        tags$hr(),
        numericInput(inputId="pca_Nr_plot1",label="Number of PC to plot",value=1,min=1,max=ncol(dat),step=1),
        tags$style(type='text/css', "#pca_Nr_plot1 { width: 100px;}"),
        textInput(inputId="pca_xlab","X-lab"),
        textInput(inputId="pca_ylab","Y-lab"),
        numericInput(inputId="pca_xticks","# X-ticks",value=10,min=2,max=100,step=1),
        tags$style(type='text/css', "#pca_xticks { width: 85px;}")    
      )
    ),
    conditionalPanel("input.tabpca == 'Scatter Plot'",
      div(
        tags$hr(),
        numericInput(inputId="pca_Nr_plot2",label="Number of PC to plot",value=1,min=1,max=ncol(dat),step=1),
        tags$style(type='text/css', "#pca_Nr_plot2 { width: 100px;}"),
        selectInput(inputId="var_splom",label="Color variable:",choices=vars,selected=NULL,multiple=F)
      )
    )
  )
})

#------------------------------------
# Reactive's 
# -----------------------------------

pca <- reactive({
  if(is.null(input$datasets)|is.null(input$explo_vars)) 
    return()
  if(is.null(input$action_pca)|input$action_pca==0)
    return()
  isolate({
    dat <- getdata()[,input$explo_vars]
    pca <- prcomp(dat,scale=input$scale_pca,center=input$center_pca)
  })
})

observe({
  if(is.null(input$pca_save) || input$pca_save == 0) return()
  isolate({
    sc <- scores(pca())[,1:input$pca_Nr]
    changedata(sc, "PC",to_mat=T)
  })
})

#------------------------------------
# Main UI
# -----------------------------------

output$pcaSummary <- renderPrint({  
  if(is.null(input$datasets))
    return(cat(""))  
  if(is.null(input$action_pca)|input$action_pca==0)
    return(cat("Select variables and press 'submit'"))      
  if(is.null(pca()))
    return(cat(""))
    dat <- pca() 
    pvar <- dat$sdev^2/sum(dat$sdev^2)
    a <- pvar>=0.0001
    dat$sdev <- dat$sdev[a]
    dat$x <- dat$x[,a]
    dat$rotation <- dat$rotation[,a]
    print(cbind(round(summary(dat)$importance,4),c("...","...","...")),quote=F)    
})

output$pcaPlot1 <- renderPlot({
  if(is.null(input$datasets)|is.null(input$explo_vars)) 
    return()
  if(is.null(input$pca_Nr_plot1))
    return()
  if(is.null(input$action_pca)|input$action_pca==0)
    return()
  Nr <- input$pca_Nr_plot1 
  input$pca_xlab;input$pca_ylab;input$pca_xticks # get reactivity
  isolate({
    dat <- pca() 
    l <- t(loadings(dat)[,1:Nr])
    p <- plotSpectra(data.frame(l,check.names=F),linetype=as.factor(1:Nr),xlab=input$pca_xlab,ylab=input$pca_ylab,brk=input$pca_xticks) + scale_colour_discrete("Loadings #") + scale_linetype_discrete("Loadings #")
    return(print(p))
  })  
})

output$pcaPlot2 <- renderPlot({
  if(is.null(input$datasets)|is.null(input$explo_vars)) 
    return() 
  if(is.null(input$pca_Nr_plot2))
    return()
  if(is.null(input$action_pca)|input$action_pca==0)
    return()
  
  Nr <- input$pca_Nr_plot2 
  var <- input$var_splom
  isolate({
    if(Nr<=1)
      return()
    dat <- pca() 
    sc <- data.frame(scores(dat)[,1:Nr])
    if(var!=""|!is.null(var)){         
      sc[,var] <- getdata()[,var]
      if(!is.factor(sc[,var])){
        sc$gr <- cut(sc[,var],quantile(sc[,var],seq(0,1,.125),na.rm=T))
        sc <- sc[!is.na(sc[,var]),]
        par.set <- list(superpose.symbol = list(col = brewer.pal(8,"Blues"),pch=19))
      } else {
        sc$gr <- sc[,var]
        sc <- sc[!is.na(sc[,var]),]
        par.set <- list(superpose.symbol = list(col = brewer.pal(min(9,levels(sc$gr)),"Set1"),pch=19))
      }
      print(splom(~sc[,-ncol(sc)],data=sc,groups=gr,par.settings=par.set,alpha=.5,auto.key=list(columns=4)))
    } else {
      print(splom(sc))
    }    
  })  
})
