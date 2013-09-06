#------------------------------------
# Sidebar UI
# -----------------------------------

output$ui_upload_model <- renderUI({
  wellPanel(
    HTML("<label>Load model file: (.rda | .rdata )</label>"),
    fileInput('upload_model', 'Choose a file',multiple=FALSE,accept=c("application/x-rlang-transport"))                            
  )
})

output$ui_prediction <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()
  wellPanel(
    selectInput(inputId = "ind_var", label = "Independent variables:", choices = vars, selected = NULL, multiple = TRUE),
    selectInput(inputId = "obs_var", label = "Observation variable (optional):", choices = vars, selected = NULL, multiple = FALSE),
    actionButton("action_predict","Get predictions") 
  )  
})

#------------------------------------
# Reactive's
# -----------------------------------

getmodel <- reactive({
  if(is.null(input$upload_model))
    return()     
  objname <- load(input$upload_model$datapath)
  get(objname)  
})    

get_newpreds <- reactive({
  if(is.null(input$action_predict)|input$action_predict==0) 
    return()
  isolate({
    dat <- getdata()
    x <- dat[,input$ind_var,drop=F]
    x <- flatten(x,keep.names=F)    
    
    if(!is.null(input$obs_var))
      yobs <- dat[,input$obs_var]
    else
      yobs <- NULL
    #preprocess according to attr call
    mod <- getmodel()
    pre <- mod$preProcessing
    for(i in 1:length(pre)){
      pre[[i]][2] <- call('x') #replace getspec() by x
      x <- eval(pre[[i]])
    }    
    ypred <- predict(mod,newdata=x)  
    data.frame(ypred=ypred,yobs=yobs)
  })  
})

observe({
  if(is.null(input$save_newpreds) || input$save_newpreds == 0) return()
  isolate({
    dat <- get_newpreds()$ypred
    changedata(dat, input$name_newpreds)
  })
})

#------------------------------------
# Main UI
# -----------------------------------

output$upload_model_text <- renderPrint({  
  if(is.null(input$upload_model)){
    cat("Please use the side bar menu to upload model")
  } else {
    mod <- getmodel()
    cat("Model summary:\n")
    cat(input$upload_model$name)
    cat("\n----------------------------------\n")
    cat(paste("Model type:",mod$method,"\n"))
    cat(paste("Preprocessing:",paste(sapply(mod$preProcessing,function(x)sub("\\(\\)","",deparse(x[1]))),collapse=", ")))
    cat("\nIndependent variables:\n")
    if(is.null(mod$wav_init))
      cat(colnames(mod$trainingData)[-length(colnames(mod$trainingData))],fill=T)
    else
      cat(mod$wav_init,fill=T)
    cat("\n----------------------------------\n")
    mod
  }
})

output$print_newpreds <- renderPrint({  
  if(is.null(input$upload_model)|is.null(input$ind_var))
    return(cat("Please use the side bar menu to select variables and press 'Get predictions'"))
  mod <- getmodel()
  x <- flatten(getdata()[,input$ind_var,drop=F],keep.names=F)    
  if(is.null(mod$wav_init))
    cn <- colnames(mod$trainingData)[-length(colnames(mod$trainingData))]
  else
    cn <- mod$wav_init
  if(length(cn)!=ncol(x))
    return(cat("Indepedent variables do not match independent variables in the uploaded model"))
  if(!all(cn%in%colnames(x)))
    return(cat("Indepedent variables do not match independent variables in the uploaded model"))
  if(is.null(input$action_predict)|input$action_predict==0) 
    return(cat("Now, press 'Get predictions'"))
  if(is.null(get_newpreds()))
    return()
  dat <- get_newpreds()
  cat("Prediction summary:")
  cat("\n")
  print(psych::describe(dat$ypred))
  cat("\n")
  if(!is.null(input$obs_var)){
    cat("Performance summary:\n")
    print(as.data.frame(data.table(dat)[,summary.DT(yobs,ypred)]))
  }
})

output$plot_newpreds <- renderPlot({  
  if(is.null(input$upload_model)|is.null(input$ind_var))
    return()
  if(is.null(input$action_predict)|input$action_predict==0) 
    return()
  if(is.null(get_newpreds()))
    return()
  p <- ggplot(data=get_newpreds(),aes(x=ypred)) + geom_histogram() + theme_bw() + xlab("Predictions")
  return(print(p))
})

