#------------------------------------
# Sidebar UI 
# -----------------------------------

output$caret_vars <- renderUI({
  vars <- varnames()
  if(is.null(vars)) 
    return()
  div(
    selectInput(inputId = "caret_var1", label = "Dependent variable:", choices = vars, selected = NULL, multiple = FALSE),
    selectInput(inputId = "caret_var2", label = "Independent variables:", choices = vars[-which(vars == input$caret_var1)], selected = NULL, multiple = TRUE),
    tags$style(type='text/css', "#caret_var2 { height: 200px; padding-bottom: 35px;}"),
    tags$hr(),
    selectInput(inputId = "caret_train", label = "Training variable:", choices = vars[!grepl("matrix",vars)], selected = NULL, multiple = FALSE)
  )
})

output$caret_train <- renderUI({
  if(is.null(input$caret_train)) return()
  dat <- getdata()
  trainset <- as.factor(dat[,input$caret_train])
  div(    
    selectInput(inputId = "caret_train_cal", label = "Select value of the train samples:", choices = c("",as.list(levels(trainset))), selected = "", multiple = FALSE),
    selectInput(inputId = "caret_train_val", label = "Select value of the test samples:", choices = c("",as.list(levels(trainset))), selected = "", multiple = FALSE),  
    selectInput(inputId = "caret_train_unk", label = "Select value of the unknown samples:", choices = c("",as.list(levels(trainset))), selected = "", multiple = FALSE)  
  )
})

output$ui_caret_regression <- renderUI({
  tabsetPanel(
    tabPanel("Variables", wellPanel(uiOutput("caret_vars"), uiOutput("caret_train"))),
    tabPanel("Model", 
             selectInput("caret_method",label="Regression model",
                         choices=list("Partial Least Square Regression" = "kernelpls","Support Vector Machine" ="svmRadial",
                                      "Random Forest" = "rf", "M5" = "M5","Stepwise lm" ="lmStepAIC", "Multivariate Adaptative Regression Spline" = "mars",
                                      "Neural Networks" = "pcaNNet","Gradient Boosted Machine" = "gbm", "Cubist" = "cubist" , "Relevant Vector Machine" ="rvmRadial"),
                         selected="kernelpls",multiple=FALSE),
             selectInput("caret_pre",label="Preprocessing options",choices = list("center","scale","PCA" = "pca"),selected=NULL,multiple=TRUE),                    
             tags$style(type='text/css', "#caret_pre { height: 60px; }"),
             conditionalPanel("input.caret_method != 'lmStepAIC'",
                              numericInput("caret_tl",label="Length of the tuning grid",value=6,min=1,step=1),
                              tags$style(type='text/css', "#caret_tl { width: 180px;}")
             )   
    ),
    tabPanel("Resampling",  
             selectInput("caret_cv_method",label="Resampling method",
                         choices=list("Cross-validation" = "cv", "Leave-One-Out CV" = "LOOCV","Leave-Group-Out" = "LGOCV", "Bootstraping"="boot","Bootstraping 632" =  "boot632"),selected="cv",multiple=FALSE),
             numericInput("caret_cv_n","Number of folds or number of resampling iterations",value=10,min=2,step=1),
             conditionalPanel(condition = "input.caret_cv_method=='LGOCV'",
                              numericInput("caret_p","training percentage",0.75,min=0,max=1,step=0.01)
             ),
             selectInput("caret_sel",label="Function to select the best tuning parameters",choices=list("Lowest RMSE" = "best","One SE rule" = "oneSE"),selected="Lowest RMSE",multiple=FALSE),
             checkboxInput("caret_LpredBounds",label="Constrain predictions to lower bound of the training set",value=FALSE),
             checkboxInput("caret_UpredBounds",label="Constrain predictions to upper bound of the training set",value=FALSE)
    )
  )  
})

#------------------------------------
# Reactive's 
# -----------------------------------

summary_stats_caret_regression <- reactive({
  if(is.null(input$action_caret)|input$action_caret==0) 
    return()
  if(is.null(caret_regression()))
    return()
  stats <- as.data.frame(data.table(getpreds())[,summary.DT(obs,pred),by=dataType])  
})

getmodel_frame <- reactive({
  if(is.null(input$action_caret)|input$action_caret==0) 
    return()
  dat <- getdata()
  #na.omit
  CALL <- attr(dat[,input$caret_var2],"call")
  wav_init <- attr(dat[,input$caret_var2],"wav_init")
  
  dat <- dat[!is.na(dat[,input$caret_var1]),]
  
  # dependent and independent variabels
  y <- dat[,input$caret_var1]
  x <- dat[,input$caret_var2,drop=F]
  x <- flatten(x,keep.names=F)    
  
  #select sets for regression and predictions
  xcal <- x[dat[,input$caret_train]==input$caret_train_cal,,drop=F]
  ycal <- y[dat[,input$caret_train]==input$caret_train_cal]
  xval <- x[dat[,input$caret_train]==input$caret_train_val,,drop=F]
  yval <- y[dat[,input$caret_train]==input$caret_train_val]
  xunk <- x[dat[,input$caret_train]==input$caret_train_unk,,drop=F]   
  
  dat <- list(x=x,xcal=xcal,ycal=ycal,xval=xval,yval=yval,xunk=xunk,preprocess_call=CALL,wav_init=wav_init)  
  return(dat)
})


getpreds <- reactive({
  if(is.null(input$caret_train))
    return()
  if(is.null(input$caret_train_cal))
    return()
  if(input$caret_train_cal=='')
    return()
  if(is.null(input$caret_train_val)|is.null(input$caret_train_unk))
    return()
  if(is.null(caret_regression())) 
    return()
  if(input$caret_train_val==''&input$caret_train_unk==''){
    preds <- extractPrediction(list(caret_regression()))
  } else {
    dat <- getmodel_frame()
    
    if (input$caret_train_val!=''&input$caret_train_unk==''){     
      preds <- extractPrediction(list(caret_regression()),testX=dat$xval,testY=dat$yval)
    } else if (input$caret_train_val==''&input$caret_train_unk!=''){
      preds <- extractPrediction(list(caret_regression()),unkX=dat$xunk)
    } else {
      preds <- extractPrediction(list(caret_regression()),
                                 testX=dat$xval,testY=dat$yval,
                                 unkX=dat$xunk)
    }
  }
  preds$resid <- preds$obs-preds$pred
  preds$dataType <- factor(preds$dataType,levels=c("Training","Test","Unknown"))
  return(preds)  
})

caret_regression <- reactive({
	if(is.null(input$action_caret)|input$action_caret==0) 
	  return()   
  isolate({
    
	  dat <- getmodel_frame()
    #feed traincontrol
    tr <- trainControl(method = input$caret_cv_method,
                       p = input$caret_p,
                       number = input$caret_cv_n,
                       returnData=TRUE,
                       selectionFunction = input$caret_sel,
                       predictionBounds=c(input$caret_LpredBounds,input$caret_UpredBounds))
    # run caret model
  	mod <- train(x=dat$xcal,y=dat$ycal,method=input$caret_method,preProcess = input$caret_pre,trControl=tr,tuneLength = input$caret_tl)  	      
    mod$preProcessing <- dat$preprocess_call
	  mod$wav_init <- dat$wav_init
    if(input$caret_method=="kernelpls"){ # fix row names for plot
      rownames(mod$finalModel$loadings) <- gsub("`","",rownames(mod$finalModel$loadings))
      rownames(mod$finalModel$coefficients) <- gsub("`","",rownames(mod$finalModel$coefficients))      
    }      
    mod
  })
})

plot_caret_reg <- function(){
  if(is.null(input$caret_plots))
    return()
  if(input$caret_plots == 0){
    return(print(plot(caret_regression())))
  } else if(input$caret_plots == 4){
    return(print(plot(caret_regression()$finalModel,"loadings",labels="numbers",legendpos="topright",seq(input$ncomp_slider[1],input$ncomp_slider[2]))))
  } else if(input$caret_plots == 5){
    return(print(plot(caret_regression()$finalModel,"scores",seq(input$ncomp_slider[1],input$ncomp_slider[2])))) 
  } else if(input$caret_plots == 6){
    return(print(plot(caret_regression()$finalModel,"corr",seq(input$ncomp_slider[1],input$ncomp_slider[2])))) 
  } else if(input$caret_plots == 7){
    return(print(plot(caret_regression()$finalModel,"coefficients",labels="numbers",input$ncomp_slider2))) 
  } else if(input$caret_plots == 8){
    return(print(plot(caret_regression()$finalModel,"pred",input$ncomp_slider2)))   
  } else {
    preds <- getpreds()
    if(input$caret_plots == 1) {
      #       lims <- extendrange(range(c(preds$obs,preds$pred)))      
      p <- ggplot(data=subset(preds,dataType!="Unknown"),aes(obs,pred)) + geom_point(alpha=.5) + facet_wrap(~dataType) + geom_abline() +
        labs(list(title = "Predicted vs Observed", x = "Observed", y = "Predicted")) + theme_bw() #+ xlim(lims) + ylim(lims)
    } else if(input$caret_plots == 2) {
      p <- ggplot(data=subset(preds,dataType!="Unknown"),aes(obs,resid)) + geom_point(alpha=.5) + facet_wrap(~dataType) + geom_hline(yintercept = 0) + geom_smooth(se = FALSE) +
        labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")) + theme_bw()  
    } else if(input$caret_plots == 3) {
      p <- ggplot(data=subset(preds,dataType=="Unknown"),aes(pred)) + geom_histogram() +
        labs(list(title = "Histogram of predictions", x = "Predicted")) + theme_bw()     
    }
    return(print(p))
  }    
}

observe({
	if(is.null(input$save_preds) || input$save_preds == 0) return()
	isolate({
    dat <- getdata()
    dat <- dat[,input$caret_var2,drop=F]
    dat <- flatten(dat,keep.names=F)    
		dat <- predict(caret_regression(),newdata=dat)
		changedata(dat, input$name_preds)
	})
})

#------------------------------------
# Main UI 
# -----------------------------------

output$caret_plot_sel <- renderUI({
  lplot <- list("Tuning parameters vs RMSE" = 0, "Actual vs Fitted" = 1, "Residuals vs Fitted" = 2)
  
  if(input$caret_train_unk!="")
    lplot <- c(lplot,list("Histogram of unknown values" = 3))
  if(input$caret_method == "kernelpls")
    lplot <- c(lplot,list("Loadings" = 4, "Scores" = 5, "Correlation Loadings" = 8, "Coefficients" = 7, "Predictions vs ncomp" = 8))  
  div(
    selectInput("caret_plots", "Regression plots:", choices = lplot,selected = "Tuning parameters vs RMSE", multiple = FALSE),
    conditionalPanel("input.caret_method=='kernelpls' && input.caret_plots>3 && input.caret_plots<7",
      sliderInput("ncomp_slider",label="Components",min=1,max=input$caret_tl,value=c(1,input$caret_tl),step=1),
      tags$style(type='text/css', "#ncomp_slider { width: 150px; }")
    ),
    conditionalPanel("input.caret_method=='kernelpls' && input.caret_plots>6",
     sliderInput("ncomp_slider2",label="Components",min=1,max=input$caret_tl,value=input$caret_tl,step=1),
     tags$style(type='text/css', "#ncomp_slider2 { width: 150px; }")
    )
  )
})


output$plot_caret_regression <- renderPlot({plot_caret_reg()})                                

output$summary_caret_regression <- renderPrint({
  if(is.null(input$caret_var2)) 
    return(cat("Please select one or more independent variables"))
  if(is.null(input$caret_train_cal)) 
    return(cat("Please select train variable"))
  if(input$caret_train_cal=='')
    return(cat("Please select a value corresponding to calibration samples"))
  if(is.null(input$action_caret)|input$action_caret==0) 
    return(cat("Now, press 'run model'"))
  if(is.null(caret_regression()))
    return()
  print(caret_regression())
  if(input$caret_method=="cubist"){
    cat("Cubist output\n")
    cat("================\n")
    print(summary(caret_regression()$finalModel))
  }
})

output$summary_stats_caret_regression_table <- renderTable({
  if(is.null(input$action_caret)|input$action_caret==0) 
    return()
  if(is.null(summary_stats_caret_regression()))
    return()
  summary_stats_caret_regression()
})

output$downloadData_reg_stats <- downloadHandler(
  filename = function() {  paste(input$datasets[1],"_stats",".",input$saveAs_reg_stats,sep="") },
  content = function(file) {
    
    ext <- input$saveAs_reg_stats
    
    robj <- input$datasets[1]
    
    dat <- summary_stats_caret_regression()
    
    assign(robj,dat)
    
    if(ext == 'rda' || ext == 'rdata') {
      save(list = robj, file = file)
    } else if(ext == 'csv') {
      write.csv(get(robj), file)
    }
  }
)

output$downloadData_model <- downloadHandler(
  filename = function() {  paste(input$name_model,".Rdata",sep="") },
  content = function(file) {
    
    robj <- input$name_model
    
    dat <- caret_regression()
    
    assign(robj,dat)
    
    save(list = robj, file = file)
    
  }
)

output$download_viz <- downloadHandler(
  filename = function(){
    paste(input$regName,input$saveAs_reg,sep=".")
  },
  # a function that actually opens a connection to a pdf and print the result to it
  content = function(f=NULL) {
    printSpectra(f,w=input$wreg,h=input$hreg,p=plot_caret_reg(),saveAs=input$saveAs_reg)    
  }
)