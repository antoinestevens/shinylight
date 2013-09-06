#------------------------------------
# sidebar UI 
# -----------------------------------

ui_import <- function() {
  wellPanel(
    tabsetPanel(
      tabPanel("Text",
               HTML("<label>Import txt file(s): (.csv | .txt )</label>"),
               fileInput('upload', 'Choose a file',
                         accept=c('text/csv', 'text/comma-separated-values,text/plain')),            
               checkboxInput('header', 'Header', TRUE),
               radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),'Semicolon'),
               radioButtons('dec', 'Decimal',c(Comma=',',Dot='.'),'Dot'),
               radioButtons('quote', 'Quote',c(None='', 'Double Quote'='"', 'Single Quote'="'"),'Double Quote')
      ),
      tabPanel("R",
               HTML("<label>Import R file(s): (.rdata | .rda )</label>"),
               fileInput("upload_rdata", "Choose a file",multiple=FALSE,accept=c("application/x-rlang-transport"))
      ),
      tabPanel("Excel",
               HTML("<label>Import xls file(s): (.xls | .xlsx )</label>"),
               fileInput("upload_xls", "Choose a file",multiple=FALSE,accept=c("application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
               numericInput("sheet","Sheet nr",value=1,step=1),
               tags$style(type='text/css', "#sheet { width: 70px;}")
      ),
      tabPanel("Matlab",
               HTML("<label>Import mat file(s): (.mat )</label>"),
               fileInput("upload_matlab", "Choose a file",multiple=FALSE)               
      ),
      tabPanel("ASD",                       
               HTML("<label>Import ASD file(s): (.txt | .asd )</label>"),
               fileInput("upload_asd", "Choose file(s)",multiple=TRUE)
      )
    )
  )
}

output$ui_import <- renderUI({
  ui_import()
})

lastFileName <- lastFileName_rdata <- lastFileName_xls <- lastFileName_asd  <- lastFileName_matlab <- ""

output$datasets <- renderUI({
  
  objname <- NULL  
  
  # loading user data
  #csv/text
  if(!is.null(input$upload)){
    if(input$upload$name!=lastFileName){
      inFile <- input$upload
      lastFileName <<- inFile$name
      objname <- robjname <- file_path_sans_ext(inFile$name)
      values[[objname]] <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, dec = input$dec,check.names=FALSE)    
    } 
  }
  #xls
  if(!is.null(input$upload_xls)){
    if(input$upload_xls$name!=lastFileName_xls){
      inFile_xls <- input$upload_xls
      lastFileName_xls <<- inFile_xls$name
      objname <- robjname <- file_path_sans_ext(inFile_xls$name)      
      values[[objname]] <- read.xlsx2(file=inFile_xls$datapath,sheetIndex=input$sheet,check.names=FALSE)    
    } 
  }
  #matlab
  if(!is.null(input$upload_matlab)){
    if(input$upload_matlab$name!=lastFileName_matlab){
      inFile_matlab <- input$upload_matlab
      lastFileName_matlab <<- inFile_matlab$name
      tmp <- readMat(con=inFile_matlab$datapath) 
      foreach(i = 1:length(tmp))%do%{
        tmp2 <- tmp[[i]]
        if(is.matrix(tmp2)) 
          values[[names(tmp)[i]]] <- as.data.frame(tmp2) # import only if matrix...
      }
    } 
  }  
  #rdata
  if (!is.null(input$upload_rdata)){
    if(input$upload_rdata$name!=lastFileName_rdata){
      inFile_rdata <- input$upload_rdata
      lastFileName_rdata <<- inFile_rdata$name
      objname <- robjname <- load(inFile_rdata$datapath)
      values[[robjname]] <- get(robjname)      
    }
  } 
  #asd
  if (!is.null(input$upload_asd)){ 
    if(input$upload_asd$name[1]!=lastFileName_asd){
      inFile_asd <- input$upload_asd
      lastFileName_asd <<- inFile_asd$name[1]
      objname <- sub("[0-9]+","",file_path_sans_ext(inFile_asd$name[1]))
      
      if(file_ext(inFile_asd$name[1])=="asd") 
        ext <- "binary"
      else
        ext <- "txt"
      
      tmp <- data.frame(ID=file_path_sans_ext(inFile_asd$name))
      tmp$spc <- readASD(inFile_asd$datapath,in_format=ext,out_format="matrix")
      values[[objname]] <- tmp             
    }
  }
  
  #populate the "datasets" string in order that the dataset will be seen by the user
  if(datasets[1] == '')
    datasets <<- c(objname)
  else
    datasets <<- unique(c(objname,datasets))
  
  # Drop-down selection of datasets
  selectInput(inputId = "datasets", label = "Datasets:", choices = datasets, selected = datasets[1], multiple = FALSE)
})

#------------------------------------
# Main UI 
# -----------------------------------

output$downloadData <- downloadHandler(
  filename = function() { paste(input$datasets[1],'.',input$saveAs, sep='') },
  content = function(file) {
    
    ext <- input$saveAs
    
    robj <- input$datasets[1]
    
    dat <- getdata()    
    
    if(ext!="rda"&ext!="rdata")
      dat <- flatten(dat)
    
    assign(robj,dat)
    
    if(ext == 'rda' || ext == 'rdata') {
      save(list = robj, file = file)
    } else if(ext == 'csv') {
      write.csv(get(robj), file)
    } else if(ext == 'xlsx'){
      write.xlsx2(robj,file)
    }	else if(ext =='mat'){
      writeMat(con=file, Robject = robj)
    }	
  }
)

output$import_summary <- renderPrint({
  
  if(is.null(input$upload)&is.null(input$upload_rdata)&is.null(input$upload_asd)&is.null(input$upload_xls)&is.null(input$upload_matlab)){
    cat("Please use the side bar menu to import datasets")
  } else if (datasets[1] == "NIRsoil"){
    cat("Import failed")
  } else {
    cat("\nImported data:\n")
    cat(datasets[1])
    cat("\n----------------------------------\n")
    tmp <- flatten(values[[datasets[1]]])
    cat(paste("- imported data has",nrow(tmp), "observations and",ncol(tmp),"variables\n"))
    cat(c("- Variable names are:\n",paste(colnames(tmp),collapse=","),"\n"))    
  }
})