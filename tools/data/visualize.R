#------------------------------------
# Sidebar UI 
# -----------------------------------

output$x_slider_viz <- renderUI({
  if(is.null(input$vizvars1)) return()      
  dat <- getdata_viewer()[,input$vizvars1]
  if(is.factor(dat)) return()    
  r <- range(dat,na.rm=T)
  sliderInput("x_slider_viz","X-axis range:",min=r[1],max=r[2],value=c(r[1],r[2]))
})

output$y_slider_viz <- renderUI({
  if(is.null(input$vizvars2)|input$vizvars2=="") return()      
  dat <- getdata_viewer()[,input$vizvars2]
  if(is.factor(dat)) return()    
  r <- range(dat,na.rm=T)
  sliderInput("y_slider_viz","Y-axis range:",min=r[1],max=r[2],value=c(r[1],r[2]))
})

# variable selection in the datatabs views
output$ui_visualize <- renderUI({
  cols <- varnames_viewer()
  if(is.null(cols)) 
    return()
  isFct <- grepl("factor",names(cols))	 
  colsFac <- cols[isFct]
  wellPanel(
    selectInput(inputId = "vizvars1", label = "X-variable", choices = as.list(cols), selected = NULL, multiple = FALSE),
    textInput(inputId = "xlab", label = "X-label", value=input$vizvar1),
    uiOutput("x_slider_viz"),
    selectInput(inputId = "vizvars2", label = "Y-variable", choices = c("None" = "",as.list(cols[-which(cols == input$vizvars1)])), selected = "", multiple = FALSE),
    conditionalPanel(condition = "input.vizvars2 != ''",
         textInput(inputId = "ylab", label = "Y-label", value=""),
         uiOutput("y_slider_viz"),
         conditionalPanel(condition="input.x_slider_viz  != '' && input.y_slider_viz !=''",
            checkboxInput('viz_label', "Add labels", value = FALSE),
            conditionalPanel(condition="input.viz_label",
              selectInput(inputId = "label_var", label = "Label variable", choices = as.list(cols), selected = NULL, multiple = FALSE)
            )
         )
    ),
    tags$hr(),
    selectInput('viz_color', 'Color', c(None = '.', as.list(cols))),
    selectInput('viz_facet_row', 'Facet row', c(None='.', as.list(colsFac))),
    selectInput('viz_facet_col', 'Facet col', c(None='.', as.list(colsFac))), 	                 
    conditionalPanel(condition = "input.vizvars2 != ''",
           tags$hr(),
           checkboxInput('viz_smooth', 'Smooth', value = FALSE),
           checkboxInput('viz_lin_eq', 'Linear Equation', value = FALSE)
    )
  )
})

#------------------------------------
# Reactive's
# -----------------------------------

makeViz <- function(){
  if(is.null(input$datasets) || is.null(input$vizvars2))
    return()
  
  # inspired by Joe Cheng's ggplot2 browser app
  # http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
  dat <- getdata_viewer()
  
  if(input$vizvars2 == "") {
    p <- ggplot(dat, aes_string(x=input$vizvars1))  + theme_bw()  + xlab(input$xlab) 
    if(!is.null(input$x_slider_viz))
      p <- p  + xlim(input$x_slider_viz)  
    if (input$viz_color != '.') 
      p <- p + geom_histogram(aes_string(fill=input$viz_color),color='black') + scale_fill_brewer() + theme_bw() 
    else 
      p <- p + geom_histogram(colour = 'black', fill = 'black')
  } else {
    dat <- cbind(dat,data.frame(x=dat[,input$vizvars1], y=dat[,input$vizvars2])) # fix for synthetically "invalid" names
    if(is.factor(dat[,input$vizvars1])) {
      p <- ggplot(dat, aes(x,y)) + geom_boxplot() + theme_bw() + xlab(input$xlab)+ ylab(input$ylab)
    } else if (is.factor(dat[,input$vizvars2])) {
      p <- ggplot(dat, aes(y,x)) + geom_boxplot() + theme_bw() + coord_flip() + ylab(input$xlab)+ xlab(input$ylab)
    } else {
      p <- ggplot(dat, aes(x,y)) + geom_point() + theme_bw() + xlab(input$xlab)+ ylab(input$ylab)
      if(!is.null(input$x_slider_viz))
        p <- p  + xlim(input$x_slider_viz)   
      if(!is.null(input$y_slider_viz))
        p <- p  + ylim(input$y_slider_viz)  
      if(!is.null(input$viz_label))
        if(input$viz_label)
          p <- p  + geom_text(aes_string(label=input$label_var),hjust=0, vjust=0)  
    }
      
    if (input$viz_smooth)
      p <- p + geom_smooth(method = "lm", size = .75, linetype = "dotdash")
    
    if (input$viz_lin_eq){
      if(input$viz_facet_row=="."&input$viz_facet_col=="."){
        llm <- data.frame(llm = lm_eqn(list(lm(y ~ x, dat)))) 
      } else if (input$viz_facet_row!="."&input$viz_facet_col==".") {
        llm <- foreach(f = levels(dat[,input$viz_facet_row]))%do%lm(y ~ x, dat[dat[,input$viz_facet_row]==f,])
        llm <- data.frame(llm = lm_eqn(llm)) 
        llm[,input$viz_facet_row]  <- levels(dat[,input$viz_facet_row])
      } else if (input$viz_facet_col!="."&input$viz_facet_row!="."){
        llm <- foreach(f = levels(dat[,input$viz_facet_col]))%do%lm(y ~ x, dat[dat[,input$viz_facet_col]==f,])
        llm <- data.frame(llm = lm_eqn(llm))
        llm[,input$viz_facet_col]  <- levels(dat[,input$viz_facet_col])
      } else {
        dat$comb <- interaction(dat[,input$viz_facet_row],dat[,input$viz_facet_col])
        llm <- foreach(f = levels(dat$comb))%do%lm(y ~ x, dat[dat[,"comb"]==f,])
        llm <- data.frame(llm = lm_eqn(llm),comb=levels(dat$comb))   
      }
      p <- p + geom_text(data=llm,aes(x = -Inf, y = Inf, label = llm),vjust=+1,hjust=-.1, colour="black", size = 5, parse=TRUE)    
    }
    
    if (input$viz_color != '.') 
      p <- p + aes_string(color=input$viz_color) + scale_fill_brewer() + theme_bw() 
    
  }
  
  facets <- paste(input$viz_facet_row,"~",input$viz_facet_col) 
  
  if (facets != '. ~ .')
    p <- p + facet_grid(facets)
  
  print(p)
}

#------------------------------------
# Main UI 
# -----------------------------------

output$visualize <- renderPlot(makeViz())

output$download_viz <- downloadHandler(
  filename = function(){
    paste(input$vizName,input$saveAs_viz,sep=".")
  },
  # a function that actually opens a connection to a pdf and print the result to it
  content = function(f=NULL) {
    printSpectra(f,w=input$wviz,h=input$hviz,p=makeViz(),saveAs=input$saveAs_viz)    
  }
)