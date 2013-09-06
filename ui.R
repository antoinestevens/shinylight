getTool <- function(inputId) {
  tagList(
    singleton(tags$head(tags$script(src = "js/navbar.js"))),
    tags$html(includeHTML('www/navbar.html'))
  )
}

# Code to make a message that shiny is loading
# Make the loading bar
# loadingBar <- tags$div(class="progress progress-striped active",
#                        tags$div(class="bar", style="width: 100%;"))
# # Code for loading message
# loadingMsg <- tags$div(class="modal", tabindex="-1", role="dialog", 
#                        "aria-labelledby"="myModalLabel", "aria-hidden"="true",
#                        tags$div(class="modal-header",
#                                 tags$h3(id="myModalHeader", "Loading...")),
#                        tags$div(class="modal-footer",
#                                 loadingBar))

shinyUI(
  
  pageWithSidebar(

    # Using a navbar rather than headerPanel to display app title
    headerPanel(''),

    sidebarPanel(

      tags$head(
        tags$link(rel="stylesheet", type="text/css", href="style.css") # from google group (Kirill Savin)        
      ),

      includeHTML("www/js/tools.js"),
      includeHTML('www/js/lr.js'), 
      getTool("tool"),

      wellPanel(
        uiOutput("datasets")        
      ),

      # only show data loading and selection options when in dataview
      conditionalPanel(condition = "input.tool == 'dataview'",
        conditionalPanel(condition = "input.datatabs == 'View' || input.datatabs == 'Summarize'",
          wellPanel(uiOutput("columns"),
            conditionalPanel("input.datatabs == 'View'",
               uiOutput("nrRows"),tags$hr(),uiOutput("ui_transform")
            )
          )
        ),
        conditionalPanel(condition = "input.datatabs == 'Import/Export'",
          uiOutput("ui_import")          
        ),          
        conditionalPanel(condition = "input.datatabs == 'Visualize'",
          uiOutput("ui_visualize")
                         
        )
       
      ),  
      conditionalPanel(condition = "input.tool == 'spec'",
          wellPanel(
            uiOutput("spec_columns"),
            conditionalPanel("input.tabspec=='Pre-Processing'",uiOutput("spec_nrRows")),
            conditionalPanel("input.tabspec=='Correlation'",uiOutput("cor_columns")),
            conditionalPanel("input.tabspec=='Summary'",uiOutput("ui_summarize_spec")),
            uiOutput("ui_labs"),
            conditionalPanel("input.tabspec=='Pre-Processing'",uiOutput("ui_spec")),
            conditionalPanel("input.tabspec=='Correlation'",uiOutput("x_slider_cor"))
          )
      ),           
      conditionalPanel(condition = "input.tool == 'calval'",
        conditionalPanel(condition = "input.calvaltabs=='Select'",uiOutput("ui_calval")),
        conditionalPanel(condition = "input.calvaltabs=='Scatter Plot'",uiOutput("ui_calval_viewer"))                       
      ),    
      conditionalPanel(condition = "input.tool == 'clus'|| input.tool == 'pca'",
        wellPanel(uiOutput("explo_vars"),      
          conditionalPanel(condition = "input.tool == 'clus'",
            uiOutput("ui_kmeansClustering"),
            conditionalPanel("input.tabclus=='Cluster means'",
              div(
                tags$hr(),
                textInput(inputId="clus_xlab","X-lab"),
                textInput(inputId="clus_ylab","Y-lab"),
                numericInput(inputId="clus_xticks","# X-ticks",value=10,min=2,max=100,step=1)
              )
            )
          ),
          conditionalPanel(condition = "input.tool == 'pca'",
            uiOutput("ui_pca"),
            uiOutput("ui_pcaPlot")         
          )
        )
      ),
      conditionalPanel(condition = "input.tool == 'caret_regression'",
        uiOutput("ui_caret_regression"),
        actionButton("action_caret","Run model")
      ),
      conditionalPanel(condition = "input.tool == 'prediction'",
        conditionalPanel("input.predictiontabs=='Upload'",
          uiOutput("ui_upload_model")
        ),
        conditionalPanel("input.predictiontabs=='Predict'",
          uiOutput("ui_prediction")                        
        )
      )
    ),
    
    mainPanel(      
      conditionalPanel(condition = "input.datasets != ''",                      
        conditionalPanel(condition = "input.tool == 'dataview'", 
          tabsetPanel(id = "datatabs",
           tabPanel("Import/Export",
              verbatimTextOutput("import_summary"),
              br(),
              selectInput("saveAs", "", choices = c('rda','csv','xlsx','mat'), selected = NULL, multiple = FALSE),
              tags$style(type='text/css', "#saveAs { width: 85px;}"),
              downloadButton('downloadData', 'Export data'),
              tags$style(type='text/css', "#downloadData { vertical-align: top; height: 18.5px; width: 100px;}")
           ),
            tabPanel("View",              
              tableOutput("dataviewer")
            ),
            tabPanel("Summarize", 
              verbatimTextOutput("summarize_data")
            ),            
            tabPanel("Visualize",
              plotAndSave(p="visualize","vizName", "wviz","hviz","saveAs_viz","download_viz",600,600)
            )            
          )
        ),
         conditionalPanel(condition = "input.tool == 'spec'",
          tabsetPanel(id="tabspec",
            tabPanel("Pre-Processing",
              h3("Raw data"),
              plotAndSave(p="SpecPlot","spcName", "wspc","hspc","saveAs_spc","download_spc",800,400),
              conditionalPanel(condition="input.spec_fun!=''",
                h3("Transformed data"),
                plotAndSave(p="TransPlot","spcNametr", "wspctr","hspctr","saveAs_spctr","download_spctr",800,400)              
              )
            ),
            tabPanel("Correlation",
              conditionalPanel(condition="input.cor_columns ==''",verbatimTextOutput("no_cor_cols")),
              conditionalPanel(condition="input.cor_columns !=''",plotAndSave(p="CorPlot","corsName", "wcors","hcors","saveAs_cors","download_cors",800,400))  
            ),
            tabPanel("Summary",
              conditionalPanel(condition="input.summary_fun ==''",verbatimTextOutput("no_fun_sum")),
              conditionalPanel(condition="input.summary_fun !='' && input.summary_table_spec != ''",
                h3("Summary"),
                tableOutput("summary_table_spec"),
                selectInput("saveAs_sum", "", choices = c('rda','csv'), selected = NULL, multiple = FALSE),
                tags$style(type='text/css', "#saveAs_sum { width: 85px;}"),
                downloadButton('downloadData_sum', 'Export data'),
                tags$style(type='text/css', "#downloadData_sum { vertical-align: top; height: 18.5px; width: 100px;}"),                     
                uiOutput("ui_summary_plot_spec")
              )
            )
          )
        ),
        conditionalPanel(condition = "input.tool == 'clus'",
          tabsetPanel(id="tabclus",
            tabPanel(title="Summary",verbatimTextOutput("kmeansClustering")),
            tabPanel(title="Cluster means",plotOutput("kmeansPlot1")),
            tabPanel(title="Scatter plot",plotOutput("kmeansPlot2",width=600,height=600))
          )
         ),
        conditionalPanel(condition = "input.tool == 'pca'",
          tabsetPanel(id="tabpca",
            tabPanel(title="Summary",conditionalPanel("input.action_pca != 0",
              verbatimTextOutput("pcaSummary"))),
            tabPanel(title="Loadings",plotOutput("pcaPlot1")),
            tabPanel(title="Scatter Plot",plotOutput("pcaPlot2",width=600,height=600))
          )
        ),
        conditionalPanel(condition = "input.tool == 'calval'",
          tabsetPanel(id="calvaltabs",
            tabPanel(title="Select",
               br(),
               conditionalPanel(condition = "input.sel_print != ''",
                  conditionalPanel("input.action_calval > 0 && $('html').hasClass('shiny-busy')",
                           img(src="loader.gif")),
                  verbatimTextOutput("sel_print"))
            ),
            tabPanel(title="Scatter Plot",
              plotAndSave(p="calPlot","calName", "wcal","hcal","saveAs_cal","download_cal",600,600)               
            )            
          )          
        ),       
       conditionalPanel(condition = "input.tool == 'caret_regression'",
          tabsetPanel(id = "carettabs",
              tabPanel(title="Summary", 
                       conditionalPanel("input.action_caret > 0 && $('html').hasClass('shiny-busy')",
                                        img(src="loader.gif")),
                       verbatimTextOutput("summary_caret_regression"),
                       textInput("name_preds", "Save predictions in variable:", "pred"),                   
                       tags$style(type='text/css', "#name_preds { width: 100px;}"),
                       actionButton("save_preds", "Save predictions"),
                       tags$style(type='text/css', "#save_preds { vertical-align: top; width: 150px;}"),
                       textInput("name_model", "Save model for later use:", "model"),                   
                       tags$style(type='text/css', "#name_model { width: 100px;}"),
                       downloadButton("downloadData_model", "Save Model"),
                       tags$style(type='text/css', "#downloadData_model { vertical-align: top; width: 100px;}")
              ),
              tabPanel(title="Summary stats",
                       tableOutput("summary_stats_caret_regression_table"),
                       selectInput("saveAs_reg_stats", "", choices = c('rda','csv'), selected = NULL, multiple = FALSE),
                       tags$style(type='text/css', "#saveAs_reg_stats { width: 85px;}"),
                       downloadButton('downloadData_reg_stats', 'Export data'),
                       tags$style(type='text/css', "#downloadData_reg_stats { vertical-align: top; height: 18.5px; width: 100px;}")
              ),
              tabPanel(title="Plots",
                       uiOutput("caret_plot_sel"), 
                       plotAndSave(p="plot_caret_regression","regName", "wreg","hreg","saveAs_reg","download_reg",800,600)
              )      
          )
       ),        
      conditionalPanel(condition = "input.tool == 'prediction'",
        tabsetPanel(id = "predictiontabs",
          tabPanel(title="Upload", verbatimTextOutput("upload_model_text")),
          tabPanel(title="Predict",
             verbatimTextOutput("print_newpreds"),     
             textInput("name_newpreds", "Save predictions in variable:", "pred"),                   
             tags$style(type='text/css', "#name_newpreds { width: 100px;}"),
             actionButton("save_newpreds", "Save predictions"),
             tags$style(type='text/css', "#save_newpreds { vertical-align: top; width: 150px;}"),
             plotOutput("plot_newpreds")
          )                  
        )
      ),
      conditionalPanel(condition= "input.tool == 'about'",includeMarkdown("about.md"))               
      )
    )
  )
)