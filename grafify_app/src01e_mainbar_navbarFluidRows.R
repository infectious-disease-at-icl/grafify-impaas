#main panel of outputs and inputs.
source("src01e_menu_links.R", local = TRUE)
mainPanel1 <- list(#width = 6,
  tags$br(),
  #tags$br(),
  fluidRow(column(8,
                  #conditionalPanel(condition = 'input.vis_tabs == 2',
                  #                 htmlOutput("graphsHelpOpen")),
                  #conditionalPanel(condition = 'input.vis_tabs == 1',
                  #                 htmlOutput("varHelpOpen")),
                  tagList(tags$div(tags$h5(
                    "grafify"),
                    tags$ul(
                      tags$li(
                        tags$h6(
                          "You can use grafify online to plot graphs, and easily perform ANOVAs and post-hoc comparisons just like the ", tags$a(href="https://grafify-vignettes.netlify.app/", "R package."))),
                      tags$li(
                        tags$h6(
                          "The main advantages of grafify are the use of ggplot2 and various colourblind-friendly palettes, and easy access to linear models and linear mixed effects analyses for ANOVAs. These are more powerful and appropriate when experiments are designed as randomised blocks or have repeated measures."))))
                    ),
                  accordion(open = TRUE,                         #starting help on data
                            accordion_panel("Readme: Getting started (can be minimised)",
                                            htmlOutput("dataHelpOpen")))
                  ),
           tags$br(),
           column(4,
                  card(card_header(tags$h5("Start here"),        #start button
                                   class = "d-flex justify-content-between",
                                   tooltip(bs_icon("info-circle"),
                                           "Upload a csv or Excel file & click 'Start', or click 'Start' to use example data, and see dropdown menus for choosing variables.")),
                       #textOutput("started"),
                       fileInput("file1",                        #file upload
                                 placeholder = "Path to file",
                                 "Upload a CSV or Excel File", 
                                 accept = c(".csv", ".xlsx", ".xls")), 
                       uiOutput("sheetSelector"),
                       actionBttn(inputId = "startBtn",         #start button
                                  size = "md", 
                                  block = TRUE,
                                  label = tooltip(
                                    trigger = list(
                                      tags$strong("Start"),
                                      bs_icon("info-circle")
                                    ),
                                    "Upload a file and click Start, or click Start to use example dataset."
                                  ), 
                                  color = "royal",
                                  style = "unite", 
                                  #icon = bs_icon("power")
                       ),
                       bsTooltip("startBtn", 
                                 title="Test Title", 
                                 trigger = "hover")))),
  tags$br(),
  tabsetPanel(id = "main",                                      #main panel - data & variables
    tabPanel(title = tags$h5("Data & variables"), value = 1,    #tab panel for data & variables value 1
             tags$br(),
             fluidRow(column(3,
                             htmlOutput("varsel1")),            #X-var varsOne
                      column(3,
                             htmlOutput("varsel2")),            #Y-var varsTwo
                      column(3, 
                             #conditional panel to show after start button
                             conditionalPanel(condition = "output.started == 'Now pick variables.'",
                             htmlOutput("addVarsOut"),
                             selectizeInput("addVarsOpt",       #Add grouping var Yes/No button
                                            label = 
                                              list(tooltip(
                                                trigger = list(
                                                  tags$h3("3"), 
                                                  tags$strong("Add a Grouping variable (Yes/No)?"),                                                     bs_icon("info-circle")),
                                                "An optional grouping variable for 2way ANOVA designs. A Grouping variable (either categoric or numeric) is required when both X and Y variables are numeric."
                                              )
                                              ),
                                            choices = c("Choose Yes/No"="",
                                                        "Yes",
                                                        "No"),
                                            options = list(dropdownParent = 'body'))
                             )),
                      column(3, uiOutput("addMoreVars"))),     #Grouping var varsFour
             htmlOutput("tab_header"),                         #Table title   
             card(DT::dataTableOutput("mytable2"))),           #Table output
    tabPanel(title = tags$h5("Graphs"), value = 2,             #Graphs tab value 2  
             tags$br(),
             accordion(open = TRUE,                            
                       accordion_panel("Readme: Graphs (can be minimised)", #Graphs tab title
                                       htmlOutput("graphsHelpOpen"))),      #Graphs tab help
             card(card_header("Select graph options",
                              tooltip(bs_icon("info-circle"), 
                                      "Please select Yes (and then a variable) or No and then press 'Variables chosen'.")),
                  fluidRow(column(3, 
                                  selectizeInput("facetingOpt",             #Add faceting variable yes/no
                                                 #width = "250px",
                                                 label = tooltip(
                                                   trigger = list(
                                                     tags$h3("4"), 
                                                     tags$strong("Add a faceting variable to graph (Yes/No)?"), 
                                                     bs_icon("info-circle")
                                                   ),
                                                   "A categorical variable to facet plots by, or choose No before proceeding."
                                                 ),
                                                 choices = c("Choose Yes/No" = "", c("Yes", "No")),
                                                 options = list(dropdownParent = 'body'))),
                           column(3, 
                                  uiOutput("addFacets"),                  #Add faceting variable varsFive                  
                                  #textOutput("FacVars")
                                  ),
                           column(3, 
                                  selectizeInput("ShapesOpt",             #Add shapes yes/no
                                                 #width = "250px",
                                                 label = tooltip(
                                                   trigger = list(
                                                     tags$h3("5"), 
                                                     tags$strong("Add a variable for matched symbols/shapes?"),
                                                     bs_icon("info-circle")
                                                   ),
                                                   "A categorical variable to map to shape of symbols or a matching variable for Before-after plots, or choose No before proceeding."
                                                 ),
                                                 choices = c("Choose Yes/No" = "", c("Yes", "No")), 
                                                 options = list(dropdownParent = 'body'))),
                           column(3, 
                                  uiOutput("addShapes"),                 #Shapes variable varsThree
                                  textOutput("ShapeLevs")),              #warning if shapes >25  
                           #textOutput("XlevelNames")
                  ),
                  column(3, 
                         actionBttn("varsDone",                          #Done with variables
                                    label = tooltip(
                                      trigger = list(
                                        tags$strong("Variables Chosen"),
                                        bs_icon("check2-circle")
                                      ),
                                      "The options available will depend on the type and number of variables you chose before clicking Done. You can choose different variables again and click 'Variables chosen' to update graph options."
                                    ),size = "sm",
                                    color = "royal",
                                    style = "unite"))),
             #textOutput("OutCatGp")
             fluidRow(column(3,
                             selectizeInput("graphType",                #GraphType dropdown list
                                            #width = "150px",
                                            label = tooltip(
                                              trigger = list(
                                                tags$h3("6"), 
                                                tags$strong("Choose a graph type."),
                                                bs_icon("info-circle")
                                              ),
                                              "After clicking 'Variables Chosen', choose a graph type and then click 'grafify my data' to visualise the graph."
                                            ), 
                                            choices = c("Boxplot",
                                                        "Bar graph",
                                                        "Violin plot",
                                                        "Point & Errorbar",
                                                        "Before-after plot",
                                                        "Density plot",
                                                        "Histogram plot",
                                                        "Numeric XY 1",
                                                        "Numeric XY 2"),
                                            selected = "Boxplot")),
                      #conditional panel if XY is numeric & boxplot wanted?
                      column(4, conditionalPanel("input.graphType == 'Numeric XY 1' || input.graphType == 'Numeric XY 2'",
                                                 selectizeInput("XYBox",              #show numeric boxplot yes/no
                                                                label = tooltip(
                                                                  trigger = list(
                                                                    tags$h3("7"), 
                                                                    tags$strong("Show box and whiskers (Yes/No)?"),
                                                                    bs_icon("info-circle")
                                                                  ),
                                                                  "Click 'Yes' and press 'grafify my data' if you would like box and whiskers plot grouped along X-axis."
                                                                ), 
                                                                choices = c("Yes", 
                                                                            "No"), 
                                                                selected = "No", 
                                                                multiple = FALSE,
                                                                options = list(dropdownParent = 'body'),
                                                 )),
                             textOutput("XforRelevel")),            #Option to reorder X-axis groups yes/no
                      column(3, conditionalPanel("output.XforRelevel == 'The X-axis variable is categorical. You can reorder groups.'",
                                                 selectizeInput("DoRelevel", 
                                                                #width = "150px",
                                                                label = tooltip(
                                                                  trigger = list(
                                                                    tags$h3("7"), 
                                                                    tags$strong("Reorder X-axis groups."),
                                                                    bs_icon("info-circle")
                                                                  ),
                                                                  "Click Yes if you would like to reorder the groups along the X-axis. NOTE: if you drop groups from graphs, they will not be used in the analyses."
                                                                ), 
                                                                choices = c("Choose one"="",
                                                                            "Yes",
                                                                            "No"),
                                                                selected = "No"))),
                      #conditional panel if reordering chosen
                      column(6, conditionalPanel("input.DoRelevel == 'Yes'",
                                                 htmlOutput("selVarsReLevel"),  #reorder X-axis groups
                                                 textOutput("newRelevel"))      #Text of new order of X-axis
                      )),
             #main graph card
             fluidRow(column(10, 
                             card(                                              
                               card_header("",
                                           class = "d-flex justify-content-between",
                                           actionBttn(inputId = "makegraph",
                                                      label = "grafify my data", width = "10px",
                                                      size = "sm",
                                                      color = "royal",
                                                      style = "unite", #block = TRUE,  
                                                      icon = bs_icon("power")),
                                           tooltip(bs_icon("info-circle"),
                                                   "Height and width options below only affect the PDF for download. They do not change appearance of graph here.")),
                               plotOutput("plotChosenGraph",                   #graph output
                                          #width = "60vw", 
                                          #height = "60vh"
                               ),
                               #textOutput("FacVars"),
                               #plotOutput("plotgraph2"),
                               #tags$br(),
                               fluidRow(column(3,                              #PDF height
                                      numericInput("g_ht",
                                                   label = tooltip(
                                                     trigger = list(
                                                       tags$strong("Graph Height (cm)"),
                                                       bs_icon("info-circle")
                                                     ),
                                                     "Adjust height of PDF file."
                                                   ), 
                                                   value = 12)),
                               column(3, numericInput("g_wid",                 #PDF width   
                                                      label = tooltip(
                                                        trigger = list(
                                                          tags$strong("Graph Width (cm)"),
                                                          bs_icon("info-circle")
                                                        ),
                                                        "Adjust width of PDF file."
                                                      ), 
                                                      value = 15)),
             #tags$br(),
             #column so that next button is not on the same line and less wide
             column(10),
             column(3, 
             downloadBttn("SaveGraph", size = "sm",                           #download button
                          label = "Save as PDF",
                          color = "royal",
                          style = "unite",
                          block = TRUE,
                          icon = bs_icon("download")))))))
  ),
  #layout_columns(col_width = c(4, 4),
  #               row_heights = c(2, 2),
  #               card(numericInput("g_ht",
  #                                 label = tooltip(
  #                                   trigger = list(
  #                                     tags$strong("Graph Height (cm)"),
  #                                     bs_icon("info-circle")
  #                                   ),
  #                                   "Adjust height of PDF file."
  #                                 ), 
  #                                 value = 12)),
  #               card(numericInput("g_wid", 
  #                                 label = tooltip(
  #                                   trigger = list(
  #                                     tags$strong("Graph Width (cm)"),
  #                                     bs_icon("info-circle")
  #                                   ),
  #                                   "Adjust width of PDF file."
  #                                 ), 
  #                                 value = 15)))
  #),#
  tabPanel(title = tags$h5("ANOVA & Comparisons"), value = 3,        #ANOVA panel value 3
           tags$br(),
           accordion(open = TRUE,
                     accordion_panel("Readme: ANOVAs and Comparisons (can be minimised)",
                                     htmlOutput("ANOVAsHelpOpen"))),       #ANOVA help
           #textOutput("depReactive"),
           #textOutput("emmPairFML"),
           fluidRow(column(3, 
                           #card(card_header(tags$h6("Type of linear model")),
                               selectizeInput("MorS", multiple = FALSE,       #Type of ANOVA simple/mixed
                                              label = tooltip(
                                                trigger = list(
                                                  tags$h3("9"), 
                                                  tags$strong( "Choose simple or mixed effects model."),
                                                  bs_icon("info-circle")
                                                ),
                                                "A simple linear model is an ordinary 1-way or 2-way ANOVA. For a mixed effects linear models, in addition to fixed factors", tags$strong("Boxes 1-3"), "a random factor is required (e.g., experimental blocks, matching or repeated-measures)."
                                              ),
                                              choices = c("Choose one"="", c("Simple", "Mixed")),
                                              options = list(dropdownParent = 'body')
                               )),
                    column(3, 
                           htmlOutput("varsel6")),                             #Random factor choice varsSix
                    #empty columns for formatting
                    column(6),
                    column(3,
                          #card(card_header(tags$h6("Click to anlayse data")),
                               actionBttn(inputId = "analyseData",             #ANOVA action button  
                                          label = tags$strong("Analyse my data"),
                                          size = "sm", 
                                          color = "royal",
                                          style = "unite",
                                          icon = bs_icon("power")))),
           fluidRow(column(10, 
                    card(card_header("Residuals plots",                        #Residuals plots card
                                           tooltip(bs_icon("info-circle"),
                                                   "These are plots of residuals of your linear model. If residuals are very skewed away from the line in QQ plot or far from 0 in the Density plot, it is likely that there is a major deviation from normal distribution. Therefore, the linear model may be a poor fit for your data. Proceed with caution and consider using data-transformations (e.g., log-data).")),
                         #htmlOutput("qq_head"),
                         fluidRow(column(5, plotOutput("ModPlot",              #QQ plot of residuals
                                                       width = "25vw", 
                                                       height = "25vw")),
                                  column(5, plotOutput("ModPlotDist",          #Density plot of residuals
                                                       width = "25vw", 
                                                       height = "25vw")),
                                  column(10, 
                                         #conditional panel for faceted plot
                                         conditionalPanel(condition = "input.MorS == 'Mixed'",
                                  
                                         plotOutput("RandFplot"))))))          #Plot faceted by random factor
                    ,
                    column(10, #card(card_header("",
                           actionButton("AnTbleHelpBtn",                       #ANOVA table help button
                                        label = "More Info on ANOVA Table", 
                                        icon = icon("info-circle"),
                                        class= "btn-info",
                                        style = "unite"
                           ),
                           tags$br(),
                           card("",
                                card_body(fillable = FALSE,
                                htmlOutput("anova_head"),               #ANOVA table header
                           DT::dataTableOutput("AnovaTab1"))),          #ANOVA table
                           tags$br(),
                           tags$br(),
                           tags$br()), 
                    column(10,
                           #card(card_header("",
                           actionButton("EmmeansHelpBtn",                   #EMMEANS table help button
                                        label = "More Info on EMMEANS table", 
                                        icon = icon("info-circle"),
                                        class= "btn-info",
                                        style = "unite"
                           ),
                           tags$br(),
                           card("",
                                card_body(fillable = FALSE,
                                          htmlOutput("emmeans_head"),        #EMMEANS table header
                           DT::dataTableOutput("Comp1"))),                    #EMMEANS table  
                           tags$br(),
                           tags$br(),
                           tags$br()),
                    column(4, 
                           #card(
                           actionButton("ContrHelper",                         #Posthoc comparisons help button
                                        label = "More Info on Comparisons", 
                                        icon = icon("info-circle"),
                                        class= "btn-info",
                                        style = "unite"),
                           card("",
                                card_body(fillable = FALSE,
                                          selectizeInput("emm_type",                          #choose contrast type
                                          label = list(tags$h3("10"),"Type of comparisons"),
                                          choices = c("Pairwise",
                                                      "Compare to reference",
                                                      "Levelwise 1",
                                                      "Levelwise 2"),
                                          selected = c("Pairwise"),  
                                          options = list(dropdownParent = 'body'),
                                          multiple = FALSE), 
                           uiOutput("emmRefType"), fill = FALSE)),                             #Post-hoc comparison output
                           tags$br()),
                    column(10, 
                           #card(card_header("",
                           actionButton("ConstrasHelpBtn",                     #Contrasts help button
                                        label = "More Info on Post-hoc comparisons", 
                                        icon = icon("info-circle"),
                                        class= "btn-info",
                                        style = "unite"
                           ),
                           tags$br(),
                           card("",
                                card_body(fillable = FALSE,
                                          htmlOutput("contrasts_head"),                      #Posthoc comparisons help button
                           DT::dataTableOutput("Comp2"))),                      #Posthoc comparisons
                           tags$br(),
                           tags$br())
           )),
  tabPanel(title = tags$h5("Linear model"), value = 4,                        #Linear model panel
           tags$br(),
           tags$h6("This is the summary of the fitted model. It shows model parameters, such as residual SE, coefficients etc. For simple or ordinar linear models, grafify uses the lm() function from base R. For mixed effects models, grafify uses the lmer() function from the lme4 and lmerTest packages. "),
           fluidRow(column(10, 
                           verbatimTextOutput("ModSummary")))                 #Linear model from R
  )
)
)
