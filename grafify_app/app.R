#This one works with various source files and generates just graphs
#adding 4d plots #with bin sizes #fixed 4dPointSD #Allows Excel files #QQ & Density & multiFacets #Added images_n_help
#Errorbar dropdown list #Images for Instructions

#APP without sidebar
library(grafify)
library(bslib)   #for theme
library(bsicons) #for icons
library(shinyBS) #for BS tooltip
library(shiny)
library(shinyWidgets) #for updated shiny
library(colourpicker) #for single colour
library(readxl)  #for file updoad
library(readr)   #for file upload
library(DT)      #for tables
library(ggplot2) #for facet_grid and others
library(lmerTest) #for lmerMod
library(emmeans) #for posthoc comparisons
library(rlang)   #for !! calls
#library(dplyr)  #
#library(ggpubr)

source("src01c2long_GraphOpts_sidebar_ui.R", local = TRUE) #for fonts, colours, symbols, box/violin transparency, errorbars
#this versin of mainbar has no sidebar & has GraphOpts next to graph card
source("src01eJ19_mainbar_navbarFluidRows.R", local = TRUE) #also sources src01e_menu_links.R
source("src01g_Help_n_Images.R", local = TRUE) #For landing page

# Define UI for application that draws a histogram
ui <- page_navbar(
  #ga G-059EWJ6910 for shiny.io
  #ga G-TMCF321TZZ for netlify.app
  tags$head(includeHTML("head_copilot.html")),
  tags$body(includeHTML("body_copilot.html")), 
  #fluid = TRUE,
  theme = bs_theme(
    "navbar-bg" = "#aa4499",
    #background colour
    bootswatch = "bootstrap",
    #theme
    bg = "#ffffff",
    fg = "#121111",
    primary = "#4444ab",
    secondary = "#55224D",
    info = "#44abab",
    success = "#296E13",
    `enable-shadows` = TRUE,
    `enable-rounded` = TRUE,
    base_font = font_google("IBM Plex Sans"),
    code_font = font_google("JetBrains Mono"),
    heading_font = font_google("Roboto Slab"),
    "progress-bar-bg" = "#445eab"
  ),
  #main title
  title = "grafify online",
  #sidebar for main page
  #main panel with graphs
  nav_panel(
    width = 12,
    title = "Graphs and Analysis",
    #main panel sourced from src01 *mainbar* file
    mainPanel(width = 12, fluidRow(
      column(2, tagList(
        tags$div(tags$img(src = "grafify.png", width = "70%"), style = "text-align: center;")
      )), column(5, tagList(tags$div(
        tags$h5("grafify"), tags$ul(tags$li(
          tags$h6(
            "You can use grafify online to plot graphs, and easily perform ANOVAs and post-hoc comparisons just like the ",
            tags$a(href = "https://grafify-vignettes.netlify.app/", "R package.")
          )
        ), tags$li(
          tags$h6(
            "The main advantages of grafify are the use of ggplot2 and various colourblind-friendly palettes, and easy access to linear models and linear mixed effects analyses for ANOVAs. These are more powerful and appropriate when experiments are designed as randomised blocks or have repeated measures."
          )
        ))
      ))), column(
        3,
        card(
          card_header(
            tags$h5("Start here"),
            #start button
            class = "d-flex justify-content-between",
            tooltip(
              bs_icon("info-circle"),
              "Upload a csv or Excel file & click 'Start', or click 'Start' to use example data, and see dropdown menus for choosing variables."
            )
          ),
          #textOutput("started"),
          fileInput(
            "file1",
            #file upload
            placeholder = "Path to file",
            "Upload a CSV or Excel File",
            accept = c(".csv", ".xlsx", ".xls")
          ),
          uiOutput("sheetSelector"),
          actionBttn(
            inputId = "startBtn",
            #start button
            size = "md",
            block = TRUE,
            label = tooltip(
              trigger = list(tags$strong("Start"), bs_icon("info-circle")),
              "Upload a file and click Start, or click Start to use example dataset."
            ),
            color = "royal",
            style = "unite",
            #icon = bs_icon("power")
          ),
          bsTooltip("startBtn", title = "Test Title", trigger = "hover")
        )
      )
    ), ),
    mainPanel(mainPanel1, width = 12)
  ),
  #nav_panel(title = ""),
  #main menu bar names and links
  nav_panel(title = "Instructions", 
            mainPanel(width = 9,
                      HTML(paste0(tags$div(tags$h6(
                        "Note: Quick help also available by hovering over ", bs_icon("info-circle"),"icons.")))),
                      tabsetPanel(
                        tabPanel(tags$h5("Instructions: Data & Variables"),
                                 tags$br(),
                                 htmlOutput("Instr_Data")),
                        tabPanel(tags$h5("Instructions: Graphs"),
                                 tags$br(),
                                 htmlOutput("Instr_Graphs")),
                        tabPanel(tags$h5("Instructions: ANOVAs & Comparisons"),
                                 tags$br(),
                                 htmlOutput("Instr_ANOVA"))
                      ),
                      #htmlOutput("pickvariables")
            )),
  #help from src02* headerhelp file
  #links on menu bar sourced from src01e_menu_links
  nav_menu(
    title = "Links",
    align = "left",
    nav_item(link_shenoy),   #shenoylab.com
    nav_item(link_github),
    #grafify github
    nav_item(link_vignettes),
    #vignettes
    nav_item(link_biostats)
  ),
  #biostats book
  #favicon for browswers in www folder
  tags$head(tags$link(rel = "shortcut icon", href = "grafify.ico"))
)

# server logic
server <- function(input, output) {
  #bslib::bs_themer()
  
  #Main uploaded file
  file1 <- reactive({
    inFile <- input$file1
    #get file extension
    file_ext <- tools::file_ext(input$file1$name)
    
    #options if file is csv, excel or null
    if (is.null(inFile)) {
      #use 2way ANOVA file
      file1 <- ggplot2::mpg
    } else if (file_ext == "csv") {
      file1 <- read_csv(inFile$datapath)
    } else if (file_ext %in% c("xlsx", "xls")) {
      file1 <-
        read_excel(input$file1$datapath, sheet = input$sheet)
    } else {
      stop("Unsupported file type")
    }
    file1
  })
  #sheet selector UI for Excel file
  output$sheetSelector <- renderUI({
    req(input$file1)
    file_ext <- tools::file_ext(input$file1$name)
    if (file_ext %in% c("xlsx", "xls")) {
      sheets <- excel_sheets(input$file1$datapath)
      selectInput("sheet", "Select Sheet", choices = sheets)
    }
  })
  
  #X-axis varsOne reactive
  v1Input <- eventReactive(input$startBtn, {
    req(file1())
    # Variable selection:
    varSelectInput(
      inputId = "varsOne",
      label = tooltip(
        trigger = list(
          tags$h3("1"),
          tags$strong("X-axis variable (categorical or numeric)"),
          bs_icon("info-circle")
        ),
        "Pick a variable to plot along the X-axis. This is typically a categorical variable for box/bar/point/violin plots. If your X-axis variable is numeric, please also select a Grouping variable."
      ),
      selected = NULL,
      data = file1(),
      selectize = TRUE,
      multiple = FALSE
    )
  })
  #varsOne UI output
  output$varsel1 <- renderUI({
    v1Input()
  })
  
  #Y-axis varsTwo reactive
  v2Input <- eventReactive(input$startBtn, {
    req(file1())
    # Variable selection:
    varSelectInput(
      inputId = "varsTwo",
      label = tooltip(
        trigger = list(
          tags$h3("2"),
          tags$strong("Y-axis variable (should be a numeric variable)"),
          bs_icon("info-circle")
        ),
        "Pick a numeric variable to plot along the Y axis."
      ),
      data = file1(),
      selectize = TRUE,
      selected = NULL,
      multiple = FALSE
    )
  })
  #varsTwo UI output
  output$varsel2 <- renderUI({
    v2Input()
  })
  
  #Shapes varsThree reactive
  v3Input <- eventReactive(input$startBtn, {
    req(file1())
    # Variable selection:
    varSelectizeInput(
      inputId = "varsThree",
      label = tooltip(
        trigger = list(
          tags$h3("5.1"),
          tags$strong("Choose a shapes/matching variable from your data."),
          bs_icon("info-circle")
        ),
        "Choose a column from your data."
      ),
      data = file1(),
      selected = NULL,
      options = list(dropdownParent = 'body'),
      multiple = FALSE
    )
  })
  #Shapes varsThree UI output
  output$varsel3 <- renderUI({
    v3Input()
  })
  
  #Grouping variable varsFour reactive
  v4Input <- eventReactive(input$startBtn, {
    req(file1())
    # Variable selection:
    varSelectInput(
      inputId = "varsFour",
      label = tooltip(
        trigger = list(
          tags$h3("3.1"),
          tags$strong("Select a Grouping variable (e.g., 2-way ANOVA designs)"),
          bs_icon("info-circle")
        ),
        "Choose a column from your data."
      ),
      selected = NULL,
      data = file1(),
      selectize = TRUE,
      multiple = FALSE
    )
  })
  #Grouping varsFour UI output
  output$varsel4 <- renderUI({
    v4Input()
  })
  #Faceting variable varsFive reactive
  v5Input <- eventReactive(input$startBtn, {
    req(file1())
    # Variable selection:
    varSelectizeInput(
      inputId = "varsFive",
      label = tooltip(
        trigger = list(
          tags$h3("4.1"),
          tags$strong("Choose faceting variables for graphs from your data."),
          bs_icon("info-circle")
        ),
        "Choose one or more columns from your data. If you choose more than one variable, faceting will use them in the chosen order to generate panels by rows and then by columns."
      ),
      data = file1(),
      multiple = TRUE,
      options = list(dropdownParent = 'body'),
      selected = ""
    )
  })
  #faceting varsFive UI output
  output$varsel5 <- renderUI({
    v5Input()
  })
  
  #Random variable varsSix reactive
  v6Input <- eventReactive(input$startBtn, {
    req(file1())
    # Variable selection:
    varSelectizeInput(
      inputId = "varsSix",
      label = tooltip(
        trigger = list(
          tags$h3("9.1"),
          tags$strong("Choose a random variable for mixed effects analysis"),
          bs_icon("info-circle")
        ),
        "Choose a column from your data."
      ),
      data = file1(),
      multiple = FALSE,
      options = list(dropdownParent = 'body'),
      selected = ""
    )
  })
  #Random varsSix UI output
  output$varsel6 <- renderUI({
    if (input$MorS == "Mixed")
      v6Input()
  })
  #UI output for Simple/Mixed ANOVA
  output$RandFac <- renderUI({
    if (input$MorS == "Mixed")
      uiOutput("RandFac")
  })
  #UI output if Shapes is yes
  output$addShapes <- renderUI({
    if (input$ShapesOpt == "Yes")
      uiOutput("varsel3")
  })
  #UI output if Faceting is yes
  output$addFacets <- renderUI({
    if (input$facetingOpt == "Yes")
      uiOutput("varsel5")
  })
  #UI output for Grouping variable
  output$addMoreVars <- renderUI({
    if (input$addVarsOpt == "Yes")
      uiOutput("varsel4")
  })
  
  #reactive to get a conditional panel match for startbutton click
  startedq <- eventReactive(input$startBtn, {
    txt <- paste("Now pick variables.")
  })
  #UI output for start reactive
  output$started <- renderText({
    startedq()
  })
  #Option to have output even when not on page
  outputOptions(output, "started", suspendWhenHidden = FALSE)
  
  #source file with updated choices of graphs based on variable types
  source("src01e_GraphTypeChoices.R",
         local = TRUE,
         echo = TRUE)
  
  #updated colour palettes for numeric XY + numeric Grouping graphs
  observe({
    if (input$graphType == "Numeric XY 2")
      updateSelectInput(
        #session = "graphType",
        inputId = "colpal",
        #label = tags$strong("Choose graph type"),
        choices = c(
          "blue_conti",
          "yellow_conti",
          "grey_conti",
          "PrGn_div",
          "OrBl_div"
        )
      )
  })
  
  #updated colour palettes for numeric XY + categorical Grouping graphs
  observe({
    if (input$graphType != "Numeric XY 2")
      updateSelectInput(
        #session = "graphType",
        inputId = "colpal",
        #label = tags$strong("Choose graph type"),
        choices = c(
          "okabe_ito",
          "bright",
          "contrast",
          "dark",
          "fishy",
          "kelly",
          "light",
          "muted",
          "pale",
          "r4",
          "safe",
          "vibrant"
        )
      )
  })
  
  #UI output for for X-axis log transformations
  output$logTransX <- renderUI({
    if (Xnum() == TRUE)
      #graph_log X
      pickerInput(
        "logTransX",
        label = tooltip(
          trigger = list(tags$strong("Log(X) axis"), bs_icon("info-circle")),
          "Pick a log-X axis if X-axis variable is also numeric. Log-transformed data will be used in analyses."
        ),
        choices = c("", "log10", "log2"),
        selected = "",
        multiple = FALSE
      )
  })
  #use UI output even when hidden
  outputOptions(output, "logTransX", suspendWhenHidden = FALSE)
  
  #UI output for violin transparency
  observe({
    if (input$graphType == "Violin plot")
      updateNumericInput(
        inputId = "box_alpha",
        min = 0, step = 0.1,
        max = 1,
        value = 0
      )
  })
  #UI output of transparency if not violins
  observe({
    if (input$graphType != "Violin plot")
      updateNumericInput(
        inputId = "box_alpha",
        min = 0, step = 0.1,
        max = 1,
        value = 1
      )
  })
  #update UI output symsize for plot_point_sd plots
  observe({
    if (input$graphType == "Point & Errorbar")
      updateNumericInput(
        inputId = "sym_size",
        min = 0, step = 1,
        max = 10,
        value = 5
      )
  })
  #update UI output if not plot_point_sd plots
  observe({
    if (input$graphType != "Point & Errorbar")
      updateNumericInput(
        inputId = "sym_size",
        min = 0, step = 1,
        max = 10,
        value = 3
      )
  })
  #update UI output for transparency for plot_point_sd
  observe({
    if (input$graphType == "Point & Errorbar")
      updateNumericInput(
        inputId = "sym_alpha",
        min = 0, step = 0.1,
        max = 1,
        value = 1
      )
  })
  #update UI output for transparency if not plot_point_sd
  observe({
    if (input$graphType != "Point & Errorbar")
      updateNumericInput(
        inputId = "sym_alpha",
        min = 0, step = 0.1,
        max = 1,
        value = 0.8
      )
  })
  #update UI output jitter for plot_point_sd
  observe({
    if (input$graphType == "Point & Errorbar")
      updateNumericInput(
        inputId = "sym_jitter",
        min = 0, step = 0.1,
        max = 1,
        value = .05
      )
  })
  #update UI output jitter if not plot_point_sd
  observe({
    if (input$graphType != "Point & Errorbar")
      updateNumericInput(
        inputId = "sym_jitter",
        min = 0, step = 0.1,
        max = 1,
        value = 0.1
      )
  })
  #UI output update for textAngle if XY numeric
  observe({
    if (Xnum() == TRUE)
      updateNumericInput(inputId = "text_angle", value = 0)
  })
  #UI output update for textAngle if not XY numeric
  observe({
    if (Xnum() == FALSE)
      updateNumericInput(inputId = "text_angle", value = 45)
  })
  #UI output update for textAngle if Histogram or Density plots
  #observe({
  #  if(input$graphType %in% c("Histogram plot",
  #                       "Density plot"))
  #     updateNumericInput(inputId = "text_angle",
  #                        value = 0)
  #})
  ##UI output update for textAngle if not Histogram or Density plots
  #observe({
  #  if(input$graphType %in% c("Boxplot",
  #                            "Bar graph",
  #                            "Violin plot",
  #                            "Point & Errorbar"))
  #    updateNumericInput(inputId = "text_angle",
  #                       value = 45)
  #})
  #Reactive for reordering X groups
  RelevelNames <- reactive({
    #force to factors and levels originally from data
    #get levels within categorical X-axis
    req(file1())
    f <- file1()
    observe(input$DoRelevel)
    if (input$DoRelevel == "Yes")
      f[[input$varsOne]] <- factor(f[[input$varsOne]], levels = input$varsReLevel)
    levels(f[[input$varsOne]])
  })
  #UI output text for user about reordering
  output$newRelevel <- renderText({
    txt <- paste("New order of X-axis variables: ",
                 paste(RelevelNames(), collapse = ", "))
    txt
  })
  #drop levels for X axis and get new table
  RelevelFile1 <- reactive({
    #force to factors and levels originally from data
    #then from user input of groups as newlevels
    #varsReLevel has names of groups from user
    #drop levels not used
    req(file1())
    f <- file1()
    observe(input$DoRelevel)
    if (Xnum() == TRUE) {
      f <- file1()
    }
    if (input$DoRelevel == "No" &
        Xnum() == FALSE) {
      f[[input$varsOne]] <- factor(f[[input$varsOne]], levels = XlevelNames())
    }
    if (input$DoRelevel == "Yes" &
        Xnum() == FALSE) {
      f[[input$varsOne]] <- factor(f[[input$varsOne]], levels = input$varsReLevel)
      newLevels <- droplevels(f[[input$varsOne]])
      f <- subset(f, f[[input$varsOne]] == newLevels)
    }
    f
  })
  
  #reactive to make user table
  tabInput <- eventReactive(input$startBtn, {
    file2 <- file1()
    yname <- colnames(file2)[colnames(file2) == input$varsTwo]
    n <- length(colnames(file2))
    DT::formatRound(DT::datatable(file2, options = list(columnDefs = list(
      list(className = 'dt-left', targets = 1:n)
    ))),
    digits = 3,
    columns = yname)
  })
  #UI output of user table
  output$mytable2 <- DT::renderDataTable({
    h3("Your data table")
    br()
    tabInput()
  })
  
  #source options for point_sd, errorbars, single colour
  source("src01f_Optional_GraphSettings.R", local = TRUE)
  #source help for tab panels & instructions menu option
  source("src02_headers_help.R", local = TRUE, echo = TRUE)
  #source of emmeans calls
  source("src03b_emmeans.R", local = TRUE, echo = TRUE)
  #source of ANOVA and Residuals plots calls
  source("src03d_anova_n_residuals_SimpMixed.R",
         local = TRUE,
         echo = TRUE)
  #source graph types without/with shapes & faceting
  source("src04_boxplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src05_barplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src05b_pointSD_plot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src06_matchplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  
  source("src08_violinplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src09_befafterplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src10a_3dviolinplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src10b_3dboxplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src10c_3dbarplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src10d_3dpointSDplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src11a_4dBoxplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src11b_4dShapesBoxplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src11c_4dBarplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src11d_4dShapesBarplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src11e_4dViolinplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src11f_4dShapesViolinplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src11g_4dPointplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src11h_4dShapesPointpoint_n_save.R",
         local = TRUE,
         echo = TRUE)
  #source with tooltips for i icons
  source("src12_tooltips.R", local = TRUE)
  source("src13_numericXYplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("src14_DensityHistogram_plot_n_save.R",
         local = TRUE,
         echo = TRUE)
  
  
  #main reactive with conditions for which graph to plot
  whichplotChosenGraph <- eventReactive(input$makegraph, {
    #boxplot w & w/o facets
    if (input$graphType == "Boxplot" &
        input$ShapesOpt == "No")
      p <- plotBox_react()
    if (input$graphType == "Boxplot"  &
        input$ShapesOpt == "No" &
        input$facetingOpt == "Yes")
      p <- fac_plotBox_react()
    #bar graph w & w/o facets
    if (input$graphType == "Bar graph" &
        input$ShapesOpt == "No")
      p <- plotBar_react()
    if (input$graphType == "Bar graph"  &
        input$ShapesOpt == "No" &
        input$facetingOpt == "Yes")
      p <- fac_plotBar_react()
    #violin graph w & w/o facets
    if (input$graphType == "Violin plot"  &
        input$ShapesOpt == "No")
      p <- plotViolin_react()
    if (input$graphType == "Violin plot"  &
        input$ShapesOpt == "No" &
        input$facetingOpt == "Yes")
      p <- fac_plotViolin_react()
    #befafter graph w & w/o facets
    if (input$graphType == "Before-after plot"  &
        input$ShapesOpt == "Yes" &
        input$facetingOpt == "No")
      p <- plotBefAfter_react()
    if (input$graphType == "Before-after plot" &
        input$ShapesOpt == "Yes" &
        input$facetingOpt == "Yes")
      p <- fac_plotBefAfter_react()
    #3dBox graphs w & w/o facets
    if (input$graphType == "Boxplot" &
        input$ShapesOpt == "Yes")
      p <- plot3dBox_react()
    if (input$graphType == "Boxplot" &
        input$ShapesOpt == "Yes" &
        input$facetingOpt == "Yes")
      p <- fac_plot3dBox_react()
    #3dViolin graphs w & w/o facets
    if (input$graphType == "Violin plot" &
        input$ShapesOpt == "Yes")
      p <- plot3dViolin_react()
    if (input$graphType == "Violin plot" &
        input$ShapesOpt == "Yes" &
        input$facetingOpt == "Yes")
      p <- fac_plot3dViolin_react()
    #3dBar graphs w & w/o facets
    if (input$graphType == "Bar graph" &
        input$ShapesOpt == "Yes")
      p <- plot3dBar_react()
    if (input$graphType == "Bar graph" &
        input$ShapesOpt == "Yes" &
        input$facetingOpt == "Yes")
      p <- fac_plot3dBar_react()
    #3dpoint graphs w & w/o facets
    if (input$graphType == "Point & Errorbar" &
        input$ShapesOpt == "Yes")
      p <- plot3dPoint_react()
    if (input$graphType == "Point & Errorbar" &
        input$ShapesOpt == "Yes" &
        input$facetingOpt == "Yes")
      p <- fac_plot3dPoint_react()
    
    #4dbox w/ w/o shapes (w/o facets)
    if (input$graphType == "Boxplot" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "Yes")
      p <- plot4dShapesBox_react()
    if (input$graphType == "Boxplot" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "No")
      p <- plot4dBox_react()
    #4dbox w w/o shapes (w/ facets)
    if (input$graphType == "Boxplot" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "Yes" &
        input$facetingOpt == "Yes")
      p <- fac_plot4dShapesBox_react()
    if (input$graphType == "Boxplot" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "No" &
        input$facetingOpt == "Yes")
      p <- fac_plot4dBox_react()
    #4dbar w/ w/o shapes (w/ facets)
    if (input$graphType == "Bar graph" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "Yes" &
        input$facetingOpt == "Yes")
      p <- fac_plot4dShapesBar_react()
    if (input$graphType == "Bar graph" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "No" &
        input$facetingOpt == "Yes")
      p <- fac_plot4dBar_react()
    #4dbar w/ w/o shapes (wo/ facets)
    if (input$graphType == "Bar graph" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "Yes"&
        input$facetingOpt == "No")
      p <- plot4dShapesBar_react()
    if (input$graphType == "Bar graph" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "No"&
        input$facetingOpt == "No")
      p <- plot4dBar_react()
    #4dviolin w/ w/o shapes (w/o facets)
    if (input$graphType == "Violin plot" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "Yes")
      p <- plot4dShapesViolin_react()
    if (input$graphType == "Violin plot" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "No")
      p <- plot4dViolin_react()
    #4dviolin w w/o shapes (w/ facets)
    if (input$graphType == "Violin plot" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "Yes" &
        input$facetingOpt == "Yes")
      p <- fac_plot4dShapesViolin_react()
    if (input$graphType == "Violin plot" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "No" &
        input$facetingOpt == "Yes")
      p <- fac_plot4dViolin_react()
    
    #4dpoint w/ w/o shapes (w/ facets)
    if (input$graphType == "Point & Errorbar" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "Yes" &
        input$facetingOpt == "Yes")
      p <- fac_plot4dShapesPoint_react()
    if (input$graphType == "Point & Errorbar" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "No" &
        input$facetingOpt == "Yes")
      p <- fac_plot4dPoint_react()
    #4dbar w/ w/o shapes (wo/ facets)
    if (input$graphType == "Point & Errorbar" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "Yes" &
        input$facetingOpt == "No")
      p <- plot4dShapesPoint_react()
    if (input$graphType == "Point & Errorbar" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "No" &
        input$facetingOpt == "No")
      p <- plot4dPoint_react()
    
    #XY numeric catGroup
    if (input$graphType == "Numeric XY 1" &
        input$addVarsOpt == "Yes" &
        input$facetingOpt == "Yes")
      p <- fac_plot_XYCat_react()
    if (input$graphType == "Numeric XY 1" &
        input$addVarsOpt == "Yes" &
        input$facetingOpt == "No")
      p <- plot_XYCat_react()
    #XY numeric nuGroup
    if (input$graphType == "Numeric XY 2" &
        input$addVarsOpt == "Yes" &
        input$facetingOpt == "Yes")
      p <- fac_plot_XYNum_react()
    if (input$graphType == "Numeric XY 2" &
        input$addVarsOpt == "Yes" &
        input$facetingOpt == "No")
      p <- plot_XYNum_react()
    #Density & Histogram
    if (input$graphType == "Density plot"  &
        input$facetingOpt == "Yes")
      p <- fac_plotDensity_react()
    if (input$graphType == "Density plot" &
        input$facetingOpt == "No")
      p <- plotDensity_react()
    if (input$graphType == "Histogram plot" &
        input$facetingOpt == "Yes")
      p <- fac_plotHistogram_react()
    if (input$graphType == "Histogram plot" &
        input$facetingOpt == "No")
      p <- plotHistogram_react()
    
    #pointSD
    if (input$graphType == "Point & Errorbar" &
        input$addVarsOpt == "No" &
        input$facetingOpt == "Yes" &
        input$ShapesOpt == "No")
      p <- fac_plotPointSD_react()
    if (input$graphType == "Point & Errorbar" &
        input$addVarsOpt == "No" &
        input$facetingOpt == "No" &
        input$ShapesOpt == "No")
      p <- plotPointSD_react()
    
    #output reactive graph p
    p
  })
  
  #output$plotChosenGraph <- renderPlot({ whichplotChosenGraph() })
  
  #add single colour on chosen graph
  PlotSingCol <- eventReactive(input$makegraph, {
    if (input$colPick == "No")
      p <- whichplotChosenGraph()
    if (input$colPick == "Yes")
      p <- whichplotChosenGraph() +
        scale_fill_manual(values = rep(input$colPick2, times = Xlevels()))
    p
  })
  
  #main UI output of graph
  output$plotChosenGraph <- renderPlot({
    PlotSingCol()
  })
  
  #UT output for download button
  output$SaveGraph <- downloadHandler(
    filename = function(file) {
      "grafify_plot.pdf"
    },
    content = function(file) {
      ggsave(
        file,
        width = input$g_wid,
        height = input$g_ht,
        units = "cm"
      )
    }
  )
  # Update file 'date creation'
}

# Run the application
shinyApp(ui = ui, server = server)
