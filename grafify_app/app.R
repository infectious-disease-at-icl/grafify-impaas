#parts of the UI spread across 8 src01Panel---.R files
#
library(grafify)
library(bslib)   #for theme
library(bsicons) #for icons
library(shinyBS) #for BS tooltip
library(shiny)
library(shinyWidgets) #for updated shiny
library(colourpicker) #for single colour
library(readxl)   #for file upload
library(readr)    #for file upload
library(ggplot2)  #for facet_grid and others
library(lmerTest) #for lmerMod
library(emmeans)  #for posthoc comparisons
library(rlang)    #for !! calls
library(gt)       #for colour tables
library(shinyjqui) #for drag and drop
library(dplyr)

#next few lines for shinylive only
#library(showtext)
#font_add("Arial", "www/fonts/arial.ttf")
#font_add("Courier", "www/fonts/cour.ttf")
#showtext_auto()

#source files for UI parts
#source("./source/src01c2long_GraphOpts_sidebar_ui.R", local = TRUE) #for fonts, colours, symbols, box/violin transparency, errorbars
#this versin of mainbar has no sidebar & has GraphOpts next to graph card
source("./source/src01e_menu_links.R", local = TRUE) #menus on navbar
source("./source/src01eFeb08_mainbar_parts.R", local = TRUE) #main page tabsets
source("./source/src01g_Help_n_Images.R", local = TRUE) #For landing page
app_path <- normalizePath("app.R")
last_updated <- format(file.info(app_path)$mtime, "%d %B %Y, %H:%M") #copilot

# Define UI for application that draws a histogram
ui <- bslib::page_navbar(
  #ga G-059EWJ6910 for shiny.io
  #ga G-TMCF321TZZ for netlify.app
  #ga G-X4RTHLTCT6 for impaas.uk
  tags$head(includeHTML("source/head_copilot.html")),
  tags$body(includeHTML("source/body_copilot.html")), 
  #fluid = TRUE,
  theme = bs_theme(preset = "cosmo",
                   "navbar-bg" = "#aa4499",
                   "progress-bar-bg" = "#445eab",
                   #background colour
                   #bootswatch = "bootstrap",
                   version = "5", 
                   #theme
                   bg = "#ffffff",
                   fg = "#121111",
                   #primary = "#4444ab",
                   #secondary = "#55224D",
                   #info = "#44abab",
                   #success = "#296E13",
                   #`enable-shadows` = TRUE,  #shinylive
                   #`enable-rounded` = TRUE,  #shinylive
                   #base_font = font_face("sans-serif", src = "/fonts"), #shinylive
                   #code_font = font_face("monospace", src = "/fonts"),  #shinylive
                   #heading_font = font_face("sans-serif", src = "/fonts") #shinylive
  ),
  #main title
  title = "grafify online",
  #sidebar for main page
  #main panel with graphs
  bslib::nav_panel(
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
        ),
        ############ for impaas
        tags$li(
          tags$h6(
            "grafify online on this website is made possible through Impaas (Imperial Platform as a Service), courtesy of",
            tags$a(href = "https://edtech.pages.doc.ic.ac.uk/", " Robert Chatley and Jason Bailey, Department of Computing, Imperial College London.")
          )
        ), tags$li(p(paste("Last updated on:", last_updated))),
        )
        ########## for impaas
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
    mainPanel(mainPanel1, #from src01e_mainbar_navbarFluidRows & src01e_menu_links.R
              width = 12)
  ),
  #nav_panel(title = ""),
  #main menu bar names and links
  nav_panel(title = "Instructions", 
            mainPanel(width = 9,
                      HTML(paste0(tags$div(tags$h6(
                        "Note: Quick help also available by hovering over ", 
                        bs_icon("info-circle"),"icons.")))),
                      tabsetPanel(
                        #from src02_headers_help.R & src_01g_Help_n_Images.R
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
            )),
  #links on menu bar sourced from src01e_menu_links
  nav_menu(
    title = "Links",
    align = "left",
    nav_item(link_shenoy),   #shenoylab.com
    nav_item(link_github),
    #grafify github
    nav_item(link_vignettes),
    #vignettes
    nav_item(link_biostats),
    nav_item(link_rcoding)
  ),
  #biostats book
  #favicon for browswers in www folder
  tags$head(tags$link(rel = "shortcut icon", href = "grafify.ico"))
)

# server logic
server <- function(input, output, session) {
  #bslib::bs_themer() #during development
  
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
  
  #X-axis varsOne reactive Box 1
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
  #varsOne UI output X-axis Box 1
  output$varsel1 <- renderUI({
    v1Input()
  })
  
  #Y-axis varsTwo reactive Box 2
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
  #varsTwo UI output Y-axis Box 2
  output$varsel2 <- renderUI({
    v2Input()
  })
  
  #Shapes Box5 varsThree reactive
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
  #Shapes varsThree UI output Box 5
  output$varsel3 <- renderUI({
    v3Input()
  })
  
  #Grouping variable varsFour reactive Box 3.1
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
  #Grouping varsFour UI output Box 3.1
  output$varsel4 <- renderUI({
    v4Input()
  })
  #Faceting variable varsFive reactive Box 4.1
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
  #faceting varsFive UI output Box 4.1
  output$varsel5 <- renderUI({
    v5Input()
  })
  
  #Random variable varsSix reactive Box 9.1
  v6Input <- eventReactive(input$startBtn, {
    req(file1())
    # Variable selection:
    varSelectizeInput(
      inputId = "varsSix",
      label = tooltip(
        trigger = list(
          tags$h3("9.1"),
          tags$strong("Choose a categorical random variable for mixed effects analysis."),
          bs_icon("info-circle")
        ),
        "Choose a column from your data. Note: a random intercepts model will be fitted."
      ),
      data = file1(),
      multiple = FALSE,
      options = list(dropdownParent = 'body'),
      selected = ""
    )
  })
  #Random varsSix UI output Box 9.1
  output$varsel6 <- renderUI({
    if (input$MorS == "Mixed")
      v6Input()
  })
  #UI output for Simple/Mixed ANOVA
  #  output$RandFac <- renderUI({
  #    if (input$MorS == "Mixed")
  #      uiOutput("RandFac")
  #  })
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
  
  #reactive to get a conditional panel match for start button click
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
  source("./source/src01e_GraphTypeChoices.R",
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
  #Reactive for reordering X groups
  RelevelNames <- reactive({
    #force to factors and levels originally from data
    #get levels within categorical X-axis
    req(file1())
    f <- file1()
    ############ always on relevel
    f[[input$varsOne]] <- factor(f[[input$varsOne]], levels = input$varsReLevel)
    flev <- levels(f[[input$varsOne]])
    flev
  })
  #UI output text for user about reordering
  output$newRelevel <- renderText({
    txt <- paste(paste(RelevelNames(), collapse = ", "))
    txt
  })
  ############## always on Grouping relevel
  output$newRelevelGp <- renderText({
    txt <- paste(paste(RelevelNamesGp(), collapse = ", "))
    txt
  })
  RelevelNamesGp <- reactive({
    #force to factors and levels originally from data
    #get levels within Grouping variable
    req(file1())
    f <- file1()
    ############ always on relevel
    f[[input$varsFour]] <- factor(f[[input$varsFour]], levels = input$varsReLevelGp)
    flev <- levels(f[[input$varsFour]])
    flev
  })
  
  #drop levels for X axis and get new table
  RelevelFile1 <- reactive({
    #force to factors and levels originally from data
    #then from user input of groups as new levels
    #varsReLevel has names of groups from user
    #drop levels not used
    req(file1())
    ######### relevel with dplyr
    req(input$varsReLevel, input$varsReLevelGp)
    observe(input$addVarsOpt)
    if(is.numeric(file1()[[input$varsOne]]) #|| 
       #is.numeric(file1()[[input$varsFour]]) 
       ) {
      return(file1())
    }
    file1() %>% 
      filter(get(input$varsOne) %in% input$varsReLevel,
             get(input$varsFour) %in% input$varsReLevelGp) %>% 
      mutate(across(all_of(input$varsOne), ~factor(.x, levels = input$varsReLevel)),
             across(all_of(input$varsFour), ~factor(.x, levels = input$varsReLevelGp)))
    })
  
  #reactive for XYCatGp
  RelevelFile1.2 <- reactive({
    #force to factors and levels originally from data
    #then from user input of groups as newlevels
    #varsReLevel has names of groups from user
    #drop levels not used
    req(file1())
    ######### relevel with dplyr
    req(input$varsReLevelGp)
    observe(input$addVarsOpt)
    file1() %>% 
      filter(get(input$varsFour) %in% input$varsReLevelGp) %>% 
      mutate(across(all_of(input$varsFour), ~factor(.x, levels = input$varsReLevelGp)))
  })
  
  ######### reactive when there is no Grouping variable 
  #drop levels for X axis and get new table
  RelevelFile1.1 <- reactive({
    #force to factors and levels originally from data
    #then from user input of groups as newlevels
    #varsReLevel has names of groups from user
    #drop levels not used
    req(file1())
    ######### relevel with dplyr
    req(input$varsReLevel)
    if(is.numeric(file1()[[input$varsOne]]) ) {
      return(file1())
    }
    file1() %>% 
      filter(get(input$varsOne) %in% input$varsReLevel) %>% 
      mutate(across(all_of(input$varsOne), ~factor(.x, levels = input$varsReLevel)))
  })
  ########
  #reactive to make user table
  tabInput <- eventReactive(input$startBtn, {
    file2 <- file1()
    yname <- colnames(file2)[colnames(file2) == input$varsTwo]
    n <- length(colnames(file2))
    gt(file2) %>% 
      fmt_auto() %>%
      cols_align(
        align = "center",
        columns = everything()) %>%
      opt_interactive()
  })
  #UI output of user table
  output$mytable2 <- render_gt({
    h3("Your data table")
    br()
    tabInput()
  })
  
  #source options for point_sd, errorbars, single colour
  source("./source/src01f_Optional_GraphSettings.R", local = TRUE)
  #source help for tab panels & instructions menu option
  source("./source/src02_headers_help.R", local = TRUE, echo = TRUE)
  #source of emmeans calls
  source("./source/src03b_emmeans.R", local = TRUE, echo = TRUE)
  #source of ANOVA and Residuals plots calls
  source("./source/src03d_anova_n_residuals_SimpMixed.R",
         local = TRUE,
         echo = TRUE)
  #source graph types without/with shapes & faceting
  source("./source/src04_boxplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src05_barplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src05b_pointSD_plot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src06_matchplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  
  source("./source/src08_violinplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src09_befafterplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src10a_3dviolinplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src10b_3dboxplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src10c_3dbarplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src10d_3dpointSDplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src11a_4dBoxplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src11b_4dShapesBoxplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src11c_4dBarplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src11d_4dShapesBarplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src11e_4dViolinplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src11f_4dShapesViolinplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src11g_4dPointplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src11h_4dShapesPointpoint_n_save.R",
         local = TRUE,
         echo = TRUE)
  #source with tooltips for i icons
  source("./source/src12_tooltips.R", local = TRUE)
  source("./source/src13_numericXYplot_n_save.R",
         local = TRUE,
         echo = TRUE)
  source("./source/src14_DensityHistogram_plot_n_save.R",
         local = TRUE,
         echo = TRUE)
  
  
  #main reactive with conditions for which graph to plot
  whichplotChosenGraph <- eventReactive(input$makegraph, {
    #boxplot w & w/o facets
    if (input$graphType == "Boxplot" &
        input$addVarsOpt == "No" &
        input$ShapesOpt == "No")
      p <- plotBox_react()
    #bar graph w & w/o facets
    if (input$graphType == "Bar graph" &
        input$addVarsOpt == "No" &
        input$ShapesOpt == "No")
      p <- plotBar_react()
    #violin graph w & w/o facets
    if (input$graphType == "Violin plot"  &
        input$addVarsOpt == "No" &
        input$ShapesOpt == "No")
      p <- plotViolin_react()
    #pointSD
    if (input$graphType == "Point & Errorbar" &
        input$addVarsOpt == "No" &
        input$ShapesOpt == "No")
      p <- plotPointSD_react()
    
    #befafter graph w & w/o facets
    if (input$graphType == "Before-after plot"  &
        input$addVarsOpt == "No" &
        input$ShapesOpt == "Yes")
      p <- plotBefAfter_react()
    
    #3dBox graphs w & w/o facets
    if (input$graphType == "Boxplot" &
        input$addVarsOpt == "No" &
        input$ShapesOpt == "Yes")
      p <- plot3dBox_react()
    #3dViolin graphs w & w/o facets
    if (input$graphType == "Violin plot" &
        input$addVarsOpt == "No" &
        input$ShapesOpt == "Yes")
      p <- plot3dViolin_react()
    #3dBar graphs w & w/o facets
    if (input$graphType == "Bar graph" &
        input$addVarsOpt == "No" &
        input$ShapesOpt == "Yes")
      p <- plot3dBar_react()
    #3dpoint graphs w & w/o facets
    if (input$graphType == "Point & Errorbar" &
        input$addVarsOpt == "No" &
        input$ShapesOpt == "Yes")
     p <- plot3dPoint_react()
    
    #4dbox w/ w/o shapes (w/o facets)
    if (input$graphType == "Boxplot" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "Yes")
      p <- plot4dShapesBox_react()
    if (input$graphType == "Boxplot" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "No")
      p <- plot4dBox_react()
    #4dbar w/ w/o shapes (wo/ facets)
    if (input$graphType == "Bar graph" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "Yes")
      p <- plot4dShapesBar_react()
    if (input$graphType == "Bar graph" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "No")
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
      #4dbar w/ w/o shapes (wo/ facets)
    if (input$graphType == "Point & Errorbar" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "Yes" )
      p <- plot4dShapesPoint_react()
    if (input$graphType == "Point & Errorbar" &
        input$addVarsOpt == "Yes" &
        input$ShapesOpt == "No" )
      p <- plot4dPoint_react()
    
    #XY numeric catGroup
    if (input$graphType == "Numeric XY 1" &
        input$addVarsOpt == "Yes")
      p <- plot_XYCat_react()
    if (input$graphType == "Numeric XY 2" &
        input$addVarsOpt == "Yes")
      p <- plot_XYNum_react()
    
    #Density & Histogram
    if (input$graphType == "Density plot")
      p <- plotDensity_react()
    if (input$graphType == "Histogram plot" )
      p <- plotHistogram_react()
    
    #output reactive graph p
    p
  })
  
  #output$plotChosenGraph <- renderPlot({ whichplotChosenGraph() })
  
  #add single colour on chosen graph
  PlotSingCol <- eventReactive(input$makegraph, {
    ifelse(input$facetingOpt == "Yes",
           p <- whichplotChosenGraph() +
             facet_grid(FacVars(),
                        scales = input$facet_scales),
           p <- whichplotChosenGraph())
    if(input$addVarsOpt == "Yes" & 
       Xnum() == FALSE & 
       CatGp() == TRUE){singColnum <- CatGplevels()}
    if(input$addVarsOpt == "No" & 
       Xnum() == FALSE) {singColnum <- Xlevels()}
    ifelse (input$colPick == "No" ,
            p <- p,
            p <- p +
              scale_fill_manual(values = rep(input$colPick2, 
                                             times = singColnum)))
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
  
  #for Inno
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
