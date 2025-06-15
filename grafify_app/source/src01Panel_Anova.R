Panel_Anova <- list(
  fluidRow(column(
    12, accordion(
      open = TRUE,
      accordion_panel(
        "Readme: ANOVAs and Comparisons (can be minimised)",
        htmlOutput("ANOVAsHelpOpen"),
        tags$br()
      )
    ),  tags$br(), 
  ),column(10, card( tags$br(), 
    fluidRow(
      column(
        3,
        #card(card_header(tags$h6("Type of linear model")),
        selectizeInput(
          "MorS",
          multiple = FALSE,
          #Type of ANOVA simple/mixed
          label = tooltip(
            trigger = list(
              tags$h3("9"),
              tags$strong("Choose simple or mixed effects model."),
              bs_icon("info-circle")
            ),
            "A simple linear model is an ordinary 1-way or 2-way ANOVA. For a mixed effects linear models, in addition to fixed factors",
            tags$strong("Boxes 1-3"),
            "a random factor is required (e.g., experimental blocks, matching or repeated-measures)."
          ),
          choices = c("Choose one" =
                        "", c("Simple", "Mixed")),
          options = list(dropdownParent = 'body')
        )
      ),
      column(4, htmlOutput("varsel6")),
      column(4, 
             conditionalPanel(
               "input.MorS == 'Mixed'",  
             selectizeInput(
               "AvgRF",
               multiple = FALSE,
               #Average RF or not
               label = tooltip(
                 trigger = list(
                   tags$h3("9.2"),
                   tags$strong("Choose whether to average replicates grouped by fixed and random factors."),
                   bs_icon("info-circle")
                 ),
                 "If a Random factor is selected, you can choose to average replicates within the levels of this variable or not."
               ),
               choices = c("Choose one" =
                             "", c("Yes", "No")),
               selected = "Yes",
               options = list(dropdownParent = 'body')
             ))),
      #Random factor choice varsSix
      #empty columns for formatting
      column(12),
      column(
        3,
        #card(card_header(tags$h6("Click to analyse data")),
        actionBttn(
          inputId = "analyseData",
          #ANOVA action button
          label = tags$strong("Analyse my data"),
          size = "sm",
          color = "royal",
          style = "unite",
          icon = bs_icon("power")
        )
      ),
      column(6,
             conditionalPanel(condition = "input.MorS == 'Mixed' && input.AvgRF == 'Yes'", 
                              HTML("Replicates, if any, within levels of the Random Factor, grouped by Fixed Factor(s) chosen in Boxes 2 and/or 3, will be averaged and their means used to fit a random intercepts model."))),
    )
  ))),
  fluidRow(
    column(10, card(
      card_header(
        "Residuals plots",
        #Residuals plots card
        tooltip(
          bs_icon("info-circle"),
          "These are plots of residuals of your linear model. If residuals are very skewed away from the line in QQ plot or far from 0 in the Density plot, it is likely that there is a major deviation from normal distribution. Therefore, the linear model may be a poor fit for your data. Proceed with caution and consider using data-transformations (e.g., log-data)."
        )
      ),
      #htmlOutput("qq_head"),
      fluidRow(
        column(5, plotOutput(
          "ModPlot", #QQ plot of residuals
          width = "25vw", height = "25vw"
        )),
        column(5, plotOutput(
          "ModPlotDist", #Density plot of residuals
          width = "25vw", height = "25vw"
        )),
        column(
          10,
          #conditional panel for faceted plot
          conditionalPanel(condition = "input.MorS == 'Mixed'", 
                           plotOutput("RandFplot", height = "40vw"))
        ), 
        column(10, 
               textOutput("RFLev_txt")),
        column(
          10,
          #conditional panel for faceted plot
          conditionalPanel(condition = "input.MorS == 'Mixed' && input.graphType != 'Numeric XY 1' && input.graphType != 'Numeric XY 2' && output.RFLev_txt == 'Levels within Random Factor are mapped to symbol shapes.'", 
                           plotOutput("avgRandFplot", height = "40vw"))
        )
        )
    ))          #Plot faceted by random factor
    ,
    column(
      10,
      #card(card_header("",
      actionButton(
        "AnTbleHelpBtn",
        #ANOVA table help button
        label = "More Info on ANOVA Table",
        icon = icon("info-circle"),
        class = "btn-info",
        style = "unite"
      ),
      tags$br(),
      htmlOutput("anova_head"),
      #ANOVA table header
      gt_output("AnovaTab1"),
      #ANOVA table
      tags$br(),
      tags$br(),
      tags$br()
    ),
    column(
      10,
      #card(card_header("",
      actionButton(
        "EmmeansHelpBtn",
        #EMMEANS table help button
        label = "More Info on EMMEANS table",
        icon = icon("info-circle"),
        class = "btn-info",
        style = "unite"
      ),
      tags$br(),
      htmlOutput("emmeans_head"),
      #EMMEANS table header
      gt_output("Comp1"),
      #EMMEANS table
      tags$br(),
      tags$br(),
      tags$br()
    ),
    column(
      6,
      #card(
      actionButton(
        "ContrHelper",
        #Posthoc comparisons help button
        label = "More Info on Comparisons",
        icon = icon("info-circle"),
        class = "btn-info",
        style = "unite"
      ),
      selectizeInput(
        "emm_type",
        #choose contrast type
        label = list(tags$h3("10"), "Type of comparisons"),
        choices = c("Pairwise", 
                    "Compare to reference", 
                    "Levelwise 1", 
                    "Levelwise 2"),
        selected = c("Pairwise"),
        options = list(dropdownParent = 'body'),
        multiple = FALSE
      ),
      uiOutput("emmRefType"),
      #Post-hoc comparison output
      tags$br()
    ),
    column(
      10,
      #card(card_header("",
      actionButton(
        "ConstrasHelpBtn",
        #Contrasts help button
        label = "More Info on Post-hoc comparisons",
        icon = icon("info-circle"),
        class = "btn-info",
        style = "unite"
      ),
      tags$br(),
      htmlOutput("contrasts_head"),
      #Posthoc comparisons help button
      tags$br(),
      gt_output("Comp2"),
      #Posthoc comparisons
      tags$br()
    ),
    column(
      10,
      tags$br(),
      card(
        card_header(
          tags$h5(
            "Linear Model",
            #sidebar title
            class = "d-flex justify-content-between",
            tooltip(
              bs_icon("info-circle"),
              "Raw output from R of the linear model fitted to data."
            )
          )
        ),
        #Linear model panel
        tags$h6(
          "This is the summary of the fitted model. It shows model parameters, such as residual SE, coefficients etc. For simple or ordinar linear models, grafify uses the lm() function from base R. For mixed effects models, grafify uses the lmer() function from the lme4 and lmerTest packages. "
        ),
        verbatimTextOutput("ModSummary"),
        tags$br()
      )                 #Linear model from R
    ),
    column(
      10,
      tags$br(),
      card(
        card_header(
          tags$h5(
            "Data used for analyses",
            #sidebar title
            class = "d-flex justify-content-between",
            tooltip(
              bs_icon("info-circle"),
              "Table showing data used for Simple or Mixed effects analyses."
            )
          )
        ),
        #Linear model panel
        HTML("The table below includes the variables used in the analysis, which were chosen in Boxes 1-3. For Mixed effects analyses, means of replicate values of the response variable, if any, (chosen in Box 2) within levels of the random variable (chosen in Box 9), along with the Median, SD and Counts are shown."),
        gt_output("avgFile_out"),
      )                 #Linear model from R
    )
  )
)
