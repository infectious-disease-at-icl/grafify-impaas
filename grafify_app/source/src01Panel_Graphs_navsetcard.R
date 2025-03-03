source("./source/src01PanelGraphs_card4_5.R", local = TRUE) #graphs panel 1
source("./source/src01PanelGraphs_card6_7.R", local = TRUE) #graphs panel 2
source("./source/src01PanelGraphs_card8.R", local = TRUE) #graphs panel 3
source("./source/src01PanelGraphs_cardPlotDownload.R", local = TRUE) #graphs panel 4

Panel_Graphs <- list(fluidRow(
  column(12, accordion(
    open = TRUE,
    accordion_panel(
      "Readme: Graphs (can be minimised)",
      #Graphs tab title
      htmlOutput("graphsHelpOpen")
    )
  )),
  column(
    12,
    #full card for side column 8
    tags$br(),
    card(
      card_header(
        tags$h5("Add more variables to graph"),
        class = "d-flex justify-content-between",
        tooltip(
          bs_icon("info-circle"),
          "Please select Yes (and then a variable) or No and then press 'Variables chosen' before proceeding."
        )
      ),
      Graphs_card4_5 #src01Panelgraphs_card4_5.R
    ),
    column(12, card(card_header(
      tags$h5("Graph choice & X-axis options")
    ), Graphs_card6_7 #src01Panelgraphs_card6_7.R))
    ),
    column(
      12,
      navset_card_underline(
        title = tagList(
          #tags$h5("Graph"),
          #tags$br(),
          actionBttn(
            inputId = "makegraph",
            label = "grafify my data",
            width = "10px",
            size = "sm",
            color = "royal",
            style = "unite",
            #block = TRUE,
            icon = bs_icon("power")
          ),
          tooltip(
            bs_icon("info-circle"),
            "Height and width options below only affect the PDF for download. They do not change appearance of graph here."
          )
        ),
        full_screen = TRUE,
        height = "800px",
        nav_panel("Graph", Graphs_PlotDownload),
        nav_panel("Graph Appearance", mainPanel(HTML(
          paste(
            tags$h3("8"),
            tags$h5(
              "Graph Appearance",
              #sidebar title
              class = "d-flex justify-content-between",
              tooltip(
                bs_icon("info-circle"),
                "Select options here and click 'grafify my data' to update the #graph."
              )
            )
          )
        ), Graphs_card8))
        ,
        #          sidebar = sidebar(width = "325px",
        #                            postion = "right",
        #                            open = FALSE,
        #            card(HTML(
        #                    paste(
        #                      tags$h3("8"),
        #                      tags$h5(
        #                        "Graph Appearance",
        #                        #sidebar title
        #                        class = "d-flex justify-content-between",
        #                        tooltip(
        #                          bs_icon("info-circle"),
        #                          "Select options here and click 'grafify my data' to update the #graph."
        #                        )
        #                      )
        #                    )
        #                  ),
        #              Graphs_card8)),
        #src01PanelGraphs_card8.R
        #Graphs_PlotDownload,
        #src01PanelGraphs_cardPlotDownload.R
      )
    )
    ))))
  
