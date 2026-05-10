landing_info <- list(tagList(tags$div(
  tags$h5("grafify (vesion 0.1)"), 
  tags$ul(tags$li(
    tags$h6(
      "You can use grafify online to plot graphs, and easily perform ANOVAs and post-hoc comparisons just like the ",
      tags$a(href = "https://grafify-vignettes.netlify.app/", "R package.")
    )
  ), tags$li(
    tags$h6(
      "The main advantages of grafify are the use of ggplot2 and various colourblind-friendly palettes, and easy access to linear models and linear mixed effects analyses for ANOVAs. These are more powerful and appropriate when experiments are designed as randomised blocks or have repeated measures."
    ), 
  ), 
  tags$li(
    tags$h6(
      "Data are available only within the user's session, saved only temporarily for graphing and analysis, and are lost upon the end of the session or user inactivity. If you want to use grafify online for sensitive data, consider running it locally using the code on ",
      tags$a(href = "https://github.com/ashenoy-cmbi/grafifyonline.shinyapp.plosbiol", "GitHub grafify online shiny app."),
      " Windows users can install an " ,
      tags$a(href = "https://sourceforge.net/projects/grafify/", "offline copy"), 
      " that runs locally within the user's browser."
    )
  ), 
  tags$li(
    tags$h6(
      "Use numbers are anonymously counted if cookies are accepted. This helps us understand usage and allocate computing resources.")
  ),
  tags$li(tags$h6(
    "grafify online on this website is made possible through Impaas (Imperial Platform as a Service), courtesy of",
    tags$a(href = "https://edtech.pages.doc.ic.ac.uk/", " Robert Chatley and Jason Bailey, Department of Computing, Imperial College London.")
  )),
  tags$li(p(paste("Last updated on:", last_updated))),)
)))