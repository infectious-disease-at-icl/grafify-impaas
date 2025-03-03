plot_density_log <- function(data, ycol, group, facet,  c_alpha = 0.2, TextXAngle = 0, LogYTrans, LogYBreaks = waiver(), Ylabels = waiver(), LogYLimits = NULL, facet_scales = "fixed", fontsize = 20, linethick, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, ...){
  if(missing(linethick)) {linethick = fontsize/22}
  ColPal <- match.arg(ColPal)
  suppressWarnings(P <- ggplot2::ggplot(data, 
                                        aes(x = {{ ycol }},
                                            fill = factor({{ group }}),
                                            colour = factor({{ group }})))+
                     geom_density(size = linethick,
                                  alpha = c_alpha)+
                     labs(y = "Density"))
  if (!missing(LogYTrans)) {
    if (!(LogYTrans %in% c("log2", "log10"))) {
      stop("LogYTrans only allows 'log2' or 'log10' transformation.")
    }
    if (LogYTrans == "log10") {
      P <- P + 
        scale_x_continuous(trans = "log10", 
                           breaks = LogYBreaks, 
                           labels = Ylabels, 
                           limits = LogYLimits, 
                           ...)+
        annotation_logticks(sides = "b", 
                            outside = TRUE,
                            base = 10, color = "grey20",
                            long = unit(7*fontsize/22, "pt"), size = unit(fontsize/22, "pt"),# 
                            short = unit(4*fontsize/22, "pt"), mid = unit(4*fontsize/22, "pt"),#
                            ...)+ 
        coord_cartesian(clip = "off", ...)
    }
    if (LogYTrans == "log2") {
      P <- P + 
        scale_x_continuous(trans = "log2", 
                           breaks = LogYBreaks, 
                           labels = Ylabels, 
                           limits = LogYLimits, 
                           ...)}
  }
  if(!missing(facet)) {
    P <- P + facet_wrap(vars({{ facet }}), 
                        scales = facet_scales, 
                        ...)
  }
  P <- P +
    labs(fill = enquo(group),
         colour = enquo(group))+
    theme_grafify(base_size = fontsize)+
    guides(x = guide_axis(angle = TextXAngle))+
    scale_fill_grafify(palette = ColPal, 
                       reverse = ColRev, 
                       ColSeq = ColSeq)+
    scale_colour_grafify(palette = ColPal, 
                         reverse = ColRev, 
                         ColSeq = ColSeq)
  P
}


plot_histogram_log <- function(data, ycol, group, facet, BinSize = 30, c_alpha = 0.8, TextXAngle = 0, LogYTrans, LogYBreaks = waiver(), Ylabels = waiver(), LogYLimits = NULL, facet_scales = "fixed", fontsize = 20, linethick, alpha, ColPal = c("okabe_ito", "all_grafify", "bright",  "contrast",  "dark",  "fishy",  "kelly",  "light",  "muted",  "pale",  "r4",  "safe",  "vibrant"), ColSeq = TRUE, ColRev = FALSE, ...){
  if(missing(linethick)) {linethick = fontsize/22}
  if (!missing("alpha")) {
    warning("Use `c_alpha` argument instead, as `alpha` is deprecated.")
    c_alpha <- substitute(alpha)}
  ColPal <- match.arg(ColPal)
  if(missing(group)) {
    suppressWarnings(P <- ggplot2::ggplot(data, 
                                          aes({{ ycol }}))+
                       geom_histogram(size = linethick,
                                      alpha = c_alpha,
                                      bins = BinSize,
                                      colour = "black",
                                      linewidth = linethick,
                                      fill = "#E69F00"))
  } else {
    suppressWarnings(P <- ggplot2::ggplot(data, 
                                          aes({{ ycol }}))+
                       geom_histogram(size = linethick,
                                      alpha = c_alpha,
                                      colour = "black",
                                      linewidth = linethick,
                                      bins = BinSize,
                                      aes(fill = factor({{ group }})))+
                       theme_grafify(base_size = fontsize)+
                       guides(x = guide_axis(angle = TextXAngle))+
                       scale_fill_grafify(palette = ColPal, 
                                          reverse = ColRev, 
                                          ColSeq = ColSeq))
  }
  if (!missing(LogYTrans)) {
    if (!(LogYTrans %in% c("log2", "log10"))) {
      stop("LogYTrans only allows 'log2' or 'log10' transformation.")
    }
    if (LogYTrans == "log10") {
      P <- P + 
        scale_x_continuous(trans = "log10", 
                           breaks = LogYBreaks, 
                           labels = Ylabels, 
                           limits = LogYLimits, 
                           ...)+
        annotation_logticks(sides = "b", 
                            outside = TRUE,
                            base = 10, color = "grey20",
                            long = unit(7*fontsize/22, "pt"), size = unit(fontsize/22, "pt"),# 
                            short = unit(4*fontsize/22, "pt"), mid = unit(4*fontsize/22, "pt"),#
                            ...)+ 
        coord_cartesian(clip = "off", ...)
    }
    if (LogYTrans == "log2") {
      P <- P + 
        scale_x_continuous(trans = "log2", 
                           breaks = LogYBreaks, 
                           labels = Ylabels, 
                           limits = LogYLimits, 
                           ...)}
  }
  if(!missing(facet)) {
    P <- P + facet_wrap(vars({{ facet }}), 
                        scales = facet_scales, 
                        ...)
  }
  P <- P +
    labs(fill = enquo(group))+
    theme_grafify(base_size = fontsize)+
    theme(strip.background = element_blank())+
    guides(x = guide_axis(angle = TextXAngle))
  P
}
