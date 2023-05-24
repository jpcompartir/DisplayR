#' theme_black
#'
#' Makes the plot dark theme - suitable for Microsoft projects.
#' @export
dr_theme_black <- function(){

  output = ggplot2::theme(plot.background = ggplot2::element_rect(fill = "black", color = "black"),
                          strip.text = ggplot2::element_text(color = "white"),
                          strip.background = ggplot2::element_rect(fill = "black"),
                          panel.background = ggplot2::element_rect(fill = 'black'),
                          axis.text.x = ggplot2::element_text(color = 'white'),
                          axis.title.x = ggplot2::element_text(color = 'white'),
                          axis.text.y = ggplot2::element_text(color = 'white'),
                          axis.line.y = ggplot2::element_line(color = 'white'),
                          axis.title.y = ggplot2::element_text(color = "white"),
                          legend.text = ggplot2::element_text(color = "white"),
                          legend.background = ggplot2::element_rect(fill = 'black'),
                          legend.title = ggplot2::element_text(color = 'white'),
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank())

  output

}

