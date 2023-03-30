theme_tq_envelope <- function (base_size = 11, base_family = "")
{
  blue <- "#2c3e50"
  green <- "#18BC9C"
  white <- "#FFFFFF"
  grey <- "grey80"
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(line = ggplot2::element_line(colour = blue,
                                                size = 0.5, linetype = 1, lineend = "butt"), rect = ggplot2::element_rect(fill = white,
                                                                                                                          colour = blue, size = 0.5, linetype = 1), text = ggplot2::element_text(family = base_family,
                                                                                                                                                                                                 face = "plain", colour = blue, size = base_size,
                                                                                                                                                                                                 lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                                                                                                                                                                                                 margin = ggplot2::margin(), debug = FALSE), axis.line = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
                   axis.ticks = ggplot2::element_line(color = grey,
                                                      size = ggplot2::rel(1/3)), axis.title = ggplot2::element_text(size = ggplot2::rel(1)),
                   panel.background = ggplot2::element_rect(fill = white,
                                                            color = NA), panel.border = ggplot2::element_rect(fill = NA,
                                                                                                              size = ggplot2::rel(1/2), color = blue), panel.grid.major = ggplot2::element_line(color = grey,
                                                                                                                                                                                                size = ggplot2::rel(1/3)), panel.grid.minor = ggplot2::element_line(color = grey,
                                                                                                                                                                                                                                                                    size = ggplot2::rel(1/3)), panel.grid.minor.x = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(0.75, "cm"), legend.key = ggplot2::element_rect(fill = white,
                                                                                                 color = NA), legend.position = "bottom", strip.background = ggplot2::element_rect(fill = blue,
                                                                                                                                                                                   color = blue), strip.text = ggplot2::element_text(color = white,
                                                                                                                                                                                                                                     size = ggplot2::rel(0.8), margin = ggplot2::margin(t = 5,
                                                                                                                                                                                                                                                                                        b = 5)), plot.title = ggplot2::element_text(size = ggplot2::rel(1.2),
                                                                                                                                                                                                                                                                                                                                    hjust = 0, margin = ggplot2::margin(t = 0, r = 0,
                                                                                                                                                                                                                                                                                                                                                                        b = 4, l = 0, unit = "pt")), plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.9),
                                                                                                                                                                                                                                                                                                                                                                                                                                           hjust = 0, margin = ggplot2::margin(t = 0, r = 0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               b = 3, l = 0, unit = "pt")), complete = TRUE)
}
