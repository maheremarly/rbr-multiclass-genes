# The paper's figure: micro-averaged F1 against the fraction of the ranked
# shortlist retained, one panel per classifier, one line per feature selection
# method.
#
# The four panels share a y axis so that they can be read against each other.


#' Long-format view of one classifier's results table.
melt_results <- function(results, stepsize, methods) {
  block <- as.data.frame(results)
  block$percent <- stepsize
  long <- reshape2::melt(block, id.vars = "percent", measure.vars = methods)
  long$percent <- as.numeric(long$percent)
  long
}


#' One panel: micro F1 against features retained, with the x axis reversed.
plot_method <- function(data, title, y_label = "F1 Score") {
  ggplot2::ggplot(data, ggplot2::aes(x = percent, y = value, color = variable)) +
    ggplot2::geom_line(ggplot2::aes(linetype = variable), linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(shape = variable)) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    ) +
    ggplot2::xlab("Features Used") +
    ggplot2::ylab(y_label) +
    ggplot2::labs(
      linetype = "FS Method", color = "FS Method", shape = "FS Method"
    ) +
    ggplot2::scale_x_reverse(
      breaks = c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1),
      labels = as.character(seq(100, 10, by = -10))
    )
}


#' The four-panel figure for one dataset.
#'
#' @param results_list Named list of fold-averaged results tables, one per
#'   classifier; the names become the panel titles.
#' @param stepsize The fractions the tables were built with.
#' @param methods Feature selection methods to draw.
#' @param path Output PDF path.
make_figure <- function(results_list, stepsize, methods, path,
                        width = 12, height = 8.5) {
  long <- lapply(results_list, melt_results, stepsize = stepsize, methods = methods)

  limits <- range(unlist(lapply(long, `[[`, "value")), na.rm = TRUE)

  panels <- lapply(names(long), function(title) {
    ggplot2::ggplotGrob(
      plot_method(long[[title]], title) +
        ggplot2::scale_y_continuous(
          limits = limits,
          breaks = scales::pretty_breaks(n = 5)
        )
    )
  })

  figure <- gridExtra::arrangeGrob(grobs = panels, nrow = 2, ncol = 2)
  ggplot2::ggsave(path, figure, width = width, height = height, units = "in")
  message("Figure written to ", path)
  invisible(path)
}
