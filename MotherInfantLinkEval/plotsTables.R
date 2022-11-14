plotScatter <- function(compareData,
                        xCount,
                        yCount,
                        xLabel = "Linked",
                        yLabel = "Not linked") {

  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
  limits <- c(0, 1)


  xCount <- formatC(xCount, format = "d", big.mark = ",")
  yCount <- formatC(yCount, format = "d", big.mark = ",")

  xLabel <- sprintf("%s \n (n = %s)", xLabel, xCount)
  yLabel <- sprintf("%s \n (n = %s)", yLabel, yCount)



  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  plot <- ggplot2::ggplot(compareData, ggplot2::aes(x = mean1, y = mean2)) +
    ggplot2::geom_point(ggplot2::aes(colour = domainId), shape = 16, size = 2) +
    ggplot2::scale_fill_manual(values = cbPalette) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_x_continuous(xLabel, limits = limits) +
    ggplot2::scale_y_continuous(yLabel, limits = limits) +
    ggplot2::theme(text = theme)
  return(plot)
}
