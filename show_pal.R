## Function to plot a color palette with hexcodes 
show_pal <- function(pal, labels = TRUE, label_size = 1, label_color = "#000000") {
  library(gplots)
  pal <- gplots::col2hex(pal)
  n <- length(pal)
  n_col <- ceiling(sqrt(n))
  n_row <- ceiling(n / n_col)
  m <- matrix(0, n_col, n_row)
  m[n + 1] <- 1
  m <- t(m)
  pal <- c(pal, rep(NA, n_row * n_col - length(pal)))
  pal <- matrix(pal, ncol = n_col, byrow = TRUE)
  plot(c(0, dim(pal)[2]), c(0, -dim(pal)[1]), type = "n", xlab = "", ylab = "",
       axes = FALSE)
  rect(col(pal) - 1, -row(pal) + 1, col(pal), -row(pal), col = pal, lwd = 3, border = "white")
  if (labels) text(col(pal) - 0.5, -row(pal) + 0.5, pal, cex = label_size, col = label_color)
}

# pal <- c("#FFDB6D", "#C4961A", "#F4EDCA",
#          "#D16103", "#C3D7A4", "#52854C",
#          "#4E84C4", "#293352", "dodgerblue")
# show_pal(pal, label_size = .8)  
