# a subplot for income_correlation_plot.R

income_plot <- function(
  x,
  xlimit = c(0,250),
  the_title,
  my_cor){
  
  plot(1~1, type = "n", xlim = xlimit, ylim = c(0,12), xlab = "",
       ylab = "", xaxt = "n", yaxt="n", bty = "n", bty = "n",
       yaxs = "i", xaxs = "i")
  
  
  #axis(2, at= seq(1,9), labels = F, tck = -.025)
  u <- par("usr")
  xlines <- seq(0,250,length.out = 3)
  for(i in 1:length(xlines)){
    if(i == 1){
      my_col <- "black"
    } else {
      my_col = "gray80"
    }
    lines(
      x = rep(xlines[i],2), y = u[3:4], lty = 1, col = my_col
    )
  }
  
  # x axis
  axis(1, at= seq(0,250, 125), labels = F, tck = -0.025, lwd = 1.5)
  axis(1, at= seq(0,250, 125/2), labels = F, tck = -0.0125, lwd = 1.5)
  mtext(
    text = sprintf("%.0f", seq(0,250, 125)), 
    1, line = 0.75, at = seq(0,250, 125), las = 1)
  
  ylines <- c(0,3,6,9,12)
  for(i in 1:length(ylines)){
    if(i == 1){
      my_col <- "black"
    } else {
      my_col = "gray80"
    }
    lines(
      y = rep(ylines[i],2), x = c(0, 250), lty = 1, col = my_col
    )
    
  }
  
  axis(2, at= seq(0,12, 6), labels = F, tck = -0.025, lwd = 1.5)
  axis(2, at= seq(0,12, 3), labels = F, tck = -0.0125, lwd = 1.5)
  mtext(
    text = sprintf("%.0f", seq(0,12, 6)), 
    2, line = 0.75, at = seq(0,12, 6), las = 1)
  
  #axis(2, at= seq(1,20, 1), labels = F, tck = -0.025)
  
  mtext(text = paste0(
    the_title,
    " (r = ", sprintf("%.2f", my_cor), ")"),
    line = 0.75,
    side = 3
  )
  for(j in 1:nrow(x)){
    lines(x = rep(x$Income[j], 2),
          y = c(x$lo[j], x$hi[j]))
  }
  points(x$med  ~ x$Income, cex = 1.2, pch = 21, bg = "#a5bfdd")
  
}