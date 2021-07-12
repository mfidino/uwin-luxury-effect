###################################
#
# Plotting out correlations among cities for income and species richness
#
# Written by M. Fidino
#
###################################

#  This script assumes you've ran species_rich_across_sites.R

library(vioplot)
pcity$Long <- gsub(",\\n",", ", pcity$Pretty)
pcity$Long <- gsub("\\n"," ", pcity$Long)

tiff("./plots/among_model/URB_correlation_color.tiff", height = 6, width = 9,
     units = "in", res = 800, 
     compression = "lzw")

# two figures in this plot.

# large right margin (2.5) because we are going to be putting species
#  names in between the two plots.
#windows(9,6)

{
  m <- matrix(
    c(1,1,2,3,1,1,4,5,1,1,6,7), ncol = 4, nrow = 3, byrow = TRUE
  )
  layout(m)
  par(mar = c(5,13,1,1))
  plot(1~1, type = "n", xlim = c(-1,1), ylim = c(1,20), xlab = "",
       ylab = "", xaxt = "n", yaxt="n", bty = "n", bty = "n")
  
  
  # sorting the species by their baseline detection probability.
  mu_t <- order(apply(urb_cor, 2 ,median), decreasing = TRUE)
  
  fancy_sp <- pcity$Long[mu_t]
  #axis(2, at= seq(1,9), labels = F, tck = -.025)
  u <- par("usr")
  lines(x = rep(-1,2), y = u[3:4], lty = 1, col = "gray80")
  lines(x = rep(-0.5,2), y = u[3:4], lty = 1, col = "gray80")
  #lines(x = rep(0,2), y = u[3:4], lty = 1, col = "gray80")
  lines(x = rep(0.5,2), y = u[3:4], lty = 1, col = "gray80")
  lines(x = rep(1,2), y = u[3:4], lty = 1, col = "gray80")
  
  # x axis
  axis(1, at= seq(-1,1, 0.5), labels = F, tck = -0.025, lwd = 1.5)
  axis(1, at= seq(-1,1, 0.25), labels = F, tck = -0.0125, lwd = 1.5)
  mtext(text = sprintf("%.1f", seq(-1,1, 0.5)), 
        1, line = 0.75, at = seq(-1,1, 0.5), las = 1)
  
  
  #axis(2, at= seq(1,20, 1), labels = F, tck = -0.025)
  
  mtext(text = fancy_sp, 
        2, line = -0.25, at = seq(1,20, 1), las = 1, cex = 0.9)
  
  mtext(text = "Correlation between species richness\nand a city's urbanization gradient",1,
        at = 0, line = 3.75)
  
  #text(x = rep(19, 8) - 0.04, y = (1:8 + 0.4) , labels = fancy_sp, pos = 1)
  #tg <- 0.3
  #for(sp_iter in 1:2){
  #	lines(x = c(0,60), y = rep(sp_iter,2),
  #				col = "gray70")
  #}
  
  par(xpd = NA)
  yo <- rev(1:20)
  u <- par("usr")
  
  
  
  for(i in 1:20){
    sp_iter <- yo[i]
    
    lines(x = c(-1,1), y = rep(sp_iter,2), col = "gray80")
    
    posterior <- urb_cor[,mu_t[sp_iter]]
    #posterior2 <- exp(rowSums(model_array[,2:3,mu_t[sp_iter]]))
    #posterior <- posterior2 / posterior
    posterior2 <- posterior[between(posterior,HDIofMCMC(posterior)[1],
                                    HDIofMCMC(posterior)[3])]
    
    my_vioplot(posterior, at = sp_iter , horizontal = TRUE, add = TRUE,
               side = "right", col = "gray40", drawRect = TRUE,
               wex = 1.75)
    my_vioplot(posterior2, at = sp_iter , horizontal = TRUE, add = TRUE,
               side = "right", col = "#a5bfdd", drawRect = TRUE,
               wex = 3)
    #text(y = sp_iter +0.4, x = median(posterior), 
    #		 labels = sprintf("%.0f", median(posterior)), cex = 0.7)
    
    # text_loc <- switch(sp_iter,
    #                    median(posterior),
    #                    median(posterior),
    #                    median(posterior),
    #                    median(posterior),
    #                    median(posterior) + 0.05,
    #                    median(posterior),
    #                    median(posterior),
    #                    median(posterior))
    # the_text <-  switch(sp_iter,
    #                     sprintf("%.2f", median(posterior)),
    #                     sprintf("%.2f", median(posterior)),
    #                     sprintf("%.2f", median(posterior)),
    #                     sprintf("%.2f", median(posterior)),
    #                     sprintf("%.2f", median(posterior)),
    #                     sprintf("%.2f", median(posterior)),
    #                     sprintf("%.2f", median(posterior)),
    #                     sprintf("%.2f", median(posterior)))
    # text(y = sp_iter +0.78, x = text_loc, 
    #      labels = the_text, cex = 0.6)
  }
  
  lines(x = rep(0,2), y = u[3:4], lwd = 2, lty = 3)
  
  legend(x = 0.4, y = 3, c("Median", "95%", "100%"), lty = c(1,NA,NA),
         pt.bg = c(NA, "#a5bfdd", "gray40"), pch = c(NA, 22, 22), title = "Posterior",
         bg = "white", pt.cex = 1.5, lwd = c(2, NA, NA),
         y.intersp = 1.076)
  
  
  # legend(x = 1, y = 10.5, c("Median", "95%", "100%"), lty = c(1,NA,NA),
  #        pt.bg = c(NA, "gray80", "gray40"), pch = c(NA, 22, 22), title = "Posterior",
  #        bg = "white", pt.cex = 1.5, lwd = c(2, NA, NA), bty = "n")
  
  # Phoenix plot
  par(mar = c(2,4,4,1)) # 2
  urb_plot(
    x = my_rich_urb$phaz,
    the_title = "Phoenix",
    my_cor = urb_quants[13,2]
  )
  # Chicago plot
  par(mar = c(2,2,4,3)) # 3
  urb_plot(
    x = my_rich_urb$chil,
    the_title = "Chicago",
    my_cor = urb_quants[3,2]
  )
  # EDAL plot
  par(mar = c(3.5,4,2.5,1)) # 4
  urb_plot(
    x = my_rich_urb$edal,
    the_title = "Edmonton",
    my_cor = urb_quants[5,2]
  )
  mtext("Species richness at site", 2, line = 3, at = 6)
  # ADD y axis thing here
  # SAFL plot
  par(mar = c(3.5,2,2.5,3)) # 5
  urb_plot(
    x = my_rich_urb$safl,
    the_title = "Sanford",
    my_cor = urb_quants[15,2]
  )
  # MAKS plot
  par(mar = c(5,4,1,1)) # 6
  urb_plot(
    x = my_rich_urb$deco,
    the_title = "Denver",
    my_cor = urb_quants[4,2]
  )
  # ICIA plot
  par(mar = c(5,2,1,3)) # 7
  par(xpd = NA)
  urb_plot(
    x = my_rich_urb$icia,
    the_title = "Iowa City",
    my_cor = urb_quants[7,2]
  )
  legend("bottomright", c("Median", "95%"), lty = c(NA,1),
         pt.bg = c("#a5bfdd", NA), pch = c(21,NA), title = "Posterior",
         bg = "white", pt.cex = 1.5, lwd = c(NA,2), y.intersp = 1.175)
  
  
  mtext("Urbanization gradient",
        1, line = 2.5, at = -6.5)
  #arrows(x0 = -3, x1 = 5, y0 = -4.5, length = 0.05)
  #arrows(x0 = -10, x1 = -17.5, y0 = -4.5, length = 0.05)
  #text(x = -3.5, y = -5.5, labels = "Greater impervious cover", pos = 4)
  #text(x = -15.5, y = -5.5, labels = "Greater NDVI", pos = 4)
}


dev.off()

