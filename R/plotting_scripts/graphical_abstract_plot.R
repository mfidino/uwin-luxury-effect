###################################
#
# Mixing figure 2 and 3 together for the graphical abstract
#
# Written by M. Fidino
#
###################################

#  This script assumes you've ran species_rich_across_sites.R

library("vioplot")

pcity$Long <- gsub(",\\n",", ", pcity$Pretty)
pcity$Long <- gsub("\\n"," ", pcity$Long)
#windows(9,6)

windows(4.5, 6)

tiff("./plots/among_model/graphical_abstract.tiff", height = 6, width = 4.5,
     units = "in", res = 800, 
     compression = "lzw")

{
  par(mar = c(5,8,1,6))
  plot(1~1, type = "n", xlim = c(-1,1), ylim = c(1,20), xlab = "",
       ylab = "", xaxt = "n", yaxt="n", bty = "n", bty = "n")
  
  
  # sorting the species by their baseline detection probability.
  mu_t <- order(apply(income_cor, 2 ,median), decreasing = FALSE)
  
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
  
  mtext(text = "Correlation between species richness\nand a city's income or urbanization gradient",1,
        at = 0, line = 3.0)
  
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
    
    posterior <- income_cor[,mu_t[sp_iter]]
    posterior <- posterior[between(posterior,HDIofMCMC(posterior)[1],
                                       HDIofMCMC(posterior)[3])]
    posterior2 <- urb_cor[,mu_t[sp_iter]]
    posterior2 <- posterior2[between(posterior2,HDIofMCMC(posterior2)[1],
                                   HDIofMCMC(posterior2)[3])]
    #posterior2 <- exp(rowSums(model_array[,2:3,mu_t[sp_iter]]))
    #posterior <- posterior2 / posterior
    #posterior2 <- posterior[between(posterior,HDIofMCMC(posterior)[1],
     #                               HDIofMCMC(posterior)[3])]
    
    my_vioplot(posterior, at = sp_iter , horizontal = TRUE, add = TRUE,
               side = "right",
               col = scales::alpha("#7fbf7b", 0.75), drawRect = TRUE,
               wex = 3)
    my_vioplot(posterior2, at = sp_iter , horizontal = TRUE, add = TRUE,
               side = "right",
               col = scales::alpha("#a6cee3", 0.75), drawRect = TRUE,
               wex = 3)
  }
  
  lines(x = rep(0,2), y = u[3:4], lwd = 2, lty = 3)
  
  legend(x = 1.05, y = 13, c("Median", "Income", "Urbanization"), lty = c(1,NA,NA),
         pt.bg = c(NA, "#7fbf7b", "#a6cee3"), pch = c(NA, 22, 22), title = "95% Posterior",
         bg = "white", pt.cex = 1.5, lwd = c(2, NA, NA),
         y.intersp = 1.175, cex = 0.8, bty = "n")
  
  

}
dev.off()

