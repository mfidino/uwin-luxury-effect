######################################
#
# Make the two plots for the intercept and
#  two slope terms.
#
# Written by M. Fidino
#
######################################

library(dplyr)
library(lubridate)
library(runjags)
library(parallel)
library(coda)
library(knitr)

model <- "./JAGS/multi_scale_occupancy_simpler_RE.R"
min_dets <- 19
include_inxs <- FALSE
# Whether or not you want to check out the gelman rubin diagnostic
#  of parameters
check_grdiag <- FALSE

source("./R/prep_data_for_model.R")


########################################################
# Setting stuff up for the two plots
########################################################

# get some values we are going to call multiple times

ncity <- data_list$ncity
nspecies <- data_list$nspecies

# read in the results
m1 <- readRDS("./results/final_model_fit.RDS")

# convert the jags output to a matrix
mc <- as.matrix(
  as.mcmc.list(m1),
  chains = TRUE
)[,-1]

# Get just the results of whether a species was available
#  in the city
mx <- mc[,grep("x", colnames(mc))]

# and then remove that from the rest of the matrix
mc <- mc[,-grep("x", colnames(mc))]

# pull the median and 95% quantiles
msum <- apply(mc, 2, quantile, probs = c(0.025,0.5,0.975))

# transpose so it's long on rows, not columns
msum <- t(msum)

# get the global averages and global standard deviations
species_average_occ <- msum[
  grep(
    "b_within\\[",
    row.names(msum)
  ),
]

tau_within <- msum[
  grep(
    "tau_within",
    row.names(msum)
  ),
]
sd_within <- sqrt(1 / tau_within)

# get the species specific estimates. We need the mcmc
#  draws for this.
bspci <- mc[
  ,
  grep(
    "b_species_city",
    colnames(mc)
    )
]

# replicate the mx mat by the number of parameters
#  in this case there are 3 (intercept, income, URB)
mx3 <- mx[
  ,
  rep(
    1:ncol(mx),
    each = 3
  )
]

# For the city specific estimates
species_city_est <- matrix(
  NA,
  ncol = 3,
  nrow = ncol(bspci)
)
row.names(species_city_est) <- colnames(bspci)

for(i in 1:ncol(bspci)){
  if(median(mx3[,i]) == 0){
    next
  }
  tmpmc <- bspci[mx3[,i] == 1,i]
  species_city_est[i,] <- quantile(tmpmc, probs = c(0.025,0.5,0.975))
}

########################################################
# For the average occupancy plot
########################################################

# get the among city averages
species_mu <- msum[
  grep(
    "^b_species\\[1",
    row.names(msum)
  ),
]
# give them some names
row.names(species_mu) <- species_map$Species

# get the species specific standard deviations
#  among cities
tau_species <- msum[
  grep(
    "tau_species\\[1"
    , row.names(msum)
  ),
]
sd_species <- sqrt(1 / tau_species)
row.names(sd_species) <- species_map$Species

# a matrix for the 95% predictive interval
species_95pi <- matrix(
  NA,
  ncol = 2,
  nrow = nspecies
)
# fill it in
for(i in 1:nspecies){
  species_95pi[i,] <- plogis(
    qnorm(
      c(0.025,0.975),
      species_mu[i,2],
      sd_species[i,2]
    )
  )
}
# convert species_mu to probability (for intercept only)
species_mu <- plogis(species_mu)

# get the ordering, from lowest to highest
species_order <- order(
  species_mu[,2],
  decreasing = FALSE
)
# sort the mu and 95% PI
species_mu <- species_mu[species_order,]
species_95pi <- species_95pi[species_order,]





species_city <- species_city_est[
  grep(
    "b_species_city\\[1",
     row.names(species_city_est)
  ),
  2
]

# split species city by species
species_city <- matrix(
  species_city,
  ncol = ncity,
  nrow = nspecies
)
species_city <- plogis(species_city)
species_city <- species_city[species_order,]

# get 95% CI as well

species_city_quantiles <- species_city_est[
  grep(
    "b_species_city\\[1",
    row.names(species_city_est)
  ),
  
  ]

# split species city by species
species_city_quantiles <- array(
  species_city_quantiles,
  dim = c(nspecies, ncity, 3),
  dimnames = list(species_map$Species, city_map$City, c("lo", "med", "hi"))

)
species_city_quantiles <- plogis(species_city_quantiles)

condition <- c("raccoon", "phaz")
species_city_quantiles[
  species_map$Species == condition[1],
  city_map$City == condition[2],
]

# And the figure!
tiff(
  "./plots/among_model/figure_4.tiff",
  height = 7,
  width = 6,
  units = "in",
  res = 800,
  compression = "lzw"
)

par(mar=c(3.5,10,0,2))
par(xpd = NA)

# y_min modifies the vertical placement of the legend in this
#  plot. I used this to play around with the appropriate placement 

y_min <- 16.5
{plot(1~1, type = "n", xlim = c(0,1), ylim = c(0.5,nspecies+0.5),
     bty = "l", xlab = "",
     ylab = "", yaxt = "n",
     xaxt = "n",
     yaxs = "i",xaxs = "i")
  
set.seed(90210)

rect(xleft = plogis(species_average_occ[1,1]),
     xright = plogis(species_average_occ[1,3]),
     ybottom = 0.5,
     ytop = nspecies+0.5,
     col = "gray80",
     border = "gray80")
lines(x = rep(plogis(species_average_occ[1,2]),2),
      y = c(0.5, nspecies+0.5),
      lwd = 2,
      lty = 3,
      col = "gray30")

axis(1, at= seq(0,1, 0.2), labels = F, tck = -0.025, lwd = 2)
axis(1, at= seq(0,1, 0.2/2), labels = F, tck = -0.0125, lwd = 2)

axis(2, at = seq(0.5, nspecies+0.5),
     labels = F, tck = 0, lwd = 2)
axis(2, at = seq(1, nspecies),
     labels = F, tck = -0.0125, lwd = 2)



tp <- gsub("_"," ", row.names(species_mu))

tp <- gsub("north american ", "", tp)

tp <- gsub("virginia", "Virginia", tp, ignore.case = FALSE)

tp <- gsub("white ", "white-", tp, ignore.case = FALSE)

mtext(
  sprintf("%.1f", seq(0,1, 0.2)),
  1,
  at = seq(0,1,0.2),
  line = 0.75,
  cex = 1.3
)

mtext(tp,
      2,
      0.5, at = seq(1, nspecies),
      cex = 1.4, las = 1)

mtext("Occupancy", 1, 2.2, cex = 1.6)

for(i in 1:nspecies){
  # plot predictive interval first
  lines(
    x = species_95pi[i,],
    y = rep(i,2), 
    col =scales::alpha("gray20", 0.75),
    lwd = 2 
  )
  # followed by a gray box to act as a base for the
  #  95% CI so we don't see the 95% PI underneath it
  rect(
    species_mu[i,1],
    i - 0.085,
    species_mu[i,3],
    i + 0.085, 
    col = "gray80",
    border = "gray80"
  )
  # Then the 95% CI
    rect(
      species_mu[i,1],
      i - 0.085,
      species_mu[i,3],
      i + 0.085, 
      col = scales::alpha("gray20", 0.75),
      border = scales::alpha("gray20", 1)
    )
    # And now the city-specific estimates for each species
    #  This is the reason we set the seed at the beginning.
    #  We are jittering the y-axis a TINY bit.
  points(
    species_city[i,],
    y = jitter(rep(i, ncity), 0.4),
    pch = 21, bg = scales::alpha("white", 0.3),
    col = scales::alpha("black", 0.6),
    cex = 1
  )
}
# Add among-city average occupancy for each species
points(
  x = species_mu[,2],
  y = 1:nspecies, 
  cex = 1.5,
  pch=21,
  bg = "#00AADE"
)
# Add the legend, and to do so allow for plotting outside
#  into the plot margin. We've got two different legends
#  here so we can split up the species-specific and community 
#  level stuff.
par(xpd = NA)
legend(
  x = 0.45,
  y = 26.5-y_min,
  legend = c(
    "E(among city)",
    "E(sampled city)",
    "95% among city CI",
    "95% unsampled city PI"
  ),
  lty = c(NA, NA, NA, 1),
  lwd = c(NA, NA, NA, 2),
  pch = c(21, 21, 15, NA),
  pt.cex = c(1.5, 1.3, 1.3, NA),
  col = c(
    "black", "black",
    scales::alpha("gray20", 0.75),
    scales::alpha("gray20", 0.75)
    ),
  pt.bg = c(
    "#00AADE",
    "white", 
    scales::alpha("gray20", 0.75),
    NA
  ),
  cex = 1,
 # box.col = "white",
  bty = "n",
  title = "Species specific",
  title.col = "white"
)
# And the community stuff
legend(
  x = 0.45,
  y = 21.5-y_min,
  legend = c(
    "E(among species)",
    "95% among species CI"
  ),
  lty = c(3, NA),
  lwd = c(2, NA),
  pch = c(NA, 15),
  pt.cex = c(NA, 1.3),
  col = c("gray30", "gray80"),
  cex = 1,
  # box.col = "white",
  bty = "n",
  title = "Community",
  title.col = "white"
)
# Doing my own titles, plus an underline. 
lines(
  x = c(0.58, 0.95),
  y = rep(25.25-y_min,2)
)
text(
  x = 0.77,
  y = 26.5-y_min,
  labels = "Species specific",
  pos = 1,
  cex = 1.1
)
lines(
  x = c(0.58, 0.95),
  y = rep(20.2-y_min,2)
)
text(
  x = 0.71,
  y = 21.4-y_min, 
  labels = "Community",
  pos = 1,
  cex = 1.1
)
}
dev.off()



# Now do the second figure, which is going to be a two-panel


# get the among city averages
income_mu <- msum[
  grep(
    "^b_species\\[2",
    row.names(msum)
  ),
  ]
# give them some names
row.names(income_mu) <- species_map$Species

urb_mu <- msum[
  grep(
    "^b_species\\[3",
    row.names(msum)
  ),
  ]
# give them some names
row.names(urb_mu) <- species_map$Species


# get the species specific standard deviations
#  among cities
tau_income <- msum[
  grep(
    "tau_species\\[2"
    , row.names(msum)
  ),
  ]
sd_income <- sqrt(1 / tau_income)
row.names(sd_income) <- species_map$Species

tau_urb <- msum[
  grep(
    "tau_species\\[3"
    , row.names(msum)
  ),
  ]
sd_urb <- sqrt(1 / tau_urb)
row.names(sd_urb) <- species_map$Species

# a matrix for the 95% predictive interval
income_95pi <- matrix(
  NA,
  ncol = 2,
  nrow = nspecies
)
# fill it in
for(i in 1:nspecies){
  income_95pi[i,] <-  qnorm(
    c(0.025,0.975),
    income_mu[i,2],
    sd_income[i,2]
  )
}

# a matrix for the 95% predictive interval
urb_95pi <- matrix(
  NA,
  ncol = 2,
  nrow = nspecies
)
# fill it in
for(i in 1:nspecies){
  urb_95pi[i,] <-  qnorm(
    c(0.025,0.975),
    urb_mu[i,2],
    sd_urb[i,2]
  )
}

# get the ordering, from lowest to highest for 
#  urbanization
species_order <- order(
  urb_mu[,2],
  decreasing = TRUE
)
# sort the mu and 95% PI
income_mu <- income_mu[species_order,]
income_95pi <- income_95pi[species_order,]

urb_mu <- urb_mu[species_order,]
urb_95pi <- urb_95pi[species_order,]

income_city <- species_city_est[
  grep(
    "b_species_city\\[2",
    row.names(species_city_est)
  ),
  2
  ]

urb_city <- species_city_est[
  grep(
    "b_species_city\\[3",
    row.names(species_city_est)
  ),
  2
  ]

# split species city by species
income_city <- matrix(
  income_city,
  ncol = ncity,
  nrow = nspecies
)
income_city <- income_city[species_order,]

urb_city <- matrix(
  urb_city,
  ncol = ncity,
  nrow = nspecies
)
urb_city <- urb_city[species_order,]


income_city_quantiles <- species_city_est[
  grep(
    "b_species_city\\[2",
    row.names(species_city_est)
  ),
  
  ]

# split species city by species
income_city_quantiles <- array(
  income_city_quantiles,
  dim = c(nspecies, ncity, 3),
  dimnames = list(species_map$Species, city_map$City, c("lo", "med", "hi"))
  
)

hm <- which(income_city_quantiles[,,3] < 0, arr.ind = TRUE)
hm[order(hm[,1]),]

hm <- which(income_city_quantiles[,,1] > 0, arr.ind = TRUE)
hm[order(hm[,1]),]

income_city_quantiles[,,3]

condition <- c("cottontail_sp", "mawi")
round(income_city_quantiles[
  species_map$Species == condition[1],
  c(1,3,6,8,10),
  ],2)

table(row.names(hm))
condition <- c("c", "phaz")
urb_city_quantiles[
  species_map$Species == condition[1],,
  #city_map$City == condition[2],
  ]




urb_city_quantiles <- species_city_est[
  grep(
    "b_species_city\\[3",
    row.names(species_city_est)
  ),
  
  ]

# split species city by species
urb_city_quantiles <- array(
  urb_city_quantiles,
  dim = c(nspecies, ncity, 3),
  dimnames = list(species_map$Species, city_map$City, c("lo", "med", "hi"))
  
)

condition <- c("cougar", "phaz")
urb_city_quantiles[
  species_map$Species == condition[1],,
  #city_map$City == condition[2],
  ]

dim(which(urb_city_quantiles[,,1] > 0, arr.ind = TRUE))
dim(which(urb_city_quantiles[,,3] < 0, arr.ind = TRUE))

tiff(
  "./plots/among_model/figure_5.tiff",
  height = 7,
  width = 8,
  units = "in",
  res = 800,
  compression = "lzw"
)

layout(matrix(c(1,2), ncol = 2, nrow = 1))
par(mar=c(3.5,2,6,2))
par(xpd = NA)

plot(1~1, type = "n", xlim = c(-2,2), ylim = c(0.5,nspecies+0.5),
     bty = "n", xlab = "",
     ylab = "", yaxt = "n",
     xaxt = "n",
     yaxs = "i",xaxs = "i")

set.seed(90210)

rect(xleft = species_average_occ[2,1],
     xright = species_average_occ[2,3],
     ybottom = 0.5,
     ytop = nspecies+0.5,
     col = "gray80",
     border = "gray80")
lines(x = rep(species_average_occ[2,2],2),
      y = c(0.5, nspecies+0.5),
      lwd = 2,
      lty = 3,
      col = "gray30")

axis(1, at= seq(-2,2, 0.5), labels = F, tck = -0.025, lwd = 2)
axis(1, at= seq(-2,2, 0.5/2), labels = F, tck = -0.0125, lwd = 2)

# axis(4, at = seq(0.5, nspecies+0.5),
#      labels = F, tck = 0, lwd = 2)
# axis(4, at = seq(1, nspecies),
#      labels = F, tck = -0.0125, lwd = 2)


# 
# tp <- gsub("_"," ", row.names(income_mu))
# 
# tp <- gsub("north american ", "", tp)

mtext(
  sprintf("%.1f", seq(-2,2, 1)),
  1,
  at = seq(-2,2,1),
  line = 0.75,
  cex = 1.3
)
# 
# mtext(tp,
#       4,
#       0.5, at = seq(1, nspecies),
#       cex = 1.4, las = 1)

mtext("Response to income", 1, 2.2, cex = 1.6)

for(i in 1:nspecies){
  # plot predictive interval first
  lines(
    x = income_95pi[i,],
    y = rep(i,2), 
    col =scales::alpha("gray20", 0.75),
    lwd = 2 
  )
  # followed by a gray box to act as a base for the
  #  95% CI so we don't see the 95% PI underneath it
  rect(
    income_mu[i,1],
    i - 0.085,
    income_mu[i,3],
    i + 0.085, 
    col = "gray80",
    border = "gray80"
  )
  # Then the 95% CI
  rect(
    income_mu[i,1],
    i - 0.085,
    income_mu[i,3],
    i + 0.085, 
    col = scales::alpha("gray20", 0.75),
    border = scales::alpha("gray20", 1)
  )
  # And now the city-specific estimates for each species
  #  This is the reason we set the seed at the beginning.
  #  We are jittering the y-axis a TINY bit.
  points(
    income_city[i,],
    y = jitter(rep(i, ncity), 0.4),
    pch = 21, bg = scales::alpha("white", 0.3),
    col = scales::alpha("black", 0.6),
    cex = 1
  )
}
# Add among-city average occupancy for each species
points(
  x = income_mu[,2],
  y = 1:nspecies, 
  cex = 1.5,
  pch=21,
  bg = "#00AADE"
)
lines(x = c(0,0), y = c(0.5, nspecies+0.5), lty = 2, lwd = 2)

############################################
# Urbanization part of the plot




# 
# tiff(
#   "./plots/among_model/figure_3.tiff",
#   height = 7,
#   width = 6,
#   units = "in",
#   res = 800,
#   compression = "lzw"
# )
#windows(6,7)
par(mar=c(3.5,2,6,2))
par(xpd = NA)

plot(1~1, type = "n", xlim = c(-2,2), ylim = c(0.5,nspecies+0.5),
     bty = "n", xlab = "",
     ylab = "", yaxt = "n",
     xaxt = "n",
     yaxs = "i",xaxs = "i")

set.seed(90210)

rect(xleft = species_average_occ[3,1],
     xright = species_average_occ[3,3],
     ybottom = 0.5,
     ytop = nspecies+0.5,
     col = "gray80",
     border = "gray80")
lines(x = rep(species_average_occ[3,2],2),
      y = c(0.5, nspecies+0.5),
      lwd = 2,
      lty = 3,
      col = "gray30")

axis(1, at= seq(-2,2, 0.5), labels = F, tck = -0.025, lwd = 2)
axis(1, at= seq(-2,2, 0.5/2), labels = F, tck = -0.0125, lwd = 2)

# axis(2, at = seq(0.5, nspecies+0.5),
#      labels = F, tck = 0, lwd = 2)
# axis(2, at = seq(1, nspecies),
#      labels = F, tck = -0.0125, lwd = 2)



tp <- gsub("_"," ", row.names(urb_mu))

tp <- gsub("north american ", "", tp)

tp <- gsub("virginia", "Virginia", tp, ignore.case = FALSE)

tp <- gsub("white ", "white-", tp, ignore.case = FALSE)

mtext(
  sprintf("%.1f", seq(-2,2, 1)),
  1,
  at = seq(-2,2,1),
  line = 0.75,
  cex = 1.3
)
for(i in 1:nspecies){
  if(tp[i] == "white-tailed deer"){
    text(x = -2.7, y = i+0.05, tp[i], adj = 0.5, cex = 1.3 )
  }else{
    text(x = -2.5, y = i+0.05, tp[i], adj = 0.5, cex = 1.3 )
  }
  
}

# mtext(tp,
#       2,
#       0.5, at = seq(1, nspecies),
#       cex = 1.4, las = 1)

mtext("Response to urbanization", 1, 2.2, cex = 1.6)

for(i in 1:nspecies){
  # plot predictive interval first
  lines(
    x = urb_95pi[i,],
    y = rep(i,2), 
    col =scales::alpha("gray20", 0.75),
    lwd = 2 
  )
  # followed by a gray box to act as a base for the
  #  95% CI so we don't see the 95% PI underneath it
  rect(
    urb_mu[i,1],
    i - 0.085,
    urb_mu[i,3],
    i + 0.085, 
    col = "gray80",
    border = "gray80"
  )
  # Then the 95% CI
  rect(
    urb_mu[i,1],
    i - 0.085,
    urb_mu[i,3],
    i + 0.085, 
    col = scales::alpha("gray20", 0.75),
    border = scales::alpha("gray20", 1)
  )
  # And now the city-specific estimates for each species
  #  This is the reason we set the seed at the beginning.
  #  We are jittering the y-axis a TINY bit.
  points(
    urb_city[i,],
    y = jitter(rep(i, ncity), 0.4),
    pch = 21, bg = scales::alpha("white", 0.3),
    col = scales::alpha("black", 0.6),
    cex = 1
  )
}
# Add among-city average occupancy for each species
points(
  x = urb_mu[,2],
  y = 1:nspecies, 
  cex = 1.5,
  pch=21,
  bg = "#00AADE"
)
lines(x = c(0,0), y = c(0.5, nspecies+0.5), lty = 2, lwd = 2)
# Add the legend, and to do so allow for plotting outside
#  into the plot margin. We've got two different legends
#  here so we can split up the species-specific and community 
#  level stuff.
par(xpd = NA)
legend(
  x = -5.4,
  y = 33,
  legend = c(
    "E(among city)",
    "E(sampled city)",
    "95% among city CI",
    "95% unsampled city PI"
  ),
  lty = c(NA, NA, NA, 1),
  lwd = c(NA, NA, NA, 2),
  pch = c(21, 21, 15, NA),
  pt.cex = c(1.5, 1.3, 1.3, NA),
  col = c(
    "black", "black",
    scales::alpha("gray20", 0.75),
    scales::alpha("gray20", 0.75)
  ),
  pt.bg = c(
    "#00AADE",
    "white", 
    scales::alpha("gray20", 0.75),
    NA
  ),
  cex = 1,
  # box.col = "white",
  bty = "n",
  title = "Species specific",
  title.col = "white"
)
# And the community stuff
legend(
  x = -2.5,
  y = 33,
  legend = c(
    "E(among species)",
    "95% among species CI"
  ),
  lty = c(3, NA),
  lwd = c(2, NA),
  pch = c(NA, 15),
  pt.cex = c(NA, 1.3),
  col = c("gray30", "gray80"),
  cex = 1,
  # box.col = "white",
  bty = "n",
  title = "Community",
  title.col = "white"
)
# Doing my own titles, plus an underline. 
lines(
  x = c(-4.8, -3.2),
  y = rep(31.45,2)
)
text(
  x = -4,
  y = 33,
  labels = "Species specific",
  pos = 1,
  cex = 1.1
)
lines(
  x = c(-1.85, -0.75),
  y = rep(31.45,2)
)
text(
  x = -1.325,
  y = 33, 
  labels = "Community",
  pos = 1,
  cex = 1.1
)

dev.off()
