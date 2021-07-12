

# This script assumes you have ran the following scripts, which write
#  data into the "./data/cleaned_data" folder:
#  "./R/cleaning_scripts/data_cleaning_script.R"
#  "./R/cleaning_scripts/covariate_cleaning_script.R"

# load functions used to clean data
functions_to_load <- list.files(
  "./R/functions/",
  full.names = TRUE
)

for(fn in functions_to_load){
  source(fn)
}

# read in the data
city_data <- read.csv(
  "./data/cleaned_data/full_capture_history.csv",
  stringsAsFactors = FALSE
)

# There are A LOT of NA's in this sampling array 
#  prop.table(table(city_data$J > 0))
#  FALSE  TRUE 
#  0.7   0.3

# Because of this we do not want to iterate over the observation's with 
#  no data as it greatly increases the amount of time it takes to fit
#  the model. We'll need to be able to index the species, city, and season.
#  To do this, it's going to be easiest to analyze these data in long-format.

# Additionally, we're going to only analyze a few species that were detected
#  a minimum of 10 times in the dataset. This will remove a number of rare
#  species, 

reduce_species <- table(
  city_data$Species,
  city_data$Y > 0
)
#reduce_species[order(reduce_species[,2]),]

first_species <- reduce_species[which(reduce_species[,2] >= min_dets),]

if(exists("my_species")){
  if(length(my_species)>0){
    first_species <- first_species[
      which(row.names(first_species) %in% my_species),
    ]
  }
}

# get just these species for now
tmp <- city_data[
  which(city_data$Species %in% row.names(first_species)),
  ]


# these data.frames will help us connect the model output back to 
#  the actual species, seasons, or cities they are associated to. These
#  data.frames will also give us the numeric index we will need for
#  the JAGS model.
species_map <- data.frame(
  Species = sort(
    unique(
      tmp$Species
    )
  )
)

species_map$Species_id <- make_index(
  species_map$Species
)

season_map <- data.frame(
  Season = unique(
    tmp$Season
  )
)

season_map$Season_id <- make_index(
  season_map$Season,
  TRUE
)

city_map <- data.frame(
  City = sort(
    unique(
      tmp$City
    )
  )
)

city_map$City_id <- make_index(
  city_map$City
)

# make pretty names for cities (for plotting).
pcity <- data.frame(
  City = city_map$City,
  Pretty = c(
    "Atlanta,\nGeorgia",
    "Austin,\nTexas",
    "Chicago,\nIllinois",
    "Denver,\nColorado",
    "Edmonton,\nAlberta",
    "Fort Collins,\nColorado",
    "Iowa City,\nIowa",
    "Indianapolis,\nIndiana",
    "Jackson,\nMississippi",
    "Manhattan,\nKansas",
    "Madison,\nWisconsin",
    "Metro LA,\nCalifornia",
    "Phoenix,\nArizona",
    "Rochester,\nNew York",
    "Sanford,\nFlorida",
    "Salt Lake\nCity, Utah",
    "Seattle,\nWashington",
    "Saint Louis,\nMissouri",
    "Tacoma,\nWashington",
    "Wilmington,\nDelaware"
  )
)

# now that we have these we can make a temporary city_data and join the maps.
tmp <- dplyr::left_join(
  tmp,
  species_map,
  by = "Species"
) %>% 
  dplyr::left_join(
    .,
    season_map,
    by = "Season"
  ) %>% 
  dplyr::left_join(
    .,
    city_map,
    by = "City"
  )

# now we just need to drop the rows where J == 0 (i.e., no sampling).
tmp <- tmp[tmp$J > 0,]

# We have multiple seasons of sampling and therefore need to account for 
#  this in the model. The issue here though is that each city does not
#  have data for each season. As such, we need to be able to index
#  the correct seasonal parameter in our long format detection array.

season_density <- t(
  table(
    tmp$Season_id,
    tmp$City_id
  )
)

# give row names that are the city names
row.names(season_density) <- city_map$City

# make this binary, 1 means that city has data on a given season.
season_density[season_density>0] <- 1

# this tells us which city and season has data.
season_has_data <- which(
  season_density ==1,
  arr.ind = TRUE
)
# give useful headers to 'season_has_data'
colnames(season_has_data) <- c(
  "city_id",
  "season_id"
)
# order by season then city (i..e, same way as species detection data).
season_has_data <- season_has_data[
  order(
    season_has_data[,"season_id"],
    season_has_data[,"city_id"]
  ),
  ]

# season_has_data is our first step, it's effectively a map that let's us
#  index what cities and seasons have data. We do model the probability
#  of each species in a city, but the seasons and cities that have data
#  vary. As such we are going to make a unique identifier for each
#  species, season, and city. 
#  It's important to note that tmp is sorted by species, season, then city.
#  Cities that do not have data on a given season are removed at this point
#  in the script as well. As such, we need to make a unique identifier
#  for every combination of species, season, and city.
combo <- tmp[
  ,
  c("Species_id", "Season_id", "City_id")
  ]
# remove duplicates, this will tell us how many parameters we are attempting
#  to estimate (for the seasonal stuff).
combo <- combo[
  -which(
    duplicated(combo)
  ),
  ]
# make the unique identifier
combo$Combo_id <- 1:nrow(combo)

# and then join this back to the species detection array
tmp <- left_join(tmp,
                 combo,
                 by = c("Species_id", "Season_id", "City_id")
)

# pull in range_data, which we use to estimate if a species is 
#  within a given city. This is for the top level part of the model
#  which helps determine the species pool of a given city (along with
#  the spatial coordinates of a city).
city_covs <- read.csv(
  "./data/cleaned_data/covariates/dist_city_to_species_edge.csv",
  row.names = 1
)

# reduce down to the species we can model, order it in the 
#  same way as species map (i.e., alphabetically), and then convert to 
#  a matrix.
city_covs <- city_covs[which(row.names(city_covs) %in% species_map$Species),]
city_covs <- city_covs[order(row.names(city_covs)),]
city_covs <- as.matrix(city_covs)


# Calculate average lat and long of each city
city_ll <- tmp[,c("City", "Lat", "Long")]
city_ll <- city_ll[-which(duplicated(city_ll)),]

city_ll <- city_ll %>% 
  dplyr::group_by(City) %>% 
  dplyr::summarise(
    Lat = mean(Lat),
    Long = mean(Long)
  )

# make sure city_ll is ordered by city
city_ll <- city_ll[order(city_ll$City),]

# make it a matrix
city_ll <- as.matrix(city_ll[,c("Lat", "Long")])

# among_covs will be input into JAGS. This is for all of the among-city
#  variables for the top-level of the model. The first level of the array
#  is all 1's for the intercept.
among_covs <- array(
  1,
  dim = c(dim(city_covs),2)
)
# add distance of species to city, and scale by standard deviation.
#  we are not subtracting the mean because we want the 0 to indicate
#  that a city is on the edge of a species range.
among_covs[,,2] <- city_covs / sd(city_covs)
# and then add the latitude 
# among_covs[,,3] <- matrix(
#   scale(city_ll[,"Lat"]),
#   ncol = ncol(city_covs),
#   nrow = nrow(city_covs),
#   byrow = TRUE
# )
# # and longitude
# among_covs[,,4] <- matrix(
#   scale(city_ll[,"Long"]),
#   ncol = ncol(city_covs),
#   nrow = nrow(city_covs),
#   byrow = TRUE
# )

# read in the within-city covariates
within_covs <- read.csv(
  "./data/cleaned_data/covariates/site_covariates.csv",
  stringsAsFactors = FALSE
)

# drop few rows from Jackson, MS (JAMS) that have no NDVI data
#  (because they didn't have data from back that far).
within_covs <- within_covs[complete.cases(within_covs),]



# This is how we prepare the data when there are no among city
#  effects.
if(model == "./JAGS/multi_scale_occupancy_simpler_RE.R"){
  
  # Before we city-mean center these data we are going to do a few things
  #  first. We are going to divide median income by the cost of a one
  #  bedroom apartment throughout each city. Doing so will help
  #  determine how far money goes in each city.

onebr <- read.csv(
  "./data/cleaned_data/covariates/cost_one_bedroom_apartment.csv",
  stringsAsFactors = FALSE
)

# join onebr to within covs
within_covs <- dplyr::inner_join(
  within_covs,
  onebr,
  by = c("City", "Season")
)

#divide Income by Price
within_covs$Income <- within_covs$Income / within_covs$Price

# drop the price column now
within_covs <- within_covs[,-which(colnames(within_covs) == "Price")]

# Second, we are going to log population density because it has a very
#  long right tail.

# log population density
within_covs$Population_density <- log(within_covs$Population_density + 1)

# calculate the mean of each variable across cities
wc_mean <- within_covs %>% 
  dplyr::group_by(City) %>% 
  dplyr::summarise_if(is.numeric, mean)

# put mean value in one site that has no buildings.
within_covs$Building_age[
  which(within_covs$Building_age == 0)] <- mean(within_covs$Building_age[
    within_covs$City == "phaz" & within_covs$Building_age > 0])

# city-mean center the data
within_covs_centered <- within_covs %>% 
  dplyr::group_by(City, Season) %>% 
  dplyr::mutate_if(is.numeric, function(x) x - mean(x)) %>% 
  dplyr::ungroup()



# now each of these covariates need to be scaled to be within a realistic
#  range. To do so we are not centering but will divide by the global standard
#  deviation.
within_covs_centered <- within_covs_centered %>% 
  dplyr::mutate_if(
    is.numeric,
    function(x) scale(x, center = FALSE, scale = TRUE)
  )


# Now do some dimension reduction for variables we simply want to account for

cov_pca <- prcomp(
  within_covs_centered[,
                       c(
                         "Impervious",
                         "Ndvi",
                         "Population_density"
                       )
                       ],
  center = FALSE,
  scale. = FALSE
)

if(include_inxs){
  within_covs_centered <- data.frame(
    within_covs_centered[,c(1:3)],
    within_covs_centered$Income,
    URB = cov_pca$x[,1],
    IncomeUrb = within_covs_centered$Income * cov_pca$x[,1]
  )
  
}else{
  within_covs_centered <- data.frame(
    within_covs_centered[,c(1:3)],
    within_covs_centered$Income,
    URB = cov_pca$x[,1]
  )
}


within_covs_centered %>% 
  group_by(City) %>% 
  summarise(min = min(within_covs_centered.Income),
            max = max(within_covs_centered.Income),
            mean = mean(within_covs_centered.Income))

# now we need to replicate this across our detection matrix.
tmp_covs <- dplyr::inner_join(
  tmp[,c("Site", "Season", "City")],
  within_covs_centered,
  by = c("Site", "Season", "City")
)



# tmp covs is ordered like tmp, so we should be good to go with this
#  for psi, we are going to use all of the covariates
# -3 columns for site, season, and city. +1 column for intercept (i.e., -2)
psi_covs <- matrix(
  1,
  ncol = ncol(tmp_covs) - 2,
  nrow = nrow(tmp_covs)
)
psi_covs[,-1] <- as.matrix(
  tmp_covs[,-which(
    colnames(tmp_covs) %in% c("Site", "Season", "City")
  )]
)

# for detection (rho) we are going to use NDVI and population density
rho_covs <- matrix(
  1,
  ncol = ncol(tmp_covs) - 2,
  nrow = nrow(tmp_covs)
)

rho_covs[,-1] <- as.matrix(
  tmp_covs[,-which(
    colnames(tmp_covs) %in% c("Site", "Season", "City")
  )]
)


# for seasonal variation
#season_covs <- scale(city_ll)
#season_covs <- cbind(
#  1, 
#  season_covs
#)

data_list <- list(
  # detection data
  y = tmp$Y,
  J = tmp$J,
  # ids to fit the model in long format
  species_idx = tmp$Species_id,
  city_idx = tmp$City_id,
  combo_species_idx = combo$Species_id,
  combo_city_idx = combo$City_id,
  combo_idx = tmp$Combo_id,
  # covariates
  among_covs = among_covs,
  psi_covs = psi_covs,
  rho_covs = rho_covs,
  #season_covs = season_covs,
  # number of species, parameters, etc.
  nspecies = max(tmp$Species_id),
  ncity = max(tmp$City_id),
  ncov_among = dim(among_covs)[3],
  ncov_within = ncol(psi_covs),
  nsamples = nrow(tmp),
  nseason_params = nrow(combo),
  #nseason_covs = ncol(season_covs),
  ncov_det = ncol(rho_covs)
)


inits <- function(chain){
  gen_list <- function(chain = chain){
    list( 
      z = rep(1, nrow(tmp)),
      x = matrix(1, ncol = data_list$ncity, nrow = data_list$nspecies),
      a_among = rnorm(data_list$ncov_among),
      tau_among = rgamma(data_list$ncov_among, 1, 1),
      b_among = matrix(rnorm(data_list$nspecies * data_list$ncov_among),
                       ncol = data_list$nspecies,
                       nrow = data_list$ncov_among),
      b_within = rnorm(data_list$ncov_within),
      tau_within = rgamma(data_list$ncov_within, 1, 1),
      b_species = matrix(
        rnorm(data_list$nspecies * data_list$ncov_within),
        ncol = data_list$nspecies,
        nrow = data_list$ncov_within),
      tau_species = matrix(
        rgamma(data_list$nspecies * data_list$ncov_within, 1, 1),
        ncol = data_list$nspecies,
        nrow = data_list$ncov_within),
      tau_shape = runif(data_list$ncov_within, 0.5,2),
      tau_rate = runif(data_list$ncov_within, 0.5,2),
      b_species_city = array(
        rnorm(data_list$ncov_within * data_list$nspecies * data_list$ncity),
        dim = c(data_list$ncov_within, data_list$nspecies, data_list$ncity)
      ),
      c_shape_psi = runif(1),
      c_rate_psi = runif(1),
      c_tau_psi = rgamma(data_list$ncity, 1, 1),
      ssc_psi = rnorm(data_list$nseason_params),
      c_shape_rho = runif(1),
      c_rate_rho = runif(1),
      c_tau_rho = rgamma(data_list$ncity, 1, 1),
      ssc_rho = rnorm(data_list$nseason_params),
      .RNG.name = switch(chain,
                         "1" = "base::Wichmann-Hill",
                         "2" = "base::Marsaglia-Multicarry",
                         "3" = "base::Super-Duper",
                         "4" = "base::Mersenne-Twister",
                         "5" = "base::Wichmann-Hill",
                         "6" = "base::Marsaglia-Multicarry",
                         "7" = "base::Super-Duper",
                         "8" = "base::Mersenne-Twister"),
      .RNG.seed = sample(1:1e+06, 1)
    )
  }
  return(switch(chain,           
                "1" = gen_list(chain),
                "2" = gen_list(chain),
                "3" = gen_list(chain),
                "4" = gen_list(chain),
                "5" = gen_list(chain),
                "6" = gen_list(chain),
                "7" = gen_list(chain),
                "8" = gen_list(chain)
  )
  )
}
}


if(model == "./JAGS/multi_scale_occupancy_among_city.R"){
  # Before we city-mean center these data we are going to do a few things
  #  first. We are going to divide median income by the cost of a one
  #  bedroom apartment throughout each city. Doing so will help
  #  determine how far money goes in each city.
  
   onebr <- read.csv(
     "./data/cleaned_data/covariates/cost_one_bedroom_apartment.csv",
     stringsAsFactors = FALSE
   )
  
   # join onebr to within covs
   within_covs <- dplyr::inner_join(
    within_covs,
     onebr,
     by = c("City", "Season")
   )

  #divide Income by Price
   within_covs$Income <- within_covs$Income / within_covs$Price
  #
  # # drop the price column now
   within_covs <- within_covs[,-which(colnames(within_covs) == "Price")]
  
  # Second, we are going to divide by 1000 (so it's per 1k individuals)
  #  and take the square root
  
  # log population density
  within_covs$Population_density <- sqrt(
    within_covs$Population_density / 1000
  )
  
  # calculate the mean of each variable across cities
  wc_mean <- within_covs %>% 
    dplyr::group_by(City) %>% 
    dplyr::summarise_if(is.numeric, mean)
  
  # put mean value in one site that has no buildings.
  within_covs$Building_age[
    which(within_covs$Building_age == 0)] <- mean(within_covs$Building_age[
      within_covs$City == "phaz" & within_covs$Building_age > 0])
  
  # city-mean center the data
  within_covs_centered <- within_covs %>% 
    dplyr::group_by(City, Season) %>% 
    dplyr::mutate_if(is.numeric, function(x) x - mean(x)) %>% 
    dplyr::ungroup()
  
  # now each of these covariates need to be scaled to be within a realistic
  #  range. To do so we are not centering but will divide by the global standard
  #  deviation.
  within_covs_centered <- within_covs_centered %>% 
    dplyr::mutate_if(
      is.numeric,
      function(x) scale(x, center = FALSE, scale = TRUE)
    )
  
  
  # Now do some dimension reduction for variables we simply want to account for
  cov_pca <- prcomp(
    within_covs_centered[,
                         c(
                           "Impervious",
                           "Ndvi",
                           "Population_density"
                         )
                         ],
    center = FALSE,
    scale. = FALSE
  )

  
  # make site in season
  within_covs_centered <- data.frame(
    within_covs_centered[,c(1:3)],
    Income = within_covs_centered$Income,
    URB = cov_pca$x[,1]
  )
  
  
 # within_covs_centered %>% 
##    group_by(City) %>% 
  #  summarise(min = min(within_covs_centered.Income),
   #           max = max(within_covs_centered.Income),
    #          mean = mean(within_covs_centered.Income))
  
  # now we need to replicate this across our detection matrix.
  tmp_covs <- dplyr::inner_join(
    tmp[,c("Site", "Season", "City")],
    within_covs_centered,
    by = c("Site", "Season", "City")
  )
  
  
  
  # tmp covs is ordered like tmp, so we should be good to go with this
  #  for psi, we are going to use all of the covariates
  # -3 columns for site, season, and city. +1 column for intercept (i.e., -2)
  psi_covs <- matrix(
    1,
    ncol = ncol(tmp_covs) - 2,
    nrow = nrow(tmp_covs)
  )
  psi_covs[,-1] <- as.matrix(
    tmp_covs[,-which(
      colnames(tmp_covs) %in% c("Site", "Season", "City")
    )]
  )
  
  precip <- read.csv("./data/cleaned_data/covariates/precip.csv")
  precip
  psi_covs_among <- cbind(
    1, precip$Precip
  )
  psi_covs_among[,2] <- scale(psi_covs_among[,2])
  #psi_covs_among[,3] <- scale(psi_covs_among[,3])
  
  # for detection (rho) we are going to use NDVI and population density
  rho_covs <- matrix(
    1,
    ncol = ncol(tmp_covs) - 2,
    nrow = nrow(tmp_covs)
  )
  
  rho_covs[,-1] <- as.matrix(
    tmp_covs[,-which(
      colnames(tmp_covs) %in% c("Site", "Season", "City")
    )]
  )
  
  
  # for seasonal variation
  #season_covs <- scale(city_ll)
  #season_covs <- cbind(
  #  1, 
  #  season_covs
  #)
  
  data_list <- list(
    # detection data
    y = tmp$Y,
    J = tmp$J,
    # ids to fit the model in long format
    species_idx = tmp$Species_id,
    city_idx = tmp$City_id,
    combo_species_idx = combo$Species_id,
    combo_city_idx = combo$City_id,
    combo_idx = tmp$Combo_id,
    # covariates
    among_covs = among_covs,
    psi_covs = psi_covs,
    psi_covs_among = psi_covs_among,
    rho_covs = rho_covs,
    #season_covs = season_covs,
    # number of species, parameters, etc.
    nspecies = max(tmp$Species_id),
    ncity = max(tmp$City_id),
    ncov_among = dim(among_covs)[3],
    ncov_among_psi = 3,
    ncov_within = ncol(psi_covs),
    nsamples = nrow(tmp),
    nseason_params = nrow(combo),
    #nseason_covs = ncol(season_covs),
    ncov_det = ncol(rho_covs)
  )
  
  # modify this to for new model
  inits <- function(chain){
    gen_list <- function(chain = chain){
      list( 
        z = rep(1, nrow(tmp)),
        x = matrix(1, ncol = data_list$ncity, nrow = data_list$nspecies),
        a_among = rnorm(data_list$ncov_among),
        tau_among = rgamma(data_list$ncov_among, 1, 1),
        b_among = matrix(rnorm(data_list$nspecies * data_list$ncov_among),
                         ncol = data_list$nspecies,
                         nrow = data_list$ncov_among),
        b_among_psi = rnorm(data_list$ncov_among_psi),
        tau_among_psi = rgamma(data_list$ncov_among_psi, 1, 1),
        b_species_among = matrix(
          rnorm(
            data_list$ncov_among_psi * data_list$nspecies
          ),
          nrow = data_list$ncov_among_psi,
          ncol = data_list$nspecies
        ),
        b_within = rnorm(data_list$ncov_within),
        tau_within = rgamma(data_list$ncov_within, 1, 1),
        b_species = matrix(
          rnorm(data_list$nspecies * data_list$ncov_within),
          ncol = data_list$nspecies,
          nrow = data_list$ncov_within),
        tau_species = matrix(
          rgamma(data_list$nspecies * data_list$ncov_within, 1, 1),
          ncol = data_list$nspecies,
          nrow = data_list$ncov_within),
        tau_shape = runif(data_list$ncov_within, 0.5,2),
        tau_rate = runif(data_list$ncov_within, 0.5,2),
        b_species_city = array(
          rnorm(data_list$ncov_within * data_list$nspecies * data_list$ncity),
          dim = c(data_list$ncov_within, data_list$nspecies, data_list$ncity)
        ),
        c_shape_psi = runif(1),
        c_rate_psi = runif(1),
        c_tau_psi = rgamma(data_list$ncity, 1, 1),
        ssc_psi = rnorm(data_list$nseason_params),
        c_shape_rho = runif(1),
        c_rate_rho = runif(1),
        c_tau_rho = rgamma(data_list$ncity, 1, 1),
        ssc_rho = rnorm(data_list$nseason_params),
        .RNG.name = switch(chain,
                           "1" = "base::Wichmann-Hill",
                           "2" = "base::Marsaglia-Multicarry",
                           "3" = "base::Super-Duper",
                           "4" = "base::Mersenne-Twister",
                           "5" = "base::Wichmann-Hill",
                           "6" = "base::Marsaglia-Multicarry",
                           "7" = "base::Super-Duper",
                           "8" = "base::Mersenne-Twister"),
        .RNG.seed = sample(1:1e+06, 1)
      )
    }
    return(switch(chain,           
                  "1" = gen_list(chain),
                  "2" = gen_list(chain),
                  "3" = gen_list(chain),
                  "4" = gen_list(chain),
                  "5" = gen_list(chain),
                  "6" = gen_list(chain),
                  "7" = gen_list(chain),
                  "8" = gen_list(chain)
    )
    )
  }
}
