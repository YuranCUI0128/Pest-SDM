library(biomod2)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(raster)
library(rasterVis)
library(dismo)
library(dplyr)
library(geodata)
library(tidyterra)
library(doParallel)
library(sp)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(geosphere)

library(png)
library(countrycode)
library(CoordinateCleaner)
library(RColorBrewer)
library(spThin)
library(terra)

setwd('E:/Pest_SDM/')
# load our species raster
# we consider only the presences of Species
A_gamma_GBIF <- read.delim('Autographa_gamma.csv')

################species data clean##################
#convert country code from ISO2c to ISO3c
A_gamma_GBIF$countryCode <-  countrycode(A_gamma_GBIF$countryCode, origin =  'iso2c', destination = 'iso3c')
#flag problems
flags <- clean_coordinates(x = A_gamma_GBIF, 
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("equal","gbif", "institutions", "countries", "seas")) 

summary(flags)
#Exclude problematic records
A_gamma_GBIF_cl <- A_gamma_GBIF[flags$.summary,]
#The flagged records
A_gamma_GBIF_fl <- A_gamma_GBIF[!flags$.summary,]
#Remove records with low coordinate precision
A_gamma_occ  <- A_gamma_GBIF_cl %>%
  filter(coordinateUncertaintyInMeters / 1000 <= 10 | is.na(coordinateUncertaintyInMeters))
#Filtering out rows with missing values in 'Longitude','Latitude',and 'Month'
A_gamma_occ <- A_gamma_occ[!is.na(A_gamma_occ$decimalLongitude), ]
A_gamma_occ <- A_gamma_occ[!is.na(A_gamma_occ$decimalLatitude), ]
A_gamma_occ <- A_gamma_occ[!is.na(A_gamma_occ$month), ]
write.csv(A_gamma_occ, file = "A_gamma.csv", row.names = FALSE)
A_gamma_occ <- read.csv("A_gamma.csv")

#Count the number of record in each months
num_occ <- data.frame(table(A_gamma_occ$month))
num_occ <- data.frame(month = month.abb, count = num_occ$Freq)
png("model_e.png",units="in", width=7, height=5,res = 300)
barplot(unlist(num_occ$count), names.arg = num_occ$month, xlab = "Month", ylab = "Count", main = "Number of A.gamma record in each Month", col="lightBlue")
dev.off()

#set a variable of all the studied time periods 12 months and year-long
periods <- c(month.abb, "year")

# Loop through months 1 to 12
for (month in 1:12) {
  # Subset the data for the current month
  A_gamma_month <- A_gamma_occ[A_gamma_occ$month == month, c("decimalLatitude", "decimalLongitude")]
  colnames(A_gamma_month) <- c("lat", "long")  
  # Select and rename columns, and add state column, 1 represent present
  A_gamma_month$state <- 1
  # Create a variable dynamically for each data set
  assign(paste0("A_gamma", substr(month.name[month], 1, 3)), A_gamma_month)
}

################spatial rarefy##########
#set folders to store outcomes of SDM built with and without land management data
dir.create("E:/Pest_SDM/A_gamma_newmanage")
dir.create("E:/Pest_SDM/A_gamma_nomanageeva")

for (month in month.abb){
  thin(loc.data = get(paste0("A_gamma", month)),
       lat.col =  "lat",
       long.col = "long",
       spec.col = "state",
       thin.par = 1,
       reps = 1,
       write.files = TRUE,
       out.dir = "A_gamma_newmanage",
       out.base = paste0("A_gamma", month)
  )
}
for (month in month.abb){
  assign(paste0("A_gamma", month), read.csv(paste0("A_gamma_newmanage/A_gamma", month, "_thin1.csv")))
}

A_gammayear <- data.frame()
for (month in month.abb) {
  A_gammayear <- rbind(A_gammayear, get(paste0("A_gamma", month)))
}
thin(loc.data = A_gammayear,
     lat.col =  "lat",
     long.col = "long",
     spec.col = "state",
     thin.par = 10,
     reps = 1,
     write.files = TRUE,
     out.dir = "E:/R-4.2.1-oldversion/grid_10/A_gamma_newmanage",
     out.base = "A_gammayear"
)
A_gammayear <- read.csv("A_gammayear_thin1.csv")

#set the studied geographical extent
model_extent_Europe <- extent(c(-25,50,35,70))
###########get prepared environmental data#######################################################
#get elevation raster
elv <- stack("elv.tif")
names(elv)<-"elv"

##Current land use and land management data
#get land management intervention raster
management <- stack("landmanagement.tif")
names(management) <- c("Nfer", "Kfer", "Pfer", "Pesti", "Irri")
#get land use raster
landuse <- stack ("landuse.tif")
names(landuse) <- c("primf", "pastr", "range", "urban", "C3ann", "c3per", "c4ann", "c4per")
# Resampling management and land use data based on elevation
management1 <-resample(management, elv)
landuse1 <-resample(landuse, elv)

##land use and land management data under SSP2
FlanduseSSP2 <- stack("FlanduseSSP2.tif")
names(FlanduseSSP2) <- c("primf", "pastr", "range", "urban", "C3ann", "c3per", "c4ann", "c4per")
FmanagementSSP2 <- stack("landmanagementSSP2.tif")
names(FmanagementSSP2) <- c("Nfer", "Kfer", "Pfer", "Pesti", "Irri")
managementSSP2 <-resample(FmanagementSSP2, elv)
landuseSSP2 <-resample(FlanduseSSP2, elv)
##land use and land management data under SSP5
FlanduseSSP5 <- stack("FlanduseSSP5.tif")
names(FlanduseSSP2) <- c("primf", "pastr", "range", "urban", "C3ann", "c3per", "c4ann", "c4per")
FmanagementSSP5 <- stack("landmanagementSSP5.tif")
names(FmanagementSSP5) <- c("Nfer", "Kfer", "Pfer", "Pesti", "Irri")
managementSSP5 <-resample(FmanagementSSP5, elv)
landuseSSP5 <-resample(FlanduseSSP5, elv)

#get Current climate data and get the all the current environmental data ready
for (month in periods) {
  # read climate data
  climate_month <- stack(paste0("CurrentCli/Current_", month, ".tif"))
  # name each layer
  names(climate_month) <- c("tmean", "tmean_dif", "pre", "pre_dif")
  climate_month <- resample(climate_month, elv)
  #stack the current monthly environmental data with land management data
  stack_month <- stack(management1, landuse1, elv, climate_month)
  names(stack_month) <- c("Nfer", "Kfer", "Pfer", "Pesti", "Irri", 
                          "primf", "pastr", "range", "urban", "c3ann", "c3per", "c4ann", "c4per", 
                          "elv", "tmean", "tmean_dif", "pre", "pre_dif")
  assign(paste0("stack", month), stack_month)
  #stack the current monthly environmental data without land management data
  stack_monthnom <- stack(landuse1, elv, climate_month)
  names(stack_monthnom) <- c("primf", "pastr", "range", "urban", "c3ann", "c3per", "c4ann", "c4per", 
                             "elv", "tmean", "tmean_dif", "pre", "pre_dif")
  assign(paste0("stack", month, "nom"), stack_monthnom)
}

#randomly set pseudo-absent data(background data)in all studied periods
for (month in periods) {
  src_crs <- crs(elv)
  assign(paste0("A_gamma", month), st_as_sf(get(paste0("A_gamma", month)), coords=c('long', 'lat'), crs=src_crs))
  # Create a simple land mask
  land <- management1[[1]] >= 0
  # How many points to create? We'll use the same as number of observations
  n_pseudo <- nrow(get(paste0("A_gamma", month)))
  # Sample the points
  pseudo_dismo <- randomPoints(mask=as(land, 'Raster'), n=n_pseudo, p=st_coordinates(get(paste0("A_gamma", month))))
  # Convert this data into an sf object, for consistency with the next example.
  pseudo_dismo <- st_as_sf(data.frame(pseudo_dismo), coords=c('x','y'), crs=src_crs)
  present <- get(paste0("A_gamma", month))
  present$state <- 1
  absent <- pseudo_dismo
  absent$state <- 0
  #rename the geometry column of absent to match so we can stack them together.
  names(absent) <- c('geometry','state')
  st_geometry(absent) <- 'geometry'
  #stack the two data frames
  assign(paste0("A_gamma", month), rbind(present, absent))
  #extract the environmental values for each of those points and add it into the data frame
  envt_data <- terra::extract(get(paste0("stack", month)), get(paste0("A_gamma", month)), fun = mean, na.rm = TRUE)
  assign(paste0("A_gamma", month), cbind(get(paste0("A_gamma", month)), envt_data))
  coordinates <- st_coordinates(get(paste0("A_gamma", month)))
  assign(paste0("A_gamma", month), data.frame(cbind(get(paste0("A_gamma", month)), coordinates[, c("X", "Y")])))
}

#get Future climate data and get the all the future environmental data ready
for (month in month.abb) {
  # read SSP2 climate data
  SSP2climate_month <- stack(paste0("SSP245Cli/SSP245_", month, ".tif"))
  # name each layer
  names(SSP2climate_month) <- c("tmean", "tmean_dif", "pre", "pre_dif")
  SSP2climate_month <- resample(SSP2climate_month, elv)
  #stack the SSP2 monthly environmental data with land management data
  FSSP2_month <- stack(managementSSP2, landuseSSP2, elv, SSP2climate_month)
  names(FSSP2_month) <- c("Nfer", "Kfer", "Pfer", "Pesti", "Irri", 
                          "primf", "pastr", "range", "urban", "c3ann", "c3per", "c4ann", "c4per", 
                          "elv", "tmean", "tmean_dif","pre", "pre_dif")
  assign(paste0("FSSP2_", month), FSSP2_month)
  #stack the SSP2 monthly environmental data without land management data
  FSSP2_monthnom <- stack(landuse1, elv, SSP2climate_month)
  names(FSSP2_monthnom) <- c("primf", "pastr", "range", "urban", "c3ann", "c3per", "c4ann", "c4per", 
                             "elv", "tmean", "tmean_dif", "pre", "pre_dif")
  assign(paste0("FSSP2_", month, "nom"), FSSP2_monthnom)
  
  # read SSP5 climate data
  SSP5climate_month <- stack(paste0("SSP585Cli/SSP585_", month, ".tif"))
  #name each layer
  names(SSP5climate_month) <- c("tmean", "tmean_dif","pre", "pre_dif")
  SSP5climate_month <- resample(SSP5climate_month, elv)
  #stack the SSP5 monthly environmental data with land management data
  FSSP5_month <- stack(managementSSP5, landuseSSP5, elv, SSP5climate_month)
  names(FSSP5_month) <- c("Nfer", "Kfer", "Pfer", "Pesti", "Irri", 
                          "primf", "pastr", "range", "urban", "c3ann", "c3per", "c4ann", "c4per", 
                          "elv", "tmean", "tmean_dif", "pre", "pre_dif")
  assign(paste0("FSSP5_", month), FSSP5_month)
  #stack the SSP5 monthly environmental data without land management data
  FSSP5_monthnom <- stack(landuse1, elv, SSP5climate_month)
  names(FSSP5_monthnom) <- c("primf", "pastr", "range", "urban", "c3ann", "c3per", "c4ann", "c4per", 
                             "elv", "tmean", "tmean_dif", "pre", "pre_dif")
  assign(paste0("FSSP5_", month, "nom"), FSSP5_monthnom)
}  
############build SDMs with land management data & get variable contributions and evaluations##################################################
#set files to store model evaluation data and environmental variable importance data
dir.create('A_gamma_newmanage/modelsscore')
dir.create('A_gamma_newmanage/var_importance')

# Loop through months and year-long period data to built SDMs
for (month in periods) { 
  # Format the data
  assign(paste0("A_gamma_", month), BIOMOD_FormatingData(
    resp.var = get(paste0("A_gamma", month))['state'],
    resp.xy = get(paste0("A_gamma", month))[, c('X', 'Y')],
    expl.var = get(paste0("A_gamma", month))[, c("Nfer", "Kfer", "Pfer", "Pesti", "Irri", 
                                                      "primf", "pastr", "range", "urban", "c3ann", "c3per", "c4ann", "c4per", 
                                                      "elv", "tmean", "tmean_dif", "pre", "pre_dif")],
    resp.name = paste0("A_gamma in ", month)
  ))
  # Perform cross-validation
  assign(paste0(month, "A_gamma_CV"), BIOMOD_CrossValidation(
    bm.format = get(paste0("A_gamma_", month)), 
    k=5, 
    nb.rep =1, 
    do.stratification = FALSE, 
    balance = c("presences", "absences")
  ))
  # build the SDM using 7 method
  assign(paste0("A_gamma_models_", month), BIOMOD_Modeling(
    bm.format = get(paste0("A_gamma_", month)),
    models = c("GLM", "GBM", "CTA", "ANN", "MARS", "RF", "MAXNET"),
    data.split.table = get(paste0(month, "A_gamma_CV")),
    var.import = 1,
    modeling.id = paste0("demo", month),
    metric.eval = "TSS"
  ))
  # Calculate evaluation scores (TSS)
  assign(paste0("A_gamma_models_", month, "_scores"), get_evaluations(get(paste0("A_gamma_models_", month))))
  write.csv(get(paste0("A_gamma_models_", month, "_scores")), file = paste0("A_gamma_newmanage/modelsscore/A_gamma_modelsscore_", month, ".csv"), row.names = FALSE)
  # record variable contribution
  assign(paste0("A_gamma_models_", month, "_var_import"), get_variables_importance(get(paste0("A_gamma_models_", month))))
  var_imp_sum <- get(paste0("A_gamma_models_", month, "_var_import")) %>%
    group_by(full.name, rand) %>%
    summarise(total_var_imp = sum(var.imp))
  var_contribution <- get(paste0("A_gamma_models_", month, "_var_import")) %>%
    left_join(var_imp_sum, by = c("full.name", "rand")) %>%
    mutate(var_contribution = var.imp / total_var_imp)
  assign(paste0("var_con_",month), var_contribution %>%
           group_by(expl.var) %>%
           summarize(se = sd(var_contribution) / sqrt(length(var_contribution)),
                     var_contribution = mean(var_contribution)))
  write.csv(get(paste0("var_con_",month)), file = paste0("A_gamma_newmanage/var_importance/A_gamma_varcon_", month, ".csv"), row.names = FALSE)
}

#################get distribution probability map and how existence probability of the species response to environmental variable change when land management interventions were considered################################################################
# Create an empty data frame to store how existence probability of the species response to environmental variable change
resvalue<-data.frame()
# Loop through each month
for (month in month.abb) {
  #define names of models and the variables that would be used in the loop
  ensemble_model <- paste0("A_gamma_ensemble_models_", month)
  models <- paste0("A_gamma_models_", month)
  proj_current <- paste0("A_gamma_ensemble_models_", month, "_proj_current")
  proj_ssp2 <- paste0("A_gamma_ensemble_models_", month, "_proj_SSP2")
  proj_ssp5 <- paste0("A_gamma_ensemble_models_", month, "_proj_SSP5")
  
  # Create ensemble model
  assign(ensemble_model, BIOMOD_EnsembleModeling(
    bm.mod = get(models),
    em.by = 'all',
    em.algo = 'EMmean',
    metric.eval = 'TSS'
  ))
  # Generate and store response curve results
  assign(paste0("res", month), bm_PlotResponseCurves(
    bm.out = get(ensemble_model),
    fixed.var = "mean"))
  assign(paste0("res_", month), get(paste0("res", month))[[1]])
  assign(paste0("res_", month), data.frame(expl.name = get(paste0("res_", month))$expl.name,
                                           expl.val = get(paste0("res_", month))$expl.val,
                                           pred.val = get(paste0("res_", month))$pred.val,
                                           month = rep(month, nrow(get(paste0("res_", month))))))
  write.csv(get(paste0("res_", month)), file = paste0("A_gamma_newmanage/A_gamma_rescurve_", month, ".csv"), row.names = FALSE)
  resvalue<-rbind(resvalue, get(paste0("res_", month)))
  
  # Generate forecasts for current scenario, SSP2, and SSP5
  assign(proj_current, BIOMOD_EnsembleForecasting(
    bm.em = get(ensemble_model),
    proj.name = paste0("CurrentEM_", month),
    new.env = get(paste0("stack", month)),
    models.chosen = "all"
  ))
  assign(proj_ssp2, BIOMOD_EnsembleForecasting(
    bm.em = get(ensemble_model),
    proj.name = paste0("SSP2EM_", month),
    new.env = get(paste0("FSSP2_", month)),
    models.chosen = 'all'
  ))
  assign(proj_ssp5, BIOMOD_EnsembleForecasting(
    bm.em = get(ensemble_model),
    proj.name = paste0("SSP5EM_", month),
    new.env = get(paste0("FSSP5_", month)),
    models.chosen = 'all'
  ))
  # Get distribution probability maps predicted by SDM incorporated with land management interventions and save as raster files
  CurrentPrediction <- get_predictions(get(proj_current))
  Currentname <- paste0("A_gamma_newmanage/Current", month, ".tif")
  writeRaster(CurrentPrediction, filename = Currentname)
  
  SSP2Prediction <- get_predictions(get(proj_ssp2))
  SSP2name <- paste0("A_gamma_newmanage/SSP2", month, ".tif")
  writeRaster(SSP2Prediction, filename = SSP2name)
  
  SSP5Prediction <- get_predictions(get(proj_ssp5)) 
  SSP5name <- paste0("A_gamma_newmanage/SSP5", month, ".tif")
  writeRaster(SSP5Prediction, filename = SSP5name)
}

# record response curve results of all months
write.csv(resvalue, "A_gamma_newmanage/resvalue.csv", row.names = FALSE)

###################################################################################################################
########build SDMs without land management data & get variable contributions and evaluations#######################
#set files to store model evaluation data and environmental variable importance data
dir.create('A_gamma_nomanageeva/modelsscore')
dir.create('A_gamma_nomanageeva/var_importance')

# Loop through months
for (month in periods) { 
  # Format the data
  assign(paste0("A_gammanom_", month), BIOMOD_FormatingData(
    resp.var = get(paste0("A_gamma", month))['state'],
    resp.xy = get(paste0("A_gamma", month))[, c('X', 'Y')],
    expl.var = get(paste0("A_gamma", month))[, c("primf", "pastr", "range", "urban", "c3ann", "c3per", "c4ann", "c4per", 
                                                      "elv", "tmean", "tmean_dif", "pre", "pre_dif")],
    resp.name = paste0("A_gamma in ", month)
  ))
  # Perform cross-validation
  assign(paste0(month, "A_gammanom_CV"), BIOMOD_CrossValidation(
    bm.format = get(paste0("A_gammanom_", month)), 
    k=5, 
    nb.rep =1, 
    do.stratification = FALSE, 
    balance = c("presences", "absences")
  ))
  # build the model using 7 method
  assign(paste0("A_gamma_modelsnom_", month), BIOMOD_Modeling(
    bm.format = get(paste0("A_gammanom_", month)),
    models = c("GLM", "GBM", "CTA", "ANN", "MARS", "RF", "MAXNET"),
    data.split.table = get(paste0(month, "A_gammanom_CV")),
    modeling.id = paste0("demo", month),
    metric.eval = "TSS"
  ))
  # model evaluation
  assign(paste0("A_gamma_modelsnom_", month, "_scores"), get_evaluations(get(paste0("A_gamma_modelsnom_", month))))
  write.csv(get(paste0("A_gamma_modelsnom_", month, "_scores")), file = paste0("A_gamma_nomanageeva/A_gamma_modelsscore_", month, ".csv"), row.names = FALSE)
}

#####get distribution probability map when land management interventions were not considered############################################
# Loop through each month
for (month in month.abb) {
  #define names of models and the variables that would be used in the loop
  ensemble_model <- paste0("A_gamma_ensemble_models_nomanage", month)
  models <- paste0("A_gamma_modelsnom_", month)
  proj_current <- paste0("A_gamma_ensemble_models_nomanage", month, "_proj_current")
  proj_ssp2 <- paste0("A_gamma_ensemble_models_nomanage", month, "_proj_SSP2")
  proj_ssp5 <- paste0("A_gamma_ensemble_models_nomanage", month, "_proj_SSP5")
  
  #Create ensemble model
  assign(ensemble_model, BIOMOD_EnsembleModeling(
    bm.mod = get(models),
    em.by = 'all',
    em.algo = 'EMmean',
    metric.eval = 'TSS'
  ))
  # Generate forecasts for current scenario, SSP2, and SSP5
  assign(proj_current, BIOMOD_EnsembleForecasting(
    bm.em = get(ensemble_model),
    proj.name = paste0("CurrentEM_", month),
    new.env = get(paste0("stack", month, "nom")),
    models.chosen = "all"
  ))
  assign(proj_ssp2, BIOMOD_EnsembleForecasting(
    bm.em = get(ensemble_model),
    proj.name = paste0("SSP2EM_", month),
    new.env = get(paste0("FSSP2_", month, "nom")),
    models.chosen = 'all'
  ))
  assign(proj_ssp5, BIOMOD_EnsembleForecasting(
    bm.em = get(ensemble_model),
    proj.name = paste0("SSP5EM_", month),
    new.env = get(paste0("FSSP5_", month, "nom")),
    models.chosen = 'all'
  ))
  # Get distribution probability maps predicted by SDM incorporated with land management interventions and save as raster files
  CurrentPrediction <- get_predictions(get(proj_current))
  Currentname <- paste0("A_gamma_nomanageeva/Current", month, ".tif")
  writeRaster(CurrentPrediction, filename = Currentname)
  
  SSP2Prediction <- get_predictions(get(proj_ssp2))
  SSP2name <- paste0("A_gamma_nomanageeva/SSP2", month, ".tif")
  writeRaster(SSP2Prediction, filename = SSP2name)
  
  SSP5Prediction <- get_predictions(get(proj_ssp5)) 
  SSP5name <- paste0("A_gamma_nomanageeva/SSP5", month, ".tif")
  writeRaster(SSP5Prediction, filename = SSP5name)
}

####show distribution probability maps####################################################################################################
folder_path <- "A_gamma_newmanage/"
file_names <- paste0("Current", month.abb)
# build an Current Raster Stack
currentDistribution <- stack()
# read distribution map files in months and add to raster stack
for (file_name in file_names) {
  file_path <- file.path(folder_path, paste0(file_name, ".tif"))
  raster <- raster::raster(file_path)
  currentDistribution <- stack(currentDistribution, raster)
}

file_names <- paste0("SSP2", month.abb)
# build an SSP2 Raster Stack
SSP2Distribution <- stack()
# add layers to Raster Stack
for (file_name in file_names) {
  file_path <- file.path(folder_path, paste0(file_name, ".tif"))
  raster <- raster::raster(file_path)
  SSP2Distribution <- stack(SSP2Distribution, raster)
}

file_names <- paste0("SSP5", month.abb)
# build SSP5 Raster Stack
SSP5Distribution_nom <- stack()
# add layers to Raster Stack
for (file_name in file_names) {
  file_path <- file.path(folder_path, paste0(file_name, ".tif"))
  raster <- raster::raster(file_path)
  SSP5Distribution_nom <- stack(SSP5Distribution_nom, raster)
}

file_names <- c("CurrentJan", "SSP2Jan", "SSP5Jan", "CurrentJul", "SSP2Jul", "SSP5Jul", "CurrentSep", "SSP2Sep", "SSP5Sep")
# build representative Raster Stack
DistributionC <- stack()
# add layers to Raster Stack
for (file_name in file_names) {
  file_path <- file.path(folder_path, paste0(file_name, ".tif"))
  raster <- raster::raster(file_path)
  DistributionC <- stack(DistributionC, raster)
}

#Draw Current distribution map
names(currentDistribution) <- month.abb
dfMonth <- as.data.frame(currentDistribution, xy = TRUE)
df <- dfMonth %>%
  pivot_longer(cols = -c(x, y), names_to = "month", values_to = "value") %>%
  mutate(month = factor(month, levels = month.abb))
ggplot(df, aes(x = x, y = y, fill = value/1000)) +
  geom_raster() +
  facet_wrap(~ month, ncol = 4) +
  scale_fill_gradient(low="#E0D599", high = "darkblue",name = "Probability (%)", limits = c(0, 1)) +
  labs(title = "Current distribution probability of A.gamma in Europe", x="Longitude", y = "Latitude")

#Draw SSP2 distribution map
names(SSP2Distribution) <- month.abb
dfMonth <- as.data.frame(SSP2Distribution, xy = TRUE)
df <- dfMonth %>%
  pivot_longer(cols = -c(x, y), names_to = "month", values_to = "value") %>%
  mutate(month = factor(month, levels = month.abb))
ggplot(df, aes(x = x, y = y, fill = value/1000)) +
  geom_raster() +
  facet_wrap(~ month, ncol = 4) +
  scale_fill_gradient(low="#E0D599", high = "darkblue",name = "Probability (%)", limits = c(0, 1000)) +
  labs(title = "2040-2060 distribution probability of A_gamma in Europe under SSP2", x="Longitude", y = "Latitude")

#Draw SSP5 distribution map
names(SSP5Distribution) <- month.abb
dfMonth <- as.data.frame(SSP5Distribution, xy = TRUE)
df <- dfMonth %>%
  pivot_longer(cols = -c(x, y), names_to = "month", values_to = "value") %>%
  mutate(month = factor(month, levels = month.abb))
ggplot(df, aes(x = x, y = y, fill = value/1000)) +
  geom_raster() +
  facet_wrap(~ month, ncol = 4) +
  scale_fill_gradient(low="#E0D599", high = "darkblue",name = "Probability(%)", limits = c(0, 1000)) +
  labs(title = "2040-2060 distribution probability of A_gamma in Europe under SSP5", x="Longitude", y = "Latitude")

#draw representative Raster Stack in 3 representative months under 3 Situations 
fn <- c("Current distribution in Jan", "Distribution in Jan under SSP2", "Distribution in Jan under SSP5",
        "Current distribution in Jul", "Distribution in Jul under SSP2", "Distribution in Jul under SSP5",
        "Current distribution in Sep", "Distribution in Sep under SSP2", "Distribution in Sep under SSP5")
names(DistributionC) <- file_names
dfMonth <- as.data.frame(DistributionC, xy = TRUE)
df <- dfMonth %>%
  pivot_longer(cols = -c(x, y), names_to = "situation", values_to = "value")
df$situation <- gsub("CurrentJan", "Current distribution in Jan", df$situation)
df$situation <- gsub("SSP2Jan", "Distribution in Jan under SSP2", df$situation)
df$situation <- gsub("SSP5Jan", "Distribution in Jan under SSP5", df$situation)
df$situation <- gsub("CurrentJul", "Current distribution in Jul", df$situation)
df$situation <- gsub("SSP2Jul", "Distribution in Jul under SSP2", df$situation)
df$situation <- gsub("SSP5Jul", "Distribution in Jul under SSP5", df$situation)
df$situation <- gsub("CurrentSep", "Current distribution in Sep", df$situation)
df$situation <- gsub("SSP2Sep", "Distribution in Sep under SSP2", df$situation)
df$situation <- gsub("SSP5Sep", "Distribution in Sep under SSP5", df$situation)
df <- df %>% mutate(situation = factor(situation, levels = fn))
ggplot(df, aes(x = x, y = y, fill = value/1000)) +
  geom_raster() +
  facet_wrap(~ situation, ncol = 3) +
  scale_fill_gradient(low="#E0D599", high = "darkblue",name = "Probability (%)", limits = c(0, 1)) +
  labs(title = "A.gamma Distribution probability  under different senarios in Jan, Jul, and Sep", x="Longitude", y = "Latitude")

###drawing response curves and record turning point in response curves#####################################################################
# read data of response curve estimated by ensemble SDMs
resvalue <- read.csv("A_gamma_newmanage/resvalue.csv")

#get response data to Nfer
resvalueNfer <- resvalue[resvalue$expl.name == "Nfer", ]
A_gammastableNfer <- data.frame()
#extract turning points between increase and leveling off with fluctuations of less than 5% with Nfer dose increasing in each month  
for(month in month.abb){
  resNfermonth <- resvalueNfer[resvalueNfer$month == month ,]
  #predicted existence probability leveling off with fluctuations of less than 0.05
  stableNferp <- max(resNfermonth$pred.val)-0.05
  resNfermonth <- resNfermonth[resNfermonth$expl.val>100 & resNfermonth$expl.val<200, ]
  #the dose of Nfer a the turning point
  stableNfer <- resNfermonth[which.min(abs(resNfermonth$pred.val - stableNferp)), "expl.val"]
  SN <- c(month, num(stableNferp), num(stableNfer))
  A_gammastableNfer <- rbind(A_gammastableNfer, SN)
}
colnames(A_gammastableNfer)<-c("month", "pred.val", "expl.val")
A_gammastableNfer$pred.val <- as.numeric(A_gammastableNfer$pred.val)*100
A_gammastableNfer$expl.val <- as.numeric(A_gammastableNfer$expl.val)
A_gammastableNfer$species <- "A_gamma"

#get response data to Kfer
resvalueKfer <- resvalue[resvalue$expl.name == "Kfer", ]
A_gammahighKfer<- data.frame()
#extract turning points when existence probability reach the peak
for(month in month.abb){
  resKfermonth <- resvalueKfer[resvalueKfer$month == month ,]
  #get the highest and the lowest existence probability and the range of existence probability change
  stableKferp <- max(resKfermonth$pred.val)
  stableKferpmin <- min(resKfermonth$pred.val)
  stableKferpran <- stableKferp-stableKferpmin
  #dose of Kfer at the turning point
  stableKfer <- resKfermonth[which.min(abs(resKfermonth$pred.val - stableKferp)), "expl.val"]
  SN <- c(month, num(stableKferp), num(stableKfer), num(stableKferpran))
  A_gammahighKfer<- rbind(A_gammahighKfer, SN)
}
colnames(A_gammahighKfer)<-c("month", "pred.val", "expl.val", "range")
A_gammahighKfer$pred.val <- as.numeric(A_gammahighKfer$pred.val)*100
A_gammahighKfer$expl.val <- as.numeric(A_gammahighKfer$expl.val)
#exclude these points with range of existence probability change < 0.1 
#If the existence probability of a species in a month varies by less than 10% as a land management factor increases, we assume that it does not significantly affect the studied environmental data (Kfer in this case)
#For some months, trend of existence probability do not show peak values, these months are excluded
A_gammahighKfer<- A_gammahighKfer[ A_gammahighKfer$range > 0.1, ]
A_gammahighKfer$species <- "A_gamma"

A_gammalowKfer<- data.frame()
#extract turning points when existence probability reach the bottom
for(month in month.abb){
  resKfermonth <- resvalueKfer[resvalueKfer$month == month ,]
  #get the highest and the lowest existence probability and the range of existence probability change
  stableKferpmax <- max(resKfermonth$pred.val)
  stableKferpmin <- min(resKfermonth$pred.val)
  stableKferpran <- stableKferpmax-stableKferpmin
  stableKferp <- min(resKfermonth$pred.val)
  stableKfer <- resKfermonth[which.min(abs(resKfermonth$pred.val - stableKferp)), "expl.val"]
  SN <- c(month, num(stableKferp), num(stableKfer), num(stableKferpran))
  A_gammalowKfer<- rbind(A_gammalowKfer, SN)
}
colnames(A_gammalowKfer)<-c("month", "pred.val", "expl.val", "range")
A_gammalowKfer$pred.val <- as.numeric(A_gammalowKfer$pred.val)*100
A_gammalowKfer$expl.val <- as.numeric(A_gammalowKfer$expl.val)
#exclude these points with range of existence probability change < 0.1 
#If the existence probability of a species in a month varies by less than 10% as a land management factor increases, we assume that it does not significantly affect the studied environmental data (Kfer in this case)
#For some months, trend of existence probability do not show bottom values, these months are excluded
A_gammalowKfer<- A_gammalowKfer[A_gammalowKfer$range > 0.1, ]
A_gammalowKfer$species <- "A_gamma"

#get response data to Pfer
resvaluePfer <- resvalue[resvalue$expl.name == "Pfer", ]
A_gammaPferstarter <- data.frame()
#extract turning points when existence probability start to increase rapidly
for(month in month.abb){
  resPfermonth <- resvaluePfer[resvaluePfer$month == month ,]
  slopes <- rep(0, nrow(resPfermonth))
  slopes[2:nrow(resPfermonth)] <- diff(resPfermonth$pred.val) / diff(resPfermonth$expl.val)
  resPfermonth$slopes <- slopes
  stablePferpslope <- max(resPfermonth$slopes)
  stablePferp <- resPfermonth[which.max(resPfermonth$slopes), "pred.val"]
  #range of existence probability change
  stablePferpmax <- max(resPfermonth$pred.val)
  stablePferpmin <- min(resPfermonth$pred.val)
  stablePferpran <- stablePferpmax-stablePferpmin
  #dose of Pfer when existence probability increase rapidly
  resPfermonth <- resPfermonth[ resPfermonth$expl.val<75, ]
  stablePfer <- resPfermonth[which.min(abs(resPfermonth$pred.val - stablePferp)), "expl.val"]
  SN <- c(month, num(stablePferp), num(stablePfer), num(stablePferpran), num(stablePferpslope))
  A_gammaPferstarter <- rbind(A_gammaPferstarter, SN)
}
colnames(A_gammaPferstarter)<-c("month", "pred.val", "expl.val", "range", "slope")
A_gammaPferstarter$pred.val <- as.numeric(A_gammaPferstarter$pred.val)*100
A_gammaPferstarter$expl.val <- as.numeric(A_gammaPferstarter$expl.val)
#exclude these points with range of existence probability change < 0.1 
#If the existence probability of a species in a month varies by less than 10% as a land management factor increases, we assume that it does not significantly affect the studied environmental data (Pfer in this case)
A_gammaPferstarter <- A_gammaPferstarter[A_gammaPferstarter$range > 0.1, ]
A_gammaPferstarter$species <- "A_gamma"

A_gammalowPfer<- data.frame()
#extract turning points when existence probability reach the bottom
for(month in month.abb){
  resPfermonth <- resvaluePfer[resvaluePfer$month == month ,]
  #get the highest and the lowest existence probability and the range of existence probability change
  stablePferpmax <- max(resPfermonth$pred.val)
  stablePferpmin <- min(resPfermonth$pred.val)
  stablePferpran <- stablePferpmax-stablePferpmin
  #dose of Pfer at the turning point
  stablePferp <- min(resPfermonth$pred.val)
  stablePfer <- resPfermonth[which.min(abs(resPfermonth$pred.val - stablePferp)), "expl.val"]
  SN <- c(month, num(stablePferp), num(stablePfer), num(stablePferpran))
  A_gammalowPfer<- rbind(A_gammalowPfer, SN)
}
colnames(A_gammalowPfer)<-c("month", "pred.val", "expl.val", "range")
A_gammalowPfer$pred.val <- as.numeric(A_gammalowPfer$pred.val)*100
A_gammalowPfer$expl.val <- as.numeric(A_gammalowPfer$expl.val)
#exclude these points with range of existence probability change < 0.1 
#If the existence probability of a species in a month varies by less than 10% as a land management factor increases, we assume that it does not significantly affect the studied environmental data (Pfer in this case)
A_gammalowPfer<- A_gammalowPfer[!(A_gammalowPfer$month %in% c("Jan", "Mar", "Apr")) & A_gammalowPfer$range > 0.1, ]
A_gammalowPfer$species <- "A_gamma"
A_gammalowPfer$slope <- 0

#In Jan, Peak existence probability occurance, calculate the Pfer dose and the existence probability at the peak
month <- "Jan"
resPfermonth <- resvaluePfer[resvaluePfer$month == month ,]
stablePferpmax <- max(resPfermonth$pred.val)
stablePfermax <- resPfermonth[which.min(abs(resPfermonth$pred.val - stablePferpmax)), "expl.val"]

resvaluePesti <- resvalue[resvalue$expl.name == "Pesti", ]
A_gammahighPesti <- data.frame()
#extract turning points when existence probability reach the highest peak
for(month in month.abb){
  resPestimonth <- resvaluePesti[resvaluePesti$month == month ,]
  stablePestip <- max(resPestimonth$pred.val)
  stablePestipmin <- min(resPestimonth$pred.val)
  stablePestipran <- stablePestip-stablePestipmin
  stablePesti <- resPestimonth[which.min(abs(resPestimonth$pred.val - stablePestip)), "expl.val"]
  SN <- c(month, num(stablePestip), num(stablePesti), num(stablePestipran))
  A_gammahighPesti <- rbind(A_gammahighPesti, SN)
}
colnames(A_gammahighPesti)<-c("month", "pred.val", "expl.val", "range")
A_gammahighPesti$pred.val <- as.numeric(A_gammahighPesti$pred.val)*100
A_gammahighPesti$expl.val <- as.numeric(A_gammahighPesti$expl.val)
#exclude these points with range of existence probability change < 0.1 
#If the existence probability of a species in a month varies by less than 10% as a land management factor increases, we assume that it does not significantly affect the studied environmental data (pesticide in this case)
A_gammahighPesti <- A_gammahighPesti[ !(A_gammahighPesti$month %in% c("Feb", "Apr","Nov")) & A_gammahighPesti$range > 0.1, ]
A_gammahighPesti$species <- "A_gamma"

##The way of mapping how existence probability change with all environmental variables
plot_list <- list()
# Loop through each expl.name
for (name in unique(resvalue$expl.name)) {
  # Extract the current expl.name data
  data <- resvalue[resvalue$expl.name == name, ]
  data$month <- factor(data$month, levels = periods)
  # Draw a response curve
  p <- ggplot(data, aes(x = expl.val, y = pred.val, colour = month)) +
    geom_line(linewidth = 0.8) +
    labs( x = name, y = NULL, color = "Month")
  # Adds the drawing to the list
  plot_list[[name]] <- p
}
# Combine all drawings on one page
combined_plot <-ggarrange(plotlist=plot_list, nrow =5, ncol =4, common.legend =TRUE, legend= "bottom")
combined_plot <- grid.arrange(combined_plot, top = "Response curve of A.gamma in 12 months", left = "probability of pest existence", bottom = "land management variables")
plot(combined_plot)

#The way of mapping how existence probability change with only Nfer, Pfer, Kfer, Pesticide
resvalue <- read.csv("A_gamma_newmanage/resvalue.csv")
#Remove outlier
resvalue <- resvalue[!(resvalue$month == "Jan" & resvalue$pred.val > 0.42 & resvalue$expl.name == "Nfer"), ]

resvalue$expl.name <- gsub ("Nfer", "Nitrogen Fertilizer (Nfer) (kg/hectare of crops)", resvalue$expl.name)
resvalue$expl.name <- gsub ("Kfer", "Potassium Fertilizer (Kfer) (kg/hectare of crops)", resvalue$expl.name)
resvalue$expl.name <- gsub ("Pfer", "Phosphate Fertilizer (Pfer) (kg/hectare of crops)", resvalue$expl.name)
resvalue$expl.name <- gsub ("Pesti", "Pesticide (kg/hectare of crops)", resvalue$expl.name)
hsignificantm <- c("Nitrogen Fertilizer (Nfer) (kg/hectare of crops)", "Potassium Fertilizer (Kfer) (kg/hectare of crops)", "Phosphate Fertilizer (Pfer) (kg/hectare of crops)", "Pesticide (kg/hectare of crops)")
selected_months <- c("Jan", "Jun","Jul", "Aug", "Sep","Oct")
selected_months <- month.abb
sresvalue <- resvalue[resvalue$month %in% month.abb, ]
plot_list <- list()
for (name in hsignificantm) {
  data <- sresvalue[sresvalue$expl.name == name, ]
  data$month <- factor(data$month, levels = periods)
  A_gammastableNfer$month <- factor(A_gammastableNfer$month, levels = periods)
  p <- ggplot(data, aes(x = expl.val, y = pred.val*100, colour = month)) +
    geom_line(linewidth = 0.8) +
    labs(x = name, y=NULL, color = "Month")
  plot_list[[name]] <- p
}
combined_plot <-ggarrange(plotlist=plot_list, ncol =2, nrow=2, common.legend =TRUE, legend= "right")
combined_plot <- grid.arrange(combined_plot, top = "Response curve of A.gamma in the 5 months that most A.gamma occurs", left = "probability of pest existence(%)", bottom = "land management variables")
plot(combined_plot)

# The way of draw how existence probability response to Nfer, Pfer, Kfer, and Pesticide change in only one month, and represent other month's response as tuning points
#(The map represent in the report)
resvalue <- read.csv("A_gamma_newmanage/resvalue.csv")
resvalue <- resvalue[!(resvalue$month == "Jan" & resvalue$pred.val > 0.42 & resvalue$expl.name == "Nfer"), ]

plot_list <- list()
resvalue$expl.name <- gsub ("Nfer", "Nitrogen Fertilizer (Nfer)", resvalue$expl.name)
name <- "Nitrogen Fertilizer (Nfer)"
sresvalue <- resvalue[resvalue$month %in% "Aug", ]
data <- sresvalue[sresvalue$expl.name == name, ]
data$month <- factor(data$month, levels = month.abb)
A_gammastableNfer$month <- factor(A_gammastableNfer$month, levels = periods)
color_mapping <- c( "Jan" = "#A6CEE3", "Feb" = "#1F78B4", "Mar" = "#B2DF8A", "Apr" = "#33A02C", "May" = "#FB9A99", "Jun" = "#E31A1C", "Jul" = "#FDBF6F", "Aug" = "#FF7F00", "Sep" = "#CAB2D6", "Oct" = "#6A3D9A", "Nov" = "#FFFF99", "Dec" = "#B15928")
co_map <- c("high existence probability" = "red", "low existence probability" = "black")
data$color <- color_mapping[data$month]
A_gammastableNfer$color <- color_mapping[A_gammastableNfer$month]
p<-ggplot(A_gammastableNfer, aes(x = expl.val, y = pred.val, colour = month))+
  geom_point(aes(color=month), shape = 20, size = 4)+
  geom_point(aes(x = expl.val, y = 0, color=month), shape = 20, size = 4)+
  scale_color_manual(values = A_gammastableNfer$color)+
  annotate("rect", xmin = min(A_gammastableNfer$expl.val), xmax = max(A_gammastableNfer$expl.val), ymin = min(A_gammastableNfer$pred.val), ymax = max(A_gammastableNfer$pred.val),
           fill = NA, color = "red", size = 1, linetype = "dashed") +
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = 0, yend =0, linetype= "high existence probability"), data = A_gammastableNfer, color = "red")+
  geom_segment(aes(x = 0, xend = 0, y = 0, yend =0, linetype= "low existence probability"), data = A_gammastableNfer, color = "black")+
  scale_linetype_manual("turning points range",values=c("high existence probability"=2, "low existence probability" =2))+
  geom_line(aes(x = expl.val, y = pred.val*100, colour = month), data=data, linewidth = 0.8)+
  labs(x = name, y=NULL, color ="month")+
  ylim(0,80)+
  # Adding the guide for turning points range with both linetype and color distinction
  guides(linetype = guide_legend(title = "turning points range", override.aes = list(color = c("red", "black"))))
plot_list[[name]] <- p

resvalue$expl.name <- gsub ("Kfer", "Potassium Fertilizer (Kfer)", resvalue$expl.name)
name <- "Potassium Fertilizer (Kfer)"
sresvalue <- resvalue[resvalue$month %in% "Aug", ]
data <- sresvalue[sresvalue$expl.name == name, ]
data$month <- factor(data$month, levels = periods)
data$color <- color_mapping[data$month]
A_gammahighKfer$color <- color_mapping[A_gammahighKfer$month]
A_gammahighKfer$month <- factor(A_gammahighKfer$month, levels = periods)
A_gammalowKfer$color <- color_mapping[A_gammalowKfer$month]
A_gammalowKfer$month <- factor(A_gammalowKfer$month, levels = periods)
p<-ggplot(A_gammahighKfer, aes(x = expl.val, y = pred.val, colour = month))+
  geom_point(aes(color=month), shape = 20, size = 4)+
  geom_point(aes(x = expl.val, y = 0, color=month), shape = 20, size = 4)+
  scale_color_manual("month", values = color_mapping)+
  annotate("rect", xmin = min(A_gammahighKfer$expl.val), xmax = max(A_gammahighKfer$expl.val), ymin = min(A_gammahighKfer$pred.val), ymax = max(A_gammahighKfer$pred.val),
           fill = NA, color = "red", size = 1, linetype = "dashed") +
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = 0, yend =0, linetype= "high existence probability"), data = A_gammahighKfer, color="red")+
  geom_point(aes(x=expl.val, y=pred.val, color=month), data=A_gammalowKfer, shape=20, size=4)+
  geom_point(aes(x=expl.val, y=0, color=month), data=A_gammalowKfer, shape=20, size=4)+
  annotate("rect", xmin = min(A_gammalowKfer$expl.val), xmax = max(A_gammalowKfer$expl.val), ymin = min(A_gammalowKfer$pred.val), ymax = max(A_gammalowKfer$pred.val),
           fill = NA, color = "black", size = 1, linetype = 2) +
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = 0, yend =0, linetype= "low existence probability"), data = A_gammalowKfer, color="black")+
  scale_linetype_manual(name="turning points range",values=c("high existence probability"= 2, "low existence probability"= 2))+ 
  geom_line(aes(x = expl.val, y = pred.val*100, colour = month), data=data, linewidth = 0.8)+
  labs(x = name, y=NULL, color ="month", linetype = "turning points range")+
  ylim(0,80)+
  guides(linetype = guide_legend(title = "turning points range", override.aes = list(color = c("red", "black"))))
# Adding the guide for turning points range with both linetype and color distinction
plot_list[[name]] <- p

resvalue$expl.name <- gsub ("Pfer", "Phosphate Fertilizer (Pfer)", resvalue$expl.name)
name <- "Phosphate Fertilizer (Pfer)"
sresvalue <- resvalue[resvalue$month %in% c("Jan", "Apr", "Aug"), ]
data <- sresvalue[sresvalue$expl.name == name, ]
data$month <- factor(data$month, levels = periods)
A_gammaPferstarter$month <- factor(A_gammaPferstarter$month, levels = periods)
A_gammalowPfer$month <- factor(A_gammalowPfer$month, levels = periods)
data$color <- color_mapping[data$month]
A_gammaPferstarter$color <- color_mapping[A_gammaPferstarter$month]
A_gammalowPfer$color <- color_mapping[A_gammalowPfer$month]
p<-ggplot(A_gammaPferstarter, aes(x = expl.val, y = pred.val, colour = month))+
  geom_point(aes(color=month), shape = 20, size = 4)+
  geom_point(aes(x = expl.val, y = 0, color=month), shape = 20, size = 4)+
  scale_color_manual(values = color_mapping)+
  annotate("rect", xmin = min(A_gammaPferstarter$expl.val), xmax = max(A_gammaPferstarter$expl.val), ymin = min(A_gammaPferstarter$pred.val), ymax = max(A_gammaPferstarter$pred.val),
           fill = NA, color = "red", size = 1, linetype = "dashed") +
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = 0, yend =0, linetype= "high existence probability"), data = A_gammaPferstarter, color = "red")+
  geom_point(aes(x=expl.val, y=pred.val, color=month), data=A_gammalowPfer, shape=20, size=4)+
  geom_point(aes(x=expl.val, y=2, color=month), data=A_gammalowPfer, shape=20, size=4)+
  annotate("rect", xmin = min(A_gammalowPfer$expl.val), xmax = max(A_gammalowPfer$expl.val), ymin = min(A_gammalowPfer$pred.val), ymax = max(A_gammalowPfer$pred.val),
           fill = NA, color = "black", size = 1, linetype = 2) +
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = 2, yend =2, linetype= "low existence probability"), data = A_gammalowPfer, color = "black")+
  scale_linetype_manual("turning points range",values=c("high existence probability"=2, "low existence probability"=2))+
  geom_line(aes(x = expl.val, y = pred.val*100, colour = month), data=data, linewidth = 0.8)+
  labs(x = name, y=NULL, color ="month")+
  ylim(0,80)+
  guides(linetype = guide_legend(title = "turning points range", override.aes = list(color = c("red", "black"))))
plot_list[[name]] <- p

resvalue$expl.name <- gsub ("Pesti", "Pesticide", resvalue$expl.name)
name <- "Pesticide"
sresvalue <- resvalue[resvalue$month %in% "Jan", ]
data <- sresvalue[sresvalue$expl.name == name, ]
data$month <- factor(data$month, levels = periods)
A_gammahighPesti$month <- factor(A_gammahighPesti$month, levels = periods)
data$color <- color_mapping[data$month]
A_gammahighPesti$color <- color_mapping[A_gammahighPesti$month]
p<-ggplot(A_gammahighPesti, aes(x = expl.val, y = pred.val, colour = month))+
  geom_point(aes(color=month), shape = 20, size = 4)+
  geom_point(aes(x = expl.val, y = 0, color=month), shape = 20, size = 4)+
  scale_color_manual(values = A_gammahighPesti$color)+
  annotate("rect", xmin = min(A_gammahighPesti$expl.val), xmax = max(A_gammahighPesti$expl.val), ymin = min(A_gammahighPesti$pred.val), ymax = max(A_gammahighPesti$pred.val),
           fill = NA, color = "red", size = 1, linetype = "dashed") +
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = 0, yend =0, linetype= "high existence probability"), data = A_gammahighPesti, color = "red")+
  geom_segment(aes(x = 0, xend = 0, y = 0, yend =0, linetype= "low existence probability"), data = A_gammahighPesti, color = "black")+
  scale_linetype_manual("turning points range",values=c("high existence probability"=2, "low existence probability"=2))+
  geom_line(aes(x = expl.val, y = pred.val*100, colour = month), data=data, linewidth = 0.8)+
  labs(x = name, y=NULL, color ="month")+
  ylim(0,80)+
  guides(linetype = guide_legend(title = "turning points range", override.aes = list(color = c("red", "black"))))
plot_list[[name]] <- p


combined_plot <-ggarrange(plotlist=plot_list, ncol =2, nrow=2, common.legend =TRUE, legend= "right")
combined_plot <- grid.arrange(combined_plot, top = "Response curve of A.gamma to land management invervention", left = "probability of pest existence(%)", bottom = "land management variables (kg/hectare of crops)")
plot(combined_plot)

#######Put data from 12 months into one file#############################################################
#Put TSS of SDMs built with land management data together
# Specify the folder path and file prefix
folder_path <- "A_gamma_newmanage/modelsscore/"
file_prefix <- "A_gamma_modelsscore_"
# Create data frames to store algorithm and merged validation data
algo_data <- data.frame(run = 1:35)
merged_data <- data.frame(run = 1:35)
# Loop through each month in periods
for (month in periods) {
  # Construct the file path for the current month
  file_path <- paste0(folder_path, file_prefix, month, ".csv")
  # Read the CSV file
  data <- read.csv(file_path)
  # Limit data to the first 35 rows (if needed)
  data <- head(data, 35)
  # Extract and rename the validation column
  validation_column_name <- paste0("v_", month)
  validation <- data[, "validation", drop = FALSE]
  colnames(validation) <- validation_column_name
  # Merge validation data to the merged_data data frame
  merged_data <- cbind(merged_data, validation)
  # Extract and rename the algo column
  algo_column_name <- paste0("algo_", month)
  algo <- data[, "algo", drop = FALSE]
  colnames(algo) <- algo_column_name
  # Merge algo data to the algo_data data frame
  algo_data <- cbind(algo_data, algo)
}
# Write merged validation data to CSV file
write.csv(merged_data, file = "A_gamma_newmanage/A_gamma_eval_scores.csv", row.names = FALSE)
# Write algo data to CSV file
write.csv(algo_data, file = "A_gamma_newmanage/A_gamma_eval_scores_algo.csv", row.names = FALSE)

#Put TSS of SDMs built without land management data together
#Do same thing to Validation data of TSS built without management data
folder_path <- "A_gamma_nomanageeva/"
file_prefix <- "A_gamma_modelsscore_"
algo_data <- data.frame(run = 1:35)
merged_data <- data.frame(run = 1:35)
for (month in periods) {
  file_path <- paste0(folder_path, file_prefix, month, ".csv")
  data <- read.csv(file_path)
  data <- head(data, 35)
  validation_column_name <- paste0("v_", month)
  validation <- data[, "validation", drop = FALSE]
  colnames(validation) <- validation_column_name
  merged_data <- cbind(merged_data, validation)
  algo_column_name <- paste0("algo_", month)
  algo <- data[, "algo", drop=FALSE]
  colnames(algo) <- algo_column_name
  algo_data <- cbind(algo_data, algo)
}
write.csv(merged_data, file = "A_gamma_newmanage/A_gamma_eval_scoresnomanage.csv", row.names = FALSE)
write.csv(algo_data, file = "A_gamma_newmanage/A_gamma_eval_scoresnomanage_algo.csv", row.names = FALSE)

#Put variable contribution data together
# Specify the folder path and file prefix
folder_path <- "A_gamma_newmanage/var_importance/"
file_prefix <- "A_gamma_varcon_"
# Create an empty data frame to store merged variable contribution data
merged_con <- data.frame()
# Loop through each month
for (month in month.abb) {
  # Construct the file path for the current month
  file_path <- paste0(folder_path, file_prefix, month, ".csv")
  # Read the CSV file
  data <- read.csv(file_path)
  # Define key columns for merging
  key_columns <- c("expl.var")
  # Merge the data with the existing merged_con data frame
  if (nrow(merged_con) == 0) {
    merged_con <- data
  } else {
    merged_con <- merge(merged_con, data, by = key_columns, all = TRUE)
  }
  # Rename columns for the current month
  validation_column_name <- paste0("con_", month)
  colnames(merged_con)[colnames(merged_con) == "var_contribution"] <- validation_column_name
  validation_se_name <- paste0("se_", month)
  colnames(merged_con)[colnames(merged_con) == "se"] <- validation_se_name
}
# Define the desired column order
column_order <- c("Nfer", "Kfer", "Pfer", "Pesti", "Irri", "primf", "pastr", "range", "urban",
                  "c3ann", "c3per", "c4ann", "c4per", "elv", "tmean", "tmean_dif", 
                  "pre", "pre_dif")
# Reorder and format the expl.var column
merged_con <- mutate(merged_con, expl.var = factor(expl.var, levels = column_order))
merged_con <- arrange(merged_con, expl.var)
# Write merged variable contribution data to a CSV file
write.csv(merged_con, file = "A_gamma_newmanage/A_gamma_eval_varcon.csv", row.names = FALSE)