library(raster)
library(sp)
library(rasterVis)
library(dplyr)

library(ISOcodes)
library(rgdal)
library(sf)
library(ncdf4)

setwd('E:/Pest_SDM/')
#set the studied geographical extent
model_extent_Europe <- extent(c(-25,50,35,70))

# transfer land use data from nc file to tiff raster stack###############################################################
####land use data were downloaded from Land-Use Harmonization (LUH2) database (Hurtt et al., 2020)
file_path <- "landuse.nc"
var_names <- c("primf", "pastr", "range", "urban", "c3ann", "c3per", "c4ann", "c4per")
landuse <- stack()
# Loop through each variable and process the data
for (var_name in var_names) {
  # get variables from netCDF files
  brick_data <- brick(file_path, varname = var_name)
  # crop the layers to Europe
  data.crop <- crop(brick_data, model_extent_Europe)
  # select the data for the studied current period 1970-2000
  selected_layers <- paste0("X", 1120:1150)
  #get the mean value of all the studied years
  cropped_data <- calc(data.crop[[selected_layers]], mean)
  landuse <- stack(landuse, cropped_data)
}
names(landuse) <- var_names
writeRaster(landuse, filename = "landuse.tif", format = "GTiff")

file_path <- "2015-2100landuseSSP2.nc"
FlanduseSSP2 <- stack()
# Loop through each variable and process the data
for (var_name in var_names) {
  # get variables from netCDF files
  brick_data <- brick(file_path, varname = var_name)
  # crop the layers to Europe
  data.crop <- crop(brick_data, model_extent_Europe)
  # select the data for the studied current period 2040-2060
  selected_layers <- paste0("X", 25:45)
  #get the mean value of all the studied years
  cropped_data <- calc(data.crop[[selected_layers]], mean)
  FlanduseSSP2 <- stack(FlanduseSSP2, cropped_data)
}
names(FlanduseSSP2) <- var_names
writeRaster(FlanduseSSP2, filename = "FlanduseSSP2.tif", format = "GTiff")

file_path <- "2015-2100landuseSSP5.nc"
FlanduseSSP5 <- stack()
# Loop through each variable and process the data
for (var_name in var_names) {
  # get variables from netCDF files
  brick_data <- brick(file_path, varname = var_name)
  # crop the layers to Europe
  data.crop <- crop(brick_data, model_extent_Europe)
  # select the data for the studied current period 2040-2060
  selected_layers <- paste0("X", 25:45)
  #get the mean value of all the studied years
  cropped_data <- calc(data.crop[[selected_layers]], mean)
  FlanduseSSP5 <- brick(stack(FlanduseSSP5, cropped_data))
}
names(FlanduseSSP5) <- var_names
writeRaster(FlanduseSSP5, filename = "FlanduseSSP5.tif", format = "GTiff")

###transfer land management data from csv to raster and estimate future land management data#######################################################################################
###elevation data were downloaded from WorldClim 2.1(Fick and Hijimans, 2017)
###land management csv data were downloaded from OurWorldInData database

#crop elevation data
elv_file <- "wc2.1_10m_elev.tif"
elvA <- stack(elv_file)
elv <- crop(elvA, model_extent_Europe)
writeRaster(elv, filename = "elv.tif", format = "GTiff", overwrite = TRUE)

# Read the CSV file containing nitrogen fertilizer data
Nfertilizers <- read.csv("Nfertilizers.csv")
Nfertilizers <- Nfertilizers %>% rename(Alpha_3 = Code)
colnames(Nfertilizers)[4] <- "Value"
#select past data and calculate the mean value of past data 
Nfertilizers <- Nfertilizers[Nfertilizers$Year <= 2010, ]
NFer<- Nfertilizers %>%
  group_by(Alpha_3) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()
#load ISO country code
merged_N<- merge(NFer, ISO_3166_1, by.x = "Alpha_3", by.y = "Alpha_3", all.x = TRUE)
# Load some (coarse) country background data (shp)
ne110 <- readOGR("ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
#load Value to country background data (shp)
values <- merged_N$Value[match(ne110$ISO_A2_EH, merged_N$Alpha_2)]
ne110$Value <- values
#Rasterize the Nfer data
rasterized <- rasterize(ne110, elvA, field = "Value")
# crop Nfer data to Europe
NFer <- crop(rasterized,model_extent_Europe)

# Read the CSV file containing potassium fertilizer data
Kfertilizers <- read.csv("Kfertilizers.csv")
Kfertilizers <- Kfertilizers %>% rename(Alpha_3 = Code)
colnames(Kfertilizers)[4] <- "Value"
#select past data and calculate the mean value of past data 
Kfertilizers <- Kfertilizers[Kfertilizers$Year <= 2010, ]
Kfer<- Kfertilizers %>%
  group_by(Alpha_3) %>%
  summarise(SD = sd(Value),Value = mean(Value)) %>%
  ungroup()
#load ISO country code
merged_K<- merge(Kfer, ISO_3166_1, by.x = "Alpha_3", by.y = "Alpha_3", all.x = TRUE)
# Load some (coarse) country background data
ne110 <- readOGR("E:/R-4.2.1-oldversion/practical_data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
#load Value to country background data (shp)
values <- merged_K$Value[match(ne110$ISO_A2_EH, merged_K$Alpha_2)]
ne110$Value <- values
#Rasterize the Kfer data
rasterized <- rasterize(ne110, elvA, field = "Value")
# crop Kfer data to Europe
Kfer <- crop(rasterized,model_extent_Europe)

# Read the CSV file containing phosphorus fertilizer data
Pfertilizers <- read.csv("Pfertilizers.csv")
Pfertilizers <- Pfertilizers %>% rename(Alpha_3 = Code)
colnames(Pfertilizers)[4] <- "Value"
#select past data and calculate the mean value of past data 
Pfertilizers <- Pfertilizers[Pfertilizers$Year <= 2010, ]
Pfer<- Pfertilizers %>%
  group_by(Alpha_3) %>%
  summarise(SD = sd(Value),Value = mean(Value)) %>%
  ungroup()
#load ISO country code
merged_P<- merge(Pfer, ISO_3166_1, by.x = "Alpha_3", by.y = "Alpha_3", all.x = TRUE)
# Load some (coarse) country background data
ne110 <- readOGR("E:/R-4.2.1-oldversion/practical_data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
#load Value to country background data (shp)
values <- merged_P$Value[match(ne110$ISO_A2_EH, merged_P$Alpha_2)]
ne110$Value <- values
#Rasterize the Pfer data
rasterized <- rasterize(ne110, elvA, field = "Value")
# crop Pfer data to Europe
Pfer <- crop(rasterized,model_extent_Europe)

#using the same way to deal with Pesticide data
Pesticide <- read.csv("pesticide-use-per-hectare-of-cropland.csv")
Pesticide <- Pesticide %>% rename(Alpha_3 = Code)
colnames(Pesticide)[4] <- "Value"
Pesticide <- Pesticide[Pesticide$Year <= 2010, ]
Pesti<- Pesticide %>%
  group_by(Alpha_3) %>%
  summarise(SD = sd(Value),Value = mean(Value)) %>%
  ungroup()
merged_Pesti<- merge(Pesti, ISO_3166_1, by.x = "Alpha_3", by.y = "Alpha_3", all.x = TRUE)
ne110 <- readOGR("E:/R-4.2.1-oldversion/practical_data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
values <- merged_Pesti$Value[match(ne110$ISO_A2_EH, merged_Pesti$Alpha_2)]
ne110$Value <- values
rasterized <- rasterize(ne110, elvA, field = "Value")
Pesti <- crop(rasterized,model_extent_Europe)

#using the same way to deal with irrigation data
Irrigation <- read.csv("percentage-of-irrigated-land-in-agricultural-land.csv")
Irrigation <- Irrigation %>% rename(Alpha_3 = Code)
colnames(Irrigation)[4] <- "Value"
Irrigation <- Irrigation[Irrigation$Year <= 2010, ]
Irri<- Irrigation %>%
  group_by(Alpha_3) %>%
  summarise(SD = sd(Value),Value = mean(Value)) %>%
  ungroup()
merged_Irri<- merge(Irri, ISO_3166_1, by.x = "Alpha_3", by.y = "Alpha_3", all.x = TRUE)
ne110 <- readOGR("E:/R-4.2.1-oldversion/practical_data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
values <- merged_Irri$Value[match(ne110$ISO_A2_EH, merged_Irri$Alpha_2)]
ne110$Value <- values
rasterized <- rasterize(ne110, elvA, field = "Value")
Irri <- crop(rasterized,model_extent_Europe)

#put past land management data into a raster stack and name it
landmanagement <- stack(NFer, Kfer, Pfer, Pesti, Irri)
names(landmanagement) <- c("Nfer", "Kfer", "Pfer", "Pesti", "Irri")
writeRaster(landmanagement, "landmanagement.tif", format = "GTiff")
#assume 2040-2060 land management data is same with past data under SSP2
writeRaster(landmanagementSSP2, "landmanagement.tif", format = "GTiff")
#assume 2040-2060 land management data doubled current land management data under SSP5
landmanagementSSP5 <- calc(landmanagement, fun = function(x) x * 2)
writeRaster(landmanagementSSP5, "landmanagementSSP5.tif", format = "GTiff")

####################################################################################
# 2040-2060 monthly maximum temperature data under SSP2 calculated by 13 GCM models was downloaded from WorldClim2.1 and stored in folder SSP2tmax
folder_path <- "SSP2tmax/"
#for raster stacks built by different GCM models, crop them to Europe and name each layer of each raster stack as what month they prodicted
file_names <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
stack_list <- list()
for (i in 1:length(file_names)) {
  raster_data <- stack(file_names[i])
  raster_data <- crop(raster_data, model_extent_Europe)
  names(raster_data) <- month.abb
  assign(paste0("Ftmax_stack", i), raster_data)
}
#calculate monthly mean tmax predicted by different models
Ftmax_stack <- stack()
for (month in month.abb) {
  mean_layer <- NULL
  for (i in 1:13) {
    layer <- get(paste0("Ftmax_stack", i))[[month]]
    if (is.null(mean_layer)) {
      mean_layer <- layer
    } else {
      mean_layer <- mean_layer + layer
    }
  }
  mean_layer <- mean_layer / 13
  Ftmax_stack <- addLayer(Ftmax_stack, mean_layer)
}
names(Ftmax_stack) <- month.abb

#2040-2060 monthly minimum temperature under SSP2 calculated by 13 GCM models was downloaded from WorldClim2.1 and stored in folder SSP2tmin
#calculate monthly mean tmin predicted by different models
folder_path <- "SSP2tmin/"
file_names <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
stack_list <- list()
for (i in 1:length(file_names)) {
  raster_data <- stack(file_names[i])
  raster_data <- crop(raster_data, model_extent_Europe)
  names(raster_data) <- month.abb
  assign(paste0("Ftmin_stack", i), raster_data)
}
Ftmin_stack <- stack()
for (month in month.abb) {
  mean_layer <- NULL
  for (i in 1:13) {
    layer <- get(paste0("Ftmin_stack", i))[[month]]
    if (is.null(mean_layer)) {
      mean_layer <- layer
    } else {
      mean_layer <- mean_layer + layer
    }
  }
  mean_layer <- mean_layer / 13
  Ftmin_stack <- addLayer(Ftmin_stack, mean_layer)
}
names(Ftmin_stack) <- month.abb

#Calculate monthly mean temperature
Ftmean_stack <- stack()
for (month in month.abb) {
  max_layer <- Ftmax_stack[[month]]
  min_layer <- Ftmin_stack[[month]]
  mean_layer <- (max_layer + min_layer) / 2
  Ftmean_stack <- addLayer(Ftmean_stack, mean_layer)
}
names(Ftmean_stack) <- month.abb

#2040-2060 precipitation under SSP2 calculated by 13 GCM models was downloaded from WorldClim2.1 and stored in folder SSP2pre
#Calculate how much monthly mean temperature differ to mean temperature in last month
monthsdiff <- c("Dec", month.abb)
Ftmean_dif <- stack()
for (i in 2:length(monthsdiff)) {
  month_current <- monthsdiff[i]
  month_previous <- monthsdiff[i-1]
  Ftmean_diff <- abs(Ftmean_stack[[month_current]] - Ftmean_stack[[month_previous]])
  names(Ftmean_diff) <- month_current
  Ftmean_dif <- stack(Ftmean_dif, Ftmean_diff)
}

#Calculate mean monthly precipitation predicted by 13 GCMs
folder_path <- "SSP2pre/"
file_names <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
stack_list <- list()
for (i in 1:length(file_names)) {
  raster_data <- stack(file_names[i])
  raster_data <- crop(raster_data, model_extent_Europe)
  names(raster_data) <- month.abb
  assign(paste0("Fpre_stack", i), raster_data)
}
Fpre_stack <- stack()
for (month in month.abb) {
  mean_layer <- NULL
  for (i in 1:13) {
    layer <- get(paste0("Fpre_stack", i))[[month]]
    if (is.null(mean_layer)) {
      mean_layer <- layer
    } else {
      mean_layer <- mean_layer + layer
    }
  }
  mean_layer <- mean_layer / 13
  Fpre_stack <- addLayer(Fpre_stack, mean_layer)
}
names(Fpre_stack) <- month.abb
#Calculate how much monthly precipitation differ to precipitation in last month
Fpre_dif <- stack()
for (i in 2:length(monthsdiff)) {
  month_current <- monthsdiff[i]
  month_previous <- monthsdiff[i-1]
  Fpre_diff <- abs(Fpre_stack[[month_current]] - Fpre_stack[[month_previous]])
  names(Fpre_diff) <- month_current
  Fpre_dif <- stack(Fpre_dif, Fpre_diff)
}

#create a folder to store the Current Climate data
dir.create('SSP245Cli')
#stack and store Climate data 
for (month in month.abb) {
  stack_name <- paste0("SSP245_", month)
  Fstack <- brick(stack(Ftmean_stack[[month]], Ftmean_dif[[month]], Fpre_stack[[month]], Fpre_dif[[month]]))
  names(Fstack) <- c("tmean", "tmean_dif", "pre", "pre_dif")
  assign(stack_name, Fstack)
  output <- paste0("SSP245Cli/", stack_name, ".tif")
  writeRaster(Fstack, filename = output, format = "GTiff")
}

#####################################################################SSP575
# 2040-2060 monthly maximum temperature data under SSP5 calculated by 13 GCM models was downloaded from WorldClim2.1 and stored in folder SSP5tmax
folder_path <- "SSP5tmax/"
#for raster stacks built by different GCM models, crop them to Europe and name each layer of each raster stack as what month they prodicted
file_names <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
stack_list <- list()
for (i in 1:length(file_names)) {
  raster_data <- stack(file_names[i])
  raster_data <- crop(raster_data, model_extent_Europe)
  names(raster_data) <- month.abb
  assign(paste0("Ftmax_stack", i), raster_data)
}
#calculate monthly mean tmax predicted by different GCMs
Ftmax_stack <- stack()
for (month in month.abb) {
  mean_layer <- NULL
  for (i in 1:13) {
    layer <- get(paste0("Ftmax_stack", i))[[month]]
    if (is.null(mean_layer)) {
      mean_layer <- layer
    } else {
      mean_layer <- mean_layer + layer
    }
  }
  mean_layer <- mean_layer / 13
  Ftmax_stack <- addLayer(Ftmax_stack, mean_layer)
}
names(Ftmax_stack) <- month.abb
#2040-2060 monthly minimum temperature under SSP5 calculated by 13 GCM models was downloaded from WorldClim2.1 and stored in folder SSP5tmin
#calculate monthly mean tmin predicted by different GCMs
folder_path <- "SSP5tmin/"
file_names <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
stack_list <- list()
for (i in 1:length(file_names)) {
  raster_data <- stack(file_names[i])
  raster_data <- crop(raster_data, model_extent_Europe)
  names(raster_data) <- month.abb
  assign(paste0("Ftmin_stack", i), raster_data)
}
Ftmin_stack <- stack()
for (month in month.abb) {
  mean_layer <- NULL
  for (i in 1:13) {
    layer <- get(paste0("Ftmin_stack", i))[[month]]
    if (is.null(mean_layer)) {
      mean_layer <- layer
    } else {
      mean_layer <- mean_layer + layer
    }
  }
  mean_layer <- mean_layer / 13
  Ftmin_stack <- addLayer(Ftmin_stack, mean_layer)
}
names(Ftmin_stack) <- month.abb

#Calculate monthly mean temperature
Ftmean_stack <- stack()
for (month in month.abb) {
  max_layer <- Ftmax_stack[[month]]
  min_layer <- Ftmin_stack[[month]]
  mean_layer <- (max_layer + min_layer) / 2
  Ftmean_stack <- addLayer(Ftmean_stack, mean_layer)
}
names(Ftmean_stack) <- month.abb

#2040-2060 precipitation under SSP5 calculated by 13 GCM models was downloaded from WorldClim2.1 and stored in folder SSP5pre
#Calculate how much monthly mean temperature differ to mean temperature in last month
Ftmean_dif <- stack()
for (i in 2:length(monthsdiff)) {
  month_current <- monthsdiff[i]
  month_previous <- monthsdiff[i-1]
  Ftmean_diff <- abs(Ftmean_stack[[month_current]] - Ftmean_stack[[month_previous]])
  names(Ftmean_diff) <- month_current
  Ftmean_dif <- stack(Ftmean_dif, Ftmean_diff)
}
#Calculate mean monthly precipitation predicted by 13 GCMs
folder_path <- "SSP5pre/"
file_names <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
stack_list <- list()
for (i in 1:length(file_names)) {
  raster_data <- stack(file_names[i])
  raster_data <- crop(raster_data, model_extent_Europe)
  names(raster_data) <- month.abb
  assign(paste0("Fpre_stack", i), raster_data)
}
Fpre_stack <- stack()
for (month in month.abb) {
  mean_layer <- NULL
  for (i in 1:13) {
    layer <- get(paste0("Fpre_stack", i))[[month]]
    if (is.null(mean_layer)) {
      mean_layer <- layer
    } else {
      mean_layer <- mean_layer + layer
    }
  }
  mean_layer <- mean_layer / 13
  Fpre_stack <- addLayer(Fpre_stack, mean_layer)
}
names(Fpre_stack) <- month.abb
#Calculate how much monthly precipitation differ to precipitation in last month
Fpre_dif <- stack()
for (i in 2:length(monthsdiff)) {
  month_current <- monthsdiff[i]
  month_previous <- monthsdiff[i-1]
  Fpre_diff <- abs(Fpre_stack[[month_current]] - Fpre_stack[[month_previous]])
  names(Fpre_diff) <- month_current
  Fpre_dif <- stack(Fpre_dif, Fpre_diff)
}

#create a folder to store the Current Climate data
dir.create('SSP585Cli')
#stack and store Climate data 
for (month in month.abb) {
  stack_name <- paste0("SSP585_", month)
  Fstack <- brick(stack(Ftmean_stack[[month]], Ftmean_dif[[month]], Fpre_stack[[month]], Fpre_dif[[month]]))
  names(Fstack) <- c("tmean", "tmean_dif", "pre", "pre_dif")
  assign(stack_name, Fstack)
  output <- paste0("SSP585Cli/", stack_name, ".tif")
  writeRaster(Fstack, filename = output, format = "GTiff")
}
###########Current climate data (monthly and year-long)############################################
# 1970-2000 monthly maximum temperature data was downloaded from WorldClim2.1 and stored in folder wc2.1_10m_tmax
#crop them to Europe and name 
file_names <- paste0("wc2.1_10m_tmax_", sprintf("%02d", 1:12), ".tif")
tmax_stack <- stack(file.path("wc2.1_10m_tmax/", file_names))
tmax_stack <- crop(tmax_stack, model_extent_Europe)
names(tmax_stack) <- month.abb
# 1970-2000 monthly minimum temperature data was downloaded from WorldClim2.1 and stored in folder wc2.1_10m_tmax
#crop them to Europe and name 
file_names <- paste0("wc2.1_10m_tmin_", sprintf("%02d", 1:12), ".tif")
tmin_stack <- stack(file.path("wc2.1_10m_tmin/", file_names))
tmin_stack <- crop(tmin_stack, model_extent_Europe)
names (tmin_stack) <- month.abb
#Calculate monthly mean temperature
tmean_stack <- stack()
for (month in month.abb) {
  max_layer <- tmax_stack[[month]]
  min_layer <- tmin_stack[[month]]
  mean_layer <- (max_layer + min_layer) / 2
  tmean_stack <- addLayer(tmean_stack, mean_layer)
}
names(tmean_stack) <- month.abb
#Calculate how much monthly precipitation differ to mean temperature in last month
tmean_dif <- stack()
for (i in 2:length(monthsdiff)) {
  month_current <- monthsdiff[i]
  month_previous <- monthsdiff[i-1]
  tmean_diff <- abs(tmean_stack[[month_current]] - tmean_stack[[month_previous]])
  names(tmean_diff) <- month_current
  tmean_dif <- stack(tmean_dif, tmean_diff)
}
# 1970-2000 monthly minimum temperature data was downloaded from WorldClim2.1 and stored in folder wc2.1_10m_prec
#crop them to Europe and name 
file_names <- paste0("wc2.1_10m_prec_", sprintf("%02d", 1:12), ".tif")
pre_stack <- stack(file.path("wc2.1_10m_prec/", file_names))
pre_stack <- crop(pre_stack, model_extent_Europe)
names (pre_stack) <- month.abb
#Calculate how much monthly precipitation differ to precipitation in last month
pre_dif <- stack()
for (i in 2:length(monthsdiff)) {
  month_current <- monthsdiff[i]
  month_previous <- monthsdiff[i-1]
  pre_diff <- abs(pre_stack[[month_current]] - pre_stack[[month_previous]])
  names(pre_diff) <- month_current
  pre_dif <- stack(pre_dif, pre_diff)
}

#create a folder to store the Current Climate data
dir.create('CurrentCli')
#stack and store Climate data 
for (month in month.abb) {
  stack_name <- paste0("Current_", month)
  stack <- brick(stack(tmean_stack[[month]], tmean_dif[[month]], pre_stack[[month]], pre_dif[[month]]))
  names(stack) <- c("tmean", "tmean_dif", "pre", "pre_dif")
  assign(stack_name, stack)
  output <- paste0("CurrentCli/", stack_name, ".tif")
  writeRaster(stack, filename = output, format = "GTiff")
}

#Calculate mean temperature monthly variation and precipitation variation over a year
m_mon_var_tmean<-calc(tmean_dif, mean)
m_mon_var_pre<-calc(pre_dif, mean)
#yearly mean temperature and yearly precipitation data was downloaded from WorldClim2.1 and stored in folder wc2.1_10m_bio as bio1 and bio12
file_list <- list.files("wc2.1_10m_bio/"
                        , pattern = "wc2.1_10m_bio_\\d+.tif", full.names = TRUE)
bio_stack <- stack(file_list)
bio_stack <- crop(bio_stack, model_extent_Europe)
#stack and store year-long climate data
Current_year <- stack(bio_stack$wc2.1_10m_bio_1, m_mon_var_tmean, bio_stack$wc2.1_10m_bio_12, m_mon_var_pre)
writeRaster(Current_year, filename = "CurrentCli/Current_year.tif", format = "GTiff")


