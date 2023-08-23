library(raster)
library(dplyr)
library(sp)
library(rasterVis)
library(ggplot2)
library(lmtest)
library(lme4)
library(gridExtra)


#set word place
setwd('E:/Pest_SDM/')

#########################Turning points of the response curves (species existence probability to Nfer, Kfer, Pfer, Pesticides)################
# Combine data from different sources
# Noted: please run this session after all the SDM for the four studied species are run (these scripts were named as species-name.R)
########make sure your R studio have stored the variables set by these SDM scripts
# Combine data from different sources
stableNfer <- rbind(A_ipsilonstableNfer, A_gammastableNfer, P_meticulosastableNfer, L_oleraceastableNfer)
highlowKfer <- rbind(A_gammalowKfer, A_gammahighKfer, A_ipsilonlowKfer, A_ipsilonhighKfer, P_meticulosalowKfer, P_meticulosahighKfer, L_oleracealowKfer, L_oleraceahighKfer)
Pferstarter <- rbind(A_gammaPferstarter, A_ipsilonPferstarter, P_meticulosaPferstarter, L_oleraceaPferstarter)
lowPfer <- rbind(A_gammalowPfer, A_ipsilonlowPfer, P_meticulosalowPfer, L_oleracealowPfer)
highPesti <- rbind(A_gammahighPesti, A_ipsilonhighPesti, P_meticulosahighPesti, L_oleraceahighPesti)

resvalue <- read.csv("A_ipsilon_newmanage/resvalue.csv")
resvalue <- resvalue[!(resvalue$month == "Jan" & resvalue$pred.val > 0.42 & resvalue$expl.name == "Nfer"), ]

sumresplot <- list()
resvalue$expl.name <- gsub ("Nfer", "Nitrogen Fertilizer (Nfer)", resvalue$expl.name)
name <- "Nitrogen Fertilizer (Nfer)"
sresvalue <- resvalue[resvalue$month %in% "Aug", ]
data <- sresvalue[sresvalue$expl.name == name, ]
sp<-ggplot(stableNfer, aes(x = expl.val,  y=species, colour = month))+
  geom_point(aes(color = month), shape = 20, size = 4)+
  scale_color_manual(values = color_mapping)+ 
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = A_ipsilonstableNfer, color = "red")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = A_gammastableNfer, color = "red")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = P_meticulosastableNfer, color = "red")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = L_oleraceastableNfer, color = "red")+
  geom_segment(aes(x = 0, xend = 0, y = species, yend =species, linetype = "low existence probability"), data = A_ipsilonstableNfer, color = "black")+
  scale_linetype_manual("turning points range",values=c("high existence probability"=2, "low existence probability"=2))+
  guides(linetype = guide_legend(title = "turning points range", override.aes = list(color = c("red", "black"))))+
  xlim(0, max(data$expl.val))+
  labs(x= name, y = NULL)+
  theme(legend.position = "none")
sumresplot[[name]]<-sp

resvalue$expl.name <- gsub ("Kfer", "Potassium Fertilizer (Kfer)", resvalue$expl.name)
name <- "Potassium Fertilizer (Kfer)"
sresvalue <- resvalue[resvalue$month %in% "Aug", ]
data <- sresvalue[sresvalue$expl.name == name, ]
sp<-ggplot(highlowKfer, aes(x = expl.val,  y=species, colour = month))+
  geom_point(aes(color = month), shape = 20, size = 4)+
  scale_colour_manual(values = color_mapping)+ 
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "low existence probability"), data = A_ipsilonlowKfer, color = "black")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = A_ipsilonhighKfer, color = "red")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "low existence probability"), data = A_gammalowKfer, color = "black")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = A_gammahighKfer, color = "red")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "low existence probability"), data = P_meticulosalowKfer, color = "black")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = P_meticulosahighKfer, color = "red")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "low existence probability"), data = L_oleracealowKfer, color = "black")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = L_oleraceahighKfer, color = "red")+
  scale_linetype_manual("turning points range",values=c("high existence probability"=2, "low existence probability"=2))+
  guides(linetype = guide_legend(title = "turning points range", override.aes = list(color = c("red", "black"))))+
  xlim(0, max(data$expl.val))+
  labs(x= name, y = NULL)+
  theme(legend.position = "none")
sumresplot[[name]]<-sp


resvalue$expl.name <- gsub ("Pfer", "Phosphate Fertilizer (Pfer)", resvalue$expl.name)
name <- "Phosphate Fertilizer (Pfer)"
sresvalue <- resvalue[resvalue$month %in% c("Aug"), ]
data <- sresvalue[sresvalue$expl.name == name, ]
sp<-ggplot(Pferstarter, aes(x = expl.val,  y=species, colour = month))+
  geom_point(aes(color = month), shape = 20, size = 4,position = position_nudge(y = -0.2))+
  geom_point(aes(x = expl.val,  y=species, colour = month), data = lowPfer, shape = 20, size = 4,position = position_nudge(y = 0.2))+
  scale_color_manual(values = color_mapping)+ 
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = A_ipsilonPferstarter, color = "red", position = position_nudge(y = -0.2))+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = A_gammaPferstarter, color = "red", position = position_nudge(y = -0.2))+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = P_meticulosaPferstarter, color = "red", position = position_nudge(y = -0.2))+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = L_oleraceaPferstarter, color = "red", position = position_nudge(y = -0.2))+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "low existence probability"), data = A_ipsilonlowPfer, color = "black", position = position_nudge(y = 0.2))+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "low existence probability"), data = A_gammalowPfer, color = "black", position = position_nudge(y = 0.2))+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "low existence probability"), data = P_meticulosalowPfer, color = "black", position = position_nudge(y = 0.2))+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "low existence probability"), data = L_oleracealowPfer, color = "black", position = position_nudge(y = 0.2))+
  scale_linetype_manual("turning points range",values=c("high existence probability"=2, "low existence probability"=2))+
  guides(linetype = guide_legend(title = "turning points range", override.aes = list(color = c("red", "black"))))+
  xlim(0, max(data$expl.val))+
  labs(x= name, y = NULL)+
  theme(legend.position = "none")
sumresplot[[name]]<-sp

resvalue$expl.name <- gsub ("Pesti", "Pesticide", resvalue$expl.name)
name <- "Pesticide"
sresvalue <- resvalue[resvalue$month %in% "Jun", ]
data <- sresvalue[sresvalue$expl.name == name, ]
sp<-ggplot(highPesti, aes(x = expl.val,  y=species, colour = month))+
  geom_point(aes(color = month), shape = 20, size = 4)+
  scale_color_manual(values = color_mapping)+ 
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = A_ipsilonhighPesti, color = "red")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = A_gammahighPesti, color = "red")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = P_meticulosahighPesti, color = "red")+
  geom_segment(aes(x = min(expl.val), xend = max(expl.val), y = species, yend =species, linetype = "high existence probability"), data = L_oleraceahighPesti, color = "red")+
  geom_segment(aes(x = 0, xend = 0, y = species, yend =species, linetype = "low existence probability"), data = P_meticulosahighPesti, color = "black")+
  scale_linetype_manual("turning points range",values=c("high existence probability"=2, "low existence probability"=2))+
  guides(linetype = guide_legend(title = "turning points range", override.aes = list(color = c("red", "black"))))+
  xlim(0, max(data$expl.val))+
  labs(x= name, y = NULL)+
  theme(legend.position = "none")
sumresplot[[name]]<-sp

combined_plot <-ggarrange(plotlist=sumresplot, nrow =4, ncol =1)
combined_plot <- grid.arrange(combined_plot, top = "Turning points of respons curves to land management interventions", left = "species", bottom = "land management variables (kg/hectare of crops)")
plot(combined_plot)

######existence probability map related data processing and representing###################################################
# Initialize data frames
Month <- character()
Scenario <- character()
Value <- numeric()
Species <- character()
management_YN <- character()

area_change_longlist <- data.frame(Month, Scenario, Value, Species)
manageHP_longlist <- data.frame(Month, Scenario, Value, Species, management_YN)
nomanageHP_longlist <- data.frame(Month, Scenario, Value, Species, management_YN)

AreadifferCurrent_SSP2 <- month.abb
AreadifferCurrent_SSP5 <- month.abb

species <- c("P_meticulosa", "A_gamma", "A_ipsilon", "L_oleracea")
situation <- c("Current", "SSP2", "SSP5")

# Loop through species and situations
for (specie in species){
  for (situ in situation) {
    manage <- stack()
    # Loop through months
    for (month in month.abb) {
      # Load and stack raster data for managed species
      assign ((paste0 (situ,month)), raster(paste0( specie, "_newmanage/", situ, month, ".tif")))
      manage <- stack(manage, get(paste0(situ, month)))
    }
    # Assign names to stacked layers
    names(manage) <- paste0(month.abb, "man")
    assign(paste0("manage_", situ), manage)
    
    nomanage<- stack()
    # Loop through months
    for (month in month.abb) {
      # Load and stack raster data for non-managed species
      assign ((paste0 (situ,month)), raster(paste0( specie, "_nomanageeva/", situ, month, ".tif")))
      nomanage <- stack(nomanage, get(paste0(situ, month)))
    }
    # Assign names to stacked layers
    names(nomanage) <- paste0(month.abb, "noman")
    assign(paste0("noman_", situ), nomanage)
  }
  
  # Initialize variables
  area_change_df <- month.abb
  area_change <- c()
  manage_HP <- c()
  nomanage_HP <- c()
  for (situ in situation) {
    manageHP <- c()
    # Loop through months
    for (month in month.abb) {
      manageraster <- rast(paste0(specie, "_newmanage/", situ, month, ".tif"))
      manageraster <- project(manageraster, "EPSG:3035")
      # Calculate managed habitat area
      HP <- (global(manageraster >= 600 , sum, na.rm =TRUE)$sum)*(res(manageraster)[1]/1000)*(res(manageraster)[2]/1000)
      manageHP <- c(manageHP,HP)
    }
    
    nomanageHP <- c()
    # Loop through months
    for (month in month.abb) {
      manageraster <- rast(paste0(specie, "_nomanageeva/", situ, month, ".tif"))
      manageraster <- project(manageraster, "EPSG:3035")
      # Calculate non-managed habitat area
      HP <- (global(manageraster >= 600 , sum, na.rm =TRUE)$sum)*(res(manageraster)[1]/1000)*(res(manageraster)[2]/1000)
      nomanageHP <- c(nomanageHP,HP)
    }
    
    manageHP <-manageHP/1000000
    # Assign managed habitat area values
    assign(paste0("manageHP", situ),  manageHP)
    manage_HP <- c(manage_HP, manageHP)
    
    nomanageHP <- nomanageHP/1000000
    nomanage_HP <- c(nomanage_HP, nomanageHP)
    
    # Calculate area change
    areachange <- manageHP - nomanageHP
    area_change <- c(area_change, areachange)
    
    area_change_df <- rbind(area_change_df, areachange)
  }
  # Calculate area differences
  areadifferCurrent_SSP2 <- round(manageHPSSP2-manageHPCurrent, 3)
  AreadifferCurrent_SSP2 <- rbind(AreadifferCurrent_SSP2, areadifferCurrent_SSP2)
  
  areadifferCurrent_SSP5 <- round(manageHPSSP5-manageHPCurrent, 3)
  AreadifferCurrent_SSP5 <- rbind(AreadifferCurrent_SSP5, areadifferCurrent_SSP5)
  
  area_change_df <- area_change_df[-1,]
  colnames(area_change_df) <- month.abb
  rownames(area_change_df) <- situation
  
  area_change_longlist_specie <-data.frame(
    Month = rep(month.abb, 3),
    Scenario = rep(situation, each = 12),
    Value = area_change,
    Species = specie
  )
  area_change_longlist <- rbind(area_change_longlist, area_change_longlist_specie)
}

area_longlist <- rbind(manageHP_longlist, nomanageHP_longlist)
area_longlist$Month <- factor(area_longlist$Month, levels = months)

#Area difference of high-potential area predicted by SDM built with and without management data. 
area_change_longlist$Month <- factor(area_change_longlist$Month, levels = month.abb)
ggplot(area_change_longlist, aes(x = Scenario, y = Value)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(title = "Area difference of high-potential area predicted by SDM built with and without management data ",
       x = "Scenario",
       y = "Area (M km^2)",
       color = "Species") +
  theme_minimal()

#the linear mixed model with the difference of area predicted by SDM built with and without 
#land management factors (Nitrogen fertilizer, Potassium fertilizer, Phosphate fertilizer, 
#Pesticide, and irrigation) as the response variable, Scenario under past and future period 
#as fixed effects, Month identity were added as random effects.
m1<-lmer(Value ~ Scenario+(1|Species)+(1|Month), data = area_change_longlist)
m2<-lmer(Value ~ Scenario+(1|Month), data = area_change_longlist)
m3<-lmer(Value ~ Scenario+(1|Species), data = area_change_longlist)
summary(m2)
#likelihood test
lrtest(m2,m1)
lrtest(m3,m1)

#######mixed liner regression and plot related to TSSs############################################################
# Initialize empty data frame for TSS comparison
Month <- character()
TSS <- numeric()
manage_YN <- character()
record_number <- numeric()
Species <- character()
TSS_compare <- data.frame(Month, TSS, manage_YN, record_number, Species)

# List of species
species <- c("P_meticulosa", "A_gamma", "A_ipsilon", "L_oleracea")

# Loop through each species
for (specie in species) {
  # Months including year
  months <- c("year", month.abb)
  # Loop through each month
  for (month in months) {
    assign(paste0("specie", month), read.csv(paste0(specie, "_newmanage/", specie, month, "_thin1.csv")))
  }
  # List of dataframes for each month
  dataframes <- list(specieyear, specieJan, specieFeb, specieMar, specieApr, specieMay, specieJun, specieJul, specieAug, specieSep, specieOct, specieNov, specieDec)
  # Number of rows in each dataframe
  row_counts <- sapply(dataframes, nrow)
  
  # Read algo data and TSS data
  algo_data <- read.csv(paste0(specie, "_newmanage/", specie, "_eval_scores_algo.csv", row.names = NULL))
  merged_data <- read.csv(paste0(specie, "_newmanage/", specie, "_eval_scores.csv", row.names = NULL))
  # Remove first column from algo_data and TSS_data
  algo_data <- algo_data[, -c(1)]
  TSS_data <- merged_data[, -c(1)]
  # Pivot and process data
  plotalgo_data <- algo_data %>%
    pivot_longer(everything(), names_to = "Month", values_to = "algo")
  plotTSS_data <- TSS_data %>%
    pivot_longer(everything(), names_to = "Month", values_to = "TSS")
  # Adjust Month column
  plotTSS_data$Month <- sub("v_", "", plotTSS_data$Month)
  plotTSS_data$Month <- factor(plotTSS_data$Month, levels = months)
  # Combine with algo data
  plotTSS_data$algo <- plotalgo_data$algo
  plotTSS_data <- plotTSS_data %>%
    mutate(manage_YN = "with land management data")
  # Arrange and mutate additional columns
  plotTSS_data <- plotTSS_data %>% arrange(Month)
  plotTSS_data <- plotTSS_data %>% mutate(record_number = rep(row_counts, each = 35))
  plotTSS_data$Species <- specie
  
  # Read nomanage algo data and TSS data
  algo_data <- read.csv(paste0(specie, "_newmanage/", specie, "_eval_scoresnomanage_algo.csv", row.names = NULL))
  nomanage_data <- read.csv(paste0(specie, "_newmanage/", specie, "_eval_scoresnomanage.csv", row.names = NULL))
  # Remove first column from algo_data and nomanage_data
  algo_data <- algo_data[, -c(1)]
  nomanage_data <- nomanage_data[, -c(1)]
  # Pivot and process data
  plotalgo_data <- algo_data %>%
    pivot_longer(everything(), names_to = "Month", values_to = "algo")
  plotnomanage_data <- nomanage_data %>%
    pivot_longer(everything(), names_to = "Month", values_to = "TSS")
  # Adjust Month column
  plotnomanage_data$Month <- sub("v_", "", plotnomanage_data$Month)
  plotnomanage_data$Month <- factor(plotnomanage_data$Month, levels = months)
  # Combine with algo data
  plotnomanage_data$algo <- plotalgo_data$algo
  plotnomanage_data <- plotnomanage_data %>%
    mutate(manage_YN = "without land management data")
  # Arrange and mutate additional columns
  plotnomanage_data <- plotnomanage_data %>% arrange(Month)
  plotnomanage_data <- plotnomanage_data %>% mutate(record_number = rep(row_counts, each = 35))
  plotnomanage_data$Species <- specie
  # Merge and assign TSS_compare dataframe
  assign(paste0("TSS_compare", specie), bind_rows(plotTSS_data, plotnomanage_data))
  TSS_compare <- rbind(TSS_compare, get(paste0("TSS_compare", specie)))
}

# Extract TSS_compare to include only rows with record_number greater than 100
TSS_compare_large <- subset(TSS_compare, record_number > 100)
# Relevel the manage_YN factor variable, setting "with land management data" as the reference level
TSS_compare_large$manage_YN <- relevel(TSS_compare_large$manage_YN, ref = "with land management data")

#Boxplot of monthly and year-long TSS of the models built with and without land management data.  
f_data <- TSS_compare_large %>%
  filter(Month %in% months)
ggplot(f_data, aes(x = Month, y = TSS, fill = manage_YN)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("with land management data" = "lightblue", "without land management data" = "grey")) +
  geom_hline(yintercept = median(f_data$TSS[f_data$Month == "year" & f_data$manage_YN == "with land management data"]), color = "red", linetype = "dashed") +
  stat_summary(aes(group = manage_YN), fun = mean, geom = "point", pch = 4, size = 3, color = "blue", position = position_dodge(width = 0.8)) +
  labs(title = "TSS of models built with and without\nlandmanagement data", x = "Month", y = "TSS", fill = "Data used to build the model") +
  theme_minimal()+
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(ncol = 1))

#The result of the linear mixed model with TSS as the response variable, the periods (year-long and each month) on
#which the model was built (PERIOD) and whether land management factors included in SDM (MANAGEMENT) as fixed effects,
#species groups (SPECIES), algorithms used for SDM establishment (ALGO), and the number of records used to establish SDMs 
#(RECORDS) were added as random effects. 
m1<-lmer(TSS ~ manage_YN+Month+(1|Species)+(1|algo)+(1|record_number), data = TSS_compare_large)
m2<-lmer(TSS ~ manage_YN+Month+(1|Species)+(1|record_number), data = TSS_compare_large)
m3<-lmer(TSS ~ manage_YN+Month+(1|algo)+(1|record_number), data = TSS_compare_large)
m4<-lmer(TSS ~ manage_YN+Month+(1|Species)+(1|algo), data = TSS_compare_large)
#likelihood test
lrtest(m1,m2)
lrtest(m1,m3)
lrtest(m1,m4)
summary(m1)


#When record number > 400,TSS of SDM do not significantly increased 
#as the number of occurrence records used in the SDM construction increased 
TSS_compare_lar <- subset(TSS_compare, record_number > 400)
summary(lm(TSS ~ record_number, data = TSS_compare_lar))
#it can also be viewed in plot
model <-lm(TSS ~ record_number, data = TSS_compare_lar)
intercept <- coef(model)[1]
slope <- coef(model)[2]
plot(TSS_compare_lar$record_number, TSS_compare_large$TSS, 
     xlab = "record_number", ylab = "TSS",
     main = "Linear Regression of TSS ~ record_number")
abline(a = intercept, b = slope, col = "red", lwd = 2)
legend("topleft", legend = paste("TSS = ", round(intercept, 2), " + ", round(slope, 2), " * record_number"),
       col = "red", lwd = 2, cex = 0.8, bty = "n")

#######variable contribution plots###############################
varchangesplot <- list()
species <- c("L_oleracea", "P_meticulosa", "A_gamma", "A_ipsilon")
for (specie in species) {
  merged_con <- read.csv(paste0( specie, "_newmanage/", specie, "_eval_varcon.csv", row.names = NULL))
  new_row_names <- c("Nitrogen fertilizer", "Potassium fertilizer", "Phosphate fertilizer", "Pesticide", "Irrigation fraction",
                     "Forested primary land fraction", "Managed pasture fraction", "Rangeland fraction", "Urban land fraction",
                     "C3 annual crops fraction", "C3 perennial crops fraction", "C4 annual crops fraction", "C4 perennial crops fraction",
                     "Elevation", "Mean temperature", "Temperature Monthly variation ", "Precipitation", "Precipitation Monthly variation ")
  new_merged_con <- merged_con
  merged_con[, 1] <- new_row_names
  con_table <- merged_con[, c("expl.var", "con_Jan", "con_Feb", "con_Mar", "con_Apr", "con_May", "con_Jun", "con_Jul", "con_Aug", "con_Sep", "con_Oct", "con_Nov", "con_Dec")]
  se_table <- merged_con[, c("expl.var", "se_Jan", "se_Feb", "se_Mar", "se_Apr", "se_May", "se_Jun", "se_Jul", "se_Aug", "se_Sep", "se_Oct", "se_Nov", "se_Dec")]
  data_long <- reshape2::melt(con_table, id.vars = "expl.var", variable.name = "month", value.name = "value")
  data_se <- reshape2::melt(se_table, id.vars = "expl.var", variable.name = "month", value.name = "se")
  data_long$se <- data_se[, ncol(data_se)]
  data_long$month <- sub("con_", "", data_long$month)
  data_long$month <- factor(data_long$month, levels = month.abb) 
  # Filter the variables based on the condition
  selected_vars <- data_long %>%
    group_by(expl.var) %>%
    filter(any(value > 0.1)) %>%
    pull(expl.var) %>%
    unique()
  # Filter the data_long dataframe based on the selected variables
  filtered_data <- data_long %>%
    filter(expl.var %in% selected_vars)
  # Plot the filtered data
  vcp<- ggplot(filtered_data, aes(x = month, y = value*100, color = expl.var, group = expl.var)) +
    geom_line(linewidth = 0.8) +
    geom_errorbar(aes(ymin = (value - se)*100, ymax = (value + se)*100, width = 0.2)) +
    scale_color_manual(values = c("Nitrogen fertilizer"= brewer.pal(9 , "Blues")[4], "Potassium fertilizer" = brewer.pal(9 , "Blues")[6], "Phosphate fertilizer" = brewer.pal(9 , "Blues")[9], "Pesticide" = brewer.pal(9 , "BuPu")[6], "Irrigation fraction" = brewer.pal(9 , "BuPu")[8],
                                  "Forested primary land fraction" = brewer.pal(9 , "Greys")[4], "Managed pasture fraction" = brewer.pal(9 , "Greys")[5], "Rangeland fraction" = brewer.pal(9 , "Greys")[6], "Urban land fraction" = brewer.pal(9 , "Greys")[7],
                                  "C3 annual crops fraction" = brewer.pal(9 , "Greens")[4], "C3 perennial crops fraction" = brewer.pal(9 , "Greens")[5], "C4 annual crops fraction" = brewer.pal(9 , "Greens")[6], "C4 perennial crops fraction" = brewer.pal(9 , "Greens")[7],
                                  "Elevation" = "Black", "Mean temperature" = brewer.pal(9 , "YlOrBr")[4], "Temperature Monthly variation " = brewer.pal(9 , "YlOrBr")[6], "Precipitation" = brewer.pal(9 , "PuRd")[4], "Precipitation Monthly variation " = brewer.pal(9 , "PuRd")[6]),
                       breaks = new_row_names)+
    labs(x = specie, y = NULL, color = "Variable")+
    ylim (0, 80)
  varchangesplot[[specie]] <- vcp
}

combined_plot <-ggarrange(plotlist=varchangesplot, nrow =2, ncol =2,common.legend =TRUE, legend= "bottom")
combined_plot <- grid.arrange(combined_plot, top = "Variable Contributions of the Most Important Variables \n (Variables contributes more than 10% in at least one month)", left = "Variable Contribution (%)")
plot(combined_plot)