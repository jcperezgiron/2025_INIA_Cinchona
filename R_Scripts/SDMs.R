# Script to perform a species distribution model using the Maxent algorithm

# Load the required libraries
library(tidyverse)
library(sf)
library(biomod2)
library(terra)
library(usdm) # check multicollinearity
library(covsel) # check multicollinearity

#### Global params ####

# initial time
tmp <- Sys.time()

# define the output directory
output_dir <- file.path("C:/SCIENCE/2025_INIA_Cinchona/SDM_output")

# create output directory if it does not exist.
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# make a copy of maxent.jar to the output directory
file.copy("./maxent/maxent.jar", output_dir)

# set the working directory to the output directory
setwd(output_dir)


#### 1.- Read presences ####
presences <- st_read("../Data/Data_C_Officinalis/Data_C_Officinalis.gdb") %>%
  st_transform(4326) %>% 
  mutate(cinchona = 1) %>%
  select(cinchona)

#### 2.- Read environmental data ####

predictor_list <- list.files("../Predictors/", pattern = ".tif$", full.names = T)

rasStack <- rast(predictor_list)

#### 3.- Check multicollinearity ####
rasValue <- presences %>%
  terra::extract(rasStack, .) %>%
  as.data.frame() %>% # Convierto a df
  dplyr::select(-ID) %>% 
  drop_na()

# By spearman correlation of 0.7
v1 <- vifcor(rasValue, th = 0.7, method = "spearman", keep = c("soc", "phh2o", "bdod"))
v1

rasStack_1 <- exclude(rasStack, v1) # remove the variables with high correlation

rasValue <- presences %>%
  terra::extract(rasStack_1, .) %>%
  as.data.frame() %>% # Convierto a df
  dplyr::select(-ID) %>% 
  drop_na()


# By variance inflation factor of 10
v2 <- vifstep(rasValue, th = 4)
v2

rasStack_2 <- exclude(rasStack_1, v2) # remove the variables with high correlation

#### 4.- Modelling parameters ####
# Select the name of the studied species
myRespName <- "cinchona"

# Get corresponding presence/absence data
myResp <- presences %>%
  vect()

# Environmental variables selected after correlation
myExpl <- rasStack_2

# PARAMETERS
myBiomodData <- BIOMOD_FormatingData(
  resp.var = myResp,
  expl.var = myExpl,
  resp.name = myRespName,
  PA.nb.rep = 10, # 1 to test processing time; 10 to run models
  PA.nb.absences = 10000, # Random number; 10000 to run models
  PA.strategy = "random",
  na.rm = T,
  filter.raster = T,
  dir.name = getwd()
)

myBiomodData

# Create the different validation datasets
# k-fold selection
cv.k <- bm_CrossValidation(
  bm.format = myBiomodData,
  strategy = "kfold",
  nb.rep = 1,
  k = 10
)

# Specify the models to be run
models <- c("MAXENT") # species presence-only methods

# Configure the models
user.MAXENT <- list("for_all_datasets" = list(
  memory_allocated = 1024
))

user.val <- list(MAXENT.binary.MAXENT.MAXENT = user.MAXENT)

opt.u <- bm_ModelingOptions(
  data.type = "binary",
  models = models,
  strategy = "user.defined",
  user.val = user.val,
  bm.format = myBiomodData,
  calib.lines = cv.k
)



#### 5.- Model fitting ####
## Single models
myBiomodModelOut <- BIOMOD_Modeling(
  bm.format = myBiomodData,
  models = models,
  OPT.strategy = "user.defined",
  OPT.user.val = user.val,
  OPT.user.base = "default",
  CV.do.full.models = FALSE,
  CV.strategy = "user.defined",
  CV.user.table = cv.k,
  var.import = 10,
  metric.eval = c("BOYCE"),
  prevalence = NULL,
  seed.val = 123,
  do.progress = F,
  nb.cpu = 8
)

myBiomodModelOut

# Get evaluation scores & variables importance
single_eval <- get_evaluations(myBiomodModelOut)
single_eval

single_varimp <- get_variables_importance(myBiomodModelOut)

# save the evaluation scores& variables importance
write.csv(single_eval, file.path(output_dir, "single_eval.csv"), row.names = F)
write.csv(single_varimp, file.path(output_dir, "single_varimp.csv"), row.names = F)

# Ensemble models


myBiomodEM <- BIOMOD_EnsembleModeling(
  bm.mod = myBiomodModelOut,
  models.chosen = "all",
  em.by = "algo",
  em.algo = c("EMmedian"),
  metric.select = c("BOYCE"),
  metric.select.thresh = c(0.7),
  metric.eval = c("BOYCE"),
  var.import = 10,
  seed.val = 123,
  nb.cpu = 8
)
myBiomodEM

# Get evaluation scores & variables importance
ensemble_eval <- get_evaluations(myBiomodEM)
ensemble_eval

ensemble_varimp <- get_variables_importance(myBiomodEM)

# save the evaluation scores
write.csv(ensemble_eval, file.path(output_dir, "ensemble_eval.csv"), row.names = F)
write.csv(ensemble_varimp, file.path(output_dir, "ensemble_varimp.csv"), row.names = F)

#### 6.- Project current models ####
# Project single models
myBiomodProj <- BIOMOD_Projection(
  bm.mod = myBiomodModelOut,
  proj.name = "Current",
  new.env = myExpl,
  models.chosen = "all",
  metric.binary = "all",
  metric.filter = "all",
  build.clamping.mask = TRUE
)

myBiomodEMProj <- BIOMOD_EnsembleForecasting(
  bm.em = myBiomodEM,
  bm.proj = myBiomodProj,
  proj.name = "Current_EM",
  models.chosen = "all",
  metric.binary = "all",
  metric.filter = "all",
  on_0_1000 = FALSE,
  nb.cpu = 8,
  na.rm = FALSE
)


myBiomodEMProj


# Load the models output and ensemble models output
myBiomodModelOut <- get(load("C:/SCIENCE/2025_INIA_Cinchona/SDM_output/cinchona/cinchona.1748044986.models.out"))
myBiomodEM <- get(load("C:/SCIENCE/2025_INIA_Cinchona/SDM_output/cinchona/cinchona.1748044986.ensemble.models.out"))

# Response curves
response_curves_data <- bm_PlotResponseCurves(bm.out = myBiomodModelOut, 
                      models.chosen = "all",
                      fixed.var = 'median')$tab

write.csv(response_curves_data, file.path(output_dir, "response_curves.csv"), row.names = F)

#### 7.- Project onto future conditions ####
scenarios <- c("ssp370", "ssp585") # RCP scenarios
years <- c("2011-2040", "2041-2070", "2071-2100") # years to project


for (year in years) {
  for (scenario in scenarios) {
    # read climate projections and correct names to match rasStack_1
    list.climProj <- list.files(file.path("../Predictors_climate_projections", scenario, year), pattern = ".tif$", full.names = T, recursive = T)
    climproj <- rast(c(list.climProj))
    
    # Load static vars based as the difference with the climatic vars
    static_vars <- subset(rasStack_2, setdiff(names(rasStack_2), names(climproj)))
    
    # Stack static and climatic vars
    myExplFuture <- c(climproj, static_vars)
    
    # Select only the layers that were included in rasStack_1
    myExplFuture <- subset(myExplFuture, names(rasStack_1))
    
    # Project onto future conditions
    myBiomodProjectionFuture <- BIOMOD_Projection(bm.mod = myBiomodModelOut,
                                                  proj.name = paste0(scenario, "_", year),
                                                  new.env = myExplFuture,
                                                  models.chosen = 'all',
                                                  metric.binary = 'BOYCE',
                                                  metric.filter = 'BOYCE',
                                                  build.clamping.mask = TRUE)
    
    # Project ensemble models
    myBiomodEMProjectionFuture <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                                             bm.proj = myBiomodProjectionFuture,
                                                             proj.name = paste0(scenario, "_", year, "_EM"),
                                                             models.chosen = 'all',
                                                             metric.binary = 'BOYCE',
                                                             metric.filter = 'BOYCE',
                                                             on_0_1000 = FALSE,
                                                             nb.cpu = 8,
                                                             na.rm = FALSE
    )
    
  }
}

# End time
Sys.time() - tmp
