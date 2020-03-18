
###############################################
#
#        Book Chapter: 'Crowdsourcing'
#
#      David Buil-Gil and Reka Solymosi
#
#         Coding begins: 10/01/2020
#          Coding ends: XX/XX/2020
#
#    Contact: david.builgil@manchester.ac.uk
#
###############################################

rm(list=ls())

### STEP 1 # Download Place Pulse data from FigShare repository ###

PP_data <- read.csv('https://ndownloader.figshare.com/files/21739137') #this may take some minutes

### STEP 2 # Study participation inequality in Place Pulse ###

Voter <- table(PP_data$voter_uniqueid) #create table of number of votes per participant

Voter <- as.data.frame(Voter) #convert table into dataframe

Voter_top1 <- subset(Voter, Freq > quantile(Freq, prob = 1 - 1/100)) #subset top 1% participants

sum(Voter_top1$Freq) / sum(Voter$Freq) * 100 #Proportion of votes by top 1% participants

Voter_top10 <- subset(Voter, Freq > quantile(Freq, prob = 1 - 10/100)) #subset top 10% participants

sum(Voter_top10$Freq) / sum(Voter$Freq) * 100 #Proportion of votes by top 10% participants

Voter_top25 <- subset(Voter, Freq > quantile(Freq, prob = 1 - 25/100)) #subset top 25% participants

sum(Voter_top25$Freq) / sum(Voter$Freq) * 100 #Proportion of votes by top 25% participants

N_votes <- as.data.frame(table(Voter$Freq)) #create dataframe of number of participants that voted X times

#install.packages("ineq") #we will need 'ineq' package to compute Gini index

library(ineq)

Gini(Voter$Freq) #Gini index where 0 is perfect equality and 1 is perfect inequality

Lc <- Lc(N_votes$Var1, n = N_votes$Freq) #Calculate Lorenz curve

plot(Lc, xlab = "Cumulative share of participants from lowest to higher number of votes",
     ylab = "Cumulative share of votes") #Plot Lorenz curve

### STEP 3 # Study participation descrease in Place Pulse ###

Days <- as.data.frame(table(PP_data$day)) #create dataframe of number of votes each day
# Note: some days have zero contributions

class(Days$Var1) #check class of the new variable 'Var1'

Days$Var1 <- as.Date(Days$Var1) #convert factor into date

All_days <- as.data.frame(seq(as.Date(min(Days$Var1)), as.Date(max(Days$Var1)), by = "days")) #create dataframe with all days between website launch and closing

colnames(All_days)[1] <- "Var1" #rename first column to 'Var1' before we merge it with the 'Days' dataset

All_days <- merge(All_days, Days, by = "Var1", all.x = TRUE) #merge 'All_days' and 'Days' datasets

All_days$Freq[is.na(All_days$Freq)] <- 0 #Days with NA votes recoded to 0

All_days$X <- (1:nrow(All_days)) #create new column from 1 to last day

#install.packages("ggplot2") #we will need ggplot2 to plot participation decrease

library(ggplot2)

ggplot(All_days, aes(X, Freq)) + geom_line() + geom_smooth(lwd = 1.5, col = "red") + theme_bw() + 
  xlab("Days since website launch") + ylab("Number of votes") #plot participation decrease
#Note: large peak beginning on July 24th 2013: publication of Salesses et al. (2013) on July 24th and press release via MIT News (http://news.mit.edu/2013/quantifying-urban-perceptions-0724)
#Note: large peak beginning on October 15th 2014: publication of Harvey's (2014) MSc thesis

### STEP 4 # Subset 'safer' votes in Atlanta ###

PP_Atl <- PP_data[ which(PP_data$place_name_right == "Atlanta" | PP_data$place_name_left == "Atlanta"), ]

PP_Atl_s <- PP_Atl[ which(PP_Atl$study_question == "safer"), ]

### STEP 5 # Download Atlanta Region Census Tracts ###

#install.packages("sf") #we will need 'sf' package for mapping

library(sf)

setwd("E:/Fellowship Manchester/Papers/Chapter crowdsourcing Reka/Data") #set working directory to download shapefiles

download.file("https://opendata.arcgis.com/datasets/547507d21cff42a9a2c654fb80f04740_17.zip", 
              destfile = "Atlanta_map.zip" , mode='wb') #Download shapefile from from Atlanta Regional Commission

unzip("Atlanta_map.zip", exdir = ".") #unzip file

file.remove("Atlanta_map.zip") #delete original zip file

Atlanta_map <- st_read("Population_by_Census_Tract_2017.shp") #load shapefile

head(Atlanta_map, n = 4)  #list first 4 attribute records

### STEP 6 # Plot map ###

map <- ggplot(data = Atlanta_map) + geom_sf() + theme_void() +
  coord_sf(xlim = c(-83.7, -85), ylim = c(33.2, 34.5), expand = FALSE) #create map

map + geom_point(data = PP_Atl_s, aes(x = lat_right, y = long_right), size = .1) +
  geom_point(data = PP_Atl_s, aes(x = lat_left, y = long_left), size = .1) #plot map with points

### STEP 7 # Data cleaning before analysing spatial patterns ###

table(PP_Atl_s$place_name_right == "Atlanta" & PP_Atl_s$place_name_left == "Atlanta") #count votes in which both images are from Atlanta

PP_Atl_s <- PP_Atl_s[order(PP_Atl_s$X), ] #order file by vote number

PP_Atl_s_dup <- PP_Atl_s[which(PP_Atl_s$place_name_right == "Atlanta" & PP_Atl_s$place_name_left == "Atlanta"), ] #create new dataset of votes in which both images are from Atlanta (We will duplicate them)

PP_Atl_s$long_Atl[PP_Atl_s$place_name_left == "Atlanta" & PP_Atl_s$place_name_right != "Atlanta"] <- PP_Atl_s$long_left[PP_Atl_s$place_name_left == "Atlanta" & PP_Atl_s$place_name_right != "Atlanta"] #allocate coordinates to votes in which left image is from Atlanta
PP_Atl_s$lat_Atl[PP_Atl_s$place_name_left == "Atlanta" & PP_Atl_s$place_name_right != "Atlanta"] <- PP_Atl_s$lat_left[PP_Atl_s$place_name_left == "Atlanta" & PP_Atl_s$place_name_right != "Atlanta"]

PP_Atl_s$long_Atl[PP_Atl_s$place_name_right == "Atlanta" & PP_Atl_s$place_name_left != "Atlanta"] <- PP_Atl_s$long_right[PP_Atl_s$place_name_right == "Atlanta" & PP_Atl_s$place_name_left != "Atlanta"] #allocate coordinates to votes in which right image is from Atlanta
PP_Atl_s$lat_Atl[PP_Atl_s$place_name_right == "Atlanta" & PP_Atl_s$place_name_left != "Atlanta"] <- PP_Atl_s$lat_right[PP_Atl_s$place_name_right == "Atlanta" & PP_Atl_s$place_name_left != "Atlanta"]

PP_Atl_s$long_Atl[PP_Atl_s$place_name_right == "Atlanta" & PP_Atl_s$place_name_left == "Atlanta"] <- PP_Atl_s$long_left[PP_Atl_s$place_name_right == "Atlanta" & PP_Atl_s$place_name_left == "Atlanta"] #allocate coordinates from left image to votes in which both images are from Atlanta
PP_Atl_s$lat_Atl[PP_Atl_s$place_name_right == "Atlanta" & PP_Atl_s$place_name_left == "Atlanta"] <- PP_Atl_s$lat_left[PP_Atl_s$place_name_right == "Atlanta" & PP_Atl_s$place_name_left == "Atlanta"]

PP_Atl_s_dup$long_Atl <- PP_Atl_s_dup$long_right #allocate coordinates from right image to votes in which both images are from Atlanta
PP_Atl_s_dup$lat_Atl <- PP_Atl_s_dup$lat_right

PP_Atl_s <- rbind(PP_Atl_s, PP_Atl_s_dup) #merge duplicated cases where both images are from Atlanta

PP_Atl_s$win[PP_Atl_s$long_right == PP_Atl_s$long_Atl & PP_Atl_s$choice == "right"] <- 1 #code 'safer' votes as 1 and 'less safe' and 'equal' as 0
PP_Atl_s$win[PP_Atl_s$long_left == PP_Atl_s$long_Atl & PP_Atl_s$choice == "left"] <- 1

PP_Atl_s$win[PP_Atl_s$long_right == PP_Atl_s$long_Atl & PP_Atl_s$choice == "left"] <- 0
PP_Atl_s$win[PP_Atl_s$long_right == PP_Atl_s$long_Atl & PP_Atl_s$choice == "equal"] <- 0

PP_Atl_s$win[PP_Atl_s$long_left == PP_Atl_s$long_Atl & PP_Atl_s$choice == "right"] <- 0
PP_Atl_s$win[PP_Atl_s$long_left == PP_Atl_s$long_Atl & PP_Atl_s$choice == "equal"] <- 0

table(PP_Atl_s$win) #count frequency of 'safer' votes in Atlanta against 'equal' and 'less safe' votes
prop.table(table(PP_Atl_s$win))*100 #count % of 'safer' votes in Atlanta against 'equal' and 'less safe' votes

### STEP 8 # Create map of proprtion of 'safer' votes per census tract ###

CRS <- st_crs(Atlanta_map) #retrieve coordinate reference system from the map of Atlanta

Points_Atl_s <- st_as_sf(PP_Atl_s, coords = c("lat_Atl", "long_Atl"), crs = CRS) #geocode 'safe' votes in Atlanta

st_crs(Points_Atl_s) == st_crs(Atlanta_map) #check if coordinate reference system is the same of both layers

map + geom_sf(data = Points_Atl_s) #visualise points on the map

#install.packages("rgeos") #we will need 'rgeos' package to overlay points and polygons

library(rgeos)

overlay_geo <- over(as_Spatial(Points_Atl_s), as_Spatial(Atlanta_map)) #overlay points (votes) and polygons (census tracts)

overlay_geo$X <- rownames(overlay_geo) #retrieve row names as a new column

Points_Atl_s2 <- merge(Points_Atl_s, overlay_geo, by = "X") #merge overlay with points file

Safe_areas <- aggregate(Points_Atl_s2$win, by=list(Points_Atl_s2$OBJECTID), FUN=mean) #calculate proportion of 'safer' votes per census tract
#Note: Salesses et al. (2013) suggest computing a Q-score corrected by the "win" and "loss" ratio of imagines with which each vote is compared
#for the purpose of this chapter we will compute a simple proportion
colnames(Safe_areas)[1] <- "OBJECTID" #rename new variables
colnames(Safe_areas)[2] <- "Safety"

summary(Safe_areas$Safety) #descriptive statistics of proportion of 'safer' votes per tract

Votes_areas <- aggregate(Points_Atl_s2$win, by=list(Points_Atl_s2$OBJECTID), FUN=length) #calculate sample size of votes per tract
colnames(Votes_areas)[1] <- "OBJECTID" #rename new variables
colnames(Votes_areas)[2] <- "n"

summary(Votes_areas$n) #Descriptive statisitcs of sample size per tract
#Note: Buil-Gil et al. (2020) propose a new method to compute estimates for areas with small sample sizes

Atlanta_map2 <- merge(Atlanta_map, Safe_areas, by = "OBJECTID") #merge proportion of 'safe' votes and census tracts polygons

#install.packages("cartography") #we will need 'cartography' package to plot coropleth map

library(cartography)

choroLayer(spdf = as_Spatial(Atlanta_map2), df = as_Spatial(Atlanta_map2),
           var = "Safety" , legend.pos = "bottomright", legend.values.rnd = 3,
           nclass = 5, method = "quantile", legend.title.txt = "Quantiles") #plot coropleth map of proportion of 'safer' votes per census tract
title("Proportion of 'safer' votes per tract (Place Pulse)")

### STEP 9 # Compute Moran's I index ###

#install.packages("spdep") #we will need 'spdep' package to compute proximity matrix
#install.packages("spatialreg") #we will need 'spatialreg' to fit spatial models

library(spdep)

nb <- poly2nb(Atlanta_map2, queen = TRUE) #compute neighbors list based on 'Queen' contiguity approach

prox <- nb2listw(nb, style = "W", zero.policy = TRUE) #compute proximity matrix (all neighbours have equal weights)

moran.test(Atlanta_map2$Safety, prox) #compute Moran's I test (p-value derived analytically)

moran.mc(Atlanta_map2$Safety, prox, nsim = 1000) #compute Moran's I test (p-value derived from Monte Carlo simulation)

### STEP 10 # Download key statistics for census tracts 2017 ###

Migration <- read.csv("https://opendata.arcgis.com/datasets/738458ea84ae4d5ea412a843960a99b4_637.csv") #download migration data
Migration <- Migration[c("GEOID", "pMoved_e")] #select variable % Moved in the last year, 2017

Ethnicity <- read.csv("https://opendata.arcgis.com/datasets/ad1c598875644ad1b8a0397107091d1c_37.csv") #download ethnicity data
Whites <- Ethnicity[c("GEOID", "pNHWhite_e")] #select variable % Not Hispanic, White alone, 2017
Blacks <- Ethnicity[c("GEOID", "pNHBlack_e")] #select variable % Not Hispanic, Black or African American alone, 2017

Occ_houses <- read.csv("https://opendata.arcgis.com/datasets/4692d4db01a14b92965addc80a00cd94_317.csv") #download housing data
Occ_houses <- Occ_houses[c("GEOID", "pOccHU_e")] #select variable % Occupied housing units, 2017

rent1000 <- read.csv("https://opendata.arcgis.com/datasets/db14dda46ba845e0b4081e67d647e15b_337.csv") #download rent data
rent1000 <- rent1000[c("GEOID", "pGrent1000P_e")] #select variable % Gross rent $1,000 or more, 2017

### STEP 11 # Fit spatial models to explain spatial distribution of 'safer' votes ###

Exp_variables <- merge(Migration, Whites, by = "GEOID") #merge all independent variables in a dataset
Exp_variables <- merge(Exp_variables, Blacks, by = "GEOID")
Exp_variables <- merge(Exp_variables, Occ_houses, by = "GEOID")
Exp_variables <- merge(Exp_variables, rent1000, by = "GEOID")

Atlanta_map3 <- merge(Atlanta_map2, Exp_variables, by = "GEOID", all.x = TRUE) #merge dependent and independent variables

Atlanta_map3$Safety <- Atlanta_map3$Safety*100 #calculate % of 'safer' votes per tract

Atlanta_map3$Safety_s <- (Atlanta_map3$Safety-mean(Atlanta_map3$Safety))/sd(Atlanta_map3$Safety) #standardise all variables to facilitate interpretation of coefficients
Atlanta_map3$pMoved_e_s <- (Atlanta_map3$pMoved_e-mean(Atlanta_map3$pMoved_e))/sd(Atlanta_map3$pMoved_e)
Atlanta_map3$pNHWhite_e_s <- (Atlanta_map3$pNHWhite_e-mean(Atlanta_map3$pNHWhite_e))/sd(Atlanta_map3$pNHWhite_e)
Atlanta_map3$pNHBlack_e_s <- (Atlanta_map3$pNHBlack_e-mean(Atlanta_map3$pNHBlack_e))/sd(Atlanta_map3$pNHBlack_e)
Atlanta_map3$pOccHU_e_s <- (Atlanta_map3$pOccHU_e-mean(Atlanta_map3$pOccHU_e))/sd(Atlanta_map3$pOccHU_e)
Atlanta_map3$pGrent1000P_e_s <- (Atlanta_map3$pGrent1000P_e-mean(Atlanta_map3$pGrent1000P_e))/sd(Atlanta_map3$pGrent1000P_e)

model <- lm(formula = Safety_s ~ pMoved_e_s +
            pNHWhite_e_s + pNHBlack_e_s + pOccHU_e_s + pGrent1000P_e_s,
            data = Atlanta_map3) #fit non-spatial linear model (OLS)

summary(model) #obtain model results

model.lag <- spatialreg::lagsarlm(formula = Safety_s ~ pMoved_e_s +
                      pNHWhite_e_s + pNHBlack_e_s + pOccHU_e_s + pGrent1000P_e_s,
                      data = Atlanta_map3, prox, zero.policy = T) #fit spatial lag model

summary(model.lag) #obtain model results

model.error <- spatialreg::errorsarlm(formula = Safety_s ~ pMoved_e_s +
                          pNHWhite_e_s + pNHBlack_e_s + pOccHU_e_s + pGrent1000P_e_s,
                          data = Atlanta_map3, prox, zero.policy = T) #fit spatial error model

summary(model.error)#obtain model results
 
logLik(model) #compare model results using log-likelihood
logLik(model.lag)
logLik(model.error)

anova(model.lag, model) #compare model results using anova
anova(model.error, model)
#Note: OLS model explains better our model than spatial models

### STEP 12 ### Visualise the distribution of the main explanatory variables ###

par(mfrow=c(2,2))

choroLayer(spdf = as_Spatial(Atlanta_map3), df = as_Spatial(Atlanta_map3), legend.values.cex = 0.7,
           var = "Safety" , legend.pos = "bottomright", legend.values.rnd = 1,
           nclass = 3, method = "quantile", legend.title.txt = "Quantiles") #plot coropleth map of proportion of 'safer' votes per census tract
title("Perceived safety (Place Pulse)")

choroLayer(spdf = as_Spatial(Atlanta_map3), df = as_Spatial(Atlanta_map3), legend.values.cex = 0.7,
           var = "pMoved_e" , legend.pos = "bottomright", legend.values.rnd = 1,
           nclass = 3, col = carto.pal(pal1 = "wine.pal", n1 = 3), method = "quantile", 
           legend.title.txt = "Quantiles") #plot coropleth map of proportion of 'safer' votes per census tract
title("% Moved in the last year")

choroLayer(spdf = as_Spatial(Atlanta_map3), df = as_Spatial(Atlanta_map3), legend.values.cex = 0.7,
           var = "pNHWhite_e" , legend.pos = "bottomright", legend.values.rnd = 1,
           nclass = 3, col = carto.pal(pal1 = "orange.pal", n1 = 3), method = "quantile",
           legend.title.txt = "Quantiles") #plot coropleth map of proportion of 'safer' votes per census tract
title("% Whites")

choroLayer(spdf = as_Spatial(Atlanta_map3), df = as_Spatial(Atlanta_map3), legend.values.cex = 0.7,
           var = "pNHBlack_e" , legend.pos = "bottomright", legend.values.rnd = 1,
           nclass = 3, col = carto.pal(pal1 = "purple.pal", n1 = 3), method = "quantile",
           legend.title.txt = "Quantiles") #plot coropleth map of proportion of 'safer' votes per census tract
title("% Blacks")
