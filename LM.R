
library("lme4")
library(mobr)


##### Creat biodiversity statistics

ZP_S <- calc_biodiv(zp_data[,-1], zp_data$Type, index = "S")

ZP_Sn <- calc_biodiv(zp_data[,-1], zp_data$Type, index="S_n", effort = 5, extrapolate = TRUE, return_NA = FALSE)

ZP_PIE <- calc_biodiv(zp_data[,-1], zp_data$Type, index="S_PIE", effort = 5, extrapolate = TRUE)


#### data frame

agri_land <- data.frame(crop_200 = landuse$Cropland_200, grass_200 = landuse$agricultural_grassland_200)

agri_land$total_agri_200 = rowSums(agri_land)

zp_biodiv <- data.frame(type = zp_data$Type, S = ZP_S$value, Sn = ZP_Sn$value , 
                        SPIE = ZP_PIE$value, crop_200 = landuse$Cropland_200, grass_200 = landuse$agricultural_grassland_200, 
                        fish = fish_data$PA_fish)



####Linear mixed model


#############################
##############################

####Species Richness (S)


Model1_zp_s <- glm(S~type + crop_200 + grass_200 + fish , family = poisson(link=log) , data = zp_biodiv)

Anova(Model1_zp_s)
summary(Model1_zp_s)
plot(allEffects(Model1_zp_s))


#### Check assumptions

plot(Model1_zp_s)



######################
######### Rarefied richness (S_n)


Model1_zp_Sn <- lm(log(Sn)~type + crop_200 + grass_200 + fish ,data = zp_biodiv)

Anova(Model1_zp_Sn)
summary(Model1_zp_Sn)
plot(allEffects(Model1_zp_Sn))


#### Check assumptions

plot(Model1_zp_Sn)
#To test for normality of the residuals (in case of a Gaussian model):

qqnorm(residuals(Model1_zp_Sn))
qqline(residuals(Model1_zp_Sn)) # check if the data are approx. aligning with the line
hist(residuals(Model1_zp_Sn)) #alternative: check visually the histogram of the residuals
shapiro.test(residuals(Model1_zp_Sn)) #alternative: do Shapiro Wilk test, W should be > 0.9




######################
######### S_PIE


Model1_zp_spie <- lm(log(SPIE)~type + crop_200 + grass_200 + fish ,data = zp_biodiv)

Anova(Model1_zp_spie)
summary(Model1_zp_spie)
plot(allEffects(Model1_zp_spie))


#### Check assumptions

plot(Model1_zp_spie)
#To test for normality of the residuals (in case of a Gaussian model):
qqnorm(residuals(Model1_zp_spie))
qqline(residuals(Model1_zp_spie)) # check if the data are approx. aligning with the line
hist(residuals(Model1_zp_spie)) #alternative: check visually the histogram of the residuals
shapiro.test(residuals(Model1_zp_spie)) #alternative: do Shapiro Wilk test, W should be > 0.9

