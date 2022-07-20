library(vegan)



######## PERMANOVA_env

adonis(env.log[,-c(1)]~type, data = env.log, method ="euclidean" )


####Permanova MPH Shore


adonis(MPH_shore[,-c(1,2)]~type, data = MPH_shore,  method = "bray")


##Permanova MPH emergent

MPH_emerg_No0 <- MPH_emerg[apply(MPH_emerg[,-c(1)], 1, function(x) !all(x==0)),] ## remove rows with only 0

adonis(MPH_emerg_No0[,-1]~type, data = MPH_emerg_No0,  method = "bray")


####Permanova MPH submerg

MPH_submerg_No0 <- MPH_submerg[apply(MPH_submerg[,-c(1)], 1, function(x) !all(x==0)),] ## remove rows with only 0

adonis(MPH_submerg_No0[,-1]~type, data = MPH_submerg_No0,  method = "bray")


###Permanova zp

###PERMANOVA

adonis(zp_data[,-c(1)]~Type, data = zp_data, method = "bray")


####Permanova Col

MI_Col_No0 <- MI_Col[apply(MI_Col[,-c(1,2)], 1, function(x) !all(x==0)),] ## remove rows with only 0


adonis(MI_Col_No0[,-c(1,2)]~type, data = MI_Col_No0,  method = "bray")



####Permanova Het

MI_Het_No0 <- MI_Het[apply(MI_Het[,-c(1,2)], 1, function(x) !all(x==0)),] ## remove rows with only 0

adonis(MI_Het_No0[,-c(1,2)]~type, data = MI_Het_No0,  method = "bray")



####Permanova Gast

MI_Gast_No0 <- MI_Gast[apply(MI_Gast[,-c(1,2)], 1, function(x) !all(x==0)),] ## remove rows with only 0

adonis(MI_Gast_No0[,-c(1,2)]~type, data = MI_Gast_No0,  method = "bray")
