library(mobr)


#####Creat mob_in object

MPH_emerg_mob_in <- make_mob_in(MPH_emerg_data, env_coord, coord_names = c('X_coord','Y_coord'), latlong = TRUE)


#### MoB statistics


MPH_emerg_specrich <- get_mob_stats(MPH_emerg_mob_in, group_var = "type", index = "S" , boot_groups = TRUE) ### S = species richness  

MPH_emerg_beta_s <- MPH_emerg_s[MPH_emerg_s$index =='beta_S',]