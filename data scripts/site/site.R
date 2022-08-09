source("config.R")
source("utility_fun.R")

site <-  load_instrument("abcd_lt01",abcd_files_path)


site$site_id_l_br = sub("site","",site$site_id_l)
site[,c("sched_delay", "site_id_l")] = NULL
site = site[site$eventname %in% c("baseline_year_1_arm_1","1_year_follow_up_y_arm_1", "2_year_follow_up_y_arm_1" ),]

write.csv(file = "outputs/site.csv",x = site, row.names = F, na = "")

