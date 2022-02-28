
source("config.R")
source("utility_fun.R")


########### School Risk and Protective Factors ########### 
rhds01 = load_instrument("abcd_rhds01",abcd_files_path)

rhds01_lean = rhds01[, grepl("^(src|interview|event|sex)|reshist_(state|addr1_(valid|status|d1a|walkindex|grndtot|p1|drugtot|drgsale|mjsale|drgposs|dui|years|elevation|adi|popdensity))", colnames(rhds01))]


#TODO check for each time point
#remove columns with more than 20% NA
# rhds01 = droplevels(rhds01[rhds01$eventname == "baseline_year_1_arm_1",])
# rhds01 = rhds01[,-which(colSums(is.na(rhds01)) >= 0.2*dim(rhds01)[1])]

summary(rhds01_lean[rhds01_lean$eventname %in% c("2_year_follow_up_y_arm_1", "1_year_follow_up_y_arm_1"),])
summary(rhds01_lean)

write.csv(file = "outputs/geo_data.csv",x = rhds01_lean, row.names = F, na = "")


