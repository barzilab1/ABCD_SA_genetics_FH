library(readr)
library(data.table)
library(plyr) 
library(psych)

demographics_baseline <- read_csv("outputs/demographics_baseline.csv")
family <- read_csv("outputs/family.csv")
genetic <- read_csv("outputs/genetic.csv")
site <- read_csv("outputs/site.csv")

family_history_items = read_csv("outputs/family_item_set.csv")

demographics_long <- read_csv("outputs/demographics_long.csv")
suicide_set <- read_csv("outputs/suicide_set.csv")



###### merge ######

# merge demo from 1&2 with site
dataset = merge(demographics_long[demographics_long$eventname %in% c("1_year_follow_up_y_arm_1", "2_year_follow_up_y_arm_1"),], site, all = T)


# new variable to use in reshape from long to wide format
dataset$timepoint = regmatches(dataset$eventname, regexpr(".*_year", dataset$eventname))
dataset$eventname = NULL
dataset$demo_prim_l = NULL
dataset_wide = reshape(dataset, direction = "wide", idvar = c("src_subject_id", "sex"), timevar = "timepoint", sep = "_")

#remove empty columns
dataset_wide = dataset_wide[,colSums(is.na(dataset_wide)) != nrow(dataset_wide)]

#remove sex variables ( not consistent cross timepoints because of NA)
dataset_wide[,c("sex_br_1_year","sex_br_2_year")] = NULL

#add baseline and genetic features
dataset_baseline = merge(demographics_baseline, family_history_items)
dataset_baseline = merge(dataset_baseline, family[,c("src_subject_id", "sex", "rel_family_id")] )

dataset_baseline$sex[dataset_baseline$src_subject_id == "NDAR_INV3Z5E0931"] = "F"
dataset_baseline$sex_br[dataset_baseline$src_subject_id == "NDAR_INV3Z5E0931"] = 1

dataset_wide = merge(dataset_baseline, dataset_wide)
dataset_wide = merge(dataset_wide, genetic )


###### suicide ######
# fix sex in 4.0
suicide_set$sex[suicide_set$src_subject_id == "NDAR_INV3Z5E0931"] = "F"

# create SA across 3 timepoints 
suicide_set$timepoint = regmatches(suicide_set$eventname, regexpr(".*_year", suicide_set$eventname))
suicide_set$eventname = NULL
suicide_wide = reshape(as.data.frame(suicide_set), direction = "wide", idvar = c("src_subject_id", "sex"), timevar = "timepoint", sep = "_")

#' 1 = if the kid has ever said “yes” to any of the questions of SA (no matter if he answered all questions or no),
#' 0 = if the kid said no to all questions. 
#' NA = In case of missing data and the kid didn’t answer “Yes” to any question related to SA
suicide_wide$SA_y_ever = apply(suicide_wide[,grep("SA_y", colnames(suicide_wide))], 1, 
                               function(r){ any(r == 1)*1 })

# select only kids that have value in SA ever
suicide_wide = suicide_wide[!is.na(suicide_wide$SA_y_ever), c("src_subject_id", "sex", "SA_y_ever", "SA_y_2_year", "SA_y_1_year", "SA_y_baseline_year")]

dataset_wide = merge(dataset_wide,suicide_wide)

###### FH ######
# select only kids that have FH
dataset_wide = dataset_wide[!is.na(dataset_wide$famhx_ss_momdad_scd_p),]




###### one of family ######
# select one of a family for those who have 2 year follow up
setDT(dataset_wide)
# check number of kids in each timepoint 
dataset_wide[,describe(.SD), .SDcols = c("age_baseline", "age_1_year", "age_2_year")]
# check number of families in 2 year follow up ==> 5193 families in 2 year follow up
dataset_wide[!is.na(age_2_year), length(unique(FID_genetic))]
set.seed(131)
one_family_member_2_year = dataset_wide[, .SD[which.max(age_2_year)] ,by = FID_genetic]

# kids that don't have 2 year follow up (and their family wasn't selected yet )
dataset_wide_no_2_year = dataset_wide[is.na(age_2_year) & !(FID_genetic %in% one_family_member_2_year$FID_genetic) ,]
# check if there are families/siblings
which(duplicated(dataset_wide_no_2_year$FID_genetic)) 
# as there are no duplicates -> no siblings -> we can add all the kids to the dataset


one_family_member = rbind.fill(one_family_member_2_year, dataset_wide_no_2_year)



###### imputation of age at 2 year ######
setDT(one_family_member)
one_family_member[,describe(.SD), .SDcols = c("age_baseline", "age_1_year", "age_2_year")]

# calculate the mean diff between 2 year and both 1 year and baseline
one_family_member[,diff_baseline_2year := age_2_year - age_baseline]
one_family_member[,diff_1year_2year := age_2_year - age_1_year]
one_family_member[,describe(.SD), .SDcols = c("age_baseline", "age_1_year", "age_2_year", "diff_baseline_2year", "diff_1year_2year")]
mean_1year_2year = one_family_member[, colMeans(.SD, na.rm = T), .SDcols = "diff_1year_2year"]
mean_baseline_2year = one_family_member[, colMeans(.SD, na.rm = T), .SDcols = "diff_baseline_2year"]

# add  mean to the last known age
one_family_member[is.na(age_2_year) & !is.na(age_1_year), age_2_year := age_1_year + mean_1year_2year ]
one_family_member[is.na(age_2_year) & is.na(age_1_year) & !is.na(age_baseline), age_2_year := age_baseline + mean_baseline_2year ]


write.csv(file = "outputs/dataset_SA_genetics_FH.csv",x = dataset_wide, row.names = F, na = "")
write.csv(file = "outputs/one_family_member.csv",x = one_family_member, row.names = F, na = "")



