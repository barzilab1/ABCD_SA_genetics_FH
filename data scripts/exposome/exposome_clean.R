
source("config.R")
source("utility_fun.R")


########### Parent Family History Summary Scores ########### 
fhxssp = load_instrument("abcd_fhxssp01",abcd_files_path)
fhxssp_item = fhxssp[,grepl("^(src|sex|inter|event)|momdad|parent", colnames(fhxssp))]


########### family relationship section ########### 
acspsw03 = load_instrument("acspsw03",abcd_files_path)
acspsw03 = acspsw03[acspsw03$eventname == "baseline_year_1_arm_1",grepl("src|inter|sex|event|fam", colnames(acspsw03))]

summary(acspsw03)




write.csv(file = "outputs/family.csv",x = acspsw03, row.names=F, na = "")
write.csv(file = "outputs/family_item_set.csv",x = fhxssp_item, row.names = F, na = "")



