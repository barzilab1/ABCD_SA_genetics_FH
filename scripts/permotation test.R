###############
# read dataset
###############
dataset_genetics = read_csv("outputs/one_family_member.csv")


###################
# prepare the data
###################
attach(dataset_genetics)
# regress out age, sex and race from suicide_PRSice_Pt0_05
mod = lm(suicide_PRSice_Pt0_05 ~ age_2_year+ sex_br + race_black , na.action=na.exclude)
residuals = scale(residuals(mod,na.action=na.exclude))
colnames(residuals)[1] = "suicide_PRSice_Pt0_05_res"
dataset_genetics = data.frame(dataset_genetics,residuals)


detach()
attach(dataset_genetics)


######################
# Difference in means
######################
original = mean(suicide_PRSice_Pt0_05_res[SA_y_ever==1]) - mean(suicide_PRSice_Pt0_05_res[ SA_y_ever==0])

distribution=c()
set.seed(123)
for(i in 1:9999){
  distribution[i]=diff(by(suicide_PRSice_Pt0_05_res, sample(SA_y_ever, length(SA_y_ever), FALSE), mean))
}
two_side_p_value = sum(abs(distribution) >= abs(original))/(10000)
two_side_p_value

#Compare to t-test
t.test(suicide_PRSice_Pt0_05_res~SA_y_ever)

hist(distribution, breaks=50, col='grey', main="distribution of PRSice Pt0.05", las=1, xlab='')
abline(v=original, lwd=3, col="red")


##############################
# regression with PRS only 
##############################
mod = lm(SA_y_ever ~ suicide_PRSice_Pt0_05_res)
original_reg = coef(mod)["suicide_PRSice_Pt0_05_res"]
  
  
distribution=c()
set.seed(123)
for(i in 1:9999){
  new_SA = sample(SA_y_ever, length(SA_y_ever), FALSE)
  mod = lm(new_SA ~ suicide_PRSice_Pt0_05_res)
  distribution[i] = coef(mod)["suicide_PRSice_Pt0_05_res"]
}
two_side_p_value = sum(abs(distribution) >= abs(original_reg))/(10000)
two_side_p_value

hist(distribution, breaks=50, col='grey', main="distribution of PRSice Pt0.05", las=1, xlab='')
abline(v=original_reg, lwd=3, col="red")



##########################################
# regression with PRS age, sex and race 
#########################################
mod = lm(SA_y_ever ~ suicide_PRSice_Pt0_05+ age_2_year + sex_br + race_black)
original_reg2 = coef(mod)["suicide_PRSice_Pt0_05"]


distribution=c()
set.seed(123)
for(i in 1:9999){
  new_SA = sample(SA_y_ever, length(SA_y_ever), FALSE)
  mod = lm(new_SA ~ suicide_PRSice_Pt0_05 + age_2_year + sex_br + race_black)
  distribution[i] = coef(mod)["suicide_PRSice_Pt0_05"]
}
two_side_p_value = sum(abs(distribution) >= abs(original_reg2))/(10000)
two_side_p_value

hist(distribution, breaks=50, col='grey', main="distribution of PRSice Pt0.05", las=1, xlab='')
abline(v=original_reg2, lwd=3, col="red")






