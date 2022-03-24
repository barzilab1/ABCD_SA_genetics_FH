library(readr)

###############
# read dataset
###############
dataset_genetics = read_csv("outputs/one_family_member.csv")


##########################################
# regression with PRS age, sex and race 
#########################################
mod = glm(SA_y_ever ~ age_2_year + sex_br + race_black + suicide_PRSice_Pt0_05, data = dataset_genetics, family = binomial)
original_OR = exp(coef(mod)["suicide_PRSice_Pt0_05"])

distribution=c()
set.seed(123)
for(i in 1:9999){
  new_SA = sample(dataset_genetics$SA_y_ever, nrow(dataset_genetics), FALSE)
  mod = glm(new_SA ~ age_2_year + sex_br + race_black + suicide_PRSice_Pt0_05, data = dataset_genetics, family = binomial)
  distribution[i] = exp(coef(mod)["suicide_PRSice_Pt0_05"])
}
two_side_p_value = 2 * (sum(distribution >= original_OR)/(10000))
two_side_p_value

hist(distribution, breaks=50, col='grey', main="", las=1, xlab='Distribution of odds ratios of residual standardized PRS at p-value threshold of 0.05, \nfrom models built using permuted suicide attempt phenotype.')
abline(v=original_OR, lwd=3, col="red")






