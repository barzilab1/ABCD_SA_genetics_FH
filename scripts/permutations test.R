library(readr)

###############
# read dataset
###############
dataset_genetics = read_csv("outputs/one_family_member.csv")


##########################################
# regression with PRS age, sex and race 
#########################################
# original OR from the meta-analysis 
original_OR = 1.3075

distribution=c()
EUR_dataset = dataset_genetics[dataset_genetics$genetic_afr==0, ]
AFR_dataset = dataset_genetics[dataset_genetics$genetic_afr==1, ]

set.seed(123)
for(i in 1:9999){
  # 1. resample SA in each ancestry separately 
  new_SA_eur = sample(EUR_dataset$SA_y_ever, nrow(EUR_dataset), FALSE)
  new_SA_afr = sample(AFR_dataset$SA_y_ever, nrow(AFR_dataset), FALSE)
  
  #2. run model on each ancestry separately 
  modEUR = glm(new_SA_eur ~ age_2_year + sex_br + suicide_PRSice_Pt0_05, data = EUR_dataset, family = binomial)
  modAFR = glm(new_SA_afr ~ age_2_year + sex_br + suicide_PRSice_Pt0_05, data = AFR_dataset, family = binomial)
  
  #3. meta analysis 
  modMETA = run_meta_analysis("suicide_PRSice_Pt0_05", modEUR, modAFR)
  
  distribution[i] = exp(modMETA$TE.fixed)
}

two_side_p_value = 2 * (sum(distribution >= original_OR)/(10000))
two_side_p_value

tiff("permutations.tiff", units="in", width=6, height=6, res=300)
hist(distribution, breaks=50, col='grey', main="", las=1, xlab='Odds ratios')
abline(v=original_OR, lwd=3, col="red")
dev.off()


