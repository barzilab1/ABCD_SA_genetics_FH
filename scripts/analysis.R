library(meta)
library(sjPlot)
library(performance)

dataset = read.csv("outputs/one_family_member.csv")

#####################################
# table one
#####################################
library(tableone)

catVars <- c( "sex_br", "famhx_ss_momdad_scd_p", "genetic_afr", "race_white", "race_black")
myVars <- c("age_2_year" ,catVars , "suicide_PRSice_Pt0_01", "suicide_PRSice_Pt0_05")


table1 = CreateTableOne(data =dataset, vars = myVars,  factorVars = catVars, strata = "SA_y_ever", addOverall = T)

table1_print = print(table1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)
table1_print


# check the tests
chisq.test(table(dataset$sex_br,dataset$SA_y_ever))
chisq.test(table(dataset$famhx_ss_momdad_scd_p,dataset$SA_y_ever))
chisq.test(table(dataset$genetic_afr,dataset$SA_y_ever))
chisq.test(table(dataset$race_black,dataset$SA_y_ever))
chisq.test(table(dataset$race_white,dataset$SA_y_ever))

oneway.test(age_2_year ~ SA_y_ever, dataset)
t.test(age_2_year ~ SA_y_ever, dataset)
oneway.test(suicide_PRSice_Pt0_01 ~ SA_y_ever, dataset)
t.test(suicide_PRSice_Pt0_01 ~ SA_y_ever, dataset)
oneway.test(suicide_PRSice_Pt0_05 ~ SA_y_ever, dataset)
t.test(suicide_PRSice_Pt0_05 ~ SA_y_ever, dataset)


write.csv(as.data.frame(table1_print), "tables/table1.csv",na = "" )




# table 1 for race X gen ancestry 
race_variables = c("race_black", "race_white", "non_hispanic_black", "non_hispanic_white", "ethnicity_hisp", "race_mixed")
table1_race = CreateTableOne(data =dataset, vars = race_variables ,factorVars = race_variables,  strata = "genetic_afr", addOverall = T)
table1_race_print = print(table1_race, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)
table1_race_print

write.csv(as.data.frame(table1_race_print), "tables/table1_race.csv",na = "" )



#####################################
# help functions
#####################################
create_meta_df <- function (feature, model1, model2) {
  mod1_coef = summary(model1)$coef
  mod2_coef = summary(model2)$coef
  
  df = data.frame(
    TE = c(mod1_coef[, "Estimate"][feature], mod2_coef[, "Estimate"][feature]),
    seTE = c(mod1_coef[, "Std. Error"][feature], mod2_coef[, "Std. Error"][feature])
  )
}


run_meta_analysis <- function(IV, modEUR, modAFR){
  df = create_meta_df(IV, modEUR, modAFR)
  met = metagen(TE, seTE, studlab = c("EUR", "AFR"),  data = df, sm = "OR")
  met
}


run_models <- function(DV ,IV = NULL, covar = NULL) {
  
  formula_str = paste0(DV, " ~ age_2_year + sex_br")
  if(!is.null(IV)){
    formula_str = paste0(formula_str, "+", IV )
    if(!is.null(covar)){
      formula_str = paste0(formula_str, "+", covar )
    }
  }
  
  formula = as.formula(formula_str)
  
  
  dataset_ = dataset
  
  modEUR = glm(formula, data = dataset_[dataset_$genetic_afr == 0,], family = binomial)
  modAFR = glm(formula, data = dataset_[dataset_$genetic_afr == 1,], family = binomial)
  
  print(tab_model(modEUR,modAFR, show.intercept = F ))
  cat("\nr2 EUR: ")
  print(round(r2_nagelkerke(modEUR), digits = 3) *100)
  cat("\nr2 AFR: ")
  print(round(r2_nagelkerke(modAFR), digits = 3) *100)
  
  cat("\n\n meta analysis for sex:\n\n")
  print(run_meta_analysis("sex_br", modEUR, modAFR))
  cat("\n\n meta analysis for age:\n\n")
  print(run_meta_analysis("age_2_year", modEUR, modAFR))
  
  if(!is.null(IV)){
    cat("\n\n meta analysis for", IV, ":\n\n")
    print(run_meta_analysis(IV, modEUR, modAFR))
  }
  
  if(!is.null(covar)){
    cat("\n\n meta analysis for", covar, ":\n\n")
    print(run_meta_analysis(covar, modEUR, modAFR))
  }
  
  
  
  return(list(modEUR =modEUR, modAFR = modAFR))
  
}


#####################################
# check best PRSics 
# Supplemental Table 2
#####################################
mod_prs_p1 = run_models("SA_y_ever", IV = "suicide_PRSice_Pt1")
mod_prs_p0_5 = run_models("SA_y_ever", IV = "suicide_PRSice_Pt0_5")
mod_prs_p0_3 = run_models("SA_y_ever", IV = "suicide_PRSice_Pt0_3")
mod_prs_p0_1 = run_models("SA_y_ever", IV = "suicide_PRSice_Pt0_1")
mod_prs_p0_05 = run_models("SA_y_ever", IV = "suicide_PRSice_Pt0_05")
mod_prs_p0_01 = run_models("SA_y_ever", IV = "suicide_PRSice_Pt0_01")
mod_prs_p0_001 = run_models("SA_y_ever", IV = "suicide_PRSice_Pt0_001")
mod_prs_p0_0001 = run_models("SA_y_ever", IV = "suicide_PRSice_Pt0_0001")



##########################################################################
# Table 2: Association of suicide attempt PRS, parental history of suicide 
# attempt/death and suicide attempt in the meta-analyzed study population
##########################################################################
mod_meta_1 = run_models("SA_y_ever")
mod_meta_2 = run_models("SA_y_ever", IV = "suicide_PRSice_Pt0_05")
mod_meta_3 = run_models("SA_y_ever", IV = "famhx_ss_momdad_scd_p")
mod_meta_4 = run_models("SA_y_ever", IV = "suicide_PRSice_Pt0_05", covar = "famhx_ss_momdad_scd_p")



##########################################################################
# Table 3: 
##########################################################################
#EUR
anova(mod_meta_1$modEUR, mod_meta_2$modEUR,test="Chisq")
anova(mod_meta_1$modEUR, mod_meta_3$modEUR,test="Chisq")
anova(mod_meta_3$modEUR, mod_meta_4$modEUR,test="Chisq")

#AFR
anova(mod_meta_1$modAFR, mod_meta_2$modAFR,test="Chisq")
anova(mod_meta_1$modAFR, mod_meta_3$modAFR,test="Chisq")
anova(mod_meta_3$modAFR, mod_meta_4$modAFR,test="Chisq")



######################################################
# Figure 1: Polygenic risk score for suicide attempt 
# (PRS-SA) and suicide attempt in Black and White youth.
######################################################
library(ggplot2)

df_eur = data.frame(dataset[dataset$genetic_afr == 0, c("src_subject_id","suicide_PRSice_Pt0_05", "genetic_afr")])
df_afr = data.frame(dataset[dataset$genetic_afr == 1, c("src_subject_id","suicide_PRSice_Pt0_05", "genetic_afr")])

df_eur =  cbind(df_eur, Pedicted_SA_probability = predict(mod_meta_2$modEUR ,type = "response"))
df_afr =  cbind(df_afr, Pedicted_SA_probability = predict(mod_meta_2$modAFR ,type = "response"))
df = rbind.fill(df_eur,df_afr)


df$ancestry = factor(df$genetic_afr, labels = c("European", "African"))
p2 <- ggplot(data=df, aes(x=suicide_PRSice_Pt0_05, y=Pedicted_SA_probability, color=ancestry)) +
  geom_point(alpha = 0.25) + scale_x_continuous(limits = c(-5, 5) + scale_y_continuous(limits = c(0, 1))) +
  geom_smooth(method = "lm", se = T) + geom_rug() + 
  labs(x="Standardized PRS-SA", y = "Probability of suicide attempt") +
  facet_grid(cols = vars(ancestry)) + 
  theme(legend.position="none")
p2
ggsave(filename = "paper/plots/model2_facet.tiff", plot = p2, width = 6, height = 4, device='tiff', dpi=700, compression = "lzw")





