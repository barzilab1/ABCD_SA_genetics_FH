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
# check best PRSics
#####################################

mod_prs_1 =      glm(SA_y_ever ~ age_2_year + sex_br + race_black + suicide_PRSice_Pt1, data = dataset, family = binomial)
mod_prs_0_5 =    glm(SA_y_ever ~ age_2_year + sex_br + race_black + suicide_PRSice_Pt0_5, data = dataset, family = binomial)
mod_prs_0_3 =    glm(SA_y_ever ~ age_2_year + sex_br + race_black + suicide_PRSice_Pt0_3, data = dataset, family = binomial)
mod_prs_0_1 =    glm(SA_y_ever ~ age_2_year + sex_br + race_black + suicide_PRSice_Pt0_1, data = dataset, family = binomial)
mod_prs_0_05 =   glm(SA_y_ever ~ age_2_year + sex_br + race_black + suicide_PRSice_Pt0_05, data = dataset, family = binomial)
mod_prs_0_01 =   glm(SA_y_ever ~ age_2_year + sex_br + race_black + suicide_PRSice_Pt0_01, data = dataset, family = binomial)
mod_prs_0_001 =  glm(SA_y_ever ~ age_2_year + sex_br + race_black + suicide_PRSice_Pt0_001, data = dataset, family = binomial)
mod_prs_0_0001 = glm(SA_y_ever ~ age_2_year + sex_br + race_black + suicide_PRSice_Pt0_0001, data = dataset, family = binomial)



tab_model(mod_prs_1,mod_prs_0_5,mod_prs_0_3,mod_prs_0_1,mod_prs_0_05,mod_prs_0_01 ,mod_prs_0_001,mod_prs_0_0001, show.intercept = F )
round(r2_nagelkerke(mod_prs_1), digits = 3) *100
round(r2_nagelkerke(mod_prs_0_5), digits = 3) *100
round(r2_nagelkerke(mod_prs_0_3), digits = 3) *100
round(r2_nagelkerke(mod_prs_0_1), digits = 3) *100
round(r2_nagelkerke(mod_prs_0_05), digits = 3) *100
round(r2_nagelkerke(mod_prs_0_01), digits = 3) *100
round(r2_nagelkerke(mod_prs_0_001), digits = 3) *100
round(r2_nagelkerke(mod_prs_0_0001), digits = 3) *100



#####################################
# main analysis
#####################################

mod_f_e0 = glm(SA_y_ever ~ age_2_year + sex_br + race_black , data = dataset, family = binomial)
mod_f_e1 = glm(SA_y_ever ~ age_2_year + sex_br + race_black + suicide_PRSice_Pt0_05, data = dataset, family = binomial)
mod_f_e2 = glm(SA_y_ever ~ age_2_year + sex_br + race_black + famhx_ss_momdad_scd_p, data = dataset, family = binomial)
mod_f_e3 = glm(SA_y_ever ~ age_2_year + sex_br + race_black + famhx_ss_momdad_scd_p + suicide_PRSice_Pt0_05, data = dataset, family = binomial)


tab_model(mod_f_e0,mod_f_e1,mod_f_e2, mod_f_e3, show.intercept = F)
round(r2_nagelkerke(mod_f_e0), digits = 3) *100
round(r2_nagelkerke(mod_f_e1), digits = 3) *100
round(r2_nagelkerke(mod_f_e2), digits = 3) *100
round(r2_nagelkerke(mod_f_e3), digits = 3) *100


anova(mod_f_e0, mod_f_e1,test="Chisq")
anova(mod_f_e0, mod_f_e2,test="Chisq")
anova(mod_f_e2, mod_f_e3,test="Chisq")






#####################################
# Supplementary 
#####################################

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
  metagen(TE, seTE, studlab = c("EUR", "AFR"),  data = df, sm = "OR")
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
  
  
  if(is.null(IV)){
    cat("\n\n meta analysis for sex:\n")
    print(run_meta_analysis("sex_br", modEUR, modAFR))
    cat("\n\n meta analysis for age:\n")
    print(run_meta_analysis("age_2_year", modEUR, modAFR))
  }else{
    cat("\n\n meta analysis for", IV, ":\n")
    print(run_meta_analysis(IV, modEUR, modAFR))
  }
  
  return(list(modEUR =modEUR, modAFR = modAFR))
  
}



#####################################################
# Supplemental Table 3- Ancestry stratified analyses
#####################################################
mod_e0 = run_models("SA_y_ever")
mod_e1 = run_models("SA_y_ever", IV = "suicide_PRSice_Pt0_05")
mod_e2 = run_models("SA_y_ever", IV = "famhx_ss_momdad_scd_p")
mod_e3 = run_models("SA_y_ever", IV = "suicide_PRSice_Pt0_05", covar = "famhx_ss_momdad_scd_p")


######################################################
# Supplemental Table 2 - Frequency of suicide attempts 
# in each of the first three ABCD Study evaluations.
######################################################

SA_variables = c("SA_y_ever", "SA_y_2_year", "SA_y_1_year", "SA_y_baseline_year")
table1_SA = CreateTableOne(data =dataset, vars = SA_variables ,factorVars = SA_variables,  strata = "race_black", addOverall = T)
table1_SA_print = print(table1_SA, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, missing = T)
table1_SA_print

write.csv(as.data.frame(table1_SA_print), "tables/table1_SA.csv",na = "" )


setDT(dataset)
dataset[,table(SA_y_baseline_year,SA_y_1_year, useNA = "ifany")]
dataset[,table(SA_y_baseline_year,SA_y_2_year, useNA = "ifany")]
dataset[,table(SA_y_1_year,SA_y_2_year, useNA = "ifany")]



