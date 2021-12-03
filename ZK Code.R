### ASSIGNMENT PART 1 ###

# loading data
pain_data = read.csv("https://tinyurl.com/ha-dataset1")

#loading necessary packages
library(tidyverse)
library(lm.beta)
library(psych)
library(car)
library(lmtest)	
library(sandwich)
library(boot) 	
library(lmboot)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)


#checking dataset for errors and irregularities
View(pain_data)

pain_data %>% 
  summary()

str(pain_data)

##summary shows that errors are in 
      #pain variable (max = 55, but score range is 0 to 10)
      #STAI_trait variable (min = 4.20, but score range is 20 to 80)

#excluding the errors from dataset
pain_data_clean <- pain_data %>% 
  filter(pain < 11,
         STAI_trait > 19)

pain_data_clean %>% 
  summary()

#model1 aims to predict pain score with age and sex as predictors
model1 <- lm(pain ~ age + sex, data = pain_data_clean)

summary(model1)

#coefficients of model1 predictors
confint(model1)
lm.beta(model1)

#model2 aims to predict pain with age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures as predictors
model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = pain_data_clean)

summary(model2)

#coefficients of model2 predictors
confint(model2)
lm.beta(model2)

#model1 and model2 comparison
anova(model1, model2)

#Akaike Information Coefficient
AIC(model1)
AIC(model2)

#model 2 is a better model to explain the variance in pain scores

### MODEL DIAGNOSTICS COME AT THE END OF THE MODEL

## identifying extreme cases in each predictor of model2

#residuals vs leverage plot
model2 %>% 	
  plot(which = 5)	

#cook's distance (threshold: cooksd > 4/N ==> 4/158 = 0.025)
model2 %>% 	
  plot(which = 4)	

#cook's distance plot shows 46, 73 and 85 as having high cook's distance ==> check seperately
pain_data_clean %>%
  slice(c(46, 73, 85))

#assumption checking

##normality

#QQ plot to check normal distribution
model2 %>%
  plot(which = 2)

#histogram to check bell curve
residuals_model2 = enframe(residuals(model2))
residuals_model2 %>%
  ggplot() + aes(x = value) + geom_histogram()

#check skew and kurtosis (if between -1 and 1 ==> normality assumption violated)
describe(residuals(model2))

##DATA IS NORMALLY DISTRIBUTED (ASSUMPTION MET) ==> skew and kurtosis are between -1 and +1 and indicate a normal distribution, outliers are not an issue in this case so we do not need to eliminate them

#linearity

#residual plots for each variable
model2 %>% 
  residualPlots()

#RELATIONSHIP BETWEEN OUTCOME AND PREDICTORS IS LINEAR (ASSUMPTION MET) ==> some curve found in the residual plots, but all non-linear tests were not significant (p > 0.05) so linearity assumption is not violated

#homoscedasticity

#plot of the standardized residuals and the predicted (fitted) values to check for equal variations
model2 %>%
  plot(which = 3)

#NCV test
model2 %>%
  ncvTest() 

#Breush-Pagan test
model2 %>%
  bptest() 

#HOMOSCEDASTICITY MAINTAINED (ASSUMPTION MET) ==> while the plot shows a funnel shape (usually not favorable), the NCV and BP tests were not significant (p > 0.05), so homoscedasticity assumption is not violated

#no multicollinearity

#VIF test
model2 %>%
  vif()
#cortisol_serum and cortisol saliva have VIF > 3 ==> could be data mutlicollinearity because they both measure cortisol levels

#correlation matrix
pain_data_clean %>%
  select(pain, age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva) %>%
  pairs.panels(col = "red", lm = T)
#cortisol_serum and cortisol_saliva have 0.85 correlation ==> better to eliminate one of the two from model (keep cortisol_saliva because they are essentially the same but cortisol_saliva has overall smaller correlation coefficients with the rest of the predictors)

#new model without cortisol_serum
final_pain_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = pain_data_clean)

summary(final_pain_model)

#coefficients of final model predictors
confint(final_pain_model)
lm.beta(final_pain_model)

#Akaike Information Coefficient
AIC(final_pain_model)

#rerun model diagnostics and assumption checking on newer model

##normality

#QQ plot to check normal distribution
final_pain_model %>%
  plot(which = 2)

#histogram to check bell curve
residuals_final = enframe(residuals(final_pain_model))
residuals_final %>%
  ggplot() + aes(x = value) + geom_histogram()

#check skew and kurtosis (if between -1 and 1 ==> normality assumption violated)
describe(residuals(final_pain_model))

##DATA IS NORMALLY DISTRIBUTED (ASSUMPTION MET) ==> outliers are not an issue in this case so we do not need to eliminate them

#linearity

#residual plots for each variable
final_pain_model %>% 
  residualPlots()

#RELATIONSHIP BETWEEN OUTCOME AND PREDICTORS IS LINEAR (ASSUMPTION MET) ==> some curve found in the residual plots, but all non-linear tests were not significant (p > 0.05) so linearity assumption is not violated

#homoscedasticity

#plot of the standardized residuals and the predicted (fitted) values to check for equal variations
final_pain_model %>%
  plot(which = 3)

#NCV test
final_pain_model %>%
  ncvTest() 

#Breush-Pagan test
final_pain_model %>%
  bptest() 

#HOMOSCEDASTICITY MAINTAINED (ASSUMPTION MET) ==> SDs are constant and independent of predictor values, and NCV and BP tests were not significant (p > 0.05), so homoscedasticity assumption is not violated

#no multicollinearity

#VIF test to check if predictors are too highly correlated (VIF < 3)
final_pain_model %>%
  vif()

#NO MULTICOLLINEARITY FOUND (ASSUMPTION MET) ==> all VIF < 3, no predictors are highly correlated and the multicollinearity assumption is not violated

#after removing cortisol_serum as a predictor, the final model is better than the ones previously suggested as it does not violate any of the assumptions

##final model comparison between the models
anova(model1, final_pain_model)
AIC(model1)
AIC(final_pain_model)

### ASSIGNMENT PART 2 ###

#initial model with all variables as predictors (except for cortisol_saliva)
initial_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = pain_data_clean)

summary(initial_model)

#coefficients of initial model predictors
confint(initial_model)
lm.beta(initial_model)

#Akaike Information Coefficient
AIC(initial_model)

##MODEL DIAGNOSTICS FOR NEW BACKWARD MODEL

##checking for influential outliers 
#residuals vs leverage plot
initial_model %>% 	
  plot(which = 5)	

#cook's distance (threshold: cooksd > 4/N ==> 4/158 = 0.025)
initial_model %>% 	
  plot(which = 4)	

#cook's distance plot shows 46, 84 and 85 as having high cook's distance ==> check seperately
pain_data_clean %>%
  slice(c(46, 84, 85))

#assumption checking

##normality

#QQ plot to check normal distribution
initial_model %>%
  plot(which = 2)

#histogram to check bell curve
residuals_initial = enframe(residuals(initial_model))
residuals_initial %>%
  ggplot() + aes(x = value) + geom_histogram()

#check skew and kurtosis (if between -1 and 1 ==> normality assumption violated)
describe(residuals(initial_model))

##DATA IS NORMALLY DISTRIBUTED (ASSUMPTION MET) ==> skew and kurtosis are between -1 and +1 and indicate a normal distribution, outliers are not an issue in this case so we do not need to eliminate them

#linearity

#residual plots for each variable
initial_model %>% 
  residualPlots()

#RELATIONSHIP BETWEEN OUTCOME AND PREDICTORS IS LINEAR (ASSUMPTION MET) ==> some curve found in the residual plots, but all non-linear tests were not significant (p > 0.05) so linearity assumption is not violated

#homoscedasticity

#plot of the standardized residuals and the predicted (fitted) values to check for equal variations
initial_model %>%
  plot(which = 3)

#NCV test
initial_model %>%
  ncvTest() 

#Breush-Pagan test
initial_model %>%
  bptest() 

#HOMOSCEDASTICITY MAINTAINED (ASSUMPTION MET) ==> while the plot shows a funnel shape (usually not favorable), the NCV and BP tests were not significant (p > 0.05), so homoscedasticity assumption is not violated

#no multicollinearity

#VIF test
initial_model %>%
  vif()

#NO MULTICOLLINEARITY FOUND (ASSUMPTION MET) ==> all VIF < 3, no predictors are highly correlated and the multicollinearity assumption is not violated

#backward regression on initial model
initial_backward <- step(initial_model, direction = "backward")

summary(initial_backward)

#regression models
backward_model <- lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, data = pain_data_clean) #derived from stepwise exclusion

summary(backward_model)

#coefficients of final model predictors
confint(backward_model)
lm.beta(backward_model)

#Akaike Information Coefficient
AIC(backward_model)

theory_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = pain_data_clean) #final model from assignment part 1

summary(theory_model)

#coefficients of final model predictors
confint(theory_model)
lm.beta(theory_model)

#Akaike Information Coefficient
AIC(theory_model)

#model comparison using AIC and anova
##if difference in AIC > 2 ==> choose model with smaller AIC as it is generally a better fit for the data

AIC(backward_model) - AIC(theory_model)
#difference in AIC = 1.82 ==> models may not be significantly different but it is better to choose the theory based model as it has a smaller AIC and would be a better fit to predict pain scores

anova(backward_model, theory_model)

#test both models on new dataset
pain_data2 <- read_csv("https://tinyurl.com/87v6emky")

pain_data2 %>% 
  summary()

#prediction performance of both backward and theory-based models
# calculate predicted values
pred_test <- predict(theory_model, pain_data2)
pred_test
pred_test_back <- predict(backward_model, pain_data2)
pred_test_back

#calculate the sum of squared residuals
RSS_test <- sum((pain_data2[, "pain"] - pred_test)^2)
RSS_test_back <- sum((pain_data2[, "pain"] - pred_test_back)^2)
RSS_test
RSS_test_back

#the RSS difference reveals that the backward model has more error in its predictions than the theory-based model found in assignment part 1

### ASSIGNMENT PART 3 ###

#loading data file 3
pain_data3 <- read_csv("https://tinyurl.com/b385chpu")

pain_data3 %>% 
  summary()

#summary shows errors are in
    #household_income variable (income is -7884)

#excluding the errors from dataset
pain_data3_clean <- pain_data3 %>% 
  filter(!household_income == "-7884")

pain_data3_clean %>% 
  summary()

#assign class as a grouping factor and organize levels in ascending order
pain_data3_clean <- pain_data3_clean %>%
  mutate(hospital = factor(hospital))
         
#build linear mixed model for data file 3 (with fixed effect predictors used in part 1)
pain3_mixed_rnd_int <- lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + (1 | hospital), data = pain_data3_clean)

summary(pain3_mixed_rnd_int)

#coefficients of model predictors
confint(pain3_mixed_rnd_int)

#marginal R squared with confidence intervals
r2beta(pain3_mixed_rnd_int, method = "nsj", data = pain_data3_clean)

#marginal and conditional R squared values
r.squaredGLMM(pain3_mixed_rnd_int)

#loading data file 4
pain_data4 <- read_csv("https://tinyurl.com/4f8thztv")

pain_data4 %>% 
  summary()

#running random intercept model on data file 4
pred_test2 <- predict(pain3_mixed_rnd_int, pain_data4, allow.new.levels = TRUE)
pred_test2

#compute variance explained by random intercept model on data file 4 (using 1-(RSS/TSS))
##calculate RSS
RSS <- sum((pain_data4[, "pain"] - pred_test2)^2)
RSS

##calculate TSS
mod_mean <- lm(pain ~ 1, data = pain_data4)
TSS <- sum((pain_data4$pain - predict(mod_mean))^2)
TSS

#R^2 = 1 - (RSS/TSS)
data4_Rsquared <- 1 - (RSS/TSS)
data4_Rsquared

#checking which variable in random intercept model was the most influential (most highly correlated)
pain_data3_clean %>%
  select(pain, age, sex, STAI_trait, pain_cat, mindfulness, cortisol_saliva) %>%
  pairs.panels(col = "red", lm = T)
###pain_cat as it has the highest correlation coefficient with pain scores (r = 0.48)

#linear mixed model with random intercept and random slope
pain3_mixed_rnd_slope = lmer(pain ~ pain_cat + (pain_cat | hospital),
                     data = pain_data3_clean)

##needs to be optimized as it failed to converge
pain3_mixed_rnd_slope_opt = lmer(pain ~ pain_cat + (pain_cat | hospital), 
                         control = lmerControl(optimizer = "Nelder_Mead"),
                         data = pain_data3_clean)

pain3_mixed_rnd_slope_opt %>% 
  summary()

pain_data3_clean <- pain_data3_clean %>%
  mutate(pred_slope = predict(pain3_mixed_rnd_slope_opt))

#Fix order
library(plyr)
pain_data3_ordered  <- arrange(transform(pain_data3_clean,
                                          hospital = factor(hospital,levels = c("hospital_1", "hospital_2", "hospital_3", "hospital_4", "hospital_5", "hospital_6", "hospital_7", "hospital_8", "hospital_9", "hospital_10"))),hospital)

#visualizing fitted regression lines for each hospital
pain_data3_ordered %>%
  ggplot() + aes(y = pain, x = pain_cat, group = hospital) +
  geom_point(aes(color = hospital), size = 2) + 
  geom_line(color = "black", aes(y = pred_slope, x = pain_cat)) + 
  facet_wrap(~hospital, ncol = 5)
