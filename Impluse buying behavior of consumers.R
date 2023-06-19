## A. Preparation for Data Analysis

# I opened my raw data file

library(foreign)
library(readxl)
CB_data <- read_excel("C:/Users/Admin/Documents/Du hoc/M1 BDEEM/Data Analysis - Software/My individual project/Consumer_behavior.xlsx")

# I installed several essential packages to do my analysis
install.packages("lavaan", dependencies=TRUE)
install.packages("lavaanPlot")
install.packages("seminr")

# I opened these packages and other already downloaded packages
library(tidyverse)
library(lavaan)
library(lavaanPlot)
library(seminr)
library(semTools)

# Because I just wanted to look into the effect of the latent variables
# "PAR" (which is Parasocial interaction)
# and "INT" (Social interaction) 
# and "HED" (Hedonic consumption) 
# and "CON" (Self-control)
# on the endogenous latent variable "URG" (Urge to buy impulsively)
# then I just selected the observed variables of these latent variables
# and formed a new dataset called "CB_new"
CB_new <- CB_data %>%
  select(PAR1, PAR2, PAR3, INT1, INT2, INT3, HED1, HED2, HED3, CON1, CON2, CON3, URG1, URG2, URG3)

## B. Simple statistical analysis
# I ran covariance of items in each variable, and round the number to the second digit
# PAR
round(cov(CB_new[,1:3]),2)
# INT
round(cov(CB_new[,4:6]),2)
# HED
round(cov(CB_new[,7:9]),2)
# CON
round(cov(CB_new[,10:12]),2)
# TEN
round(cov(CB_new[,13:14]),2)

## C. Confirmatory Factor Analysis (CFA)

# I ran a one-factor CFA with items PAR1, PAR2, PAR3 
# as indicators of PAR (Parasocial interaction)
# using the function cfa built in "lavaan" package
PAR  <- ' f  =~ PAR1 + PAR2 + PAR3'
onefac3items_PAR <- cfa(PAR, data=CB_new)
# Marker method (default the estimation of the first item is 1, which is PAR1)
summary(onefac3items_PAR)
# Standardized method (help better interpret the factor loadings)
summary(onefac3items_PAR, standardized=TRUE)

# Interpretation of CFA: 
# For one standardized deviation in Parasocial interaction, 
# item PAR1 goes up by 0.704 standard deviation points. 
# Variance of the factor is scaled to 1.
# I can compare the relative between each item with each other.
# Which one has the highest absolute value is the item which has the strongest magnitude.
# Also negative and positive relationship (the sign of the value) with the latent variable.
# Look at the Std.all in the CFA summary table, if the values are between [0.4,0.8] then it's pretty good.

# I did CFA with the "INT" variable
INT  <- ' f  =~ INT1 + INT2 + INT3'
onefac3items_INT <- cfa(INT, data=CB_new)
summary(onefac3items_INT, standardized=TRUE)

# I did CFA with the "HED" variable
HED <- ' f  =~ HED1 + HED2 + HED3'
onefac3items_HED <- cfa(HED, data=CB_new)
summary(onefac3items_HED, standardized=TRUE)

# I did CFA with the "CON" variable
CON <- ' f  =~ CON1 + CON2 + CON3'
onefac3items_CON <- cfa(CON, data=CB_new)
summary(onefac3items_CON, standardized=TRUE)

# I did CFA with the "URG" variable
URG <- ' f  =~ URG1 + URG2 + URG3'
onefac3items_URG <- cfa(URG, data=CB_new)
summary(onefac3items_URG, standardized=TRUE)

## D. Structural Estimation Model

# Method 1: Bootstrapping

# I created my SEM model
ConsumerBehav_SEM <- '
# Measurement model
PAR  =~ PAR1 + PAR2 + PAR3
INT  =~ INT1 + INT2 + INT3
HED  =~ HED1 + HED2 + HED3
CON  =~ CON1 + CON2 + CON3
URG  =~ URG1 + URG2 + URG3

# Structural regressions
URG ~ PAR + INT + HED + CON
'
fitSEM <- sem(ConsumerBehav_SEM, data=CB_new)
summary(fitSEM, standardized=TRUE, fit.measures=TRUE)

# Path diagrams

# I labeled my latent variables
labels = list(URG = "Urge to buy impulsively", PAR = "Parasocial Interaction", INT = "Social Interaction", HED = "Hedonic Consumption", CON = "Self-control")
# I drew significant paths
lavaanPlot(model = fitSEM, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, sig = 0.05, digits = 2)
# I drew my path diagrams
lavaanPlot(model = fitSEM, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE, digits = 2)
# I drew standardized regression paths, showing only paths with p<= .05
lavaanPlot(model = fitSEM, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE, digits = 2, sig = 0.05)

# Method 2: OLS

# I built the Measurement model
con_beh_mm_mod <- constructs(
  composite("par", multi_items("PAR", 1:3), weights = mode_B),
  composite("int", multi_items("INT", 1:3), weights = mode_B),
  composite("hed", multi_items("HED", 1:3), weights = mode_B),
  composite("con", multi_items("CON", 1:3), weights = mode_B),
  composite("urg", multi_items("URG", 1:3)),
  interaction_term( iv = "hed", moderator = "con", method = two_stage))

# I created the Structural model without the moderator variable 
# which would be tested in the Moderation Analysis
con_beh_sm_mod <- relationships(
  paths(from = c("par", "int", "hed", "con"), to = c("urg")),
  paths(from = c("con"), to = c("hed"))
)

# I estimated the structural model using the estimate_pls function in the package "seminR"
con_beh_pls_model_est <- estimate_pls(
  data = CB_new,
  measurement_model = con_beh_mm_mod,
  structural_model = con_beh_sm_mod,
  missing = mean_replacement,
  missing_value = "-99")

# I summarized the results of the model estimation
summary_con_beh_est <- summary(con_beh_pls_model_est)

# I bootstrapped the model
boot_con_beh_est <- bootstrap_model(
  seminr_model = con_beh_pls_model_est,
  nboot = 1000,
  cores = parallel::detectCores(),
  seed = 123)

# I summarized the results of the bootstrap
summary_boot_con_beh_est <- summary(boot_con_beh_est,
                                    alpha = 0.05)

# I inspected the structural model collinearity VIF
summary_con_beh_est$vif_antecedents

# Interpretation:
#1 As can be seen, all VIF values are clearly below the threshold of 5.
#2 Therefore, I can conclude that collinearity among predictor constructs 
# is likely not a critical issue in the structural model, 
# and I can continue examining the result report.

# Next, in the structural model assessment procedure, 
# I need to evaluate the relevance and significance of the structural paths.
# The results of the bootstrapping of structural paths can be accessed 
# by inspecting the bootstrapped_paths element 
# nested in the summary_boot_con_est object.

# I inspected the structural paths
summary_boot_con_beh_est$bootstrapped_paths

# Interpretation:
# I looked at the values of the column "Original est."
#1 Self-control (denoted by "con"), surprisingly, has a strong positive impact on Hedonic consumption ("hed") (0.311) 
# and low but still positive impact on the Urge to buy impulsively ("urg") (0.128).
#2 Parasocial interaction ("par") and Hedonic consumption ("hed") 
# also has a relatively strong positive effect on the Urge to buy impulsively ("urg") (0.306 and 0.276 respectively).
#3 On the contrary, Social interaction ("int") exerts a much lower impact on the Urge to buy impulsively ("urg") as evidenced in path coefficient estimates of 0.043.

# Statistical significance
# Assuming a 5% significance level 
# (as specified with the parameter alpha = 0.05 in the bootstrap_model() function), 
# the t-values (T Stat. column) estimated from the bootstrapping should exceed the value of 1.960.

# Interpretation:
#1 I find that several relationships are significant, including three of the exogenous driver construct relationships 
# (par -> urg, t = 5.509; hed -> urg, t = 4.555; con -> hed, t = 4.382; con -> urg, t = 1.895). 
#2 At the same time, however, one exogenous driver relationship is not statistically significant (int -> urg, t = 0.672).

# I inspected the total effects
summary_boot_con_beh_est$bootstrapped_total_paths

# In Step 3 of the structural model assessment procedure, 
# I need to consider the models explanatory power by analyzing the R2 of the endogenous constructs 
# and the f2 effect size of the predictor constructs. 
# To start with, I need to examine the R2 values of the endogenous constructs.

# I inspected the model R-squared
summary_con_beh_est$paths

# Interpretation:
#1 As can be seen, only "hed" has the explanatory power to the dependent "urg"
# but at very low percent, only 9.7%.

# I inspected the effect sizes (##explain what is effect sizes)
summary_con_beh_est$fSquare

# Interpretation:
#0 Figure shows the f2 values for all combinations of endogenous constructs (represented by the columns) 
# and corresponding exogenous (i.e., predictor) constructs (represented by the rows). 
#1 "par" has a medium effect size of 0.112 on "urg"; "con" has a medium effect size of 0.107 on "hed". 
# On the contrary, "int" is considered to have no effect on "urg" (0.002). 
# The rank order of effect sizes is identical to the rank order on the grounds of the path coefficients.

# I generated the model predictions
predict_con_beh_est <- predict_pls(
  model = con_beh_pls_model_est,
  technique = predict_DA,
  noFolds = 10,
  reps = 10)

# I summarized the prediction results
sum_predict_con_beh_est <- summary(predict_con_beh_est)

# I analyzed the distribution of prediction error
par(mfrow=c(1,3))
plot(sum_predict_con_beh_est,
     indicator = "URG1")
plot(sum_predict_con_beh_est,
     indicator = "URG2")
plot(sum_predict_con_beh_est,
     indicator = "URG3")
par(mfrow=c(1,1))

# I computed the prediction statistics
sum_predict_con_beh_est

## E. Moderation Analysis

# I crated the structural model including the moderator
con_beh_sm_mod <- relationships(
  paths(from = c("par", "int", "hed", "con"), to = c("urg")),
  paths(from = c("con"), to = c("hed")),
  paths(from = c("hed*con"), to = c("urg"))
)

# Estimate the new model with moderator
con_beh_pls_model_mod <- estimate_pls(
  data = CB_new,
  measurement_model = con_beh_mm_mod,
  structural_model = con_beh_sm_mod,
)

# Extract the summary
sum_con_beh_mod <- summary(con_beh_pls_model_mod)
# Bootstrap the model
boot_con_beh_mod <- bootstrap_model(
  seminr_model = con_beh_pls_model_mod,
  nboot = 1000)

# Summarize the results of the bootstrap
sum_boot_con_beh_mod <- summary(boot_con_beh_mod, alpha = 0.05)

# Inspect the bootstrapped structural paths
sum_boot_con_beh_mod$bootstrapped_paths

# Simple slope analysis plot
slope_analysis(
  moderated_model = con_beh_pls_model_mod,
  dv = "URG",
  moderator = "CON",
  iv = "HED",
  leg_place = "bottomright")