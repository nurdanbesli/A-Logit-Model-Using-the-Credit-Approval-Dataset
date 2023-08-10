###########################################################################
#		Advanced Econometrics                                                 #
#   Project Codes                                                         #
#   University of Warsaw, Faculty of Economic Sciences                    #
#   Nurdan Beþli, Mustafa Sanli                                    #
###########################################################################

setwd("C:/Users/nurdanbesli/Desktop/econometrics/3/AE_Lab_03")
Sys.setenv(LANG = "en")
options(scipen = 5)

# Libraries
library("BaylorEdPsych")
library("dplyr")
library("ggplot2")
library("lmtest")
library("margins")
library("ResourceSelection")
library("stargazer")
library("webshot")


# UPLOAD DATA
credit = read.csv2(file="clean_dataset.csv", header=TRUE, sep=",")
credit = na.omit(credit)


# PIE CHART OF DEPENDENT VARIABLE
# Count the occurrences of each status (0 and 1) in the dependent variable
counts <- table(credit$Approved)

# Create a vector of labels with custom names
labels <- ifelse(names(counts) == "0", "Disapproved", "Approved")

# Create a vector of colors
colors <- ifelse(names(counts) == "0", "#FFCCCC", "#CCFFCC")

# Create the pie chart with customizations
pie(counts, labels = paste0(labels, " (", counts, ")"), main = "Distribution of Approved",
    col = colors, border = "black", lwd = 2)


# CONVERSION OF FEATURES
# Convert variables to numeric
credit$Age <- as.numeric(credit$Age)
credit$Debt <- as.numeric(credit$Debt)
credit$YearsEmployed <- as.numeric(credit$YearsEmployed)
credit$Income <- as.numeric(credit$Income)
credit$Debt <- as.numeric(credit$Debt)

# Convert variables to factors
credit$Citizen <- as.factor(credit$Citizen)
credit$Industry <- as.factor(credit$Industry)
credit$Ethnicity <- as.factor(credit$Ethnicity)


# Apply one-hot encoding
credit <- credit %>%
  mutate(
    Citizen = as.numeric(Citizen),
    Industry = as.numeric(Industry),
    Ethnicity = as.numeric(Ethnicity)
  )


# CHOOSE LOGIT OR PROBIT
# Probit model estimation
myprobit <- glm(Approved~Gender+Married+Age+Debt+BankCustomer+Industry+Ethnicity+YearsEmployed+PriorDefault+Employed+CreditScore+DriversLicense+Citizen+Income, data=credit, 
 family=binomial(link="probit"))

# Logit model estimation
mylogit <- glm(Approved~Gender+Married+Age+Debt+BankCustomer+Industry+Ethnicity+YearsEmployed+PriorDefault+Employed+CreditScore+DriversLicense+Citizen+Income, data=credit, 
               family=binomial(link="logit"))

# Compare AIC and BIC
cat("Probit Model: AIC =", AIC(myprobit), " BIC =", BIC(myprobit), "\n")
cat("Logit Model: AIC =", AIC(mylogit), " BIC =", BIC(mylogit), "\n")


# LOGIT MODEL
# General model
mylogit <- glm(Approved~Gender+Married+Age+Debt+BankCustomer+Industry+Ethnicity+YearsEmployed+PriorDefault+Employed+CreditScore+DriversLicense+Citizen+Income, data=credit, 
               family=binomial(link="logit"))
summary(mylogit)


# LIKELIHOOD RATIO TESTS
# Joint insignificance of all variables test
null_logit =glm(Approved~1, data=credit, family=binomial(link="logit"))
lrtest(mylogit, null_logit)

# Remove all insignificant variables at once or not
retricted_logit = glm(Approved~Industry+PriorDefault+Employed+CreditScore+Citizen+Income, data=credit, family=binomial(link="logit"))
lrtest(mylogit, retricted_logit)

# Restricted logit model
final_logit = glm(Approved~Industry+PriorDefault+Employed+CreditScore+Citizen+Income, data=credit, family=binomial(link="logit"))
summary(final_logit)


# NONLINEAR 
# Create transformed variables
credit$CreditScore_squared <- credit$CreditScore^2
credit$Income_squared <- credit$Income^2

# Add a small positive constant to CreditScore and Income
credit$CreditScore_positive <- credit$CreditScore + 0.1
credit$Income_positive <- credit$Income + 0.1

# Apply logarithmic transformations to the positive variables
credit$CreditScore_log <- log(credit$CreditScore_positive)
credit$Income_log <- log(credit$Income_positive)

# Logit model with nonlinear terms
final_logit <- glm(Approved ~ Industry + PriorDefault + Employed + CreditScore + CreditScore_squared + CreditScore_log + Citizen + Income + Income_squared + Income_log, data = credit, family = binomial(link = "logit"))
summary(final_logit)


# INTERACTION EFFECTS
# Interaction between variables
credit$PriorDefault_Income <- credit$PriorDefault * credit$Income
credit$PriorDefault_Employed <- credit$PriorDefault * credit$Employed
credit$PriorDefault_CreditScore <- credit$PriorDefault * credit$CreditScore
credit$Employed_Income <- credit$Employed * credit$Income
credit$Income_CreditScore <- credit$Income * credit$CreditScore

# Logit model with interaction terms
final_logit = glm(Approved~Industry+PriorDefault+Employed+CreditScore+Citizen+Income+PriorDefault_Income+PriorDefault_Employed+PriorDefault_CreditScore+Employed_Income+Employed_CreditScore+Income_CreditScore, data=credit, family=binomial(link="logit"))
summary(final_logit)

# Logit model with PriorDefault_Income
final_logit = glm(Approved~Industry+PriorDefault+Employed+CreditScore+Citizen+Income+PriorDefault_Income, data=credit, family=binomial(link="logit"))
summary(final_logit)

# FINAL LOGIT MODEL
final_logit = glm(Approved~Industry+PriorDefault+Employed+CreditScore+Citizen+Income, data=credit, family=binomial(link="logit"))
summary(final_logit)


# MARGINAL EFFECTS
marg_effects <- margins(final_logit)
print(marg_effects)


# ODD RATÝOS
summary_final <- summary(final_logit)
odds_ratios <- exp(summary_final$coefficients)
print(odds_ratios)


# LINKTEST
source("linktest.R")
linktest_result = linktest(final_logit)
summary(linktest_result)


# GOODNESS OF FIT TESTS
# Count & adj. count results
PseudoR2(final_logit)

# Hosmer-Lemeshow test
hoslem.test(fitted(final_logit), credit$Approved)


# STARGAZER TABLE
# Create a stargazer table
stargazer(mylogit, final_logit, 
          title = "Regression Results",
          out = "stargazer_table.html")  # saves to HTML

# Get the stargazer table as png format
webshot::install_phantomjs()

webshot("stargazer_table.html", 
        "stargazer_table.png",  # output file
        vwidth = 480, vheight = 480)

