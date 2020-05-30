library(tidyverse) # Load the tidyverse packages
library(Hmisc) # Needed for correlation
library(car) # Needed for VIF calculations and DW test
library(MASS) # Needed for maths functions
library(broom) # Allows us to turn the output of models into dataframes
library(leaps) # Plots all-subsets regression models
library(olsrr) # Needed for ols regression
library(stargazer) # Handy regression tables
library(gvlma) # Easy test of linear model assumptions
library(rockchalk) # Helps visualise effect of moderators
library(multilevel) # Needed for Sobel test to interpret mediation via Baron and Kenny method
library(mediation) # Needed to use the mediate funciton
library(ggcorrplot) # Needed to visualisation the correlation plot

# First we're going to create the data
Region <- seq(1:250)

set.seed(1)
House_price <- rnorm(250, mean = 200000, sd = 10000)

set.seed(2)
Population <- rnorm(250, mean = 50000, sd = 2500)
Crime <- rnorm(250, mean = 20, sd = 5)
Average_age <- rnorm(250, mean = 75, sd = 5)
Household_income <- rnorm(250, mean = 20000, sd = 2000)
result1 <- tidy(rcorr(House_price, Population))
result2 <- tidy(rcorr(House_price, Crime))

# Find the first solution where House prices correlate (p < .01) with both Population and Crime
i = 2
while (!(result1$p.value < .01 & result2$p.value < .01)) {
  set.seed(i)
  Population <- rnorm(250, mean = 50000, sd = 2500)
  Crime <- rnorm(250, mean = 20, sd = 5)
  result1 <- tidy(rcorr(House_price, Population))
  result2 <- tidy(rcorr(House_price, Crime))
  i <- i+1
}

my_data <- as_tibble(cbind(Region, as.integer(House_price), as.integer(Population), 
                        as.integer(Crime), Average_age, Household_income))

colnames(my_data) <- c("Region", "House_price", "Population", "Crime", "Average_age", "Household_income")
#write.csv(data, "Mult_regression.csv")
# Check the correlation structure

# We need to drop Region from the correlation grid as it is a factor
cor(dplyr::select(my_data, -Region))

my_data <- read_csv("data_files/Mult_regression.csv")

# Can house prices be predicted by (one or more of) Population, Crime (per 10000 people), 
# Average age, or Household income in a region?  We have data from 1,000 regions.
# First let's plot some individual graphs

ggplot(my_data, aes(x = House_price, y = Population)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(my_data, aes(x = House_price, y = Crime)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(my_data, aes(x = House_price, y = Average_age)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(my_data, aes(x = House_price, y = Household_income)) + 
  geom_point() + 
  geom_smooth(method = "lm")

rcorr(my_data$House_price, my_data$Population)
rcorr(my_data$House_price, my_data$Crime)
rcorr(my_data$House_price, my_data$Average_age)
rcorr(my_data$House_price, my_data$Household_income)

# We need to drop Region from the correlation grid as it is a factor - we also 
# column X1 as it is a repeat of the Region information (and is a factor)
corr <- cor(dplyr::select(my_data, -Region, -X1))

ggcorrplot(corr , hc.order = TRUE, type = "lower",
           lab = TRUE)

# First let's build a null model
model0 <- lm(House_price ~ 1, data = my_data)

# First let's build a model with all predictors
model1 <- lm(House_price ~ Population + Crime + Average_age + Household_income, 
             data = my_data)

# Let's check the model assumptions
check_model(model1)

# Do we have any multi-colinearity issues?
vif(model1)

# Check to see if the full model is better than the null model
anova(model0, model1)

# Now get the summary of model1
summary(model1)

# Notice that Average_age and Household_income do not seem to predict house prices
# Let's drop them in model2
model2 <- lm(House_price ~ Population + Crime, data = my_data)

check_model(model2)

# Is model2 now better model1?
anova(model2, model1)

AIC(model1)
AIC(model2)

# Let's validate and look at some diagnostic plots
hist(residuals(model2))
qqnorm(residuals(model2))
qqline(residuals(model2))
plot(model2)

durbinWatsonTest(model2)

# Now let's do some stepwise regression to see what we end up with
steplimitsboth <- step(model0, scope = list(upper = model1), direction = "both")

summary(steplimitsboth)

pmodel <- ols_step_forward_p(model1)
pmodel

leapsmodels <- regsubsets(House_price ~ Population + Crime + Average_age + 
                             Household_income, data = my_data)
plot(leapsmodels, scale = "adjr2", main = "Models")


confint(steplimitsboth, level=0.95)

vif(steplimitsboth)

# Bootstrappinng
set.seed(5)
speed <- as.integer(rnorm(25, 1000, 50))
speed[18] < 800
speed[2] <- 800
hist(speed)

a <- numeric(10000)
for (i in 1:10000) {
  a[i] <- mean(sample(speed, replace = TRUE))
}
hist(a)

sum(a < 950)

# The following mediation and moderation content is from the interactive R book by 
# Alexander Demos & Carlos Salas.
# email: ademos@uic.edu
# The full book with lots more content is available from here: http://ademos.people.uic.edu/index.html

# Create data for mediation analysis
set.seed(123) # Standardizes the numbers generated by rnorm
N <- 100 # Number of participants; graduate students
X <- rnorm(N, 175, 7) # IV; hours since dawn
M <- 0.7*X + rnorm(N, 0, 5) # Suspected mediator; coffee consumption 
Y <- 0.4*M + rnorm(N, 0, 5) # DV; wakefulness
Meddata <- data.frame(X, M, Y)

#using the mediation package
fitM <- lm(M ~ X,     data = Meddata) #IV on M; Hours since dawn predicting coffee consumption
fitY <- lm(Y ~ X + M, data = Meddata) #IV and M on DV; Hours since dawn and coffee predicting wakefulness

fitMed <- mediate(fitM, fitY, treat = "X", mediator = "M")
summary(fitMed)

fitMedBoot <- mediate(fitM, fitY, boot = TRUE, sims = 10000, treat = "X", mediator = "M")
summary(fitMedBoot)

# Moderation example
set.seed(123) # Standardizes the numbers generated by rnorm; see Chapter 5
N  <- 100 # Number of participants; graduate students
X  <- abs(rnorm(N, 6, 4)) # IV; Hours of sleep
X1 <- abs(rnorm(N, 60, 30)) # Adding some systematic variance for our DV
Z  <- rnorm(N, 30, 8) # Moderator; Ounces of coffee consumed
Y  <- abs((-0.8*X) * (0.2*Z) - 0.5*X - 0.4*X1 + 10 + rnorm(N, 0, 3)) # DV; Attention Paid
Moddata <- data.frame(X, X1, Z, Y)

# Centering Data
Xc    <- c(scale(X, center = TRUE, scale = FALSE)) # Centering IV; hours of sleep
Zc    <- c(scale(Z,  center = TRUE, scale = FALSE)) # Centering moderator; coffee consumption

# Moderation "By Hand"
fitMod <- lm(Y ~ Xc + Zc + Xc*Zc) # Model interacts IV & moderator
summary(fitMod)

summary(Moddata)

gvlma(fitMod)

ps <- plotSlopes(fitMod, plotx = "Xc", modx = "Zc", xlab = "Sleep", 
                 ylab = "Attention Paid", modxVals = "std.dev")

