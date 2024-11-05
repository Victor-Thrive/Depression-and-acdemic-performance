#--------EXPLORATORY FACTOR ANALYSIS-------
# library
library(psych)
library(GPArotation)
library(mice)
library(tidyverse)
library(ggplot2)
library(reshape2)

survey <- read.csv("data/survey.csv", stringsAsFactors = TRUE) |>
  mutate(row_id = row_number())

# males = 56% and female = 44%
survey |>
  count(gender) |>
  mutate(prop = n /sum(n))

  
master<- survey |>
         select(X,matches("^E([1-4]|[6-9]|[12][0-9]|30)$")) # select X, E1:E4 & E5:E30 


# handling missing data
# replacing NA's with 3
master[is.na(master)] <- 0

head(master)

##missing
percentmissing <- function(x){ sum(is.na(x))/length(x) * 100}
missing <- apply(master, 1, percentmissing)
table(missing)

##accuracy
summary(master)

##outliers
cutoff <- qchisq(1-.001,ncol(master[,-1]))
mahal <- mahalanobis(master[,-1],colMeans(master[-1]),
                     cov(master[-1]))

# we exclude variable E5 in line 13 because of the error below
# Error in solve.default(cov, ...) : 
# Lapack routine dgesv: system is exactly singular: U[5,5] = 0
cutoff ##cutoff score

summary(mahal < cutoff)

##exclude outlier
noout <- subset(master, mahal < cutoff)

##additive
correl <- cor(noout[,-1], use = "pairwise.complete.obs")

#check variablewith sd = 0
sapply(noout, function(x) sd(x, na.rm = TRUE))

# remove variable with sd = 0
noout <- noout[, sapply(noout, function(x) sd(x, na.rm = TRUE) > 0)]

##additive
correl <- cor(noout[,-1], use = "pairwise.complete.obs")

symnum(correl)
correl

# Compute and reshape correlation matrix
correl_melted <- melt(correl[,-1])

# Plot with ggplot2
ggplot(data = correl_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, 
                       limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))
set.seed(190)
##assumption set up
random <- rchisq(nrow(noout), 5)
fake <- lm(random~., data = noout[,-1])

standardized <- rstudent(fake)
fitted <- scale(fake$fitted.values)

## normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

#homogeneity
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

##running the EFA analysis

##correlation adequacy Bartlett's test
cortest.bartlett(correl[,-1], n = nrow(noout))

##sampling adequacy KMO test
KMO(correl[,-1])

##how many factors?
nofactors <- fa.parallel(noout[,-1], fm = "ml", fa = "fa")
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a three factor model
round1 <- fa(noout[,-1], nfactors = 3, rotate = "oblimin", fm = "ml")
round1
# round 1 list of useful variables c(E2,E11,E20,E29)

round2 <- fa(noout[ , -c(1,3,6,8,15,23,25,26)], nfactors = 3, rotate = "oblimin", fm = "ml")
round2

# round 2 list of useful variable c(E2,E26,E18,e24,E22,E30)

# combine list of useful variables c(E2,E11,E20,E29,E26,E18,E24,E22E30)

noout_useful <- noout %>%
  select(E2, E11, E20, E29, E26, E18, E24, E22, E30)

# Perform factor analysis on the selected useful variables
finalmodel <- fa(noout_useful, nfactors = 3, rotate = "oblimin", fm = "ml")

##get cfi
1 - ((finalmodel$STATISTIC - finalmodel$dof)/(finalmodel$null.chisq - finalmodel$null.dof))


## Reliability Analysis
# Define factors as character vectors of column names
  # Assuming "E30" is the name of the column
factor1 <- c("E30","E29")
factor2 <- c("E2", "E29", "E20", "E26")  # All column names in quotes
factor3 <- c("E11", "E18", "E24", "E22")  # All column names in quotes

# Calculate Cronbach's alpha for each factor
alpha_factor1 <- psych::alpha(noout[, factor1])
alpha_factor2 <- psych::alpha(noout[, factor2])
alpha_factor3 <- psych::alpha(noout[, factor3])

# Print results
#print(alpha_factor1)

# Assuming noout is your data frame
noout <- noout %>%
  mutate(
    f1 = ceiling(rowMeans(select(., all_of(factor1)), na.rm = TRUE)),  # Create average score for factor1
    f2 = ceiling(rowMeans(select(., all_of(factor2)), na.rm = TRUE)),  # Create average score for factor2
    f3 = ceiling(rowMeans(select(., all_of(factor3)), na.rm = TRUE))   # Create average score for factor3
  )


summary(noout)
sd(noout$f1)
sd(noout$f2)
sd(noout$f3)


View(noout)

# Inner join on the 'id' column
final_df <- inner_join(survey, noout, by = "X") |>
  select(c("age","gender","college","department",
           "level","religion","G.P.A","C.G.P", 
           "status","sponsor","stress_score","stress_level","f1","f2","f3"))

#write_rds(survey,"new_my_survey.rds")
write.csv(final_df,"data/final.csv")
