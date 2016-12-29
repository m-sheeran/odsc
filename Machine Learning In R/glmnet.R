#glmnet
#Takes Care of Multi-Colinearity

acs = read.table("http://www.jaredlander.com/data/acs_ny.csv", sep = ",", header = TRUE,
                 stringsAsFactors = FALSE)


acs$Income = acs$FamilyIncome >= 150000


ggplot(acs, aes(x = FamilyIncome)) + geom_density(fill = "grey", color = "grey") + 
                                      geom_vline(xintercept = 150000) +
                                    scale_x_continuous(labels = useful::multiple.dollar, limits = c(0, 1000000))


library(glmnet)
library(useful)


head(model.matrix( ~OwnRent, data = acs))

head(build.x( ~OwnRent + HeatingFuel, data = acs, contrasts = FALSE))

#Drop Intercept column since glmnet already supplies intercepts
head(build.x( ~OwnRent + HeatingFuel -1, data = acs, contrasts = FALSE))

class(acsFormula)

#Contrasts = FALSE gives all columns. Contrasts gives you baseline (e.g. between Own Outright, Mortgage, and Rent - only Own Outright and Rent)
acsX = build.x(formula = acsFormula, data = acs, contrasts = FALSE)

corner(acsX)

View(acsX)

# No Contrasts in Y Matrix
acsY = build.y(formula = acsFormula, data = acs)
head(acsY, 15)


income1 = glmnet(x = acsX, y = acsY, family = "binomial")

summary(income1)

plot(income1, xvar = 'lambda')

#Choose Right value for lambda based off of Cross Validation
#Rule of thumb for cross validation folds: sqrt of nrow
#5 folds reasonable base

library(boot)


incomeg1 = glm(Income ~ NumBedrooms + FamilyType + OwnRent,
               data = acs, family = binomial)


#Cross Validation
income_gcv1 = cv.glm(data = acs, glmfit = incomeg1, K = 5)

income_gcv1$delta
#First number is cross-validated error. Second number is leave one out cross validated error (approximated)

#Can up-sample and down-sample stuff for add tech - Only 1% maybe click on adds. Have to make sure training
#set is representative

income2 = cv.glmnet(x =acsX, y = acsY, family = "binomial", nfolds = 5)
plot(income2)

#For a different value of lambda, what is the error?
#Dot represents error, bars are uncertainty in error
#Rule of parsimony - choose right vertical line - error almost as good as model on left line. Within one standard error of more complicated
#model

#Left line
income2$lambda.min
#Right line
income2$lambda.1se

library(coefplot)

coefplot(income2, lambda = 'lambda.1se')

#To get Ridge set alpha to zero
income3 = glmnet(x = acsX, y = acsY, family = "binomial", alpha = 0)

plot(income3, xvar = 'lambda')

#In ridge, variables never really go to zero. Just shrinks coefficients down
#Now cross validate
income4 = cv.glmnet(x = acsX, y = acsY, family = "binomial", alpha = 0, nfolds = 5)

#Lasso is better but not by much
income2$cvm[income2$lambda == income2$lambda.1se]
income4$cvm[income4$lambda == income4$lambda.1se]


income5 = cv.glmnet(x = acsX, y = acsY, family = 'binomial', nfolds = 5, alpha = .7)
income5$cvm[income5$lambda == income5$lambda.1se]

coefplot(income2, lambda = 'lambda.1se', sort = 'mag')

#To optimally choose alpha and lambda choose use Caret package
