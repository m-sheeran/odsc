#GLM
# What if response variable is binary?

acs = read.table("http://www.jaredlander.com/data/acs_ny.csv", header = TRUE,
                 stringsAsFactors = FALSE, sep = ",")

acs$Income = acs$FamilyIncome >= 150000

income1 = glm(Income ~ HouseCosts + NumWorkers + OwnRent + 
                NumBedrooms + FamilyType,
              data = acs, family = binomial(link = "logit"))

library(coefplot)

coefplot(income1, sort = "mag")

children = glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent,
               data = acs, family = poisson(link = 'log'))

children2 = glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent,
               data = acs, family = quasipoisson(link = 'log'))

multiplot(children, children2)
