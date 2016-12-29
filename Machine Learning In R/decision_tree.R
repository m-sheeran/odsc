#Decision Trees

library(rpart)

acs = read.table("http://www.jaredlander.com/data/acs_ny.csv", sep = ",", header = TRUE,
                 stringsAsFactors = FALSE)

acs$Income = acs$FamilyIncome >= 150000

acsFormula = Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms + NumUnits + 
  NumVehicles + NumWorkers + OwnRent + YearBuilt + ElectricBill + 
  FoodStamp + HeatingFuel + Insurance + Language



incomeTree1 = rpart(acsFormula, data = acs, method = "class")

#Plot Decision Tree
library(rpart.plot)

rpart.plot(incomeTree1, extra = 4)

#Shows That Insurance will split
incomeTree2 = rpart(Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms + NumUnits + 
                      NumVehicles + NumWorkers + OwnRent + YearBuilt + ElectricBill + 
                      FoodStamp + HeatingFuel + Language, data = acs,
                    method = "class")

rpart.plot(incomeTree2, extra = 4)


load(url('http://www.jaredlander.com/data/credit.rdata'))

head(credit)


creditTree = rpart(Credit ~ CreditAmount + Age + CreditHistory + Employment, 
                   data = credit)

rpart.plot(creditTree, extra = 4)

#Shows percentage of Data that falls into bucket
rpart.plot(creditTree, extra = 104)

rpart.plot(creditTree, extra = 4, branch = .1, under = TRUE)

library(randomForest)

creditForest1 = randomForest(Credit ~ CreditAmount + Age + CreditHistory + Employment, 
                             data = credit)

#Create Design Matrix

library(useful)


creditX = build.x(Credit ~ CreditAmount + Age + CreditHistory + Employment - 1, 
                  data = credit)

creditY = build.y(Credit ~ CreditAmount + Age + CreditHistory + Employment - 1, 
                  data = credit)


creditForest = randomForest(x = creditX, y = creditY)
plot(creditForest)

#How often variable was used - How many times it affected the splits
varImpPlot(creditForest)



library(caret)

#repeats is number of times to do CV, number is number of folds
treeControls = trainControl(method = 'repeatedcv', repeats = 3, number = 5)

#Tress can be optimized over complexity or max depth. Can't optimize both at same time

#Caret willl fit trees off of this max depth
treeGrid = data.frame(maxdepth = 1:10)

tree1 = train(Credit ~ CreditAmount + Age + CreditHistory + Employment,
              data = credit, method = 'rpart2', trControl = treeControls, tuneGrid = treeGrid)



plot(tree1)
varImp(tree1)

