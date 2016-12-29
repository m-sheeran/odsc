housePredict = predict(object = house2, newdata = housingNew,
                       se.ft = TRUE, interval = 'prediction')
head(housePredict)


View(housePredict)

AIC(house2)
read.table("http://www.jaredlander.com/data/housing1.csv")

ggplot(house2, aes(x = .fitted, y = .resid)) + geom_point(aes(color = Boro)) 
+ geom_hline(yintercept = 0) 
+ geom_smooth(se = FALSE)