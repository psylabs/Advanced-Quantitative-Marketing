COFFEE <- read.csv("CoffeeData.csv")

# ---------------------------
# WEEK 5 Aggregate Logit
# ---------------------------

m1   <- lm(log(Share/Outside) ~ 0+Brand.1+Brand.2+Brand.3+Brand.4+Price+Feature+Display+F.D, data=COFFEE)
summary(m1)

m2   <- lm(Price ~ 0+Brand.1+Brand.2+Brand.3+Brand.4+Feature+Display+F.D + Spot.1+Spot.2+Spot.3+Spot.4+Spot.5+Spot.6, data=COFFEE)
summary(m2)
m2.pred <- predict(m2)

m3   <- lm(log(Share/Outside) ~ 0+Brand.1+Brand.2+Brand.3+Brand.4+m2.pred+Feature+Display+F.D, data=COFFEE)
summary(m3)
