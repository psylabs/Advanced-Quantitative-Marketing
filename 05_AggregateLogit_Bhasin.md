# WEEK 5 Aggregate Logit (Coffee)
========================================================
---
title: "Sample Document"
output:
  html_document:
    toc: true
    theme: united
---


```r
COFFEE <- read.csv("CoffeeData.csv")

m1   <- lm(log(Share/Outside) ~ 0+Brand.1+Brand.2+Brand.3+Brand.4+Price+Feature+Display+F.D, data=COFFEE)
summary(m1); BIC (m1)
```

```
## 
## Call:
## lm(formula = log(Share/Outside) ~ 0 + Brand.1 + Brand.2 + Brand.3 + 
##     Brand.4 + Price + Feature + Display + F.D, data = COFFEE)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.8373 -0.2058 -0.0264  0.1761  1.8811 
## 
## Coefficients:
##           Estimate Std. Error t value Pr(>|t|)    
## Brand.1 -4.4655910  0.1887880 -23.654  < 2e-16 ***
## Brand.2 -6.1197602  0.1139906 -53.687  < 2e-16 ***
## Brand.3 -4.0071862  0.1224083 -32.736  < 2e-16 ***
## Brand.4 -5.2927252  0.1277089 -41.444  < 2e-16 ***
## Price   -0.0994008  0.0381110  -2.608 0.009406 ** 
## Feature  0.0044948  0.0007022   6.401  3.9e-10 ***
## Display  0.0078291  0.0020048   3.905 0.000109 ***
## F.D     -0.0054117  0.0025813  -2.097 0.036598 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3179 on 448 degrees of freedom
## Multiple R-squared:  0.9964,	Adjusted R-squared:  0.9964 
## F-statistic: 1.566e+04 on 8 and 448 DF,  p-value: < 2.2e-16
```

```
## [1] 295.9821
```

```r
m2   <- lm(Price ~ 0+Brand.1+Brand.2+Brand.3+Brand.4+Feature+Display+F.D + Spot.1+Spot.2+Spot.3+Spot.4+Spot.5+Spot.6, data=COFFEE)
summary(m2)
```

```
## 
## Call:
## lm(formula = Price ~ 0 + Brand.1 + Brand.2 + Brand.3 + Brand.4 + 
##     Feature + Display + F.D + Spot.1 + Spot.2 + Spot.3 + Spot.4 + 
##     Spot.5 + Spot.6, data = COFFEE)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.97414 -0.13728 -0.00425  0.14210  0.89021 
## 
## Coefficients:
##           Estimate Std. Error t value Pr(>|t|)    
## Brand.1  3.7741914  0.0726716  51.935  < 2e-16 ***
## Brand.2  1.7739697  0.0719385  24.660  < 2e-16 ***
## Brand.3  1.9093820  0.0759060  25.155  < 2e-16 ***
## Brand.4  2.1587197  0.0713839  30.241  < 2e-16 ***
## Feature -0.0003172  0.0006727  -0.471  0.63754    
## Display  0.0059110  0.0019538   3.025  0.00263 ** 
## F.D      0.0033903  0.0024524   1.382  0.16754    
## Spot.1   0.0007234  0.0006100   1.186  0.23632    
## Spot.2   0.0015576  0.0007744   2.011  0.04488 *  
## Spot.3   0.0011466  0.0007834   1.464  0.14402    
## Spot.4   0.0012091  0.0008041   1.504  0.13338    
## Spot.5  -0.0008714  0.0008176  -1.066  0.28710    
## Spot.6   0.0026617  0.0006525   4.079 5.36e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2992 on 443 degrees of freedom
## Multiple R-squared:  0.9934,	Adjusted R-squared:  0.9932 
## F-statistic:  5109 on 13 and 443 DF,  p-value: < 2.2e-16
```

Spot prices do a poor job in explaining the variance in price, p-values show that all but Spot.6 are poor predictors of Price


```r
m2.pred <- predict(m2)

m3   <- lm(log(Share/Outside) ~ 0+Brand.1+Brand.2+Brand.3+Brand.4+m2.pred+Feature+Display+F.D, data=COFFEE)
summary(m3); BIC (m3)
```

```
## 
## Call:
## lm(formula = log(Share/Outside) ~ 0 + Brand.1 + Brand.2 + Brand.3 + 
##     Brand.4 + m2.pred + Feature + Display + F.D, data = COFFEE)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.89770 -0.19891 -0.02227  0.16170  1.85954 
## 
## Coefficients:
##           Estimate Std. Error t value Pr(>|t|)    
## Brand.1 -3.9190975  0.2831446 -13.841  < 2e-16 ***
## Brand.2 -5.7974662  0.1687393 -34.358  < 2e-16 ***
## Brand.3 -3.6669249  0.1795261 -20.426  < 2e-16 ***
## Brand.4 -4.9289331  0.1898287 -25.965  < 2e-16 ***
## m2.pred -0.2115853  0.0577032  -3.667 0.000275 ***
## Feature  0.0045769  0.0006979   6.558  1.5e-10 ***
## Display  0.0076016  0.0019923   3.816 0.000155 ***
## F.D     -0.0051640  0.0025645  -2.014 0.044641 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3156 on 448 degrees of freedom
## Multiple R-squared:  0.9965,	Adjusted R-squared:  0.9964 
## F-statistic: 1.589e+04 on 8 and 448 DF,  p-value: < 2.2e-16
```

```
## [1] 289.3702
```

Using a 2 Stage Least Squares (Instrumental Variable) to predict market share, we find that the new model has a smaller BIC, indicating that the model is better.

Now that we can make a causal inference assuming no price endogenity we see that the price elasticity \( \beta_p \) is more negative, indicating a stronger sensitivity to price.

The 99% R^2 is suspicious.
