library(ISLR)
library(tidyverse)
library(leaps)
library(patchwork)
library(glmnet)

file <- file.path("C:","Projects","GreenNewDeal","GNDScenarioAnalysis1.3-test.xlsx")

scn <- 
  xlsx::read.xlsx(file = file, sheetIndex = 1, 
                  rowIndex = c(3:148), colIndex = c(3:115), stringsAsFactors=FALSE)

scn <- 
  scn %>%
  filter(!is.na(reserves)) %>%
  select(-contains("NA.")) %>%
  select(-initial.population, -natural.gas.emission.factor) %>%
  mutate_all(as.numeric) %>%
  dplyr::select(-Net.immigration.rate.2050) %>%
  filter(total.costs.S.bil < 200) %>%
  filter(natural.gas.cost != 0) %>%
  dplyr::select(9:87)

  
x = model.matrix(reserves~.,scn)
y = scn$reserves

grid = 10^seq(10,-2,length = 131)

#training and test sets

train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]


lasso.mod <- glmnet(x[train,], y[train], alpha = 1)

plot(lasso.mod)

cv.out = cv.glmnet(x[train,], y[train], alpha = 1)

plot(cv.out)

bestlam = cv.out$lambda.min

lasso.pred = predict(lasso.mod, s=bestlam, newx = x[test,])

mean((lasso.pred-y.test)^2)

out = glmnet(x,y,alpha = 1, lambda = grid)

lasso.coef = predict(out, type="coefficients", s=bestlam)[1:79,]

lasso.coef

lasso.coef[lasso.coef!=0]
