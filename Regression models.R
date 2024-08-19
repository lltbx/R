library(ggplot2)
library(corrplot)
library(car)
library(olsrr)

data<-read.table(file.choose(),header=TRUE)
attach(data)

#1Матрична діаграма розсіювання данних 
pairs(data[2:21],col="darkgreen")

#2Кореляційна матриця змінних 
M<-cor(data[2:21])
M

#2.1) Кольорова кореляційна матриця данних 
corrplot(M,method="color")

#3 Модель множинної регрессії COL_incl_rent від решти змінних.
COLIR<-lm(COL_incl_rent ~ Food_Costs... + Womens_Clothing... +Mens_Clothing... +iPhone_4S.hr. +Clothing_Index +Hours_Worked +Wage_Gross +Wage_Net
          +Vacation_Days +COL_Excl_rent +Pur_Power_Gross +Pur_Power_Net +Pur_Power_Annual +BigMac +Bread.kg_in_min. +Rice.kg_in_min.
          + Goods_and_Services... +Food_Index,data=data)
summary(COLIR)
alias(COLIR)
#3.1 Оцінка за критерієм Фішера
hypothesis <- "Pur_Power_Gross - 1.2 * Pur_Power_Net= 0"
linearHypothesis(COLIR, hypothesis)

#4 Лінійна регрессійна модель COL_incl_rent від найбільш значущих змінних
COLINSA<- lm(COL_incl_rent ~ Goods_and_Services... + Hours_Worked + iPhone_4S.hr. + Clothing_Index + Wage_Gross + Pur_Power_Annual + Vacation_Days + Rice.kg_in_min., data=data)


#4.1 Регрессія назад
backward_model <- ols_step_backward_p(COLINSA)
summary(backward_model$model)
#4.2 Регрессія вперед
forward_model <- ols_step_forward_p(COLINSA)
summary(forward_model$model)
#4.3 Регресія комбінована
stepwise_model <- ols_step_both_p(COLINSA)
summary(stepwise_model$model)
#4.4 Predicted vs residuals, нормальність залишків
qqnorm(COLINSA$residuals)
qqline(COLINSA$residuals, col=5)
plot(COLINSA$fitted.values,COLINSA$residuals, col=3)

#5 Лінійна регрессійна модель BigMac від найбільш значущих змінних
MAC<-lm(BigMac ~  Hours_Worked +Wage_Gross +Wage_Net+Vacation_Days+COL_Excl_rent +Pur_Power_Gross+Bread.kg_in_min. +Rice.kg_in_min. ,data=data)

#5.1 Регрессія назад
backward_model <- ols_step_backward_p(MAC)
summary(backward_model$model)
#5.2 Регрессія вперед
forward_model <- ols_step_forward_p(MAC)
summary(forward_model$model)
#5.3 Регресія комбінована
stepwise_model <- ols_step_both_p(MAC)
summary(stepwise_model$model)
#5.4 Predicted vs residuals, нормальність залишків
qqnorm(MAC$residuals)
qqline(MAC$residuals, col=5)
plot(MAC$fitted.values,MAC$residuals, col=3)
