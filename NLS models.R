library(ggplot2)
library(GGally)

data <- data.frame(
  Team = c("Arizona_Cardinals", "Atlanta_Falcons", "Baltimore_Ravens", "Buffalo_Bills", "Carolina_Panthers", 
           "Chicago_Bears", "Cincinnati_Bengals", "Cleveland_Browns", "Dallas_Cowboys", "Denver_Broncos", 
           "Detroit_Lions", "Green_Bay_Packers", "Houston_Texans", "Indianapolis_Colts", "Jacksonville_Jaguars", 
           "Kansas_City_Chiefs", "Los_Angeles_Rams", "Miami_Dolphins", "Minnesota_Vikings", "New_England_Patriots", 
           "New_Orleans_Saints", "New_York_Giants", "New_York_Jets", "Oakland_Raiders", "Philadelphia_Eagles", 
           "Pittsburgh_Steelers", "San_Diego_Chargers", "San_Francisco_49ers", "Seattle_Seahawks", 
           "Tampa_Bay_Buccaneers", "Tennessee_Titans", "Washington_Redskins"),
  Wins = c(7, 11, 7, 7, 6, 3, 6, 1, 13, 9, 9, 10, 9, 8, 3, 12, 4, 10, 8, 14, 7, 11, 5, 7, 7, 11, 5, 2, 10, 9, 9, 8),
  Avg_Age = c(27.4, 26.45, 26.6, 26.7, 26.75, 26.28, 25.52, 24.25, 26.04, 26.11, 25.77, 25.85, 25.6, 25.62, 25.64, 
              25.87, 25.23, 26.62, 26.06, 26.42, 26.55, 25.68, 25.58, 26.09, 26.36, 26.06, 25.66, 25.77, 25.75, 
              26.23, 26.53, 25.94),
  Active = c(154384100.00, 157666798.00, 134670306.00, 139720093.00, 154994278.00, 144061149.00, 139855574.00, 
             113317279.00, 137061990.00, 149908645.00, 141941975.00, 150044453.00, 133467065.00, 135822350.00, 
             153411580.00, 141076528.00, 146914285.00, 130477781.00, 136268561.00, 142473404.00, 127273255.00, 
             155321043.00, 121122908.00, 158310806.00, 145518303.00, 152459177.00, 138405987.00, 109640818.00, 
             153648618.00, 141511896.00, 150611974.00, 155812636.00),
  Salary = c(154.3841, 157.666798, 134.670306, 139.720093, 154.994278, 144.061149, 139.855574, 113.317279, 
             137.06199, 149.908645, 141.941975, 150.044453, 133.467065, 135.82235, 153.41158, 141.076528, 
             146.914285, 130.477781, 136.268561, 142.473404, 127.273255, 155.321043, 121.122908, 158.310806, 
             145.518303, 152.459177, 138.405987, 109.640818, 153.648618, 141.511896, 150.611974, 155.812636)
)


# 1. Побудова матричної діаграми розсіювання
ggpairs(data[, c("Wins", "Avg_Age", "Active", "Salary")])

# 2. Побудова графіків "ящики з вусами"
ggplot(data, aes(x = "", y = Wins)) +
  geom_boxplot() +
  ggtitle("Boxplot for Wins") +
  ylab("Wins")

ggplot(data, aes(x = "", y = Avg_Age)) +
  geom_boxplot() +
  ggtitle("Boxplot for Average Age") +
  ylab("Average Age")

ggplot(data, aes(x = "", y = Active)) +
  geom_boxplot() +
  ggtitle("Boxplot for Active") +
  ylab("Active")

ggplot(data, aes(x = "", y = Salary)) +
  geom_boxplot() +
  ggtitle("Boxplot for Salary") +
  ylab("Salary")
#2
linear_model <- lm(Salary ~ Active, data = data)
model_summary <- summary(linear_model)
model_summary

# 3. NLS
start_values <- list(a = 1, b = 5, c = 2)
non_linear_model <- nls(Active ~ a + b * exp(c * Wins), data=data, start=start_values)
summary(non_linear_model)
plot(predict(non_linear_model) ~ resid(non_linear_model), main="Predicted vs Residuals")

#4 Модель за допомогою лінеарезації та NLS, розрахунок похибки
log_linear_model <- lm(log(Active) ~ log(Wins) + log(Avg_Age), data=data)
summary(log_linear_model)

start_values <- list(a=0.6065, b=0.2569, c=-0.0170)
alt_non_linear_model <- nls(Active ~ a *(Wins)^b *(Avg_Age)^c, data=data, algorithm = "port", start=start_values, upper = c(exp(-0.00005), Inf, Inf), lower = c(exp(-0.5), -Inf, -Inf))
summary(alt_non_linear_model)

plot(predict(alt_non_linear_model) ~ resid(alt_non_linear_model), main="Predicted vs Residuals")
identify(alt_non_linear_model&predict.alt_non_linear_model$residuals)

#5 Обрання кращої моделі за допоогою критеріїв AIC, BIC
AIC(linear_model, non_linear_model, log_linear_model, alt_non_linear_model)
BIC(linear_model, non_linear_model, log_linear_model, alt_non_linear_model)

