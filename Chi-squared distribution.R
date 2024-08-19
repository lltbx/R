library(readxl)
library(ggplot2)

data <- read_excel("C:/Users/misha/OneDrive/Documents/My cloud projects/R/Variant1.xlsx")

#Графік до вилучення викидів 
ggplot(data,  aes(x = 1:nrow(data), y = V)) +
  geom_point() +
  labs(title = "Графік даних до вилучення викиду", x = "Номер спостереження", y = "Значення") +
  theme_minimal()

# Пошук квантилів та вилучення викиду 
Q1 <- quantile(data$V, 0.25)
Q3 <- quantile(data$V, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data_no_outlier <- data %>% filter(V > lower_bound & V < upper_bound)

#Графіік після вилучення викду 
ggplot(data_no_outlier,  aes(x = 1:nrow(data_no_outlier), y = V)) +
  geom_point() +
  labs(title = "Графік даних після вилучення викиду ", x = "Номер спостереження", y = "Значення") 

# Оцінка середнього, середньоквадратичного відхилення, мінімуму та максимуму
mean_est <- mean(data_no_outlier$V)
sd_est <- sd(data_no_outlier$V)
min_est <- min(data_no_outlier$V)
max_est <- max(data_no_outlier$V)

#Гістограми
ggplot(data_no_outlier, aes(x = V)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue") + 
  stat_function(fun = function(x) dnorm(x, mean = mean_est, sd = sd_est), color = "red", size = 1) + 
  labs(title = "Гістограма нормального розподілу", x = "Значення", y = "Щільність") + 
  theme_minimal()

ggplot(data_no_outlier, aes(x = V)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue") + 
  stat_function(fun = function(x) dunif(x, min = min_est, max = max_est), color = "green", size = 1) + 
  labs(title = "Гістограма рівномірного розподілу", x = "Значення", y = "Щільність") + 
  theme_minimal()

# Ящик з вусами 
ggplot(data_no_outlier, aes(y = V)) + 
  geom_boxplot() + 
  labs(title = "Ящик з вусами", y = "Значення")

#Розрахунок емпіричних частот, теоретичних частот та побудова фреймів
ecdf_data <- ecdf(data_no_outlier$V)
empirical_probs <- ecdf_data(data_no_outlier$V)
theoretical_probs_normal <- pnorm(data_no_outlier$V, mean = mean_est, sd = sd_est)
theoretical_probs_uniform <- punif(data_no_outlier$V, min = min_est, max = max_est)
df_normal <- data.frame(empirical = empirical_probs, theoretical = theoretical_probs_normal)
df_uniform <- data.frame(empirical = empirical_probs, theoretical = theoretical_probs_uniform)

# P-P та Q-Q діаграми
ggplot(df_normal, aes(x = empirical, y = theoretical)) + 
  geom_point() + 
  geom_abline( color = "red") + 
  labs(title = "P-P діаграма для нормального розподілу", x = "Емпіричні квантилі", y = "Теоретичні квантилі")
ggplot(df_uniform, aes(x = empirical, y = theoretical)) + 
  geom_point() + 
  geom_abline(color = "green") + 
  labs(title = "P-P діаграма для рівномірного розподілу", x = "Емпіричні квантилі", y = "Теоретичні квантилі")
ggplot(data_no_outlier, aes(sample = V)) + 
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q діагрма для нормального розподілу", x = "Теоретичні квантилі", y = "Квантилі з вибірки")
ggplot(data_no_outlier, aes(sample = V)) + 
  stat_qq(distribution = qunif, dparams = list(min = min_est, max = max_est)) +
  stat_qq_line(distribution = qunif, dparams = list(min = min_est, max = max_est)) +
  labs(title = "Q-Q діаграма для рівномірного розподілу ", x = "Теоретичні квантиілі", y = "Квантилі з вибірки")

# Розбиття на 10 інтервалів (11 меж)
num_intervals <- 10
breaks <- seq(min = min_est, max = max_est, length.out = num_intervals + 1)
observed_freq <- hist(data_no_outlier$V, breaks = breaks, plot = FALSE)$counts
print(observed_freq)

# Обчислення очікуваних частот
expected_freq_normal <- diff(pnorm(breaks, mean = mean_est, sd = sd_est)) * length(data_no_outlier$V)
expected_freq_uniform <- rep(length(data_no_outlier$V) / num_intervals, num_intervals)
print(expected_freq_normal)
print(expected_freq_uniform)

# Обчислення хі-квадрат та p-value
chi_sq_normal <- sum((observed_freq - expected_freq_normal)^2 / expected_freq_normal)
p_value_normal <- 1 - pchisq(chi_sq_normal, df = num_intervals - 2)
chi_sq_uniform <- sum((observed_freq - expected_freq_uniform)^2 / expected_freq_uniform)
p_value_uniform <- 1 - pchisq(chi_sq_uniform, df = num_intervals - 2)

cat("χ2 =", chi_sq_normal, "p-value =", p_value_normal, "\n")
cat("χ2 =", chi_sq_uniform, "p-value =", p_value_uniform, "\n")

# Тести Колмогорова-Смірнова 
print(ks.test(data_no_outlier$V, "pnorm", mean = mean_est, sd = sd_est))
print(ks.test(data_no_outlier$V, "punif", min = min_est, max = max_est))
