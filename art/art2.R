library(dplyr)
library(ggplot2)
library(peopleanalyticsdata)

graduates <- graduates %>%
  dplyr::filter(Median_salary <= 75000)

graduates$scaled <- rescale(graduates$Median_salary, to = c(max(graduates$Unemployment_rate), min(graduates$Unemployment_rate)))

model <- lm(Median_salary ~ scaled, graduates)

ggplot(graduates, aes(x = Unemployment_rate, y = Median_salary)) +
  geom_point(color = "blue") +
  geom_jitter(color = "blue") +
  geom_function(fun = function(x) {y = model$coefficients[1] + model$coefficients[2]*x}) +
  geom_smooth(method='lm', formula = y~x, se = FALSE, color = "red", linetype = "dashed") +
  xlab("Unemployment Rate") +
  ylab("Median Salary") +
  theme_minimal()
