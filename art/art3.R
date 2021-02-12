library(peopleanalyticsdata)
library(MASS)
library(plotly)

simpler_formula <- "rating ~ sales + new_customers"

employee_performance$rating <- as.factor(employee_performance$rating)

## create simpler model
simpler_model <- polr(data = employee_performance,
                      formula = simpler_formula)


p <- plot_ly(data = employee_performance) %>%
  add_trace(z = simpler_model$fitted.values[ , 1], x = ~sales, y = ~new_customers, type = "mesh3d",
            name = "P(Low Performance)") %>%
  add_trace(z = simpler_model$fitted.values[ , 2], x = ~sales, y = ~new_customers, type = "mesh3d",
            name = "P(Middle Performance)") %>%
  add_trace(z = simpler_model$fitted.values[ , 3], x = ~sales, y = ~new_customers, type = "mesh3d",
            name = "P(High Performance)") %>%
  layout(scene = list(xaxis = list(title = 'sales'), yaxis = list(title = 'new_customers'),
                      zaxis = list(title = 'Probability'), aspectmode='cube'))
