library(ggplot2)
library(ggimage)
library(ggthemes)

# if needed, download ugtests data
url <- "http://peopleanalytics-regression-book.org/data/ugtests.csv"
ugtests <- read.csv(url)

s <- data.frame(
  Yr3 = c(20, 50, 85, 105,  125, 145, 180, 190),
  Final = c(20, 100, 30, 150, 170, 70, 150, 110)
)

image <- "art/personicon.png"

model <- lm(Final ~ Yr3, data = s)


ggplot2::ggplot(data = s, aes(x = Yr3, y = Final)) +
  geom_image(aes(image = image), size = .15) +
  geom_smooth(method = "lm", se = FALSE, size = 2, linetype = "dashed", color = "#c10e95") +
  ggplot2::geom_segment(aes(x = Yr3[1], y = model$fitted.values[1], xend = Yr3[1], yend = Final[1]), color = "#c10e95") +
  ggplot2::geom_segment(aes(x = Yr3[2], y = model$fitted.values[2], xend = Yr3[2], yend = Final[2]), color = "#c10e95") +
  ggplot2::geom_segment(aes(x = Yr3[3], y = model$fitted.values[3], xend = Yr3[3], yend = Final[3]), color = "#c10e95") +
  ggplot2::geom_segment(aes(x = Yr3[4], y = model$fitted.values[4], xend = Yr3[4], yend = Final[4]), color = "#c10e95") +
  ggplot2::geom_segment(aes(x = Yr3[5], y = model$fitted.values[5], xend = Yr3[5], yend = Final[5]), color = "#c10e95") +
  ggplot2::geom_segment(aes(x = Yr3[6], y = model$fitted.values[6], xend = Yr3[6], yend = Final[6]), color = "#c10e95") +
  ggplot2::geom_segment(aes(x = Yr3[7], y = model$fitted.values[7], xend = Yr3[7], yend = Final[7]), color = "#c10e95") +
  ggplot2::geom_segment(aes(x = Yr3[8], y = model$fitted.values[8], xend = Yr3[8], yend = Final[8]), color = "#c10e95") +
  xlim(0, 200) +
  ylim(0, 200) +
  theme_void()
