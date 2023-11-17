library(tidyverse)
library(tibble)

df    <-
  tibble(
    x_1    =    c(5, 7 , 9 , 12 , 13 , 1 , 2 , 4 , 6 , 8),
    x_2    =    c(1, 1, 4, 7, 6, 3, 1, 5, 9, 7),
    y    =    c(rep(-1,    5),    rep(1, 5))
  )    %>%
  mutate(colour = ifelse(y == -1, yes = "blue", no = "magenta"))

ggplot()    +
  geom_point(data    =    df,
             mapping    =    aes(x    =    x_1,
                                 y    =    x_2,    colour    =    colour))    +
  scale_colour_manual(NULL,
                      values = c("blue"  = "#1A85FF", "magenta" = "#D41159"))

slope    <-    1.4
intercept    <-    -5

ggplot()    +
  geom_point(data    =    df,
             mapping    =    aes(x    =    x_1,
                                 y    =    x_2,    colour    =    colour))    +
  scale_colour_manual(NULL,
                      values    =    c("blue"    =    "#1A85FF",    "magenta"    =    "#D41159"))    +
  geom_abline(slope    =    slope,    intercept    =    intercept)    +
  coord_equal(xlim    =    c(0,    15),    ylim    =    c(0, 10))    +
  theme(legend.position    =    "none")






#But other values might work just ﬁne:
slope_2    <-    1
intercept_2    <-    -2.5

ggplot()    +
  geom_point(data    =    df,
             mapping    =    aes(x    =    x_1,
                                 y    =    x_2,    colour    =    colour))    +
  scale_colour_manual(NULL,    values    =    c("blue"    =    "#1A85FF",    "magenta"    =    "#D41159"))    +
  geom_abline(slope    =    slope,    intercept    =    intercept)    +
  geom_abline(slope    =    slope_2,
              intercept    =    intercept_2,
              colour = "#FE6100")    +
  geom_label(
    data    =    tibble(x_1    = 8,    x_2 = 9,    lab    =    "x_2    =    1.4x_1    -    5"),
    mapping    =    aes(x    =    x_1,    y    =    x_2,    label    =    lab),
    fill    =    "black",
    colour    =    "white"
  )    +
  geom_label(
    data    =    tibble(x_1    =    10,    x_2 = 6,    lab    =    "x_2    =    x_1    -    2.5"),
    mapping    =    aes(x    =    x_1,    y    =    x_2,    label    =    lab),
    fill    =    "#FE6100",
    colour    =    "black"
  )    +
  coord_equal(xlim    =    c(0,    15),    ylim    =    c(0, 10))



data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris, kernel = 'linear')

model$decision.values
