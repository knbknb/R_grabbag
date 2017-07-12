library(ggplot2)
library(dplyr) # data_frame, filter
data(mtcars)
mod <- loess(hwy ~ displ, data = mpg)
grid <- data_frame(displ = seq(min(mpg$displ), max(mpg$displ), length = 50))
grid$hwy <- predict(mod, newdata = grid)
std_resid <- resid(mod) / mod$s
outlier <- filter(mpg, abs(std_resid) > 2)
outlier
ggplot(mpg, aes(displ, hwy)) +
        geom_point() +
        geom_line(data = grid, colour = "blue", size = 1.5) +
        geom_text(data = outlier, aes(label = model))

# exercise 5.3.1
# generate some summary statistics about
# each class of car
classes <- mpg %>%
        group_by(class) %>%
        summarise(n = n(), hwy = median(hwy))

ggplot(classes, aes(class, hwy)) +
        geom_jitter(data = mpg[, c("class", "hwy")], size = 3.5, width = 0.2) +
        geom_point(colour = "red", size = 3.5) +
        geom_text(aes(y= 10, label =  paste0("n=", classes$n)))
