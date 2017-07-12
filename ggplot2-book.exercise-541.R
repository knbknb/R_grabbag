# 5.4.3 Exercises
# 1. Simplify the following plot specifications:
library(ggplot2)
library(dplyr) # data_frame, filter
#data(mtcars)
data(mpg)
data("diamonds")
data("msleep")

# before
ggplot(mpg) +
        geom_point(aes(mpg$displ, mpg$hwy))

#after
ggplot(mpg, aes(displ, hwy)) +
        geom_point()


# before
ggplot() +
        geom_point(mapping = aes(y = hwy, x = cty), data = mpg) +
        geom_smooth(data = mpg, mapping = aes(cty, hwy))

# after
ggplot(data = mpg) +
        geom_point(aes(cty, hwy)) +
        geom_smooth(mapping = aes(cty, hwy), method = "lm")

# before
ggplot(diamonds, aes(carat, price)) +
        geom_point(aes(log(brainwt), log(bodywt)), data = msleep)

#after
# ggplot(diamonds, aes(carat, price)) +
#         geom_point(aes(log(brainwt), log(bodywt)), data = msleep)

# 2. What does the following code do? Does it work? Does it make sense?
# Why/why not?
ggplot(mpg) +
        geom_boxplot(aes(class, cty), alpha=.3)+
        geom_point(aes(class, cty))

# 3. What happens if you try to use a continuous variable on the x axis in one
# layer, and a categorical variable in another layer? What happens if you do
# it in the opposite order?
