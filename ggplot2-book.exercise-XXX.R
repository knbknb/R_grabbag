# ~/code/svn/eclipse38_dynlang/R_one-offs
library(ggplot2)
summary(seals) # Vector field of seal movements

z = 1
r = 1
a <- ggplot(seals, aes(x=long, y=lat))
a + geom_curve(aes(xend=long + delta_long, yend = lat + delta_lat, curvature=z))
a + geom_spoke(aes(xend=long + delta_long, yend = lat + delta_lat, angle=z, radius=r))


df <- data.frame(
        x = c(1,3,2,5),
        y = c("a","c","d","c")
)

ggplot(df, aes(x, y)) +
        geom_point() +
        geom_text(aes(label = y))

ggplot(df, aes(x, y)) +
        geom_point() +
        geom_text(aes(label = y), position = position_nudge(y = 0.1))
