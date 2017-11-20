# https://dirkschumacher.github.io/ompr/>

library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

result <- MIPModel() %>%
        add_variable(x, type = "integer") %>%
        add_variable(y, type = "continuous", lb = 0) %>%
        set_bounds(x, lb = 0) %>%
        set_objective(x + y, "max") %>%
        add_constraint(x + y <= 11.25) %>%
        solve_model(with_ROI(solver = "glpk"))
get_solution(result, x)
get_solution(result, y)

mydf <- read.table(text = "Mean_1     Mean_2        Nb_element_1     Nb_element_2
42.66667   51.89474      3                38
65.00000   -85.87500     1                8
163.32653  -117.96970    49               33
22.83333   247.00000     6                1", header=TRUE)
(mydf2 <- mydf %>%
                mutate(Concol1 = abs(Mean_1) - abs(Mean_2) + abs(Nb_element_1) + abs(Nb_element_2)) %>%
                mutate(Concol2 = 1.1 * abs(Mean_1) > abs(Mean_2)) %>%
                mutate(Concol3 = abs(Mean_2) > 0.75 * abs(Mean_1)) %>%
                filter(Concol2 == TRUE & Concol3 == TRUE) %>%
                filter(Concol1 == min(Concol1)))

# does not work when MIPModel does not contain anything
result <- MIPModel() %>%
        add_variable(Mean_1, type = "continuous") %>%
        add_variable(Mean_2, type = "continuous") %>%
        add_variable(Nb_element_1, type = "continuous") %>%
        add_variable(Nb_element_2, type = "continuous") %>%
        set_bounds(x, lb = 0) %>%
        set_objective(x, "min") %>%
        add_constraint(1.1 * abs(Mean_1) - abs(Mean_2) >= 0) %>%
        add_constraint( abs(Mean_2) - 0.75* abs(Mean_1) >= 0 ) %>%
        set_bounds(Mean_1, ub = 42.6667, lb = 42.6667) %>%
        set_bounds(Mean_2, ub = 51.89474, lb = 51.89474) %>%
        set_bounds(Nb_element_1, ub = 3,  lb = 3) %>%
        set_bounds(Nb_element_2, ub = 38, lb = 38) %>%
        solve_model(with_ROI(solver = "glpk"))

get_solution(result, Mean_1)
get_solution(result, y)
