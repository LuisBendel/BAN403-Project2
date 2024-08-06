library(tidyverse)
library(readxl)

shift1 <- paste(seq(from = 8, to = 11.25, by = 0.25), collapse = " ")

shift2 <- paste(seq(from = 12, to = 15.25, by = 0.25), collapse = " ") 

length(shift2)

length(seq(from = 12, to = 15.25, by = 0.25))

paste(shift1, shift2, collapse = " ")


seq(from = 12, to = 15.25, by = 0.25)




##########################################
Table4 <- read_excel('MillerPainTreatmentCenterData.xlsx', sheet = 4) %>% 
  as_tibble() %>% 
  rename(R_A = `Resident & Attending`,
         R_P = `Resident & Patient`,
         R_R = `Resident Review`)


# Load required libraries
library(flexsurv)
library(survival)
library(fitdistrplus)

# Fit a log-logistic survival model
model <- flexsurvreg(Surv(time, status) ~ 1, data = list(Table4$R_A), dist = "llogis")

# Print the model summary
summary(model)


fit <- fitdist(Table4$R_A, "llogis")




# Create a density plot of the data
p <- ggplot(data.frame(x = Table4$R_A), aes(x)) +
  geom_density(fill = "lightblue") +
  theme_minimal()

# Overlay the PDF of the fitted distribution
p <- p + stat_function(fun = function(x) dlnorm(x, meanlog = fit$estimate[1], sdlog = fit$estimate[2]), 
                       aes(color = "Fitted"), lwd = 1.2)

# Add a legend
p <- p + scale_color_manual("", breaks = "Fitted", values = "red")

# Print the plot
print(p)
