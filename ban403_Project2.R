library(tidyverse)
library(readxl)

shift1 <- paste(seq(from = 8, to = 11.25, by = 0.25), collapse = " ")

shift2 <- paste(seq(from = 13, to = 16.25, by = 0.25), collapse = " ") 

paste(shift1, shift2, collapse = " ")




##########################################
Table4 <- read_excel('MillerPainTreatmentCenterData.xlsx', sheet = 4) %>% 
  as_tibble() %>% 
  rename(R_A = `Resident & Attending`,
         R_P = `Resident & Patient`,
         R_R = `Resident Review`)


# Load required libraries
library(flexsurv)

# Fit a log-logistic survival model
model <- flexsurvreg(Surv(time, status) ~ 1, data = pbc, dist = "loglogistic")

# Print the model summary
summary(model)
