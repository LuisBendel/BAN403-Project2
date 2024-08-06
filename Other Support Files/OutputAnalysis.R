library(tidyverse)

# load the output of the scenario simulations
output <- read.delim("MultipleRuns_V7.dat")

# JaamSim by default exports the expressions as columnnames (stupid)
# We did not find a way to have custom columnnames in jaamsim runoutputlist
# so we define all columnnames here
cols <- c(
  "Scenario",
  "Replication",
  "CycleTime", "CI_CycleTime",
  "CycleTimeNew", "CI_CycleTimeNew",
  "CycleTimeReturn", "CI_CycleTimeReturn",
  "CycleTimeFU", "CI_CycleTimeFU",
  "AvgQTimeNR", "CI_AvgQTimeNR",
  "AvgQTimeFU", "CI_AvgQTimeFU",
  "AvgQTimeDoc", "CI_AvgQTimeDoc",
  "DocUtil", "CI_DocUtil",
  "PAUtil", "CI_PAUtil",
  "CAUtil", "CI_CAUtil",
  "Rejected", "CI_Rejected",
  "RejectedClosingNR", "CI_RejectedClosingNR",
  "RejectedClosingFU", "CI_RejectedClosingFU"
)

colnames(output) <- cols

# Means of all output metrics for each scenario
output_means <- output %>% 
  filter(!is.na(Replication)) %>% 
  select(-c(starts_with("CI_"),Replication)) %>% 
  group_by(Scenario) %>% 
  summarise_all(mean, na.rm=T)

output_best <- output %>% 
  select(-starts_with("CI_")) %>% 
  filter(Scenario == 16)


# print the best schedule
best_schedule <- df_long %>% 
  filter(sim == 16)


1/1.5
