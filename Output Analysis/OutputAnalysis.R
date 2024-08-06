library(tidyverse)

# load the output of the scenario simulations
output <- read.delim("224250-proj2-att-ScheduleOptimization.dat")

cols <- c(
  "Scenario",
  "Replication",
  "Util",
  "ID",
  "NumberProcessed",
  "CTNumberProcessed",
  "CycleTime",
  "WaitingTime",
  "Rejection"
)

output_clean <- output %>% 
  select(1,2,3,5,7,9,11,27,31) %>% 
  filter(is.na(Replication))

colnames(output_clean) <- cols

output_clean %>% filter(Util == 26) %>% arrange(CycleTime) %>% head(1)

output_clean %>% filter(Util == 27) %>% arrange(CycleTime) %>% tail(1)

output_clean %>% filter(Util == 28) %>% arrange(CycleTime) %>% head(1)

output_best <- output_clean %>% 
  group_by(Util) %>% 
  summarise(best_ct = min(CycleTime),
            best_wt = min(WaitingTime))

output_clean %>% 
  ggplot(aes(x = CycleTime)) +
  geom_histogram()

sd(output[1:10,] %>% select(X.CycleTime..SampleAverage..1.h.) %>% pull(.)) / 0.002644402


# JaamSim by default exports the expressions as columnnames (stupid)
# We did not find a way to have custom columnnames in jaamsim runoutputlist
# so we define all columnnames here
cols <- c(
  "Scenario",
  "Replication",
  "Util", "CI_Util",
  "ID", "CI_ID",
  "NumberProcessed", "CI_NumberProcessed",
  "CTNumberProcessed", "CI_CTNumberProcessed",
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
