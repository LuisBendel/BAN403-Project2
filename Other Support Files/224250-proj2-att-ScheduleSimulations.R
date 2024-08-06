library(tidyverse)
library(readxl)
library(writexl)

# create all appointment times, 15 minute time intervals
# first shift from 8:00 to 11:15, second from 12:00 to 15:15
shift1 <- seq(from = 8, to = 11.25, by = 0.25)
shift2 <- seq(from = 12, to = 15.25, by = 0.25)
workday <- c(shift1, shift2)

# Create an empty dataframe with 42 rows per day or simulation
n_cols <- 42
n_sim <- 50
df_long <- tibble(
  sim = as.integer(),
  util = as.integer(),
  appID = as.integer()
)
for (k in c(26:28)) {
  for (s in c(1:n_sim)) {
    for (c in c(1:n_cols)) {
      df_long <- add_row(
        df_long,
        sim = s,
        util = k,
        appID = c
      )
    }
  }
}

# create tibbles to store simulated schedules
sim_nr <- tibble(
  sim = as.numeric(),
  util = as.numeric(),
  appID = as.numeric(),
  time = as.numeric(),
  p = as.integer()
)

sim_f <- tibble(
  sim = as.numeric(),
  util = as.numeric(),
  appID = as.numeric(),
  time = as.numeric(),
  p = as.integer()
)


# define appointment utilization and set proportions for patient types correctly
# we simulate utilizing either 26, 27, or 28 of the 28 appointment slots (2 shifts per day a 14 slots)
daily_app <- 28
util_vec <- c(28, 27, 26)

for (k in c(1:3)) {
  util <- util_vec[k]
  no_app <- daily_app - util
  p_new <- 3/7
  p_return <- 4/7
  p_fu <- p_new
  sub <- c(1,2,3)
  
  # with the proportions, calculate number of each patient type, rounded
  n_new <- round(util * p_new)
  n_return <- round(util * p_return)
  n_fu <- round(util * p_fu)
  
  
  # start simulating
  set.seed(123)
  for (s in c(1:n_sim)) {
    
    # new and return can not be scheduled on the same time together, but followup can be (with new or return)
    # create a sequence for new and return, one for follow up
    # create a substitute sequence in case rounding leads to fewer than no of appointments
    # appiontment slots not utilized will be filled with 0s
    # shuffle the sequences for each simulation newly
    nr_sub <- ifelse(util > n_new + n_return, sample(sub, util - (n_new + n_return)), NA)
    nr_seq <- sample(c(rep(1, n_new), rep(2, n_return), nr_sub, rep(0, daily_app))[1:daily_app], daily_app)
    fu_seq <- sample(c(rep(3, n_fu), rep(0, daily_app - n_fu))[1:daily_app], daily_app)
    
    # for each appointment time, populate the dataframe with randomly drawn schedules
    for (c in c(1:daily_app)) {
      
      # populate new and return dataframe
      sim_nr <- add_row(
        sim_nr,
        sim = s,
        util = util,
        appID = 0,
        time = workday[c],
        p = nr_seq[c]
      )
      
      # populate followup dataframe
      sim_f <- add_row(
        sim_f,
        sim = s,
        util = util,
        appID = 0,
        time = workday[c],
        p = fu_seq[c]
      )
      
    }
  }
}

  
  
# filter out not utilized appointments
sim_nr <- sim_nr %>% filter(!p == 0)
sim_f <- sim_f %>%  filter(!p == 0)

# combine follow up and new return
sims <- bind_rows(sim_nr, sim_f) %>% 
  arrange(util, sim, time) %>% 
  group_by(util, sim) %>% 
  mutate(appID = row_number())

# join the template dataframe
# because all days must have same number of rows (42) to pivot the data later
# to make it suitable for how we want to import it in JaamSim
df_long <- df_long %>% 
  left_join(., sims, by = c("sim", "util", "appID"))

# set NAs to 0
df_long[is.na(df_long)] <- 0

# pivot wider for patient, need wider format for input in jaamsim
InputPatients <- df_long %>% 
  select(sim, util, appID, p) %>% 
  pivot_wider(names_from = appID, values_from = p)

# pivot wider for time, need wider format for input in jaamsim
InputSchedule <- df_long %>% 
  select(sim, util, appID, time) %>% 
  pivot_wider(names_from = appID, values_from = time)


# save simulations
# write_xlsx(InputPatients, "224250-proj2-att-OptimizationPatientInput.xlsx")
# write_xlsx(InputSchedule, "224250-proj2-att-OptimizationScheduleInput.xlsx")

write.table(InputPatients, "224250-proj2-att-OptimizationPatientInput.txt", sep = "\t", row.names = FALSE)
write.table(InputSchedule, "224250-proj2-att-OptimizationScheduleInput.txt", sep = "\t", row.names = FALSE)

