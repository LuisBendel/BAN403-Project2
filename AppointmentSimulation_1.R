library(tidyverse)
library(readxl)
library(lubridate)

delay <- read_excel('MillerPainTreatmentCenterData.xlsx', sheet = 3) %>% 
  as_tibble() %>% 
  rename(before = `Appointment Time Minus Arrival Time Before Policy Change`,
         after = `Appointment Time Minus Arrival Time After Policy Change`) %>% 
  mutate(average_delay_before = before + 19.5)










# Define start and end times of shift
start_time <- as.POSIXct("1970-01-01 08:00:00", format = "%Y-%m-%d %H:%M:%S")
end_time <- as.POSIXct("1970-01-01 11:15:00", format = "%Y-%m-%d %H:%M:%S")


# Generate sequence of datetime values
app_times <- seq(from = start_time, to = end_time, by = "15 min")

# probabilities of new or return or none of these two
p_new <- 3/7 # 5/14
p_return <- 4/7 # 7/14
p_neither <- 0 # 2/14

newreturn_proportions <- c(
  rep("new", 100 * p_new),
  rep("return", 100 * p_return),
  rep("none", 100 * p_neither)
)

# probability for follow up
p_followup <- 0.43 # 5/14

followup_proportions <- c(
  rep("followup", 100 * p_followup),
  rep("none", 100 * (9/14))
)

# probability for resident case (3 residents, 2 each shift: 3*2 = 6 of 14 appointments)
p_resident <- 6/14

resident_proportions <- c(
  rep(TRUE, 100 * p_resident),
  rep(FALSE, 100 * (8/14))
)

# create tibbles to store simulated schedules
arrival_f <- tibble(
  day = as.numeric(),
  app_id = as.numeric(),
  patient_type = as.character(),
  app_time = as.POSIXct(character()),
  delay = as.numeric(),
  arr_time = as.POSIXct(character()),
  resident_case = NA
)

arrival_nr <- tibble(
  day = as.numeric(),
  app_id = as.numeric(),
  patient_type = as.character(),
  app_time = as.POSIXct(character()),
  delay = as.numeric(),
  arr_time = as.POSIXct(character()),
  resident_case = NA
)

# define number of schedule simulations
n_sim <- 1000

set.seed(123)
for (d in c(1:n_sim)) {
  for (s in c(1:length(app_times))) {
    nr <- sample(newreturn_proportions, 1)
    f <- sample(followup_proportions, 1)
    arr_minus_app <- rlogis(1, location = -22.58, scale = 12.63)
    resident_case <- sample(resident_proportions, 1)
    
    arrival_nr <- add_row(arrival_nr,
                          day = d,
                          app_id = s,
                          patient_type = nr,
                          app_time = app_times[s],
                          delay = arr_minus_app,
                          arr_time = NA,
                          resident_case = resident_case)
    
    arrival_f <- add_row(arrival_f,
                         day = d,
                         app_id = s,
                         patient_type = f,
                         app_time = app_times[s],
                         delay = arr_minus_app,
                         arr_time = NA,
                         resident_case = NA)
    
  }
}

# combine follow-up with new and return
arrivals <- bind_rows(arrival_nr, arrival_f)

arrivals <- arrivals %>% filter(!patient_type == "none")

n_patients <- arrivals %>% count()

arrivals <- arrivals %>% 
  arrange(day, app_time) %>% 
  mutate(is_followup = (patient_type == "followup"),
         arr_time = app_time + minutes(round(delay))) %>% 
  group_by(day, is_followup) %>% 
  mutate(diff = as.numeric(difftime(app_time, lag(app_time), units = "mins")),
         diff2 = as.numeric(difftime(arr_time, lag(arr_time), units = "mins"))) %>%
  ungroup() %>% 
  replace_na(list(diff = 0, diff2 = 0))


arrivals %>% 
  group_by(patient_type) %>% 
  summarise(count = n()/n_patients)

arrivals %>% 
  filter(patient_type %in% c("new", "return"),
         app_id > 1) %>% 
  summarise(mean = mean(diff2),
            std = sd(diff2))

arrivals %>% 
  filter(patient_type %in% c("followup"),
         diff2 > 0) %>% 
  summarise(mean = mean(diff2),
            std = sd(diff2))


arrivals %>% 
  filter(patient_type %in% c("new", "return"), app_id > 1) %>% 
  ggplot(aes(x = diff2)) +
  geom_density() +
  theme_minimal()

arrivals %>% 
  filter(patient_type %in% c("followup"), diff2 > 0) %>% 
  ggplot(aes(x = diff2)) +
  geom_density() +
  theme_minimal()




#############################################
# First Arrival Time follow-up
#############################################
test <- arrivals %>% 
  filter(patient_type == "followup") %>% 
  group_by(day) %>% 
  mutate(first_arrival = min(app_id)) %>% 
  ungroup() %>% 
  filter(app_id == first_arrival) %>% 
  mutate(first_arrival_diff = as.numeric(difftime(arr_time, "1970-01-01 08:00:00", units = "mins")))

test %>% 
  ggplot(aes(x = first_arrival_diff)) +
  geom_density() +
  theme_minimal()

test %>% 
  summarise(mean = mean(first_arrival_diff),
            std = sd(first_arrival_diff))


#############################################
# First Arrival Time new and return
#############################################
arrivals %>% 
  filter(app_id == 1) %>% 
  ggplot(aes(x = delay)) +
  geom_density() +
  theme_minimal()

arrivals %>% 
  filter(app_id == 1) %>% 
  summarise(mean = mean(delay),
            std = sd(delay))



#############################################
# Resident Case assigned or not
#############################################
resident_cases <- arrivals %>% 
  filter(patient_type %in% c("new", "return")) %>% 
  group_by(day) %>% 
  summarise(resident_cases = sum(resident_case))

resident_cases %>% 
  ggplot(aes(x = as.factor(resident_cases))) +
  geom_bar() +
  theme_minimal()

resident_cases %>% 
  group_by(resident_cases) %>% 
  summarise(probability = n()/n_sim) %>% 
  pull(probability) %>% 
  paste(., collapse = " ")




