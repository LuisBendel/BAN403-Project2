library(tidyverse)
library(readxl)

arrival <- read_excel('MillerPainTreatmentCenterData.xlsx', sheet = 3) %>% 
  as_tibble() %>% 
  rename(before = `Appointment Time Minus Arrival Time Before Policy Change`,
         after = `Appointment Time Minus Arrival Time After Policy Change`) %>% 
  mutate(average_delay_before = before + 19.5)

arrival %>% 
  ggplot(aes(x=before)) +
  geom_density() +
  theme_minimal()








# Define start and end times of shift
start_time <- as.POSIXct("1970-01-01 08:00:00", format = "%Y-%m-%d %H:%M:%S")
end_time <- as.POSIXct("1970-01-01 11:15:00", format = "%Y-%m-%d %H:%M:%S")


# Generate sequence of datetime values
app_times <- seq(from = start_time, to = end_time, by = "15 min")

# probabilities of new or return or none of these two
p_new <- 3/7
p_return <- 4/7
p_neither <- 0

newreturn_proportions <- c(
  rep("new", 100 * p_new),
  rep("return", 100 * p_return),
  rep("none", 100 * p_neither)
)

# probability for follow up
p_followup <- 0.25

followup_proportions <- c(
  rep("followup", 100 * p_followup),
  rep("none", 100 * 0.75)
)

# create tibbles to store simulated schedules
arrival_f <- tibble(
  day = as.numeric(),
  patient_type = as.character(),
  app_time = as.POSIXct(character()),
  arr_time = as.POSIXct(character())
)

arrival_nr <- tibble(
  day = as.numeric(),
  patient_type = as.character(),
  app_time = as.POSIXct(character()),
  arr_time = as.POSIXct(character())
)

# define number of schedule simulations
n_sim <- 1000

for (d in c(1:n_sim)) {
  for (s in c(1:length(app_times))) {
    nr <- sample(newreturn_proportions, 1)
    f <- sample(followup_proportions, 1)
    
    arrival_nr <- add_row(arrival_nr,
                          day = d,
                          patient_type = nr,
                          app_time = app_times[s],
                          arr_time = NA)
    
    arrival_f <- add_row(arrival_f,
                          day = d,
                          patient_type = f,
                          app_time = app_times[s],
                          arr_time = NA)
  }
}

# combine follow-up with new and return
arrivals <- bind_rows(arrival_nr, arrival_f)

arrivals <- arrivals %>% filter(!patient_type == "none")

n_patients <- arrivals %>% count()

arrivals %>% 
  filter(!patient_type == "none") %>% 
  group_by(patient_type) %>% 
  summarise(count = n()/n_patients)

arrivals <- arrivals %>% 
  arrange(day, app_time) %>% 
  mutate(is_followup = (patient_type == "followup")) %>% 
  group_by(day, is_followup) %>% 
  mutate(diff = as.numeric((app_time - lag(app_time)))) %>% 
  ungroup() %>% 
  replace_na(list(diff = 0))


arrivals %>% 
  filter(patient_type %in% c("followup"),
         diff2 > 0) %>% 
  summarise(mean = mean(diff),
            std = sd(diff))


arrivals %>% 
  filter(patient_type %in% c("followup")) %>% 
  ggplot(aes(x = diff)) +
  geom_density() +
  theme_minimal()



1695
