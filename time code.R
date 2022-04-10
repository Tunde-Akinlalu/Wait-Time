library(RMySQL)
library(plotly)
library(tidyverse)
library(qcc)

con1  = dbConnect(MySQL(), 
                 user = 'root', 
                 password = '@newconvenant', 
                 dbname = 'garkidb',
                 host = 'localhost')
dbListTables()


wait_time = "SELECT start_date, doctor_start_time, triaged_on, signed_on,
timestampdiff(minute, triaged_on, signed_on) as nurse_to_doctor,
timestampdiff(minute, doctor_start_time, signed_on) as consulting_time, 
CONCAT(firstname, ' ' , lastname) AS provider,
departments.name as department,
staff_type as specialization,
year(start_date) AS visit_year,
month(start_date) AS visit_month,
dayofweek(start_date) as weekday
FROM encounter
LEFT JOIN staff_directory ON staff_directory.staffId = encounter.consulted_by
left join departments on departments.id = encounter.department_id
left join staff_specialization on staff_specialization.id = encounter.specialization_id
WHERE (start_date > '2017-12-31 23:59:59') AND
(triaged_on IS NOT NULL AND consulted_on IS NOT NULL) AND
TIMESTAMPDIFF(DAY, consulted_on, triaged_on) < 1 AND
TIMESTAMPDIFF(DAY, start_date, consulted_on) < 1"


wtime = dbGetQuery(con1, wait_time)
wtime = tbl(wtime)

qtime = tbl(wtime)


#############################################################################
# Use qcc package to do a SPC on the waiting time
############################################################################

#Rename to month_name
wtime = wtime %>% mutate(visit_month = factor(visit_month, levels = c('1', '2', '3', '4', '5', 
                                                              '6', '7', '8', '9', '10', '11', '12'),
                                      labels = c('January', 'February', 'March', 'April', 'May', 'June',
                                                 'July', 'August', 'September', 'October', 'November',
                                                 'December')))

#Recode is a better option
#wtime = wtime %>% mutate(visit_month = recode(visit_month, '1' = 'January, '2' = February, etc))

#Filter out the data you are interested in
Paed_time_July = wtime %>% select(visit_month, visit_year, consulting_time, department, provider) %>% 
  group_by(visit_month) %>% 
  filter(visit_year == 2019 & department == 'Paediatrics' & 
           consulting_time < 50 ) %>% na.omit()

# Consulting time: that is the time a patient spent with the doctor
qtime = wtime %>% select(visit_month, visit_year, weekday, consulting_time, department, provider) %>% 
  group_by(visit_month) %>% 
  filter(visit_year == 2019 & department == 'Paediatrics' & 
           consulting_time < 50 & visit_month == 12) %>% na.omit()

# Wait time between nurses triage and seeing the doctor.
# Focus is on the department, month and year
fam_wait = wtime %>% select(visit_month, visit_year, weekday, nurse_to_doctor, department) %>% 
  group_by(visit_month) %>% 
  filter(visit_year == 2020 & department == 'General Medicine' & 
           nurse_to_doctor < 200 & visit_month == 12) %>% na.omit()

##############################################

aggregate(nurse_to_doctor ~ weekday, data = fam_wait, mean) #Shows the mean & other statistic of interest
#Using Dplyr, aggregate (mean) = sumamarise(avg = mean(nurse_to_doctor))

fam_wait %>% select(weekday, nurse_to_doctor) %>%
  group_by(weekday) %>% summarise(weekly_mean = mean(nurse_to_doctor)) %>% 
  ggplot(aes(x = weekday, y = weekly_mean)) + geom_line(color = 'blue') + theme_classic()


#################################################################################
#Using the qcc package, I group, then plot on the applicable SPC chart.

fgroup = qcc.groups(fam_wait$nurse_to_doctor, fam_wait$weekday) #Group the time based on weekday

qcc(fgroup, type = "xbar", std.dev = "UWAVE-SD") #Using the XBAR to check if in control or not


# Playing Around to know doctors that spend too long consulting
wtime %>% select(visit_year, visit_month, department, provider, consulting_time) %>% 
  group_by(provider) %>% 
filter(consulting_time > 20  & consulting_time <= 50) %>% na.omit() %>% 
  mutate(avg_cons_time = mean(consulting_time)) %>% arrange(desc(avg_cons_time)) %>%
  select(visit_year, visit_month, provider, department, avg_cons_time) %>% distinct()

# Months with long waiting period
wtime %>% select(visit_year, visit_month, department, provider, nurse_to_doctor) %>% 
  group_by(department) %>% 
  filter(nurse_to_doctor <= 120 & nurse_to_doctor > 0) %>% na.omit() %>% 
  mutate(avg_wait_time = mean(nurse_to_doctor)) %>% arrange(desc(avg_wait_time)) %>% distinct() %>%
  ggplot(aes(visit_month, avg_wait_time, group = visit_year, colour = visit_year)) + 
  theme_classic() + geom_line()

###################################################################################################

wtime %>% select(visit_year, visit_month, department, provider, nurse_to_doctor) %>% 
  group_by(department) %>% 
  filter(nurse_to_doctor <= 120 & nurse_to_doctor > 0) %>% na.omit() %>% 
  mutate(avg_wait_time = mean(nurse_to_doctor)) %>% arrange(desc(avg_wait_time)) %>% distinct() %>%
  ggplot(aes(visit_month, avg_wait_time, group = visit_year, colour = visit_year)) + 
  theme_classic() + geom_line()