
library(tidyverse)
getwd()
setwd("C:/Users/srini/Downloads/EDA Labs")
hospital_data_1 = read.table("hospital_data_1.csv", header = T, sep = ",", stringsAsFactors = TRUE)
View(hospital_data_1)
hospital_data_2 = read.table("hospital_data_2.csv", header = T, sep = ",", stringsAsFactors = TRUE)
View(hospital_data_2)

#1

#Cleaning
hospital_data_1 <- hospital_data_1 %>%
  mutate(Gender = str_to_title(Gender)) 

hospital_data_1 <- hospital_data_1 %>%
  mutate(Age = as.numeric(str_extract(Age, "\\d+"))) 

hospital_data_1 <- hospital_data_1 %>%
  pivot_longer(cols = c(Routine, Emergency, Elective), 
               names_to = "Admission_Type",  
               values_to = "Heart_Rate")

hospital_data_2 <- hospital_data_2 %>%
  mutate(Days_In_Hospital = as.numeric(str_extract(Days_In_Hospital, "\\d+")), Follow_Up_Days = as.numeric(str_extract(Follow_Up_Days, "\\d+")))

hospital_data_2 <- hospital_data_2 %>%
  separate(Exercise_Smoking, into = c("Exercise", "Smoking"), sep = "\\|") #note: "|" = or 

#Remove NA + Distinct

hospital_data_1 <- hospital_data_1 %>% 
  drop_na()

hospital_data_2 <- hospital_data_2 %>% 
  drop_na()

#Merge

hospital_data <- left_join(hospital_data_2, hospital_data_1, by = c("BMI", "Blood_Pressure", "Heart_Rate")) 

View(hospital_data)

missing_values <- sum(is.na(hospital_data))
print(missing_values)


hospital_data <- hospital_data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))


#2

#p1
count <- hospital_data %>%
  group_by(Admission_Type) %>%
  summarise (count = n())

print(count)

max_count <- count %>%
  filter(count == max(count))

print(max_count)

#p2

mean <- hospital_data %>%
  group_by(Department) %>%
  summarise (mean = mean(Days_In_Hospital))

print(mean)

max_stay <- mean %>%
  filter(mean == max(mean))

print(max_stay)

#p3

smoker <- hospital_data %>%
  group_by(Gender, Smoking) %>%
  summarise (count = n(), .groups = "drop") %>%
  filter(Smoking == 'Yes')

print(smoker)

percentage <- hospital_data %>%
  group_by(Gender) %>%
  summarise(count = sum(Smoking == "Yes", na.rm = TRUE), total = n(),
            percentage = (sum(Smoking == "Yes", na.rm = TRUE) / n()) * 100)

print(percentage)

#p4

summary_stats <- hospital_data %>%
  mutate(days = ifelse(Follow_Up_Days <= 12, "Follow Up", "No Follow Up")) %>%
  group_by(days)%>%
  summarise(mean = mean(Age), median = median(Age), sd = sd(Age))

print(summary_stats)

#They tend to be younger

#p5

hospital <- hospital_data %>%
  group_by(Insurance_Status) %>%
  summarise(mean = mean(Days_In_Hospital), median = median(Days_In_Hospital))

print(hospital)

#The average amount of days spent for someone uninsured is higher than both individuals who are insured and on a government plan. The median amount of days in the hospital was the same for all insurance status'. Individuals on a government plan had the lowest mean average days in the hospital(by very little).

#3

ggplot(hospital_data) +
  
  geom_bar(mapping = aes(x = Smoking, fill = Exercise)) + 
  labs(title = "Exercise Across Smoking Status Groups", x = "Smoking Status", y = "Count") +
  
  scale_fill_manual(values = c("Sedentary" = "salmon", "Moderate" = "green","Active" = "blue")) +
  
  geom_text(aes(x = Smoking, fill = Exercise, label = scales::percent((..count..)/sum(..count..))),
            
            stat = "count",
            
            position = position_stack(vjust = 0.5),
            
            size = 4,
            
            colour = "black")


#4

ggplot(hospital_data) + 
  geom_density(aes(x = BMI, fill = Department), alpha = 0.4) +
  labs(title = "BMI by Department", x = "BMI",
       y = "Density") + theme_minimal() 


#5

hospital_data1 <- hospital_data %>%
  filter(Chronic_Conditions %in% c("None", "Diabetes", "Hypertension", "Asthma", "COPD")) %>%
  mutate(Age_Group = case_when(Age >= 13 & Age <= 30 ~ "13-30",Age >= 31 & Age <= 50 ~ "31-50",Age >= 51 & Age <= 70 ~ "51-70", Age >= 71 ~ "71+"))

proportions <- hospital_data1 %>%
  count(Age_Group, Chronic_Conditions) %>%
  group_by(Age_Group) %>%
  mutate(Proportion = n / sum(n)) %>%
  ungroup()

print(proportions)

ggplot(data = proportions) +
  geom_bar(mapping = aes(x = Age_Group, y = Proportion, fill = Chronic_Conditions),
           stat = "identity", position = "fill") +
  labs(title = "Proportions of Chronic Conditions by Age Group",
       x = "Age Group",
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() 
