#ANA515 Practicum Project

# Load and clean the data 

install.packages("readxl")
install.packages("writexl")
install.packages("hunspell")
library(readxl)
library(writexl)
library(hunspell)
library(readxl)
library(dplyr)

# Loaded both sheets from MentalHealthServey using read_excel() function

mentalHealthServey1 <- read_excel("/Users/Rohini/Documents/MentalHealthSurvey.xlsx",sheet = "Sheet1")
mentalHealthServey2 <- read_excel("/Users/Rohini/Documents/MentalHealthSurvey.xlsx",sheet = "Sheet2")

# clean data - Timestamp column in sheet 2 is in double and in sheet 1 it is in datetime format so converting column in same data type
# fixed error - Can't combine `..1$Timestamp` <datetime<UTC>> and `..2$Timestamp` <double>.

mentalHealthServey2$Timestamp <- as.POSIXct(mentalHealthServey2$Timestamp*86400, origin="1970-01-01", tz = "UTC")

# Combined both sheets into single excel file using bind_rows() function , Combining two sheets together makes it easier to analyze and manipulate the data.

mentalHealthServey <- bind_rows(mentalHealthServey1, mentalHealthServey2)
head(mentalHealthServey)

# Identifying missing values using is.na() function in the dataset. It is important to identify and handle missing values
# because it can lead to incorrect results when analyzing the data.

missing_values <- is.na(mentalHealthServey)

# Replacing missing values with NA in the dataset.

mentalHealthServey[missing_values] <- NA
view(mentalHealthServey)

# Arrange Age column using the arrange() function to sort data.

data <- arrange(mentalHealthServey, Age)

view(mentalHealthServey)

# Filtering all negative age values from dataset

negative_age <- mentalHealthServey %>% filter(Age < 0)

# Replacing the negative ages with a specific value, like the mean or median age
  
mean_age <- mean(mentalHealthServey$Age[mentalHealthServey$Age >= 0], na.rm = TRUE)
mentalHealthServey$Age[mentalHealthServey$Age < 0] <- mean_age
view(mentalHealthServey)

# Replace the ages greater than 100 with the mean age.
mean_age <- mean(mentalHealthServey$Age[mentalHealthServey$Age <= 100], na.rm = TRUE)
mentalHealthServey$Age[mentalHealthServey$Age > 100] <- mean_age


# Replacing Gender column values which are invalid
mentalHealthServey <- mentalHealthServey %>%
  mutate(Gender = case_when(
    Gender %in% c("M", "male","m") ~ "Male",
    Gender %in% c("F", "female","f","p") ~ "Female",
    Gender %in% c("something kinda male?", "A little about you","f","p","") ~ "NA",
    TRUE ~ as.character(Gender)
  ))

# If there are any other values in Gender column than Male and Female replace with NA

mentalHealthServey$Gender <- ifelse(mentalHealthServey$Gender %in% c("Male", "Female"), mentalHealthServey$Gender, NA)
view(mentalHealthServey)

# Data visualization for Mental Health Survey dataset with 2 variables. Data visualization helps in conveying information quickly and efficiently. 
# Plot1
plot<-ggplot(mentalHealthServey, aes(x=Country, y=mental_health_consequence)) +
  
  geom_point(color = "yellow", fill = "red", size = 3,shape = 21, , stroke = 1)+theme(plot.background = element_rect(fill = "cyan")) +
  
  labs(x="Country", y="freq_mental_health_consequence", title="Scatterplot of Countires with frequency of mental health consequences")

print(plot)

# Plot2
ggplot(mentalHealthServey, aes(x = Gender, y = mental_health_consequence)) +  
  
  geom_bar(stat = "identity", fill = "purple") +   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  
  labs(title = "Bar Plot shows specific Gender has mental health consequence or not", x = "Gender", y = "mental_health_consequence")

# Export final cleaned dataset
write_xlsx(mentalHealthServey, "/Users/Rohini/Documents/Cleaned_dataset.xlsx")
