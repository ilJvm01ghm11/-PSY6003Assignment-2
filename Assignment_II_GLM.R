library(tidyverse)
library(haven)
library(flexplot)
library(psych)
library(apaTables)
library(dplyr)
library(visualize)
library(apaTables)

# Importing spss data set 
data <- read_sav("PSYR6003.A2.sav")
sum(is.na(data)) # Missing Value Count
print(data)
glimpse(data)

# Re-coding sex from string to factor variable 
data1 <- data %>%
  mutate(sex = factor(sex, levels = c("Female", "Male"), labels = c(0, 1)))

# Finding out total missing values
sum(is.na(data1)) 

# Removing Missing Values 

data1 <- na.omit(data1)  

# Creating an object called data2 
# with variable from  Reverse coding of  tipm.CONS2.3y variable 

data2 <- data1 %>%
  mutate(tipm.CONS2.3y = 6 - tipm.CONS2.3y)  

# Compute sub scale totals mean  using pipe operator for 
# Socially Prescribed Perfectionism (SPP)
#  Conscientiousness
# Negative Affect
# Create new data set with the above called data3 

data3 <- data2 %>%
  mutate(SPP_total = select(., starts_with("mps.SPP")) %>% rowMeans(na.rm = TRUE),
         Conscientiousness_total = select(., starts_with("tipm.CONS")) %>% rowMeans(na.rm = TRUE),
         NegativeAffect_total = select(., starts_with("guilt"), starts_with("dep"), starts_with("fear"), starts_with("host")) %>% rowMeans(na.rm = TRUE))




# Descriptive statistics for entire data set 
summary_stats <- data3 %>% 
  select(SPP_total, Conscientiousness_total, NegativeAffect_total, sex) %>% 
  summarise(
    Mean_SPP = mean(SPP_total, na.rm = TRUE),
    SD_SPP = sd(SPP_total, na.rm = TRUE),
    Min_SPP = min(SPP_total, na.rm = TRUE),
    Max_SPP = max(SPP_total, na.rm = TRUE),
    
    Mean_Conscientiousness = mean(Conscientiousness_total, na.rm = TRUE),
    SD_Conscientiousness = sd(Conscientiousness_total, na.rm = TRUE),
    Min_Conscientiousness = min(Conscientiousness_total, na.rm = TRUE),
    Max_Conscientiousness = max(Conscientiousness_total, na.rm = TRUE),
    
    Mean_NegativeAffect = mean(NegativeAffect_total, na.rm = TRUE),
    SD_NegativeAffect = sd(NegativeAffect_total, na.rm = TRUE),
    Min_NegativeAffect = min(NegativeAffect_total, na.rm = TRUE),
    Max_NegativeAffect = max(NegativeAffect_total, na.rm = TRUE),
    
    Mean_Sex = mean(as.numeric(sex), na.rm = TRUE),  # Mean for categorical variables can indicate proportion
    SD_Sex = sd(as.numeric(sex), na.rm = TRUE),
    Min_Sex = min(as.numeric(sex), na.rm = TRUE),
    Max_Sex = max(as.numeric(sex), na.rm = TRUE)
  )


#Descriptive statistic by gender 

summary_stats2 <- data3 %>% 
  select(SPP_total, Conscientiousness_total, NegativeAffect_total, sex) %>% 
  group_by(sex) %>%  # Group by sex
  summarise(
    Mean_SPP = mean(SPP_total, na.rm = TRUE),
    SD_SPP = sd(SPP_total, na.rm = TRUE),
    Min_SPP = min(SPP_total, na.rm = TRUE),
    Max_SPP = max(SPP_total, na.rm = TRUE),
    
    Mean_Conscientiousness = mean(Conscientiousness_total, na.rm = TRUE),
    SD_Conscientiousness = sd(Conscientiousness_total, na.rm = TRUE),
    Min_Conscientiousness = min(Conscientiousness_total, na.rm = TRUE),
    Max_Conscientiousness = max(Conscientiousness_total, na.rm = TRUE),
    
    Mean_NegativeAffect = mean(NegativeAffect_total, na.rm = TRUE),
    SD_NegativeAffect = sd(NegativeAffect_total, na.rm = TRUE),
    Min_NegativeAffect = min(NegativeAffect_total, na.rm = TRUE),
    Max_NegativeAffect = max(NegativeAffect_total, na.rm = TRUE)
  )

#Exporting object with descriptive statistic by gender to csv format
write.csv(summary_stats2, "summary_statistics.csv", row.names = FALSE)

# Counting the   Categorical Variable Gender 

sex_counts <- data3 %>%
  count(sex) 


print(sex_counts)



# Visualizing the Univariate distribution 

flexplot(SPP_total~1, data=data3)
flexplot(Conscientiousness_total~1, data=data3)
flexplot(NegativeAffect_total~1, data=data3)
flexplot(sex~1, data=data3)

# Visualizing Bivariate distribution 

flexplot(NegativeAffect_total~SPP_total, data=data3)
flexplot(NegativeAffect_total~Conscientiousness_total, data=data3)
flexplot(NegativeAffect_total~sex, data=data3)

# multivariate visualization
flexplot(NegativeAffect_total~Conscientiousness_total+SPP_total+sex, data=data3, method="lm")
flexplot(NegativeAffect_total~Conscientiousness_total+SPP_total|sex, data=data3, method="lm")



# Multiple Linear Regression Model   

model <- lm(NegativeAffect_total~Conscientiousness_total+SPP_total +sex, data=data3)

#Model Diagnostic Plot
visualize(model, plot="residuals")

# Extract residuals and perform the Shapiro-Wilk test
residuals <- residuals(model)
shapiro.test(residuals)


# Full and reduced model for Hypothesis 1 

full <- lm(NegativeAffect_total~Conscientiousness_total+SPP_total+sex,data=data3)
reduced<- lm(NegativeAffect_total~1, data=data3)

#Comparing a reduced and full Model for Hypothesis 1

model.comparison(reduced, full)

estimates(full)

#Statistical Summary of the Model for hypothesis 1

summary(model)


# Full and reduced  model  for Hypothesis 2 with control on sex and Conscientiousness 
full <- lm(NegativeAffect_total~Conscientiousness_total+SPP_total+sex,data=data3)
reduced<- lm(NegativeAffect_total~Conscientiousness_total+sex, data=data3)

##Comparing a reduced and full Model  for Hypothesis 2

model.comparison(reduced, full)

estimates(full)

#Statistical Summary of the Model for hypothesis 2

summary(model)









