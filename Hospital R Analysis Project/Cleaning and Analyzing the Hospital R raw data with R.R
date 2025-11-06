#loading packages for cleaning
library(tidyverse)   #A collection of packages (dplyr, ggplot2, readr, tidyr, etc.) for data wrangling, visualization, and analysis in a consistent “tidy” style.
library(data.table)    #Fast data manipulation, especially for large datasets.
library(lubridate)   #Makes working with dates and times easy. 
library(janitor)  #Cleaning data, e.g., column names, duplicate rows, or formatting tables.
library(readr)


# Loading the dataset
hospital_data<- read_csv("D:/MERCYBUNDI/OneDrive/Desktop/data science with r/R Projects (Practice)/Hospital R/Hospital_R_admissions_raw.csv")

#keeping the original data
hospital_data_clean <- hospital_data

#get a view of the data
head(hospital_data_clean)
tail(hospital_data_clean)
str(hospital_data_clean)
glimpse(hospital_data_clean)
summary(hospital_data_clean)
view(hospital_data_clean)



### DATA CLEANING
##standardize column names to lowercase, no spaces/special characters
hospital_data_clean <- hospital_data_clean %>% clean_names()


##Changing data type of columns and also creating length of stay(los) column
hospital_data_clean <- hospital_data_clean %>%
                              mutate(
                                admission_date = mdy(admission_date),
                                discharge_date = mdy(discharge_date),
                                los = as.numeric(discharge_date - admission_date) + 1
                                )
#checking to see if the values have been converted to NA
hospital_data_clean[,c("admission_date","discharge_date","los")]

#checking to see if there is a negative in los column incase of date error
hospital_data_clean["los" <= 0]

#checking to see if the column type has been changed
                      #sapply applies function to several columns
sapply(hospital_data_clean[, c("admission_date", "discharge_date")], class)



##From summary i have seen that wbc has negative values which is clinically impossible
hospital_data_clean <- hospital_data_clean %>% mutate(wbc = ifelse(wbc <= 0, NA, wbc))
hospital_data_clean["wbc"<=0]



##Checking the missing values, that is, cells that have NA
# Want to create a column showing sum of NA for each row
  # This helps in removing the rows with too much missing values per row
  #filtering rows with their total sum of NA's being more than 1
na_data <- hospital_data_clean %>% mutate(na_count = rowSums(is.na(.)))
na_data %>% filter(na_count > 1) %>% select(admission_id, na_count)

# checking sum of NA's for each column
colSums(is.na(hospital_data_clean))



##Filling in the missing values

# We have determined that its only numerical columns that have missing values
# From Summary(), the mean and the median are close to each(symmetrical), so its safe to use mean()

#hospital_data_clean$creatinine[is.na(hospital_data_clean$creatinine)] <- mean(hospital_data_clean$creatinine, na.rm = TRUE)
#hospital_data_clean$wbc[is.na(hospital_data_clean$wbc)] <- mean(hospital_data_clean$wbc, na.rm = TRUE)

#using mutate()
hospital_data_clean <- hospital_data_clean %>%
                            mutate(
                              creatinine = round(ifelse(is.na(creatinine), mean(creatinine, na.rm = TRUE), creatinine),2),
                              wbc       = round(ifelse(is.na(wbc), mean(wbc, na.rm = TRUE), wbc),1)
                              )

#checking to see if all missing values have been filled
colSums(is.na(hospital_data_clean[, c("creatinine", "wbc")]))



##Standardizing columns with character type values
#checking for unique values in each char type column
unique(hospital_data_clean$sex)
unique(hospital_data_clean$diagnosis_code)

#standardizing the sex column
hospital_data_clean <- hospital_data_clean %>% 
                                            mutate(
                                              sex = str_to_title(str_trim(sex)),
                                              sex = case_when(
                                                sex %in% c("M", "Male") ~ "Male",
                                                sex %in% c("F", "Female") ~ "Female",
                                                TRUE ~ sex
                                              )
                                            )

#Standardizing the diagnosis_code column
hospital_data_clean <- hospital_data_clean %>% 
                            mutate(diagnosis_code = str_to_upper(str_trim(diagnosis_code)))

#checking to see if it has standardized
unique(hospital_data_clean$sex)
unique(hospital_data_clean$diagnosis_code)



##Dealing with Duplicates
# Check for duplicates
sum(duplicated(hospital_data_clean))

# View the duplicated rows
hospital_data_clean[duplicated(hospital_data_clean),]

#Remove the duplicated rows
hospital_data_clean <- hospital_data_clean %>% distinct()



##converting some columns to factor and others to interger
hospital_data_clean <- hospital_data_clean %>%
  mutate(
    sex = factor(sex, levels = c("Male", "Female")),
    icu_admission = as.integer(icu_admission),
    readmitted_30 = as.integer(readmitted_30),
    in_hospital_death = as.integer(in_hospital_death)
  )



glimpse(hospital_data_clean)
summary(hospital_data_clean)

## Saving the cleaned data as a csv file
getwd()
write.csv(hospital_data_clean, "D:\\MERCYBUNDI\\OneDrive\\Desktop\\data science with r\\R Projects (Practice)\\Hospital R\\hospital_data_clean.csv", row.names = FALSE)




###ANALYZING THE DATASET


##Loading packages
#Packages for data manipulation & summaries
library(tableone) #create summary tables stratified by outcomes.
library(finalfit) #easy regression tables and summary statistics for predictors vs outcomes.

#Packages for visualization
library(ggplot2) #plots for numeric and categorical variables (distributions, boxplots, bar charts, scatter plots).
library(factoextra) #optional if doing clustering or PCA.

#Packages for Statistical tests & modeling
# stats(built-in) -> t-tests, chi-square tests, correlations, linear/logistic regression.
library(broom) #tidy outputs from regression models.
library(pROC) #evaluate predictive models (ROC curves, AUC).
library(caret) #for predictive modelling.
library(glmnet) #for predictive modelling.
library(randomForest)  #for predictive modelling.
library(survival)  #does the modeling and math.
library(survminer)  #makes it beautiful and easy to interpret.


#Packages for reporting & tables
library(knitr)   #dynamic reports of results.
library(rmarkdown)  #dynamic reports of results.
library(flextable)  #pretty tables for reporting or dashboards.
library(gt)  #pretty tables for reporting or dashboards.
library(DT)   #pretty tables for reporting or dashboards.



###Exploratory Data Analysis

##PATIENT DEMOGRAPHICS AND CHARACTERISTICS
#Age Distributions
age1 <- ggplot(hospital_data_clean,aes(x = age))+
  geom_histogram(binwidth = 5)+
  labs(title = "Distribution of Age")
age1

#creating age_group column
hospital_data_clean <- hospital_data_clean %>%
  mutate(age_group = cut(
    age,
    breaks = c(18, 35, 50, 65, 80, Inf),
    labels = c("19–35", "36–50", "51–65", "66–80", "80+"),
    right = FALSE
  ))

  #bar graph of the age_group
agegroup1 <- ggplot(hospital_data_clean, aes(x = age_group)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = after_stat(count), vjust = -0.2)) +
  labs(title = "Admissions by Age_group", x = "Age_group", y = "Number of Admissions")
agegroup1

#Comparing sex across admissions
sex1 <- ggplot(hospital_data_clean, aes(x = sex, fill = sex)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
  labs(title = "Admissions by Sex", x = "Sex", y = "Number of Admissions")
sex1

#Finding the common diagnosis
diagnos1 <- ggplot(hospital_data_clean, aes(x = diagnosis_code, fill = diagnosis_code))+
  geom_bar()+
  labs(title = "Distribution of Diagnosis", x = "Diagnosis_code", y = "Number of Patients")
diagnos1

#Admissions monthly, checking which months have high patient admissions
monthly_trend <- hospital_data_clean %>%
  mutate(month = lubridate::month(admission_date, label = TRUE, abbr = TRUE)) %>%
  count(month, sex)

  #graphical presentation
monthly1 <- ggplot(monthly_trend, aes(x = month, y = n, color = sex, group = sex))+
  geom_line(size = 1.2) +
  geom_point(size = 2)+
  labs(title = "Monthly trend admission by sex", x = "month", y = "Number of patients", color = "sex")
monthly1


#Admissions across the years(by month)
admissions_yearly <- hospital_data_clean %>%
  mutate(month = floor_date(admission_date, unit = "month")) %>%
  count(month, sex) %>% arrange(month)

  #graphical presentation
admission1 <- ggplot(admissions_yearly, aes(x = month, y = n, color = sex, group = sex))+
  geom_line(size = 1.2) +
  geom_point(size = 2)+
  labs(title = "Admissions over time by sex", x = "month-year", y = "Number of patients", color = "sex")
admission1



### CLINICAL MEASUREMENTS
##Checking how varying clinical measurements by sex,age_group,comorbidity_score
#clinical measurements by sex
vars <- c("heart_rate", "bp_systolic", "creatinine", "wbc")
table_sex <- tableone::CreateTableOne(vars = vars, strata = "sex", data = hospital_data_clean, test = TRUE)
print(table_sex, smd = TRUE)

#clinical measurements by age_group
table_agegroup <- tableone::CreateTableOne(vars = vars, strata = "age_group", data = hospital_data_clean, test = TRUE)
print(table_agegroup, smd = TRUE)

#clinical measurements by comorbidity_score
table_score <- tableone::CreateTableOne(vars = vars, strata = "comorbidity_score", data = hospital_data_clean, test = TRUE)
print(table_score, smd = TRUE)


##Finding outliers in the vital signs and lab results
heartrate_boxplot <- ggplot(hospital_data_clean, aes(y = heart_rate)) +
  geom_boxplot()
heartrate_boxplot

bpsystolic_boxplot <- ggplot(hospital_data_clean, aes(y = bp_systolic)) +
  geom_boxplot()
bpsystolic_boxplot

creatinine_boxplot <- ggplot(hospital_data_clean, aes(y = creatinine)) +
  geom_boxplot()
creatinine_boxplot

wbc_boxplot <- ggplot(hospital_data_clean, aes(y = wbc)) +
  geom_boxplot()
wbc_boxplot


##Checking if there is a relationship between comorbidity_score and vital signs or lab results
# Interpretation: If correlation coefficient > 0.4 (positive) or < -0.4 (negative), the relationship is moderately strong.
cor.test(hospital_data_clean$comorbidity_score,hospital_data_clean$heart_rate)
cor.test(hospital_data_clean$comorbidity_score,hospital_data_clean$bp_systolic)
cor.test(hospital_data_clean$comorbidity_score,hospital_data_clean$creatinine)
cor.test(hospital_data_clean$comorbidity_score,hospital_data_clean$wbc)


##Hospital Outcomes
hospital_outcomes <- hospital_data_clean %>%
        summarise(
              ICU = mean(icu_admission) * 100,
              Readmitted_30 = mean(readmitted_30) * 100,
              Death = mean(in_hospital_death) * 100
              )
hospital_outcomes



##Testing if length of stay (los) vary by ICU admission, readmission, or in-hospital death

#first i check if the LOS is normally distributed to know the test to use
#shapiro.test() is used to check if a data normally distributed
#If a data is normally distributed we use t.test() and if not, we use wilcoxon.test()
#Interpretation:
  #p-value < 0.05 → there’s a significant difference in LOS between groups.
  #p-value ≥ 0.05 → no statistically significant difference.

# ICU vs non-ICU
t.test(los ~ icu_admission, data = hospital_data_clean)

# Readmitted vs not
t.test(los ~ readmitted_30, data = hospital_data_clean)

# Died vs survived
t.test(los ~ in_hospital_death, data = hospital_data_clean)



##Relationship between variables
#Are older patients more likely to be admitted to ICU or have longer stays
table(hospital_data_clean$age_group, hospital_data_clean$icu_admission)
chisq.test(hospital_data_clean$age_group, hospital_data_clean$icu_admission)
anova_result1 <- aov(los ~ age_group, data = hospital_data_clean)
summary(anova_result1)


#Do patients with higher comorbidity scores have higher rates of ICU admission, readmission, or death
#ICU admission
chisq.test(table(factor(hospital_data_clean$comorbidity_score), hospital_data_clean$icu_admission))

#readmission
chisq.test(table(factor(hospital_data_clean$comorbidity_score), hospital_data_clean$icu_admission))

#death
chisq.test(table(factor(hospital_data_clean$comorbidity_score), hospital_data_clean$icu_admission))


#Checking if there any association between sex and outcomes (ICU, readmission, death)
#ICU
chisq.test(hospital_data_clean$sex, hospital_data_clean$icu_admission)

#readmission
chisq.test(hospital_data_clean$sex, hospital_data_clean$readmitted_30)

#death
chisq.test(hospital_data_clean$sex, hospital_data_clean$in_hospital_death)


##Checking if lab values (creatinine, WBC) are predictive of the outcomes(ICU, readmission, death)
#icu
model_icu <- glm(icu_admission ~ creatinine + wbc, data = hospital_data_clean, family = binomial)
summary(model_icu)

#readmission
model_readmitted <- glm(readmitted_30 ~ creatinine + wbc, data = hospital_data_clean, family = binomial)
summary(model_readmitted)

#in_death
model_indeath <- glm(in_hospital_death ~ creatinine + wbc, data = hospital_data_clean, family = binomial)
summary(model_indeath)



