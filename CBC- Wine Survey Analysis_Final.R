library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(openxlsx)
library(reshape2)

setwd("/Users/erlanggaroesdyoko/Documents/MADS/Master Thesis/R Code")
wine <- read.xlsx("Wine Dataset.xlsx")

summary(wine)
head(wine)

# Data Exploration ----
# Data Transformation for 
wine$Age.Group <- ordered(wine$Age.Group, 
                          levels = c("18 - 29", "30 - 40", "41 - 50", "51 - 60", "60 +")) # Ordinal
wine$Gender <- as.factor(wine$Gender) # Categorical
wine$Occupation <- as.factor(wine$Occupation) # Categorical
wine$Last.Education <- ordered(wine$Last.Education, 
                          levels = c("High School", "Bachelor", "Master", "PhD"))  # Ordinal
wine$Physical.Activity <- ordered(wine$Physical.Activity, 
                          levels = c("None", "Low", "Moderate", "High")) # Ordinal
wine$How.Often <- as.factor(wine$How.Often) # Categorical
wine$Occasion <- as.factor(wine$Occasion) # Categorical

# Data Cleaning for Location
wine <- wine %>%
  mutate(
    # Clean and unify Location
    Location_clean = str_trim(str_to_title(Location)),
    
    # Fix known noise (newline, extra whitespace, duplicates)
    Location_clean = case_when(
      str_detect(Location_clean, regex("Indones", ignore_case = TRUE)) ~ "Indonesia",
      str_detect(Location_clean, regex("Netherlands", ignore_case = TRUE)) ~ "Netherlands",
      str_detect(Location_clean, regex("Germany", ignore_case = TRUE)) ~ "Germany",
      str_detect(Location_clean, regex("France", ignore_case = TRUE)) ~ "France",
      str_detect(Location_clean, regex("Spain", ignore_case = TRUE)) ~ "Spain",
      TRUE ~ "Other"
    ),
    
    # Final nationality variable
    Location_clean = factor(Location_clean)
  )

summary(wine)
wine_unique_resp <- wine %>%
  filter(!is.na(Age.Group)) %>%
  distinct(Respondent.ID, .keep_all = TRUE)

wine_unique_elig <- wine_unique_resp %>%
  filter(How.Often != "Never")

# Respondent.ID = 94, 70, 38 -> Choose None for everything / almost everything
wine_unique_elig <- wine_unique_elig[!(wine_unique_elig$Respondent.ID %in% c(94, 70, 38)), ]
table(wine_unique_resp$How.Often)
summary(wine_unique_resp)



## Descriptive Statistics ----
wine_unique_elig <- wine_unique_elig %>%
  mutate(Occasion = case_when(
    Occasion == "Casual occasion (for example: birthday, hangout)" ~ "Casual",
    Occasion == "Celebratory occasion (or as a gift)" ~ "Celebratory",
    Occasion == "Food-centered occasion (to compliment the food)" ~ "Food-centered",
    Occasion == "Personal moments (for example: thinking, journaling, quality time)" ~ "Personal",
    TRUE ~ Occasion  # leave NA or other unmatched values unchanged
  ))

wine_unique_elig <- wine_unique_elig %>%
  mutate(Price = as.numeric(Price),
         Netherlands     = as.numeric(Location_clean == "Netherlands"),
         Germany = as.numeric(Location_clean == "Germany"),
         France  = as.numeric(Location_clean == "France"),
         Spain   = as.numeric(Location_clean == "Spain"),
         Indonesia = as.numeric(Location_clean == "Indonesia"),
         Other = as.numeric(Location_clean == "Other")
  )

# Percentages (rounded to 1 decimal)
prop.table(table(wine_unique_elig$Age.Group)) * 100
prop.table(table(wine_unique_elig$Gender)) * 100
prop.table(table(wine_unique_elig$Location)) * 100
prop.table(table(wine_unique_elig$Last.Education)) * 100
prop.table(table(wine_unique_elig$Occupation)) * 100
prop.table(table(wine_unique_elig$Occasion, useNA = "ifany")) * 100

# Bar plots
# Age Group
age_counts <- table(wine_unique_elig$Age.Group)
age_counts <- age_counts[age_counts > 0]  # Remove age groups with 0 respondents

bp_age <- barplot(age_counts,
                  main = "Distribution of Age Groups",
                  ylim = c(0, max(age_counts) + 5),
                  col = "gray")

text(x = bp_age, y = age_counts, labels = age_counts, pos = 3)

# Gender
gender_counts <- table(wine_unique_elig$Gender)
bp_gender <- barplot(gender_counts, main = "Gender Distribution", ylim = c(0, max(gender_counts) + 5))
text(x = bp_gender, y = gender_counts, labels = gender_counts, pos = 3)

# Location
loc_counts <- table(wine_unique_elig$Location_clean)
loc_counts <- loc_counts[loc_counts > 0]
bp_loc <- barplot(loc_counts,
                  main = "Location Distribution",
                  ylim = c(0, max(loc_counts) + 5),
                  xaxt = "n",       # Suppress x-axis tick labels
                  col = "gray")

text(x = bp_loc, y = loc_counts, labels = loc_counts, pos = 3)
text(x = bp_loc,
     y = par("usr")[3] - 1.5,            # Slightly below the x-axis
     labels = names(loc_counts),
     srt = 30,                           # Rotate 30 degrees
     adj = 1,                            # Right-align text
     xpd = TRUE,                         # Allow drawing outside plot region
     cex = 0.8)                          # Shrink label text if needed

# Last Education
edu_counts <- table(wine_unique_elig$Last.Education)
edu_counts <- edu_counts[edu_counts > 0]

bp_edu <- barplot(edu_counts,
                  main = "Education Levels",
                  ylim = c(0, max(edu_counts) + 5),
                  xaxt = "n",  # hide x-axis labels
                  col = "gray")

text(x = bp_edu, y = edu_counts, labels = edu_counts, pos = 3)

text(x = bp_edu,
     y = par("usr")[3] - 1.5,
     labels = names(edu_counts),
     srt = 30,
     adj = 1,
     xpd = TRUE,
     cex = 0.8)

# Occupation
occ_counts <- table(wine_unique_elig$Occupation)
occ_counts <- occ_counts[occ_counts > 0]
bp_occ <- barplot(occ_counts,
                  main = "Occupation Distribution",
                  ylim = c(0, max(occ_counts) + 5),
                  xaxt = "n",
                  col = "gray")

text(x = bp_occ, y = occ_counts, labels = occ_counts, pos = 3)
text(x = bp_occ,
     y = par("usr")[3] - 1.5,
     labels = names(occ_counts),
     srt = 30,
     adj = 1,
     xpd = TRUE,
     cex = 0.8)

# Occassion
occasion_counts <- table(wine_unique_elig$Occasion, useNA = "no")
occasion_counts <- occasion_counts[occasion_counts > 0]
bp_occasion <- barplot(occasion_counts,
                       main = "Wine Consumption Occasion",
                       ylim = c(0, max(occasion_counts) + 5),
                       xaxt = "n",
                       col = "gray")

text(x = bp_occasion, y = occasion_counts, labels = occasion_counts, pos = 3)
text(x = bp_occasion,
     y = par("usr")[3] - 1.5,
     labels = names(occasion_counts),
     srt = 30,
     adj = 1,
     xpd = TRUE,
     cex = 0.8)

# Facet wrap
# Reshape all categorical variables into long format
long_data <- wine_unique_elig %>%
  select(Gender, Age.Group, Location_clean, Last.Education, Occupation, Occasion) %>%
  rename(Location = Location_clean) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>%
  filter(!is.na(Value))

ggplot(long_data, aes(x = Value)) +
  geom_bar(fill = "gray") +
  facet_wrap(~ Category, scales = "free_x") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(x = NULL, y = "Count", title = "Distribution of Respondent Characteristics") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14)
  )

# Plot using percentages
wine_unique_elig$Last.Education <- recode_factor(
  wine_unique_elig$Last.Education,
  "High School" = "1. High School",
  "Bachelor"    = "2. Bachelor",
  "Master"      = "3. Master",
  "PhD"         = "4. PhD"
)

long_data_2 <- wine_unique_elig %>%
  select(Gender, Age.Group, Location_clean, Last.Education, Occupation, Occasion) %>%
  rename(Location = Location_clean) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  group_by(Category, Value) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Category) %>%
  mutate(Percentage = Count / sum(Count) * 100)

ggplot(long_data_2, aes(x = Value, y = Percentage)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3.5) +
  facet_wrap(~ Category, scales = "free_x") +
  labs(x = NULL, y = "Percentage", title = "Distribution of Respondent Characteristics") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14)
  )

# Data Analysis ----
## Data Transformation ----
# Replace None to NAs
wine$Price[wine$Price == "None"] <- 0
wine$Type[wine$Type == "None"] <- 0
wine$Alcohol[wine$Alcohol == "None"] <- 0
wine$Aging.Time[wine$Aging.Time == "None"] <- 0

# Create Dummy for Type of Wine
wine_t1 <- wine %>%
  filter(How.Often != "Never") %>% # Remove the Non - Eligible Respondents
  mutate(
    Type_Red        = if_else(Type == "Red wine",        1, 0),
    Type_White      = if_else(Type == "White wine",      1, 0),
    Type_Rose       = if_else(Type == "Rosé wine",       1, 0),
    Type_Sparkling  = if_else(Type == "Sparkling wine",  1, 0)
  )

# Respondent.ID = 94, 70, 38 -> Choose None for everything / almost everything
wine_t1 <- wine_t1[!(wine_t1$Respondent.ID %in% c(94, 70, 38)), ]

# Create Dummy for Gender and Interaction with Price
wine_t1 <- wine_t1 %>%
  mutate(Gender = as.character(Gender),
    Gender_Male   = if_else(Gender == "Male",   1, 0),
    Gender_Female = if_else(Gender == "Female", 1, 0),
    Price_Gender_Male   = as.numeric(Gender_Male)   * as.numeric(Price),
    Price_Gender_Female = as.numeric(Gender_Female) * as.numeric(Price))
wine_t1$Gender <- as.factor(wine_t1$Gender)
  
# Create Dummy for Location and Interaction with Price
wine_t1 <- wine_t1 %>%
  mutate(Price = as.numeric(Price),
    NL      = as.numeric(Location_clean == "Netherlands"),
    Germany = as.numeric(Location_clean == "Germany"),
    France  = as.numeric(Location_clean == "France"),
    Spain   = as.numeric(Location_clean == "Spain"),
    ID = as.numeric(Location_clean == "Indonesia"),
    Other = as.numeric(Location_clean == "Other"),
    Price_NL      = NL      * Price,
    Price_Germany = Germany * Price,
    Price_France  = France  * Price,
    Price_Spain   = Spain   * Price,
    Price_ID      = ID      * Price,
    Price_Other   = Other * Price
  )

# Remove Q.Reference as it might affect the model - it is the same as Task
wine_t1$Q.Reference <- NULL
sum(is.na(wine_t1))

# Change Choice to Logical
wine_t1$Choice <- as.logical(wine_t1$Choice)

# NC variables
wine_t1 <- wine_t1 %>%
  mutate(
    # 1. Identify no-choice rows
    NC = if_else(Alternative == "None", 1, 0),
    
    # 2. Convert variables to numeric format
    Price = as.numeric(Price),
    Alcohol = as.numeric(str_remove(as.character(Alcohol), "%")),  # strip % if present
    Aging.Time = case_when(
      str_detect(as.character(Aging.Time), "4\\+") ~ 4,
      str_detect(as.character(Aging.Time), "year") ~ parse_number(as.character(Aging.Time)),
      TRUE ~ as.numeric(Aging.Time)
    )
  ) %>%
  
  # 3. Set numeric attributes to 0 for 'None' rows only
  mutate(across(
    c(Price, Alcohol, Aging.Time),
    ~ if_else(NC == 1, 0, .)
  ))

# Data Transformation for non-linear (Categorical)
# Price dummy variables (ref = 6)
wine_t1 <- wine_t1 %>%
  mutate(
    Price_12 = if_else(Price == 12, 1, 0),
    Price_18 = if_else(Price == 18, 1, 0),
    Price_24 = if_else(Price == 24, 1, 0),
    Price_6  = if_else(Price == 6, 1, 0),
    
    Alcohol_7  = if_else(Alcohol == 7, 1, 0),
    Alcohol_12 = if_else(Alcohol == 12, 1, 0),
    Alcohol_18 = if_else(Alcohol == 18, 1, 0),
    Alcohol_5.5 = if_else(Alcohol == 5.5, 1, 0),
    
    Aging_2 = if_else(Aging.Time == 2, 1, 0),
    Aging_3 = if_else(Aging.Time == 3, 1, 0),
    Aging_4 = if_else(Aging.Time == 4, 1, 0),
    Aging_1 = if_else(Aging.Time == 1, 1, 0),
    
    # Interaction Price * Gender
    Price_6_Male = as.numeric(Price == 6 & Gender_Male == 1), 
    Price_12_Male = as.numeric(Price == 12 & Gender_Male == 1),
    Price_18_Male = as.numeric(Price == 18 & Gender_Male == 1),
    Price_24_Male = as.numeric(Price == 24 & Gender_Male == 1), 
    
    Price_6_Female = as.numeric(Price == 6 & Gender_Female == 1), 
    Price_12_Female = as.numeric(Price == 12 & Gender_Female == 1),
    Price_18_Female = as.numeric(Price == 18 & Gender_Female == 1),
    Price_24_Female = as.numeric(Price == 24 & Gender_Female == 1), 
    
    # Interaction Price * Location
    Price_6_France = Price_6 * France,
    Price_12_France = Price_12 * France,
    Price_18_France = Price_18 * France,
    Price_24_France = Price_24 * France,
    
    Price_6_Germany = Price_6 * Germany,
    Price_12_Germany = Price_12 * Germany,
    Price_18_Germany = Price_18 * Germany,
    Price_24_Germany = Price_24 * Germany,
    
    Price_6_Netherlands = Price_6 * NL,
    Price_12_Netherlands = Price_12 * NL,
    Price_18_Netherlands = Price_18 * NL,
    Price_24_Netherlands = Price_24 * NL,
    
    Price_6_ID = Price_12 * ID,
    Price_12_ID = Price_12 * ID,
    Price_18_ID = Price_18 * ID,
    Price_24_ID= Price_24 * ID,
    
    Price_6_Spain = Price_6 * Spain,
    Price_12_Spain = Price_12 * Spain,
    Price_18_Spain = Price_18 * Spain,
    Price_24_Spain = Price_24 * Spain,
    
    Price_6_Other = Price_6 * Other,
    Price_12_Other = Price_12 * Other,
    Price_18_Other = Price_18 * Other,
    Price_24_Other = Price_24 * Other
  )

# unique choice‑set id
wine_t1$chid <- paste(wine_t1$Respondent.ID, wine_t1$Task, sep = "_")

### Train Test Split ----
cbc_split <- wine_t1
set.seed(123)

# Get unique choice task IDs (chid)
unique_chids <- unique(cbc_split$chid)

# Sample 25% of choice sets (or choose a fixed number if preferred)
test_chids <- sample(unique_chids, size = round(0.25 * length(unique_chids)))

# Assign the split based on choice set (chid), not Respondent.ID
cbc_split <- cbc_split %>%
  mutate(split = ifelse(chid %in% test_chids, "test", "train"))

# Create train and test sets
train_data <- filter(cbc_split, split == "train")
test_data  <- filter(cbc_split, split == "test")

# Ensure Alternative is a factor
train_data$Alternative <- as.factor(train_data$Alternative)
test_data$Alternative <- as.factor(test_data$Alternative)

# Save training data
write.csv(train_data, "train_data.csv", row.names = FALSE)
# Save test data
write.csv(test_data, "test_data.csv", row.names = FALSE)

# Sanity Check
cbc_split %>% 
  group_by(split) %>%
  summarise(n_respondents = n_distinct(Respondent.ID))

table(train_data$Alternative[train_data$Choice == TRUE])
table(test_data$Alternative[test_data$Choice == TRUE])

## Classical Statistical MNL ----
library(mlogit)
# convert to format required for mlogit
mlogit_data_train <- mlogit.data(
  data     = train_data,
  choice   = "Choice",
  shape    = "long",
  alt.var  = "Alternative",
  id.var   = "Respondent.ID",
)

# MNL Model - Linear
mnl_num <- mlogit(
  Choice ~ Price + Alcohol + Aging.Time + Type_Red + Type_White + Type_Rose + NC | 0,
  data = mlogit_data_train,
  reflevel = "A")
summary(mnl_num)

mnl_IP <- mlogit(
  Choice ~ Price^2 + Alcohol^2 + Aging.Time^2 + Type_Red + Type_White + Type_Rose + NC | 0,
  data = mlogit_data_train,
  reflevel = "A")
summary(mnl_IP)

# Interaction Terms - Linear
mnl_num_int <- mlogit(
  Choice ~ Price + Alcohol + Aging.Time + Type_Red + Type_White + Type_Rose  + NC
   + Price_Gender_Male + Price_NL + Price_Germany + Price_France + Price_Spain + Price_Other 
  | 0,
  data = mlogit_data_train,
  reflevel = "A")
summary(mnl_num_int)

# Interaction Terms - Quadratic
mnl_qua_int <- mlogit(
  Choice ~ Price + I(Price^2) + Alcohol + Aging.Time + I(Aging.Time^2)  + Type_Red + Type_White + Type_Rose  + NC
  + Price_Gender_Male + Price_NL + Price_Germany + Price_France + Price_Spain + Price_Other 
  | 0,
  data = mlogit_data_train,
  reflevel = "A")
summary(mnl_qua_int)

# Model without Interaction - Categorical
mnl_cat <- mlogit(
  Choice ~ Price_12 + Price_18 + Price_24 +
    Alcohol_7 + Alcohol_12 + Alcohol_18 +
    Aging_2 + Aging_3 + Aging_4 +
    Type_Red + Type_White + Type_Rose +
    NC | 0,
  data = mlogit_data_train,
  reflevel = "A"
)

summary(mnl_cat)

mnl_cat_IP <- mlogit(
  Choice ~ Price_12^2 + Price_18^2 + Price_24^2 +
    Alcohol_7^2 + Alcohol_12^2 + Alcohol_18^2 +
    Aging_2^2 + Aging_3^2 + Aging_4^2 +
    Type_Red + Type_White + Type_Rose +
    NC | 0,
  data = mlogit_data_train,
  reflevel = "A"
)

summary(mnl_cat_IP)

# Model with Interaction
mnl_cat_int <- mlogit(
  Choice ~ Price_12 + Price_18 + Price_24 +
    Price_12_France + Price_18_France + Price_24_France +
    Price_12_Germany + Price_18_Germany + Price_24_Germany +
    Price_12_Netherlands + Price_18_Netherlands + Price_24_Netherlands +
    Price_12_Spain + Price_18_Spain + Price_24_Spain +
    Price_12_Other + Price_18_Other + Price_24_Other +
    Price_12_Male + Price_18_Male + Price_24_Male +
    Type_Red + Type_White + Type_Rose +
    Alcohol_7 + Alcohol_12 + Alcohol_18 +
    Aging_2 + Aging_3 + Aging_4 +
    NC
  | 0,
  data = mlogit_data_train,
  reflevel = "A"
)

summary(mnl_cat_int)

### Validation ----
# LR-Test
null_model <- mlogit(Choice ~ 1, data = mlogit_data_train, reflevel = "A")
lrtest(null_model, mnl_num, mnl_num_int, mnl_cat, mnl_cat_int)

# McFadden's test
# McFadden's Pseudo R² for mnl_num_int
logLik_null <- as.numeric(logLik(null_model))
logLik_model <- as.numeric(logLik(mnl_cat_int))

# McFadden's R² formula
mcfadden_r2 <- 1 - (logLik_model / logLik_null)
mcfadden_r2

# Statistics test
eigen(mnl_num_int$hessian)$values

# Multicollinearity test
library(car)
X <- model.matrix(mnl_cat_int)
X_df <- as.data.frame(X)

# Fit a linear model with a dummy response to use vif()
dummy_y <- rnorm(nrow(X_df))  # just to satisfy lm() requirements

lm_vif <- lm(dummy_y ~ ., data = X_df)
vif(lm_vif)

# LR-Test
lrtest(mnl_num, mnl_num_int, mnl_cat, mnl_cat_int)

# AIC
AIC(mnl_num)        # Model 1: Basic linear
AIC(mnl_num_int)    # Model 2: + interactions
AIC(mnl_cat)       # Model 3: + non-linear effects
AIC(mnl_cat_int)   # Model 4: + non-linear + interactions

model_aic <- data.frame(
  Model = c("Linear (M1)", 
            "Linear + Interactions (M2)", 
            "Non-linear (M3)", 
            "Non-linear + Interactions (M4)"),
  LogLik = c(logLik(mnl_num), logLik(mnl_num_int), logLik(mnl_cat), logLik(mnl_cat_int)),
  AIC = c(AIC(mnl_num), AIC(mnl_num_int), AIC(mnl_cat), AIC(mnl_cat_int)),
  Parameters = c(length(coef(mnl_num)), 
                 length(coef(mnl_num_int)), 
                 length(coef(mnl_cat)), 
                 length(coef(mnl_cat_int)))
)

model_aic <- model_aic[order(model_aic$AIC), ] # Sort by AIC

print(model_aic)

### Hit rate ----
# Function
get_hit_rate <- function(model,
                         data,
                         task_id = "chid",
                         alt_id  = "Alternative",
                         choice  = "Choice") {
  
  ## 1. Predict probabilities for each task–alternative
  prob_mat <- predict(model, newdata = data, type = "probabilities")
  
  ## 2. Wide ➜ long
  prob_long <- melt(prob_mat) # chid | alt | prob
  names(prob_long) <- c(task_id, alt_id, "pred_prob")
  
  ## 3. Harmonise key types
  prob_long[[task_id]] <- as.character(prob_long[[task_id]])
  prob_long[[alt_id ]] <- as.character(prob_long[[alt_id ]])
  
  data[[task_id]]     <- as.character(data[[task_id]])
  data[[alt_id ]]     <- as.character(data[[alt_id ]])
  
  ## 4. Join predictions back to the long data
  data_pred <- left_join(data, prob_long, by = c(task_id, alt_id))
  
  ## 5. One row per task: was the top-prob alt the chosen one?
  task_hits <- data_pred %>%
    group_by(.data[[task_id]]) %>%
    summarise(
      chosen_alt    = .data[[alt_id]][.data[[choice]] == 1][1],
      top_pred_alt  = .data[[alt_id]][ which.max(pred_prob) ],
      hit           = as.integer(chosen_alt == top_pred_alt),
      .groups = "drop"
    )
  
  ## 6. Return classic hit rate (0–1 scale)
  mean(task_hits$hit, na.rm = TRUE)
}

# MNL Data Format
mlogit_data_test <- mlogit.data(
  data     = test_data,
  choice   = "Choice",
  shape    = "long",
  alt.var  = "Alternative",
  id.var   = "Respondent.ID",
)

in_hit_1  <- get_hit_rate(mnl_num, mlogit_data_train)   # in-sample
out_hit_1 <- get_hit_rate(mnl_num, mlogit_data_test)    # out-of-sample
in_hit_2  <- get_hit_rate(mnl_num_int, mlogit_data_train)   # in-sample
out_hit_2<- get_hit_rate(mnl_num_int, mlogit_data_test)    # out-of-sample
in_hit_3  <- get_hit_rate(mnl_cat, mlogit_data_train)   # in-sample
out_hit_3 <- get_hit_rate(mnl_cat, mlogit_data_test)    # out-of-sample
in_hit_4  <- get_hit_rate(mnl_cat_int, mlogit_data_train)   # in-sample
out_hit_4 <- get_hit_rate(mnl_cat_int, mlogit_data_test)    # out-of-sample

hit_rate_table <- tibble(
  Model          = c("Linear",
                     "Linear + Interaction",
                     "Categorical",
                     "Categorical + Interaction"),
  In_sample      = round(c(in_hit_1,  in_hit_2,  in_hit_3,  in_hit_4), 3),
  Out_of_sample  = round(c(out_hit_1, out_hit_2, out_hit_3, out_hit_4), 3)
)

hit_rate_table

### WTP ----
# WTP Part-worth Model (categorical)
# Model with Interaction
mnl_wtp <- mlogit(
  Choice ~ Price + 
    #Price_12_France + Price_18_France + Price_24_France +
    #Price_12_Germany + Price_18_Germany + Price_24_Germany +
    #Price_12_Netherlands + Price_18_Netherlands + Price_24_Netherlands +
    #Price_12_Spain + Price_18_Spain + Price_24_Spain +
    #Price_12_Other + Price_18_Other + Price_24_Other +
    #Price_12_Male + Price_18_Male + Price_24_Male +
    Type_Red + Type_White + Type_Rose +
    Alcohol_7 + Alcohol_12 + Alcohol_18 +
    Aging_2 + Aging_3 + Aging_4 +
    NC
  | 0,
  data = mlogit_data_train,
  reflevel = "A"
)

summary(mnl_wtp)
coef_mnl <- summary(mnl_cat_int)$coefficients  # Replace 'model_4' with your actual model object

# For simplicity, assume Price_6 (or equivalent) is the omitted reference level.
# So, Price_12, Price_18, and Price_24 are compared to it.

# Choose a base price coefficient for denominator (e.g., Price_12)
price_12_coef <- coef_mnl["Price_12"]

# Compute WTPs
wtp_red   <- coef_mnl["Type_Red"]   /   price_12_coef
wtp_white <- coef_mnl["Type_White"] /   price_12_coef
wtp_rose  <- coef_mnl["Type_Rose"]  /   price_12_coef

wtp_alcohol_7  <- coef_mnl["Alcohol_7"]  /  price_12_coef
wtp_alcohol_12 <- coef_mnl["Alcohol_12"] /  price_12_coef
wtp_alcohol_18 <- coef_mnl["Alcohol_18"] /   price_12_coef

wtp_aging_2 <- coef_mnl["Aging_2"] /  price_12_coef
wtp_aging_3 <- coef_mnl["Aging_3"] /   price_12_coef
wtp_aging_4 <- coef_mnl["Aging_4"] /  price_12_coef

# Create WTP table
wtp_table <- data.frame(
  Attribute = c("Red Wine", "White Wine", "Rosé Wine",
                "Alcohol 7%", "Alcohol 12%", "Alcohol 18%",
                "Aging 2 Years", "Aging 3 Years", "Aging 4 Years"),
  WTP_Euro = round(c(wtp_red, wtp_white, wtp_rose,
                     wtp_alcohol_7, wtp_alcohol_12, wtp_alcohol_18,
                     wtp_aging_2, wtp_aging_3, wtp_aging_4), 2)
)

print(wtp_table)

# WTP non-linear
# 1. Extract coefficients from Model 3
coef_model3 <- summary(mnl_cat)$coefficients  # point estimates only

# 2. Define price levels and extract part-worths
price_utils <- c(
  "Price_6"  = 0,  # reference level
  "Price_12" = coef_model3["Price_12"],
  "Price_18" = coef_model3["Price_18"],
  "Price_24" = coef_model3["Price_24"]
)

# 3. Compute span of price utility and price range
span_utility_price <- max(price_utils) - min(price_utils)
price_range_euro   <- 24 - 6  # max - min

# 4. Compute €/utility conversion factor
euros_per_utility <- price_range_euro / span_utility_price

# 5. Calculate WTPs for other attributes
wtp_table <- data.frame(
  Attribute = c("Red Wine", "White Wine", "Rosé Wine",
                "Alcohol 7%", "Alcohol 12%", "Alcohol 18%",
                "Aging 2 Years", "Aging 3 Years", "Aging 4 Years"),
  Utility = c(coef_model3["Type_Red"], coef_model3["Type_White"], coef_model3["Type_Rose"],
              coef_model3["Alcohol_7"], coef_model3["Alcohol_12"], coef_model3["Alcohol_18"],
              coef_model3["Aging_2"], coef_model3["Aging_3"], coef_model3["Aging_4"])
)

# 6. Compute WTP
wtp_table$WTP_Euro <- round(wtp_table$Utility * euros_per_utility, 2)

# 7. View results
print(wtp_table)


### Market Share ----
coef <- summary(mnl_cat_int)$coefficients  # replace with your model object if needed

# Define 3 Product Profiles + No-Choice 
profiles <- data.frame(
  # Price dummies
  Price_12 = c(1, 0, 0, 0),
  Price_18 = c(0, 1, 0, 0),
  Price_24 = c(0, 0, 1, 0),
  
  # Wine type dummies (base = Sparkling)
  Type_Red   = c(1, 0, 0, 0),
  Type_White = c(0, 1, 0, 0),
  Type_Rose  = c(0, 0, 1, 0),
  
  # Alcohol content dummies (base omitted)
  Alcohol_7  = c(0, 0, 1, 0),
  Alcohol_12 = c(1, 0, 0, 0),
  Alcohol_18 = c(0, 1, 0, 0),
  
  # Aging time dummies (base = 1 year)
  Aging_2 = c(0, 1, 0, 0),
  Aging_3 = c(0, 0, 1, 0),
  Aging_4 = c(1, 0, 0, 0),
  
  # No-choice indicator
  NC = c(0, 0, 0, 1)
)

# Align Coefficients with Profile Columns
coefs_to_use <- coef[names(coef) %in% colnames(profiles)]
X <- as.matrix(profiles[, names(coefs_to_use)])

# Compute Utilities and Market Shares
U <- X %*% coefs_to_use
expU <- exp(U)
market_share <- round(expU / sum(expU), 3)

# Result
market_share_df <- cbind(profiles, Market_Share = market_share)
print(market_share_df)


### Partworth - MNL ----
# Extract coefficients
summary_obj <- summary(mnl_num_int)
coefs_table <- summary_obj$CoefTable

partworths_df <- data.frame(
  Attribute = c("Price", "Type", "Type", "Type", "Type", "Aging Time", "Alcohol", "No Choice"),
  Level = c("Per €1 increase", "Sparkling (ref)", "Red", "White", "Rosé", "Per year increase", "Per % alcohol", "None"),
  Estimate = c(
    coefs_table["Price", "Estimate"],
    0,
    coefs_table["Type_Red", "Estimate"],
    coefs_table["Type_White", "Estimate"],
    coefs_table["Type_Rose", "Estimate"],
    coefs_table["Aging.Time", "Estimate"],
    coefs_table["Alcohol", "Estimate"],
    coefs_table["NC", "Estimate"]
  ),
  p_value = c(
    coefs_table["Price", "Pr(>|z|)"],
    NA,
    coefs_table["Type_Red", "Pr(>|z|)"],
    coefs_table["Type_White", "Pr(>|z|)"],
    coefs_table["Type_Rose", "Pr(>|z|)"],
    coefs_table["Aging.Time", "Pr(>|z|)"],
    coefs_table["Alcohol", "Pr(>|z|)"],
    coefs_table["NC", "Pr(>|z|)"]
  )
)


partworths_df$p_value <- as.numeric(partworths_df$p_value)
partworths_df$Estimate <- round(partworths_df$Estimate, 3)
partworths_df$p_value <- round(partworths_df$p_value, 3)

partworths_df$Signif <- cut(
  partworths_df$p_value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", "")
)
partworths_df$Signif[is.na(partworths_df$p_value)] <- "ref"

# View
print(partworths_df[, c("Attribute", "Level", "Estimate", "p_value", "Signif")], row.names = FALSE)


## Hierarchical Bayes - brms ----
library(brms)
library(bayesplot)

# Step 1: Ensure Choice is a factor with "chosen" = level 1
brms_train <- train_data %>%
  mutate(Respondent.ID = as.factor(Respondent.ID),
         Alternative = as.factor(Alternative),
         Type = as.factor(Type),
         Choice = as.numeric(Choice)
  )

brms_test <- test_data %>%
  mutate(Respondent.ID = as.factor(Respondent.ID),
         Alternative = as.factor(Alternative),
         Type = as.factor(Type),
         Choice = as.numeric(Choice)
  )


# Step 2: Ensure all predictors are properly coded
# (brms can handle numeric, ordinal, and factors)

# Check levels (do NOT recode anything here unless needed)
str(brms_train)

# Step 3: Check for NA and remove them
brms_train <- brms_train %>%
  filter(complete.cases(Price, Alcohol, Aging.Time, Type))


# Step 4: Confirm final structure
glimpse(brms_train)

# Linear
bayes_num <- brm(Choice ~ NC + Price + Alcohol + Aging.Time + Type_Red + Type_White + Type_Rose +
                       (1 + Price + Type_Red + Type_White + Type_Rose  + Alcohol + Aging.Time | Respondent.ID), 
                     family= bernoulli(link = "logit"),data = brms_train, iter = 4000, control = list(adapt_delta = 0.95)) #gr(Respondent.ID, by = Location_clean)

summary(bayes_num)

bayes_num_int <- brm(Choice ~ NC + Price + Alcohol + Aging.Time + Type_Red + Type_White + Type_Rose +  
               Price * Gender_Male + Price * NL + Price * Germany + Price * France + Price * Spain + Price * Other +# population effects
               (1 + Price + Type_Red + Type_White + Type_Rose  + Alcohol + Aging.Time | Respondent.ID), 
               family= bernoulli(link = "logit"),data = brms_train, iter = 4000, control = list(adapt_delta = 0.95)) #gr(Respondent.ID, by = Location_clean)

summary(bayes_num_int)

# Part worth model
bayes_cat <- brm(Choice ~ NC + Price_12 + Price_18 + Price_24 +
                   Alcohol_7 + Alcohol_12 + Alcohol_18 +
                   Aging_2 + Aging_3 + Aging_4 +
                   Type_Red + Type_White + Type_Rose +   
                   (1 + Price + Type_Red + Type_White + Type_Rose  + Alcohol + Aging.Time | Respondent.ID), 
                 family= bernoulli(link = "logit"),data = brms_train, iter = 4000, control = list(adapt_delta = 0.95))

summary(bayes_cat)

bayes_cat_int <- brm(Choice ~ NC + Price_12 + Price_18 + Price_24 +
                          Alcohol_7 + Alcohol_12 + Alcohol_18 +
                          Aging_2 + Aging_3 + Aging_4 +
                          Type_Red + Type_White + Type_Rose +   
                          Price_12_France + Price_18_France + Price_24_France +
                          Price_12_Germany + Price_18_Germany + Price_24_Germany +
                          Price_12_Netherlands + Price_18_Netherlands + Price_24_Netherlands +
                          Price_12_Spain + Price_18_Spain + Price_24_Spain +
                          Price_12_Other + Price_18_Other + Price_24_Other +
                          Price_12_Male + Price_18_Male + Price_24_Male +
                          (1 + Price + Type_Red + Type_White + Type_Rose  + Alcohol + Aging.Time | Respondent.ID), 
                          family= bernoulli(link = "logit"),data = brms_train, iter = 4000, control = list(adapt_delta = 0.95))

summary(bayes_cat_int)

# Validation
pp_check(bayes_num)
pp_check(bayes_num_int)
pp_check(bayes_cat)
pp_check(bayes_cat_int)
pp_check(bayes_cat, type = "bars")  

# PP Plot for Bayes Cat Model
pp_check(
  bayes_num,
  ndraws = 10
) +
  ggplot2::labs(
    title = "PPC: Model 1",
    x = "Predicted Probability of Choice",
    y = "Density"
  ) +
  ggplot2::theme_minimal(base_size = 14)

pp_check(
  bayes_num_int,
  ndraws = 10
) +
  ggplot2::labs(
    title = "PPC: Model 2",
    x = "Predicted Probability of Choice",
    y = "Density"
  ) +
  ggplot2::theme_minimal(base_size = 14)

pp_check(
  bayes_cat,
  ndraws = 10
) +
  ggplot2::labs(
    title = "PPC: Model 3",
    x = "Predicted Probability of Choice",
    y = "Density"
  ) +
  ggplot2::theme_minimal(base_size = 14)

pp_check(
  bayes_cat_int,
  ndraws = 10
) +
  ggplot2::labs(
    title = "PPC: Model 4",
    x = "Predicted Probability of Choice",
    y = "Density"
  ) +
  ggplot2::theme_minimal(base_size = 14)

# WAIC and LOO
waic(bayes_num)
loo(bayes_num)

waic(bayes_num_int)
loo(bayes_num_int)

waic(bayes_cat)
loo(bayes_cat)

waic(bayes_cat_int)
loo(bayes_cat_int)

# Hit Rate - Bayes x
get_hit_rate_brm <- function(fit, data_long,
                             id_chid = "chid",
                             alt_col = "Alternative",
                             choice_col = "Choice") {
  
  # posterior mean *without* respondent-specific effects
  p_hat <- fitted(fit,
                  newdata    = data_long,
                  re_formula = NA,        # <- key line
                  scale      = "response",
                  summary    = TRUE)[,"Estimate"]
  
  data_long$.prob <- p_hat
  
  data_long %>%
    group_by(.data[[id_chid]]) %>%
    mutate(.pred = as.integer(.prob == max(.prob))) %>%
    ungroup() %>%
    summarise(hit = mean(.pred == .data[[choice_col]]), .groups = "drop") %>%
    pull(hit)
}


# Example usage
bayes_hit_in_m1  <- get_hit_rate_brm(bayes_num, brms_train)   # in-sample
bayes_hit_out_m1 <- get_hit_rate_brm(bayes_num, brms_test)    # hold-out (if you have a test set)

bayes_hit_in_m1
bayes_hit_out_m1

bayes_hit_in_m2  <- get_hit_rate_brm(bayes_num_int, brms_train)   # in-sample
bayes_hit_out_m2 <- get_hit_rate_brm(bayes_num_int, brms_test)    # hold-out (if you have a test set)

bayes_hit_in_m2
bayes_hit_out_m2

bayes_hit_in_m3  <- get_hit_rate_brm(bayes_cat, brms_train)   # in-sample
bayes_hit_out_m3 <- get_hit_rate_brm(bayes_cat, brms_test)    # hold-out (if you have a test set)

bayes_hit_in_m3
bayes_hit_out_m3

bayes_hit_in_m4  <- get_hit_rate_brm(bayes_cat_int, brms_train)   # in-sample
bayes_hit_out_m4 <- get_hit_rate_brm(bayes_cat_int, brms_test)    # hold-out (if you have a test set)

bayes_hit_in_m4
bayes_hit_out_m4

N_train <- n_distinct(brms_train$chid)
N_test  <- n_distinct(brms_test$chid)

se_train <- sqrt(bayes_hit_in * (1 - bayes_hit_in) / N_train)
se_test  <- sqrt(bayes_hit_out * (1 - bayes_hit_out) / N_test)

diff  <- bayes_hit_out - bayes_hit_in
se_diff <- sqrt(se_train^2 + se_test^2)      # two independent samples
z      <- diff / se_diff
p_val  <- 2 * (1 - pnorm(abs(z)))
p_val

### WTP ----
# Extract posterior samples for relevant coefficients
posterior <- as.data.frame(as_draws_df(bayes_cat_int))  # You can also use as_draws_df() from posterior package

# Use Price_12 as denominator for WTP
# (If reference is Sparkling and Price_12 is the base, use Price_12 as denominator)
# Example: WTP for White Wine = Type_White / Price_12 (per draw)

# For robust estimates, use median and credible intervals
wtp_white  <- posterior$b_Type_White / posterior$b_Price_18
wtp_red    <- posterior$b_Type_Red   / posterior$b_Price_18
wtp_rose   <- posterior$b_Type_Rose  / posterior$b_Price_18

wtp_alc7   <- posterior$b_Alcohol_7  / posterior$b_Price_18
wtp_alc12  <- posterior$b_Alcohol_12 / posterior$b_Price_18
wtp_alc18  <- posterior$b_Alcohol_18 / posterior$b_Price_18

wtp_aging2 <- posterior$b_Aging_2 / posterior$b_Price_18
wtp_aging3 <- posterior$b_Aging_3 / posterior$b_Price_18
wtp_aging4 <- posterior$b_Aging_4 / posterior$b_Price_18

# Summarize (median and 95% CI)
wtp_summary <- data.frame(
  Attribute = c("White Wine", "Red Wine", "Rosé Wine", 
                "Alcohol 7%", "Alcohol 12%", "Alcohol 18%",
                "Aging 2 Years", "Aging 3 Years", "Aging 4 Years"),
  Median_WTP = round(c(median(wtp_white), median(wtp_red), median(wtp_rose),
                       median(wtp_alc7), median(wtp_alc12), median(wtp_alc18),
                       median(wtp_aging2), median(wtp_aging3), median(wtp_aging4)), 2),
  CI_lower = round(c(quantile(wtp_white, 0.025), quantile(wtp_red, 0.025), quantile(wtp_rose, 0.025),
                     quantile(wtp_alc7, 0.025), quantile(wtp_alc12, 0.025), quantile(wtp_alc18, 0.025),
                     quantile(wtp_aging2, 0.025), quantile(wtp_aging3, 0.025), quantile(wtp_aging4, 0.025)), 2),
  CI_upper = round(c(quantile(wtp_white, 0.975), quantile(wtp_red, 0.975), quantile(wtp_rose, 0.975),
                     quantile(wtp_alc7, 0.975), quantile(wtp_alc12, 0.975), quantile(wtp_alc18, 0.975),
                     quantile(wtp_aging2, 0.975), quantile(wtp_aging3, 0.975), quantile(wtp_aging4, 0.975)), 2)
)
print(wtp_summary)

### Market Share ----
# Get posterior means for relevant coefficients
coefs <- as.numeric(fixef(bayes_cat_int)[, "Estimate"])
names(coefs) <- rownames(fixef(bayes_cat_int))

# Align the profile columns with the coefficients
used_coefs <- coefs[names(coefs) %in% colnames(profiles)]
X <- as.matrix(profiles[, names(used_coefs)])
U <- as.vector(X %*% used_coefs)

# Calculate choice probabilities (market shares)
expU <- exp(U)
market_share <- expU / sum(expU)
market_share <- round(market_share, 3)

# Add to the profiles for display
market_share_df <- cbind(profiles, Market_Share = market_share)
print(market_share_df)

# Plot
library(patchwork)
bayes_plot <- plot(bayes_cat_int, 
     variable = c("b_Price_18", 
              "b_Type_White", 
              "b_Aging_4",
              "b_Price_12_Male",
              "b_Price_24_Netherlands"))

# Combine and add title
combined_plot <- wrap_plots(bayes_plot) + 
  plot_annotation(title = "Posterior Distributions and Trace Plots for Key Predictors")

# Show it
combined_plot

# Extract posterior draws
posterior <- as.array(bayes_cat_int)

# Histogram and trace plot (faceted)
mcmc_combo(
  posterior,
  pars = c("b_muTRUE_Intercept", "b_muTRUE_Price", "b_muTRUE_Type_Red", 
           "b_muTRUE_Type_White", "b_muTRUE_Type_Rose"),
  combo = c("dens", "trace")
)

## Machine Learning ----

### Regularization ----
# Load required packages
library(glmnet)
library(dplyr)
library(caret)

# Prepare the model matrix
# Remove non-predictor columns like 'Choice', 'Respondent.ID'
train_data$Choice <- as.numeric(train_data$Choice == 1)
test_data$Choice <- as.numeric(test_data$Choice == 1)
# Num - Linear Model
# 1
x_train_num <- model.matrix(Choice ~ Price + Alcohol + Aging.Time + Type_Red + Type_White + Type_Rose  + NC
                                , data = train_data)
y_train_num <- train_data$Choice

x_test_num <- model.matrix(Choice ~ Price + Alcohol + Aging.Time + Type_Red + Type_White + Type_Rose  + NC
                               , data = test_data)
y_test_num <- test_data$Choice

# 2
x_train_num_int <- model.matrix(Choice ~ Price + Alcohol + Aging.Time + Type_Red + Type_White + Type_Rose  + NC
                            + Price_Gender_Male + Price_NL + Price_Germany + Price_France + Price_Spain + Price_Other 
                            , data = train_data)
y_train_num_int <- train_data$Choice

x_test_num_int <- model.matrix(Choice ~ Price + Alcohol + Aging.Time + Type_Red + Type_White + Type_Rose  + NC
                           + Price_Gender_Male + Price_NL + Price_Germany + Price_France + Price_Spain + Price_Other
                           , data = test_data)
y_test_num_int <- test_data$Choice

# Cat - Part worth Model
# 1
x_train_cat <- model.matrix(Choice ~ Price_12 + Price_18 + Price_24 +
                                  Type_Red + Type_White + Type_Rose + 
                                  Alcohol_7 + Alcohol_12 + Alcohol_18 +
                                  Aging_2 + Aging_3 + Aging_4 + NC,
                                data = train_data)
y_train_cat <- train_data$Choice

x_test_cat <- model.matrix(Choice ~ Price_12 + Price_18 + Price_24 +
                                 Type_Red + Type_White + Type_Rose +
                                 Alcohol_7 + Alcohol_12 + Alcohol_18 +
                                 Aging_2 + Aging_3 + Aging_4 + NC, data = test_data)
y_test_cat <- test_data$Choice

# 2
x_train_cat_int <- model.matrix(Choice ~ Price_12 + Price_18 + Price_24 +
                              Price_12_France + Price_18_France + Price_24_France +
                              Price_12_Germany + Price_18_Germany + Price_24_Germany +
                              Price_12_Netherlands + Price_18_Netherlands + Price_24_Netherlands +
                              Price_12_Spain + Price_18_Spain + Price_24_Spain +
                              Price_12_Other + Price_18_Other + Price_24_Other +
                              Price_12_Male + Price_18_Male + Price_24_Male +
                              Type_Red + Type_White + Type_Rose + 
                              Alcohol_7 + Alcohol_12 + Alcohol_18 +
                              Aging_2 + Aging_3 + Aging_4 + NC,
                              data = train_data)
y_train_cat_int <- train_data$Choice

x_test_cat_int <- model.matrix(Choice ~ Price_12 + Price_18 + Price_24 +
                             Price_12_France + Price_18_France + Price_24_France +
                             Price_12_Germany + Price_18_Germany + Price_24_Germany +
                             Price_12_Netherlands + Price_18_Netherlands + Price_24_Netherlands +
                             Price_12_Spain + Price_18_Spain + Price_24_Spain +
                             Price_12_Other + Price_18_Other + Price_24_Other +
                             Price_12_Male + Price_18_Male + Price_24_Male +
                             Type_Red + Type_White + Type_Rose +
                             Alcohol_7 + Alcohol_12 + Alcohol_18 +
                             Aging_2 + Aging_3 + Aging_4 + NC, data = test_data)
y_test_cat_int <- test_data$Choice


### LASSO Logistic Regression (alpha = 1)
# Num
# 1
cv_lasso_num <- cv.glmnet(x_train_num, y_train_num, alpha = 1, family = "binomial", nfolds = 5, type.measure = "deviance")
best_lambda_lasso_num <- cv_lasso_num$lambda.min

lasso_model_num <- glmnet(x_train_num, y_train_num, alpha = 1, lambda = best_lambda_lasso_num, family = "binomial")

# 2
cv_lasso_num_int <- cv.glmnet(x_train_num_int, y_train_num_int, alpha = 1, family = "binomial", nfolds = 5, type.measure = "deviance")
best_lambda_lasso_num_int <- cv_lasso_num_int$lambda.min

lasso_model_num_int <- glmnet(x_train_num_int, y_train_num_int, alpha = 1, lambda = best_lambda_lasso_num_int, family = "binomial")

# Cat
# 1
cv_lasso_cat <- cv.glmnet(x_train_cat, y_train_cat, alpha = 1, family = "binomial", nfolds = 5, type.measure = "deviance")
best_lambda_lasso_cat <- cv_lasso_cat$lambda.min

lasso_model_cat <- glmnet(x_train_cat, y_train_cat, alpha = 1, lambda = best_lambda_lasso_cat, family = "binomial")

# 2
cv_lasso_cat_int <- cv.glmnet(x_train_cat_int, y_train_cat_int, alpha = 1, family = "binomial", nfolds = 5, type.measure = "deviance")
best_lambda_lasso_cat_int <- cv_lasso_cat_int$lambda.min

lasso_model_cat_int <- glmnet(x_train_cat_int, y_train_cat_int, alpha = 1, lambda = best_lambda_lasso_cat_int, family = "binomial")

# Predict probabilities and reshape to identify chosen alternative
# Num
# 1
prob_lasso_num <- predict(lasso_model_num, newx = x_test_num, type = "response")
prob_lasso_num_train <- predict(lasso_model_num, newx = x_train_num, type = "response")

# 2
prob_lasso_num_int <- predict(lasso_model_num_int, newx = x_test_num_int, type = "response")
prob_lasso_num_int_train <- predict(lasso_model_num_int, newx = x_train_num_int, type = "response")

# Cat
# 1
prob_lasso_cat <- predict(lasso_model_cat, newx = x_test_cat, type = "response")
prob_lasso_cat_train <- predict(lasso_model_cat, newx = x_train_cat, type = "response")

# 2
prob_lasso_cat_int <- predict(lasso_model_cat_int, newx = x_test_cat_int, type = "response")
prob_lasso_cat_int_train <- predict(lasso_model_cat_int, newx = x_train_cat_int, type = "response")

### Ridge Logistic Regression (alpha = 0)
# Num
# 1
cv_ridge_num <- cv.glmnet(x_train_num, y_train_num, alpha = 0, family = "binomial", nfolds = 5, type.measure = "deviance")
best_lambda_ridge_num <- cv_ridge_num$lambda.min

ridge_model_num <- glmnet(x_train_num, y_train_num, alpha = 0, lambda = best_lambda_ridge_num, family = "binomial")
prob_ridge_num <- predict(ridge_model_num, newx = x_test_num, type = "response")
prob_ridge_num_train <- predict(ridge_model_num, newx = x_train_num, type = "response")

# 2
cv_ridge_num_int <- cv.glmnet(x_train_num_int, y_train_num_int, alpha = 0, family = "binomial", nfolds = 5, type.measure = "deviance")
best_lambda_ridge_num_int <- cv_ridge_num_int$lambda.min

ridge_model_num_int <- glmnet(x_train_num_int, y_train_num_int, alpha = 0, lambda = best_lambda_ridge_num_int, family = "binomial")
prob_ridge_num_int <- predict(ridge_model_num_int, newx = x_test_num_int, type = "response")
prob_ridge_num_int_train <- predict(ridge_model_num_int, newx = x_train_num_int, type = "response")

# Cat
# 1
cv_ridge_cat <- cv.glmnet(x_train_cat, y_train_cat, alpha = 0, family = "binomial", nfolds = 5, type.measure = "deviance")
best_lambda_ridge_cat <- cv_ridge_cat$lambda.min

ridge_model_cat <- glmnet(x_train_cat, y_train_cat, alpha = 0, lambda = best_lambda_ridge_cat, family = "binomial")
prob_ridge_cat <- predict(ridge_model_cat, newx = x_test_cat, type = "response")
prob_ridge_cat_train <- predict(ridge_model_cat, newx = x_train_cat, type = "response")

# 2
cv_ridge_cat_int <- cv.glmnet(x_train_cat_int, y_train_cat_int, alpha = 0, family = "binomial", nfolds = 5, type.measure = "deviance")
best_lambda_ridge_cat_int <- cv_ridge_cat_int$lambda.min

ridge_model_cat_int <- glmnet(x_train_cat_int, y_train_cat_int, alpha = 0, lambda = best_lambda_ridge_cat_int, family = "binomial")
prob_ridge_cat_int <- predict(ridge_model_cat_int, newx = x_test_cat_int, type = "response")
prob_ridge_cat_int_train <- predict(ridge_model_cat_int, newx = x_train_cat_int, type = "response")

### Elastic-Net Logistic Regression (alpha = 0)
# Num
# 1
cv_elnet_num <- cv.glmnet(x_train_num, y_train_num, alpha = 0.5, family = "binomial", nfolds = 5, type.measure = "deviance")
best_lambda_elnet_num <- cv_elnet_num$lambda.min

elnet_model_num <- glmnet(x_train_num, y_train_num, alpha = 0.5, lambda = best_lambda_elnet_num, family = "binomial")
prob_elnet_num <- predict(elnet_model_num, newx = x_test_num, type = "response")
prob_elnet_num_train <- predict(elnet_model_num, newx = x_train_num, type = "response")

# 2
cv_elnet_num_int <- cv.glmnet(x_train_num_int, y_train_num_int, alpha = 0.5, family = "binomial", nfolds = 5, type.measure = "deviance")
best_lambda_elnet_num_int <- cv_elnet_num_int$lambda.min

elnet_model_num_int <- glmnet(x_train_num_int, y_train_num_int, alpha = 0.5, lambda = best_lambda_elnet_num_int, family = "binomial")
prob_elnet_num_int <- predict(elnet_model_num_int, newx = x_test_num_int, type = "response")
prob_elnet_num_int_train <- predict(elnet_model_num_int, newx = x_train_num_int, type = "response")

# Cat
# 1
cv_elnet_cat <- cv.glmnet(x_train_cat, y_train_cat, alpha = 0.5, family = "binomial", nfolds = 5, type.measure = "deviance")
best_lambda_elnet_cat <- cv_elnet_cat$lambda.min

elnet_model_cat <- glmnet(x_train_cat, y_train_cat, alpha = 0.5, lambda = best_lambda_elnet_cat, family = "binomial")
prob_elnet_cat <- predict(elnet_model_cat, newx = x_test_cat, type = "response")
prob_elnet_cat_train <- predict(elnet_model_cat, newx = x_train_cat, type = "response")

# 2
cv_elnet_cat_int <- cv.glmnet(x_train_cat_int, y_train_cat_int, alpha = 0.5, family = "binomial", nfolds = 5, type.measure = "deviance")
best_lambda_elnet_cat_int <- cv_elnet_cat_int$lambda.min

elnet_model_cat_int <- glmnet(x_train_cat_int, y_train_cat_int, alpha = 0.5, lambda = best_lambda_elnet_cat_int, family = "binomial")
prob_elnet_cat_int <- predict(elnet_model_cat_int, newx = x_test_cat_int, type = "response")
prob_elnet_cat_int_train <- predict(elnet_model_cat_int, newx = x_train_cat_int, type = "response")

# Function to compute Hit Rate from predicted probabilities
get_hit_rate_from_probs <- function(prob_vector, true_choice, task_size = 4) {
  hit <- 0
  total_tasks <- length(prob_vector) / task_size
  
  for (i in 0:(total_tasks - 1)) {
    start_idx <- i * task_size + 1
    end_idx <- start_idx + task_size - 1
    
    group_probs <- prob_vector[start_idx:end_idx]
    group_true <- true_choice[start_idx:end_idx]
    
    pred_idx <- which.max(group_probs)
    true_idx <- which.max(group_true)
    
    if (pred_idx == true_idx) {
      hit <- hit + 1
    }
  }
  return(hit / total_tasks)
}

### Compute hit rates
##### In Sample ----
# Num
# 1
hit_lasso_num_train     <- get_hit_rate_from_probs(prob_lasso_num_train, y_train_num, task_size = 4)
hit_ridge_num_train     <- get_hit_rate_from_probs(prob_ridge_num_train, y_train_num, task_size = 4)
hit_elnet_num_train     <- get_hit_rate_from_probs(prob_elnet_num_train, y_train_num, task_size = 4)

# 2
hit_lasso_num_int_train <- get_hit_rate_from_probs(prob_lasso_num_int_train, y_train_num_int, task_size = 4)
hit_ridge_num_int_train <- get_hit_rate_from_probs(prob_ridge_num_int_train, y_train_num_int, task_size = 4)
hit_elnet_num_int_train <- get_hit_rate_from_probs(prob_elnet_num_int_train, y_train_num_int, task_size = 4)

# Cat
# 1
hit_lasso_cat_train     <- get_hit_rate_from_probs(prob_lasso_cat_train, y_train_cat, task_size = 4)
hit_ridge_cat_train     <- get_hit_rate_from_probs(prob_ridge_cat_train, y_train_cat, task_size = 4)
hit_elnet_cat_train     <- get_hit_rate_from_probs(prob_elnet_cat_train, y_train_cat, task_size = 4)

# 2
hit_lasso_cat_int_train <- get_hit_rate_from_probs(prob_lasso_cat_int_train, y_train_cat_int, task_size = 4)
hit_ridge_cat_int_train <- get_hit_rate_from_probs(prob_ridge_cat_int_train, y_train_cat_int, task_size = 4)
hit_elnet_cat_int_train <- get_hit_rate_from_probs(prob_elnet_cat_int_train, y_train_cat_int, task_size = 4)

# Print Result
cat("Hit Rate (Train) - LASSO - Model 1 (Num):", round(hit_lasso_num_train * 100, 2), "%\n")
cat("Hit Rate (Train) - Ridge - Model 1 (Num):", round(hit_ridge_num_train * 100, 2), "%\n")
cat("Hit Rate (Train) - El Net - Model 1 (Num):", round(hit_elnet_num_train * 100, 2), "%\n")

cat("Hit Rate (Train) - LASSO - Model 2 (Num + Int):", round(hit_lasso_num_int_train * 100, 2), "%\n")
cat("Hit Rate (Train) - Ridge - Model 2 (Num + Int):", round(hit_ridge_num_int_train * 100, 2), "%\n")
cat("Hit Rate (Train) - El Net - Model 2 (Num + Int):", round(hit_elnet_num_int_train * 100, 2), "%\n")

cat("Hit Rate (Train) - LASSO - Model 3 (Cat):", round(hit_lasso_cat_train * 100, 2), "%\n")
cat("Hit Rate (Train) - Ridge - Model 3 (Cat):", round(hit_ridge_cat_train * 100, 2), "%\n")
cat("Hit Rate (Train) - El Net - Model 3 (Cat):", round(hit_elnet_cat_train * 100, 2), "%\n")

cat("Hit Rate (Train) - LASSO - Model 4 (Cat + Int):", round(hit_lasso_cat_int_train * 100, 2), "%\n")
cat("Hit Rate (Train) - Ridge - Model 4 (Cat + Int):", round(hit_ridge_cat_int_train * 100, 2), "%\n")
cat("Hit Rate (Train) - El Net - Model 4 (Cat + Int):", round(hit_elnet_cat_int_train * 100, 2), "%\n")

##### Out Sample ----
# Num
# 1
hit_lasso_num <- get_hit_rate_from_probs(prob_lasso_num, y_test_num, task_size = 4)
hit_ridge_num <- get_hit_rate_from_probs(prob_ridge_num, y_test_num, task_size = 4)
hit_elnet_num <- get_hit_rate_from_probs(prob_elnet_num, y_test_num, task_size = 4)

# 2
hit_lasso_num_int <- get_hit_rate_from_probs(prob_lasso_num_int, y_test_num_int, task_size = 4)
hit_ridge_num_int <- get_hit_rate_from_probs(prob_ridge_num_int, y_test_num_int, task_size = 4)
hit_elnet_num_int <- get_hit_rate_from_probs(prob_elnet_num_int, y_test_num_int, task_size = 4)

# Cat
# 1
hit_lasso_cat <- get_hit_rate_from_probs(prob_lasso_cat, y_test_cat, task_size = 4)
hit_ridge_cat <- get_hit_rate_from_probs(prob_ridge_cat, y_test_cat, task_size = 4)
hit_elnet_cat <- get_hit_rate_from_probs(prob_elnet_cat, y_test_cat, task_size = 4)

# 2
hit_lasso_cat_int <- get_hit_rate_from_probs(prob_lasso_cat_int, y_test_cat_int, task_size = 4)
hit_ridge_cat_int <- get_hit_rate_from_probs(prob_ridge_cat_int, y_test_cat_int, task_size = 4)
hit_elnet_cat_int <- get_hit_rate_from_probs(prob_elnet_cat_int, y_test_cat_int, task_size = 4)

### Print results
# Num
# 1
cat("Hit Rate (LASSO) - Num:", round(hit_lasso_num * 100, 2), "%\n")
cat("Hit Rate (Ridge) - Num:", round(hit_ridge_num * 100, 2), "%\n")
cat("Hit Rate (ElNet) - Num:", round(hit_elnet_num * 100, 2), "%\n")

head(predict(lasso_model_num, s = best_lambda_lasso_num, newx = x_test_num, type = "response"))
head(predict(ridge_model_num, s = best_lambda_ridge_num, newx = x_test_num, type = "response"))
head(predict(elnet_model_num, s = best_lambda_elnet_num, newx = x_test_num, type = "response"))

# 2
cat("Hit Rate (LASSO) - Num + Int:", round(hit_lasso_num_int * 100, 2), "%\n")
cat("Hit Rate (Ridge) - Num + Int:", round(hit_ridge_num_int * 100, 2), "%\n")
cat("Hit Rate (ElNet) - Num + Int:", round(hit_elnet_num_int * 100, 2), "%\n")

head(predict(lasso_model_num_int, s = best_lambda_lasso_num_int, newx = x_test_num_int, type = "response"))
head(predict(ridge_model_num_int, s = best_lambda_ridge_num_int, newx = x_test_num_int, type = "response"))
head(predict(elnet_model_num_int, s = best_lambda_elnet_num_int, newx = x_test_num_int, type = "response"))

# Cat
# 1
cat("Hit Rate (LASSO) - Cat:", round(hit_lasso_cat * 100, 2), "%\n")
cat("Hit Rate (Ridge) - Cat:", round(hit_ridge_cat * 100, 2), "%\n")
cat("Hit Rate (ElNet) - Cat:", round(hit_elnet_cat * 100, 2), "%\n")

head(predict(lasso_model_cat, s = best_lambda_lasso_cat, newx = x_test_cat, type = "response"))
head(predict(ridge_model_cat, s = best_lambda_ridge_cat, newx = x_test_cat, type = "response"))
head(predict(elnet_model_cat, s = best_lambda_elnet_cat, newx = x_test_cat, type = "response"))

# 2
cat("Hit Rate (LASSO) - Cat + Int:", round(hit_lasso_cat_int * 100, 2), "%\n")
cat("Hit Rate (Ridge) - Cat + Int:", round(hit_ridge_cat_int * 100, 2), "%\n")
cat("Hit Rate (ElNet) - Cat + Int:", round(hit_elnet_cat_int * 100, 2), "%\n")

head(predict(lasso_model_cat_int, s = best_lambda_lasso_cat_int, newx = x_test_cat_int, type = "response"))
head(predict(ridge_model_cat_int, s = best_lambda_ridge_cat_int, newx = x_test_cat_int, type = "response"))
head(predict(elnet_model_cat_int, s = best_lambda_elnet_cat_int, newx = x_test_cat_int, type = "response"))

# Non-zero Coeff
nonzero_coef <- function(glmnet_fit, lambda_value) {
  # coef() returns a sparse matrix for binomial models
  beta <- as.matrix(coef(glmnet_fit, s = lambda_value))
  
  keep <- rownames(beta)[beta[, 1] != 0]          # non-zero rows
  setdiff(keep, "(Intercept)")                    # drop intercept if present
}

# Num
# 1
nz_lasso <- nonzero_coef(lasso_model_num,  best_lambda_lasso_num)
nz_ridge <- nonzero_coef(ridge_model_num,  best_lambda_ridge_num)
nz_elnet <- nonzero_coef(elnet_model_num,  best_lambda_elnet_num)

cat("LASSO keeps",  length(nz_lasso), "predictors:\n",  paste(nz_lasso,  collapse = ", "), "\n\n")
cat("Ridge keeps",  length(nz_ridge), "predictors  (ridge rarely zeroes out cols)\n\n")
cat("Elastic-net keeps", length(nz_elnet), "predictors:\n", paste(nz_elnet, collapse = ", "), "\n")

# 2
nz_lasso_2 <- nonzero_coef(lasso_model_num_int,  best_lambda_lasso_num_int)
nz_ridge_2 <- nonzero_coef(ridge_model_num_int,  best_lambda_ridge_num_int)
nz_elnet_2 <- nonzero_coef(elnet_model_num_int,  best_lambda_elnet_num_int)

cat("LASSO keeps",  length(nz_lasso_2), "predictors:\n",  paste(nz_lasso_2,  collapse = ", "), "\n\n")
cat("Ridge keeps",  length(nz_ridge_2), "predictors  (ridge rarely zeroes out cols)\n\n")
cat("Elastic-net keeps", length(nz_elnet_2), "predictors:\n", paste(nz_elnet_2, collapse = ", "), "\n")


# Cat
# 1
nz_lasso_cat <- nonzero_coef(lasso_model_cat,  best_lambda_lasso_cat)
nz_ridge_cat <- nonzero_coef(ridge_model_cat,  best_lambda_ridge_cat)
nz_elnet_cat <- nonzero_coef(elnet_model_cat,  best_lambda_elnet_cat)

cat("LASSO cat keeps",  length(nz_lasso_cat), "predictors:\n",  paste(nz_lasso,  collapse = ", "), "\n\n")
cat("Ridge cat keeps",  length(nz_ridge_cat), "predictors  (ridge rarely zeroes out cols)\n\n")
cat("Elastic-net cat keeps", length(nz_elnet_cat), "predictors:\n", paste(nz_elnet, collapse = ", "), "\n")

# 2
nz_lasso_cat_2 <- nonzero_coef(lasso_model_cat_int,  best_lambda_lasso_cat_int)
nz_ridge_cat_2 <- nonzero_coef(ridge_model_cat_int,  best_lambda_ridge_cat_int)
nz_elnet_cat_2 <- nonzero_coef(elnet_model_cat_int,  best_lambda_elnet_cat_int)

cat("LASSO cat keeps",  length(nz_lasso_cat_2), "predictors:\n",  paste(nz_lasso_cat_2,  collapse = ", "), "\n\n")
cat("Ridge cat keeps",  length(nz_ridge_cat_2), "predictors  (ridge rarely zeroes out cols)\n\n")
cat("Elastic-net cat keeps", length(nz_elnet_cat_2), "predictors:\n", paste(nz_elnet_cat_2, collapse = ", "), "\n")

### Random Forest ----
library(randomForest)

# Categorical
# 1
x_train_rf_cat <- train_data %>%
  select(Price_12, Price_18, Price_24, Price_6,
         Type_Red , Type_White , Type_Rose, Type_Sparkling,
         Alcohol_7 , Alcohol_12 , Alcohol_18 , Alcohol_5.5,
         Aging_2 , Aging_3 , Aging_4, Aging_1, NC)  # predictors only
y_train_rf_cat <- as.factor(train_data$Choice)

x_test_rf_cat <- test_data %>%
  select(Price_12, Price_18, Price_24, Price_6,
         Type_Red , Type_White , Type_Rose, Type_Sparkling,
         Alcohol_7 , Alcohol_12 , Alcohol_18 , Alcohol_5.5,
         Aging_2 , Aging_3 , Aging_4, Aging_1, NC)
y_test_rf_cat <- test_data$Choice


# 2
x_train_rf_cat_int <- train_data %>%
  select(Price_12, Price_18, Price_24, Price_6,
         #Price_12_France , Price_18_France , Price_24_France , Price_6_France,
         #Price_12_Germany , Price_18_Germany , Price_24_Germany , Price_6_Germany,
         #Price_12_Netherlands , Price_18_Netherlands , Price_24_Netherlands , Price_6_Netherlands,
         #Price_12_Spain , Price_18_Spain , Price_24_Spain , Price_6_Spain,
         #Price_12_Other , Price_18_Other , Price_24_Other , Price_6_Other,
         #Price_12_ID , Price_18_ID , Price_24_ID , Price_6_ID,
         #Price_12_Male , Price_18_Male, Price_24_Male, Price_6_Male,
         #Price_12_Female , Price_18_Female, Price_24_Female, Price_6_Female,
         France, Germany, NL, Spain, Other, ID, Gender_Male, Gender_Female,
         Type_Red , Type_White , Type_Rose, Type_Sparkling,
         Alcohol_7 , Alcohol_12 , Alcohol_18 , Alcohol_5.5,
         Aging_2 , Aging_3 , Aging_4, Aging_1, NC)  # predictors only
y_train_rf_cat_int <- as.factor(train_data$Choice)

x_test_rf_cat_int <- test_data %>%
  select(Price_12, Price_18, Price_24, Price_6,
         #Price_12_France , Price_18_France , Price_24_France , Price_6_France,
         #Price_12_Germany , Price_18_Germany , Price_24_Germany , Price_6_Germany,
         #Price_12_Netherlands , Price_18_Netherlands , Price_24_Netherlands , Price_6_Netherlands,
         #Price_12_Spain , Price_18_Spain , Price_24_Spain , Price_6_Spain,
         #Price_12_Other , Price_18_Other , Price_24_Other , Price_6_Other,
         #Price_12_ID , Price_18_ID , Price_24_ID , Price_6_ID,
         #Price_12_Male , Price_18_Male, Price_24_Male, Price_6_Male,
         #Price_12_Female , Price_18_Female, Price_24_Female, Price_6_Female,
         France, Germany, NL, Spain, Other, ID, Gender_Male, Gender_Female,
         Type_Red , Type_White , Type_Rose, Type_Sparkling,
         Alcohol_7 , Alcohol_12 , Alcohol_18 , Alcohol_5.5,
         Aging_2 , Aging_3 , Aging_4, Aging_1, NC)
y_test_rf_cat_int <- test_data$Choice

# Numerical
# 1
x_train_rf_num <- train_data %>%
  select(Price, Alcohol, Aging.Time, Type, #Type_Red, Type_Rose, Type_White, Type_Sparkling,
         #Price_Gender_Male, Price_Gender_Female, Price_NL, Price_Germany, Price_France, Price_Spain, Price_ID, Price_Other,
         #Gender, Location_clean, 
         NC) 
y_train_rf_num <- as.factor(train_data$Choice)

x_test_rf_num <- test_data %>%
  select(Price, Alcohol, Aging.Time, Type, #Type_Red, Type_Rose, Type_White, Type_Sparkling,
         #Price_Gender_Male, Price_Gender_Female, Price_NL, Price_Germany, Price_France, Price_Spain, Price_ID, Price_Other,
         #Gender, Location_clean, 
         NC) 
y_test_rf_num <- test_data$Choice

# 2
x_train_rf_num_int <- train_data %>%
  select(Price, Alcohol, Aging.Time, Type, #Type_Red, Type_Rose, Type_White, Type_Sparkling,
         #Price_Gender_Male, Price_Gender_Female, Price_NL, Price_Germany, Price_France, Price_Spain, Price_ID, Price_Other,
         Gender, Location_clean, NC) 
y_train_rf_num_int <- as.factor(train_data$Choice)

x_test_rf_num_int <- test_data %>%
  select(Price, Alcohol, Aging.Time, Type, #Type_Red, Type_Rose, Type_White, Type_Sparkling,
         #Price_Gender_Male, Price_Gender_Female, Price_NL, Price_Germany, Price_France, Price_Spain, Price_ID, Price_Other,
         Gender, Location_clean, NC) 
y_test_rf_num_int <- test_data$Choice

# Model
set.seed(123)  # for reproducibility

rf_model_cat <- randomForest(
  x = x_train_rf_cat,
  y = y_train_rf_cat,
  ntree = 500,
  mtry = floor(sqrt(ncol(x_train_rf_cat))),  # typical heuristic
  importance = TRUE)

rf_model_cat_int <- randomForest(
  x = x_train_rf_cat_int,
  y = y_train_rf_cat_int,
  ntree = 500,
  mtry = floor(sqrt(ncol(x_train_rf_cat_int))),  # typical heuristic
  importance = TRUE)

rf_model_num <- randomForest(
  x = x_train_rf_num,
  y = y_train_rf_num,
  ntree = 500,
  mtry = floor(sqrt(ncol(x_train_rf_num))),  # typical heuristic
  importance = TRUE)

rf_model_num_int <- randomForest(
  x = x_train_rf_num_int,
  y = y_train_rf_num_int,
  ntree = 500,
  mtry = floor(sqrt(ncol(x_train_rf_num_int))),  # typical heuristic
  importance = TRUE)

# Hit Rate Function
get_hit_rate_from_probs_rf <- function(prob_vector, true_choice, task_size = 4) {
  hit <- 0
  total_tasks <- length(prob_vector) / task_size
  
  for (i in 0:(total_tasks - 1)) {
    start_idx <- i * task_size + 1
    end_idx <- start_idx + task_size - 1
    
    group_probs <- prob_vector[start_idx:end_idx]
    group_true <- true_choice[start_idx:end_idx]
    
    pred_idx <- which.max(group_probs)
    true_idx <- which.max(group_true)
    
    if (pred_idx == true_idx) {
      hit <- hit + 1
    }
  }
  return(hit / total_tasks)
}

# Hit Rate - Cat
# 1
# In
rf_probs_cat_in <- predict(rf_model_cat, x_train_rf_cat, type = "prob")[, 2]  # assuming class "1" is choice
hit_rf_cat_in <- get_hit_rate_from_probs_rf(rf_probs_cat_in, y_train_rf_cat)
cat("Hit Rate (Random Forest) - Cat:", round(hit_rf_cat_in * 100, 2), "%\n")

# Out
rf_probs_cat <- predict(rf_model_cat, x_test_rf_cat, type = "prob")[, 2]  # assuming class "1" is choice
hit_rf_cat <- get_hit_rate_from_probs_rf(rf_probs_cat, y_test_rf_cat)
cat("Hit Rate (Random Forest) - Cat:", round(hit_rf_cat * 100, 2), "%\n")

# 2
# In
rf_probs_cat_int_in <- predict(rf_model_cat_int, x_train_rf_cat_int, type = "prob")[, 2]  # assuming class "1" is choice
hit_rf_cat_int_in <- get_hit_rate_from_probs_rf(rf_probs_cat_int_in, y_train_rf_cat_int)
cat("Hit Rate (Random Forest) - Cat:", round(hit_rf_cat_int_in * 100, 2), "%\n")

# Out
rf_probs_cat_int <- predict(rf_model_cat_int, x_test_rf_cat_int, type = "prob")[, 2]  # assuming class "1" is choice
hit_rf_cat_int <- get_hit_rate_from_probs_rf(rf_probs_cat_int, y_test_rf_cat_int)
cat("Hit Rate (Random Forest) - Cat:", round(hit_rf_cat_int * 100, 2), "%\n")


# Hit Rate - Num
# 1
# In
rf_probs_num_in <- predict(rf_model_num, x_train_rf_num, type = "prob")[, 2]  # assuming class "1" is choice
hit_rf_num_in <- get_hit_rate_from_probs_rf(rf_probs_num_in, y_train_rf_num)
cat("Hit Rate (Random Forest) - Num:", round(hit_rf_num_in * 100, 2), "%\n")

# Out
rf_probs_num <- predict(rf_model_num, x_test_rf_num, type = "prob")[, 2]  # assuming class "1" is choice
hit_rf_num <- get_hit_rate_from_probs_rf(rf_probs_num, y_test_rf_num)
cat("Hit Rate (Random Forest) - Num:", round(hit_rf_num * 100, 2), "%\n")

# 2
# In
rf_probs_num_int_in <- predict(rf_model_num_int, x_train_rf_num_int, type = "prob")[, 2]  # assuming class "1" is choice
hit_rf_num_int_in <- get_hit_rate_from_probs_rf(rf_probs_num_int_in, y_train_rf_num_int)
cat("Hit Rate (Random Forest) - Num:", round(hit_rf_num_int_in * 100, 2), "%\n")

# Out
rf_probs_num_int <- predict(rf_model_num_int, x_test_rf_num_int, type = "prob")[, 2]  # assuming class "1" is choice
hit_rf_num_int <- get_hit_rate_from_probs_rf(rf_probs_num_int, y_test_rf_num_int)
cat("Hit Rate (Random Forest) - Num:", round(hit_rf_num_int * 100, 2), "%\n")

# OOB
# Function to compute task-level OOB hit rate
get_hit_rate_from_oob <- function(rf_model, true_choices, task_size = 4) {
  oob_probs <- rf_model$votes[, 2]  # assumes class "1" (Choice = TRUE or 1)
  hit <- 0
  total_tasks <- length(oob_probs) / task_size
  
  for (i in 0:(total_tasks - 1)) {
    start_idx <- i * task_size + 1
    end_idx <- start_idx + task_size - 1
    
    group_probs <- oob_probs[start_idx:end_idx]
    group_true <- true_choices[start_idx:end_idx]
    
    pred_idx <- which.max(group_probs)
    true_idx <- which.max(group_true)
    
    if (pred_idx == true_idx) {
      hit <- hit + 1
    }
  }
  return(hit / total_tasks)
}

# Converts factor to numeric (0/1), assuming "1" is the level for choice
y_train_rf_cat_bin <- as.numeric(y_train_rf_cat == "1")
y_train_rf_cat_int_bin <- as.numeric(y_train_rf_cat_int == "1")
y_train_rf_num_bin <- as.numeric(y_train_rf_num == "1")
y_train_rf_num_int_bin <- as.numeric(y_train_rf_num_int == "1")

# Compute task-level OOB hit rate
hit_oob_rf_cat <- get_hit_rate_from_oob(rf_model_cat, y_train_rf_cat_bin)
hit_oob_rf_cat_int <- get_hit_rate_from_oob(rf_model_cat_int, y_train_rf_cat_int_bin)
hit_oob_rf_num <- get_hit_rate_from_oob(rf_model_num, y_train_rf_num_bin)
hit_oob_rf_num_int <- get_hit_rate_from_oob(rf_model_num_int, y_train_rf_num_int_bin)

# Print results
cat("OOB Hit Rate (Task-Level) - Model 1 (Num):", round(hit_oob_rf_num * 100, 2), "%\n")
cat("OOB Hit Rate (Task-Level) - Model 2 (Num + Int):", round(hit_oob_rf_num_int * 100, 2), "%\n")
cat("OOB Hit Rate (Task-Level) - Model 3 (Cat):", round(hit_oob_rf_cat * 100, 2), "%\n")
cat("OOB Hit Rate (Task-Level) - Model 4 (Cat + Int):", round(hit_oob_rf_cat_int * 100, 2), "%\n")


# Plot
varImpPlot(rf_model_cat_int, main = "Variable Importance (Random Forest)")
varImpPlot(rf_model_num_int, main = "Variable Importance (Random Forest)")

#### SHAP ----
library(fastshap)

# Wrapper Function
predict_rf_prob <- function(model, newdata) {
  preds <- predict(model, newdata = newdata, type = "prob")
  return(as.numeric(preds[, "1"]))  # assuming "1" is the positive class
}

# Compute SHAP values
# Compute SHAP values using fastshap
set.seed(123)
shap_rf_num_int <- explain(
  object = rf_model_num_int,
  X = x_train_rf_num_int,
  pred_wrapper = predict_rf_prob,
  nsim = 100  # More simulations = better approximation, but slower
)

# Mean absolute SHAP values
shap_means_num_int <- apply(abs(shap_rf_num_int), 2, mean)
shap_df_num_int <- data.frame(Feature = names(shap_means_num_int), SHAP = shap_means_num_int)

# Plot
ggplot(shap_df_num_int, aes(x = reorder(Feature, SHAP), y = SHAP)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "SHAP Feature Importance (Random Forest)",
    x = "Feature",
    y = "Mean |SHAP Value|"
  ) +
  theme_minimal()

library(dplyr)

shap_df_num_int <- shap_df_num_int %>%
  mutate(Feature = ifelse(Feature == "Location_clean", "Location", Feature))

ggplot(shap_df_num_int, aes(x = reorder(Feature, SHAP), y = SHAP)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "SHAP Feature Importance (Random Forest)",
    x = "Feature",
    y = "Mean |SHAP Value|"
  ) +
  theme_minimal()

# For example, explain the 5th observation in your training set
shap_local_num_5 <- explain(
  object = rf_model_num_int,
  X = x_train_rf_num_int,
  pred_wrapper = predict_rf_prob,
  nsim = 100,
  newdata = x_train_rf_num_int[5, , drop = FALSE]
)

print(shap_local_num_5)

# Effect of Price on Predictions
ggplot(data.frame(Price = x_train_rf_num_int$Price, SHAP = shap_rf_num_int[, "Price"]),
       aes(x = Price, y = SHAP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "SHAP Dependence: Price", x = "Price", y = "SHAP Value") +
  theme_minimal()

# Force Plot
# Resp 5
shap_5_num <- shap_rf_num_int[5, ]
shap_long_num_int_5 <- data.frame(Feature = names(shap_5_num), SHAP = as.numeric(shap_5_num)) %>%
  mutate(Feature = ifelse(Feature == "Location_clean", "Location", Feature)) %>%
  arrange(desc(abs(SHAP)))

ggplot(shap_long_num_int_5, aes(x = reorder(Feature, SHAP), y = SHAP, fill = SHAP > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "SHAP Force Plot (Resp 5)",
       y = "SHAP Value", x = "Feature") +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"))

# Resp 25
shap_23_num <- shap_rf_num_int[23, ]
shap_long_num_int_23 <- data.frame(Feature = names(shap_23_num), SHAP = as.numeric(shap_23_num)) %>%
  mutate(Feature = ifelse(Feature == "Location_clean", "Location", Feature)) %>%
  arrange(desc(abs(SHAP)))

ggplot(shap_long_num_int_23, aes(x = reorder(Feature, SHAP), y = SHAP, fill = SHAP > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "SHAP Force Plot (Resp 25)",
       y = "SHAP Value", x = "Feature") +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"))

# SHAP Force Plot ver 1.1
# Filter Task 1 of Respondent 5
task_data <- train_data %>% filter(Respondent.ID == 5) %>% slice(1:4)  # assumes 4 alts per task

# Find the row number in the full SHAP matrix that corresponds to the chosen alternative
chosen_row_index <- which(train_data$Respondent.ID == 5)[which(task_data$Choice == 1)]

# Now extract SHAP values for that row
shap_5_chosen <- shap_rf_num_int[chosen_row_index, ]
shap_long_5_chosen <- data.frame(Feature = names(shap_5_chosen), SHAP = as.numeric(shap_5_chosen)) %>%
  arrange(desc(abs(SHAP)))

# Plot
ggplot(shap_long_5_chosen, aes(x = reorder(Feature, SHAP), y = SHAP, fill = SHAP > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "Force Plot: Respondent 5's Chosen Alternative (Task 1)",
       y = "SHAP Value", x = "Feature") +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"))

# Filter Task 1 of Respondent 25
task_data <- train_data %>% filter(Respondent.ID == 25) %>% slice(1:4)  # assumes 4 alts per task

# Find the row number in the full SHAP matrix that corresponds to the chosen alternative
chosen_row_index <- which(train_data$Respondent.ID == 25)[which(task_data$Choice == 1)]

# Now extract SHAP values for that row
shap_25_chosen <- shap_rf_num_int[chosen_row_index, ]
shap_long_25_chosen <- data.frame(Feature = names(shap_25_chosen), SHAP = as.numeric(shap_25_chosen)) %>%
  arrange(desc(abs(SHAP)))

# Plot
ggplot(shap_long_25_chosen, aes(x = reorder(Feature, SHAP), y = SHAP, fill = SHAP > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "Force Plot: Respondent 25's Chosen Alternative (Task 1)",
       y = "SHAP Value", x = "Feature") +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"))

# SHAP Force Plot ver 2
# Number of tasks per respondent
tasks_per_respondent <- 7
alts_per_task <- 4
rows_per_respondent <- tasks_per_respondent * alts_per_task

# Function to get row index for a specific respondent and task
get_row_index <- function(resp_id, task_id = 1, alt_id = 1, tasks_per_respondent = 7, alts_per_task = 4) {
  start_idx <- (resp_id - 1) * tasks_per_respondent * alts_per_task
  row_index <- start_idx + ((task_id - 1) * alts_per_task) + alt_id
  return(row_index)
}

# Get SHAP row for Respondent 5, Task 1, Alt 1
row_5 <- get_row_index(resp_id = 5, task_id = 1, alt_id = 1)
shap_5_num <- shap_rf_num_int[row_5, ]

# Get SHAP row for Respondent 25, Task 1, Alt 1
row_25 <- get_row_index(resp_id = 25, task_id = 1, alt_id = 1)
shap_25_num <- shap_rf_num_int[row_25, ]

shap_long_num_5 <- data.frame(Feature = names(shap_5_num), SHAP = as.numeric(shap_5_num)) %>%
  arrange(desc(abs(SHAP)))

shap_long_num_25 <- data.frame(Feature = names(shap_25_num), SHAP = as.numeric(shap_25_num)) %>%
  arrange(desc(abs(SHAP)))

# plot
ggplot(shap_long_num_5, aes(x = reorder(Feature, SHAP), y = SHAP, fill = SHAP > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "SHAP Force Plot (Respondent 5, Task 1, Alt 1)",
       y = "SHAP Value", x = "Feature") +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"))

ggplot(shap_long_num_25, aes(x = reorder(Feature, SHAP), y = SHAP, fill = SHAP > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "SHAP Force Plot (Respondent 25, Task 1, Alt 1)",
       y = "SHAP Value", x = "Feature") +
  theme_minimal() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"))

# Function to get row indices for a respondent's task
get_task_rows <- function(resp_id, task_id = 1, tasks_per_respondent = 7, alts_per_task = 4) {
  start_index <- (resp_id - 1) * tasks_per_respondent * alts_per_task
  task_start <- start_index + (task_id - 1) * alts_per_task + 1
  return(task_start:(task_start + alts_per_task - 1))
}

# Get rows for Respondent 5, Task 1
rows_resp5_task1 <- get_task_rows(resp_id = 5, task_id = 1)

# Create SHAP plots for all 4 alternatives
plots <- lapply(1:4, function(i) {
  shap_row <- shap_rf_num_int[rows_resp5_task1[i], ]
  shap_long <- data.frame(
    Feature = names(shap_row),
    SHAP = as.numeric(shap_row),
    Alt = paste0("Alternative ", i)
  ) %>% arrange(desc(abs(SHAP)))
  
  ggplot(shap_long, aes(x = reorder(Feature, SHAP), y = SHAP, fill = SHAP > 0)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
    labs(title = paste("SHAP Plot – Respondent 5, Task 1,", shap_long$Alt[1]),
         y = "SHAP Value", x = "Feature") +
    theme_minimal()
})

# Display all plots (patchwork for side-by-side if installed)
library(patchwork)
wrap_plots(plots, ncol = 2)

# Get rows for Respondent 5, Task 1
rows_resp25_task1 <- get_task_rows(resp_id = 25, task_id = 1)

# Create SHAP plots for all 4 alternatives
plots <- lapply(1:4, function(i) {
  shap_row <- shap_rf_num_int[rows_resp25_task1[i], ]
  shap_long <- data.frame(
    Feature = names(shap_row),
    SHAP = as.numeric(shap_row),
    Alt = paste0("Alternative ", i)
  ) %>% arrange(desc(abs(SHAP)))
  
  ggplot(shap_long, aes(x = reorder(Feature, SHAP), y = SHAP, fill = SHAP > 0)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
    labs(title = paste("SHAP Plot – Respondent 25, Task 1,", shap_long$Alt[1]),
         y = "SHAP Value", x = "Feature") +
    theme_minimal()
})

# Display all plots (patchwork for side-by-side if installed)
library(patchwork)
wrap_plots(plots, ncol = 2)

# Class Probabilities
rf_probs_num <- predict(rf_model_num, x_test_rf_num, type = "prob")
hist(rf_probs_num[, 2], main = "Predicted Probabilities for Choice", xlab = "Probability", col = "lightblue")

## Split by Occasion ----
table(wine_unique_elig$Occasion)

train_data_casual <- train_data %>%
  filter(Occasion == "Casual occasion (for example: birthday, hangout)")

train_data_food <- train_data %>%
  filter(Occasion == "Food-centered occasion (to compliment the food)")

test_data_casual <- test_data %>%
  filter(Occasion == "Casual occasion (for example: birthday, hangout)")

test_data_food <- test_data %>%
  filter(Occasion == "Food-centered occasion (to compliment the food)")

# Data Transformation
mlogit_data_casual <- mlogit.data(
  data     = train_data_casual,
  choice   = "Choice",
  shape    = "long",
  alt.var  = "Alternative",
  id.var   = "Respondent.ID",
  chid.var = "chid")

mlogit_data_food <- mlogit.data(
  data     = train_data_food,
  choice   = "Choice",
  shape    = "long",
  alt.var  = "Alternative",
  id.var   = "Respondent.ID",
  chid.var = "chid")

mlogit_test_casual <- mlogit.data(
  data     = test_data_casual,
  choice   = "Choice",
  shape    = "long",
  alt.var  = "Alternative",
  id.var   = "Respondent.ID",
  chid.var = "chid")

mlogit_test_food <- mlogit.data(
  data     = test_data_food,
  choice   = "Choice",
  shape    = "long",
  alt.var  = "Alternative",
  id.var   = "Respondent.ID",
  chid.var = "chid")


# Model
# Numeric - Casual
mnl_num_int_casual <- mlogit(
  Choice ~ Choice ~ Price + Alcohol + Aging.Time +
    Price_France + Price_Germany + Price_NL + Price_Other +
    Type_Red + Type_White + Type_Rose +
    NC
  | 0,
  data = mlogit_data_casual,
  reflevel = "A"
)

summary(mnl_num_int_casual)

# Numeric - Food
mnl_num_int_food <- mlogit(
  Choice ~ Price + Alcohol + Aging.Time +
    Price_France + Price_Spain + Price_Germany + Price_NL + Price_Other +
    Type_Red + Type_White + Type_Rose +
    NC
  | 0,
  data = mlogit_data_food,
  reflevel = "A"
)

summary(mnl_num_int_food)

# Categorical - Casual
mnl_cat_int_casual <- mlogit(
  Choice ~ Price_12 + Price_18 + Price_24 +
    #Price_12_France + Price_18_France + Price_24_France +
    #Price_12_Germany + Price_18_Germany + Price_24_Germany +
    #Price_12_Netherlands + Price_18_Netherlands + Price_24_Netherlands +
    #Price_12_Spain + Price_18_Spain + Price_24_Spain +
    #Price_12_Other + Price_18_Other + Price_24_Other +
    #Price_12_Male + Price_18_Male + Price_24_Male +
    Type_Red + Type_White + Type_Rose +
    Alcohol_7 + Alcohol_12 + Alcohol_18 +
    Aging_2 + Aging_3 + Aging_4 +
    NC
  | 0,
  data = mlogit_data_casual,
  reflevel = "A"
)

summary(mnl_cat_int_casual)

# Categorical - Food
mnl_cat_int_food <- mlogit(
  Choice ~ Price_12 + Price_18 + Price_24 +
    Price_12_France + Price_18_France + Price_24_France +
    Price_12_Germany + Price_18_Germany + Price_24_Germany +
    Price_12_Netherlands + Price_18_Netherlands + Price_24_Netherlands +
    Price_12_Spain + Price_18_Spain + Price_24_Spain +
    Price_12_Other + Price_18_Other + Price_24_Other +
    #Price_12_Male + Price_18_Male + Price_24_Male +
    Type_Red + Type_White + Type_Rose +
    Alcohol_7 + Alcohol_12 + Alcohol_18 +
    Aging_2 + Aging_3 + Aging_4 +
    NC
  | 0,
  data = mlogit_data_food,
  reflevel = "A"
)

summary(mnl_cat_int_food)

# McFadden's test
# McFadden's Pseudo R² for mnl_num_int
logLik_null <- as.numeric(logLik(null_model))
logLik_model_food <- as.numeric(logLik(mnl_cat_int_food))
logLik_model_casual <- as.numeric(logLik(mnl_cat_int_casual))

# McFadden's R² formula
mcfadden_r2_food <- 1 - (logLik_model_food / logLik_null)
mcfadden_r2_food

mcfadden_r2_casual <- 1 - (logLik_model_casual / logLik_null)
mcfadden_r2_casual

# Hit Rate - DOES NOT MAKE ANY SENSE
model_5 <- mnl_num_int_casual          # Numeric
model_6 <- mnl_num_int_food       # Numeric + Interaction
model_7 <- mnl_cat_int_casual           # Categorical
model_8 <- mnl_cat_int_food       # Categorical + Interaction

in_hit_5  <- get_hit_rate(mnl_num_int_casual, mlogit_test_casual)   # in-sample
out_hit_5 <- get_hit_rate(mnl_num_int_casual, mlogit_test_casual)    # out-of-sample
in_hit_6  <- get_hit_rate(mnl_num_int_food, mlogit_test_food)   # in-sample
out_hit_6<- get_hit_rate(mnl_num_int_food, mlogit_test_food)    # out-of-sample
in_hit_7  <- get_hit_rate(mnl_cat_int_casual, mlogit_test_casual)   # in-sample
out_hit_7 <- get_hit_rate(mnl_cat_int_casual, mlogit_test_casual)    # out-of-sample
in_hit_8  <- get_hit_rate(mnl_cat_int_food, mlogit_test_food)   # in-sample
out_hit_8 <- get_hit_rate(mnl_cat_int_food, mlogit_test_food)    # out-of-sample

hit_rate_table_oc <- tibble(
  Model          = c("Num + Int - Casual",
                     "Num + Int - Food",
                     "Cat + Int - Casual",
                     "Cat + Int - Casual"),
  In_sample      = round(c(in_hit_5,  in_hit_6,  in_hit_7,  in_hit_8), 3),
  Out_of_sample  = round(c(out_hit_5, out_hit_6, out_hit_7, out_hit_8), 3)
)

hit_rate_table_oc

