sleep_health <- read.csv("/home/randompc/Downloads/Sleep_health_and_lifestyle_dataset.csv")
head(sleep_health)
str(sleep_health)
#extra work
mean_age <- mean(sleep_health$Age, na.rm = TRUE)
print(mean_age)


#Age analyze
median(sleep_health$Age)
mean(sleep_health$Age)
sd(sleep_health$Age)
min(sleep_health$Age)
max(sleep_health$Age)
quartiles <- quantile(sleep_health$Age, probs = seq(0,1,0.25), na.rm = TRUE)
print(quartiles)


#sleep duration Analyzes

mean(sleep_health$Sleep.Duration)
median(sleep_health$Sleep.Duration)
sd(sleep_health$Sleep.Duration)
min(sleep_health$Sleep.Duration)
max(sleep_health$Sleep.Duration)
quartiles_sd <- quantile(sleep_health$Sleep.Duration, probs = seq(0,1,0.25), na.rm = TRUE)
print(quartiles_sd)


#quality of sleep 

mean(sleep_health$Quality.of.Sleep)
median(sleep_health$Quality.of.Sleep)
sd(sleep_health$Quality.of.Sleep)
min(sleep_health$Quality.of.Sleep)
max(sleep_health$Quality.of.Sleep)
quartiles_qs <- quantile(sleep_health$Quality.of.Sleep, probs = seq(0,1,0.25), na.rm = TRUE)
print(quartiles_qs)


#physical activity level  

mean(sleep_health$Physical.Activity.Level)
median(sleep_health$Physical.Activity.Level)
sd(sleep_health$Physical.Activity.Level)
min(sleep_health$Physical.Activity.Level)
max(sleep_health$Physical.Activity.Level)
quartiles_fa <- quantile(sleep_health$Physical.Activity.Level, probs = seq(0,1,0.25), na.rm = TRUE)
print(quartiles_fa)


#stress level  

mean(sleep_health$Stress.Level)
median(sleep_health$Stress.Level)
sd(sleep_health$Stress.Level)
min(sleep_health$Stress.Level)
max(sleep_health$Stress.Level)
quartiles_sl <- quantile(sleep_health$Stress.Level, probs = seq(0,1,0.25), na.rm = TRUE)
print(quartiles_sl)



#Blood pressure  preparation
sleep_health$sbp <- as.numeric(sub("/.*", "",sleep_health$Blood.Pressure))
sleep_health$dbp <- as.numeric(sub("/.*", "",sleep_health$Blood.Pressure))

sleep_health$MAP <- sleep_health$sbp + (1/3) * (sleep_health$sbp - sleep_health$dbp)
print(sleep_health$MAP)




mean(sleep_health$MAP)
median(sleep_health$MAP)
sd(sleep_health$MAP)
min(sleep_health$MAP)
max(sleep_health$MAP)
quartiles_bp <- quantile(sleep_health$MAP, probs = seq(0,1,0.25), na.rm = TRUE)
print(quartiles_bp)



#Heart Rate 

mean(sleep_health$Heart.Rate)
median(sleep_health$Heart.Rate)
sd(sleep_health$Heart.Rate)
min(sleep_health$Heart.Rate)
max(sleep_health$Heart.Rate)
quartiles_hr <- quantile(sleep_health$Heart.Rate, probs = seq(0,1,0.25), na.rm = TRUE)
print(quartiles_hr)

#Daily Steps 

mean(sleep_health$Daily.Steps)
median(sleep_health$Daily.Steps)
sd(sleep_health$Daily.Steps)
min(sleep_health$Daily.Steps)
max(sleep_health$Daily.Steps)
quartiles_hr <- quantile(sleep_health$Daily.Steps, probs = seq(0,1,0.25), na.rm = TRUE)
print(quartiles_hr)





#DATA CLEANSING

#missing values

colSums(is.na(sleep_health))

#check duplicates

sum(duplicated(sleep_health))



#outliers using z-score

z_scores <- scale(sleep_health$Age)
new_outliers <- sleep_health[abs(z_scores)> 3]
print(new_outliers)


#Fixing BMI
sleep_health$BMI.Category <- gsub("Normal Weight","Normal", sleep_health$BMI.Category)
print(sleep_health$BMI.Category)


#============================== PART 1 ==============================

#box plots
library(ggplot2)
ggplot(sleep_health, aes(x = "", y = Heart.Rate)) + geom_boxplot() + ylab("Heart Rate") +
  ggtitle("Heart Rate Boxplot") + theme_minimal()


ggplot(sleep_health, aes(x = Gender, y = Heart.Rate)) + geom_boxplot() + 
  ylab("Heart Rate (bpm)") + ggtitle("Heart Rate by Gender") + 
  theme_minimal()

#histograms

ggplot(sleep_health, aes(x = BMI.Category)) + geom_bar(fill = "pink", color = "black")+
  xlab("BMI") + ylab("Frequency") + ggtitle("Distribution by BMI")+
  theme_minimal()

ggplot(sleep_health, aes(x = Age))+ geom_histogram(binwidth = 1, fill = "lightblue", color = "black")+
  xlab("Age") + ylab("Frequency") + ggtitle("Age Histogram")+ theme_minimal()

ggplot(sleep_health, aes(x = Sleep.Duration, fill = Sleep.Disorder)) + geom_histogram(aes(y = ..density..), bins = 10, alpha = 0.5, position = "Identity")+
  geom_density(alpha = 0.7, size = 1) + labs(title = "Sleep Duration by Sleep Disorder") +
  theme_minimal

ggplot(sleep_health, aes(x = Physical.Activity.Level, fill = Sleep.Disorder)) +
  geom_histogram(aes(y = ..density..), bins = 10, alpha = 0.5, position = "Identity")+
  geom_density(alpha = 30, size = 1) + labs(title = "Physical Activity by Sleep Disorder") +
  theme_minimal()

ggplot(sleep_health, aes(x = Stress.Level, fill = Sleep.Disorder)) + geom_histogram(aes(y =..density..), bins = 10, alpha = 0.5, position = "Identity")+
  geom_density(alpha = 0.7, size = 1) + labs(title = "Stress Level by Sleep Disorder") +
  theme_minimal()



#barplots

ggplot(sleep_health, aes(x = Gender, fill = Sleep.Disorder)) + 
  geom_bar(position = "dodge") + labs(title = "Sleep Disorder by Gender", x = "Gender", y = "Count") + 
  scale_fill_brewer(palette = "Set3")

ggplot(sleep_health, aes(x = Occupation, fill = Quality.of.Sleep)) + 
  geom_bar(position = "fill") + labs(title = "Quality of Sleep by Occupation") 


ggplot(sleep_health, aes(x = Occupation, fill = Sleep.Disorder)) + geom_bar(position = "dodge") + 
  labs(title = "Sleep Disorder by Occupation", x = "Occupation", y = "Count") + 
  scale_fill_brewer(palette = "Set3")

sleep_health$Blood.Pressure <- ifelse(sleep_health$sbp > 130, "HIGH", "NORMAL")
ggplot(sleep_health, aes(x = Occupation, fill = Blood.Pressure)) + 
  geom_bar(position = "fill") + labs(title = "Blood Pressure by Occupation")


# ==============================Part 2 + PART 3 ========================

#Scatterplot
ggplot(sleep_health, aes(x = Stress.Level, y = Sleep.Duration, color = Stress.Level)) + geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "black")+ 
  labs(title = "Stress Level Vs Sleep Duration", x = "Stress Level", y = "Sleep Duration (HRS)")
+scale_color_brewer(palette = "Set2")


ggplot(sleep_health, aes(x = Physical.Activity.Level, y = Sleep.Duration, color = Sleep.Duration))+
  geom_smooth(method = "lm", se = FALSE, color = "black")+ 
  geom_point() + labs(title = "Physical Activity vs. Sleep Duration", x = "Physical Activity", y = "Sleep Duration")  
  + scale_color_brewer(palette = "Set3")


# ==========================Part 4===========================================

library(dplyr)
#pie
gender_count <- sleep_health %>%
  count(Gender)

ggplot(gender_count, aes(x = "", y = n, fill = Gender)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar(theta = "y")+
  labs(title="Gender Distribution")+
  theme_void()


#pie
sleep_disorder_count <- sleep_health %>%
  count(Sleep.Disorder)


ggplot(sleep_disorder_count, aes(x = "", y = n, fill = Sleep.Disorder)) +
  geom_bar(stat = "identity", width = 1) + coord_polar(theta = "y") +  
  geom_text(aes(label = paste0(Sleep.Disorder, ": ", n)), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Distribution of Sleep Disorders") + scale_fill_brewer(palette = "Set2") +  
  theme_void() 


