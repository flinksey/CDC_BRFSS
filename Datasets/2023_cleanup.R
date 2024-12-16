library(tidyverse)
library(readr)
library(haven)
library(ggplot2)
library(plyr)
library(dplyr)
library(caret)

CSV_data <- read.csv("2023-brfss-raw.csv",header=TRUE)

# subset
feat_18 = c('CVDSTRK3','ADDEPEV3','RFHLTH','PHYS14D','MENT14D','HLTHPL1','PA150R4','RFHYPE6','RFCHOL3','MICHD','RACE','SEX','AGEG','RFBMI5','EDUCAG','INCOMG1','SMOKER3','RFDRHV8','DIABETE4')
df_18 = CSV_data[,feat_18] #all rows, columns restricted to the chosen features

print(paste0("pre-na.omit dimensions: ",dim(df_18))) # 433323 samples

# remove all rows with at least 1 NA value for any of the 20 features
df_18 <- df_18[complete.cases(df_18),]
print(paste0("post-na.omit dimensions: ",dim(df_18))) # 377733 samples

#DIABETE4 - 1-Yes,4-pre,2/3-No,7/9/NA-missing becomes 0-No, 1-Yes/pre
df_18 <- df_18[!(df_18$DIABETE4 %in% c(7,9)),]
df_18$DIABETE4[df_18$DIABETE4 == 2 | df_18$DIABETE4 == "3"] <- 0
df_18$DIABETE4[df_18$DIABETE4 == 4] <- 1
names(df_18)[names(df_18) == "DIABETE4"] <- "Diabetes"

#RFCHOL3 - high chol; 1-No, 2-Yes, 9/NA-missing
df_18 <- df_18[!(df_18$RFCHOL3 %in% c(9)),]
df_18$RFCHOL3[df_18$RFCHOL3 == 1] <- 0
df_18$RFCHOL3[df_18$RFCHOL3 == 2] <- 1
names(df_18)[names(df_18) == "RFCHOL3"] <- "Hi_Cholesterol"

#CVDSTRK3 - 1-Yes, 2-No (becomes 0), 7/9/NA-drop
df_18 <- df_18[!(df_18$CVDSTRK3 %in% c(7,9)),]
df_18$CVDSTRK3[df_18$CVDSTRK3 == 2] <- 0
names(df_18)[names(df_18) == "CVDSTRK3"] <- "Had_Stroke"

#ADDEPEV3 - 1-Yes, 2-No (becomes 0), 7/9/NA-missing
df_18 <- df_18[!(df_18$ADDEPEV3 %in% c(7,9)),]
df_18$ADDEPEV3[df_18$ADDEPEV3 == 2] <- 0
names(df_18)[names(df_18) == "ADDEPEV3"] <- "Depression"

#RFHLTH - health status; 1-Good/Better, 2-Poor/Fair (becomes 0), 9-drop
df_18 <- df_18[!(df_18$RFHLTH %in% c(9)),]
df_18$RFHLTH[df_18$RFHLTH == 2] <- 0
names(df_18)[names(df_18) == "RFHLTH"] <- "GenHealth"

#PHYS14D - physical health; 1-healthy in past month, 2/3-merge, 9 drop
df_18 <- df_18[!(df_18$PHYS14D %in% c(9)),]
df_18$PHYS14D[df_18$PHYS14D == 2 | df_18$PHYS14D == 3] <- 0
names(df_18)[names(df_18) == "PHYS14D"] <- "PhysHealth"

#MENT14D - mental health; 1-healthy in past month, 2/3-merge, 9 drop
df_18 <- df_18[!(df_18$MENT14D %in% c(9)),]
df_18$MENT14D[df_18$MENT14D == 2 | df_18$MENT14D == 3] <- 0
names(df_18)[names(df_18) == "MENT14D"] <- "MentalHealth"

#HLTHPL1 - health insurance; 1-with, 0-without, 9 drop
df_18 <- df_18[!(df_18$HLTHPL1 %in% c(9)),]
df_18$HLTHPL1[df_18$HLTHPL1 == 2] <- 0
names(df_18)[names(df_18) == "HLTHPL1"] <- "withHealthInsur"

#PA150R4 - amount of exercise; 1-150+ mins, 2/3-merge, 9 drop
df_18 <- df_18[!(df_18$PA150R4 %in% c(9)),]
df_18$PA150R4[df_18$PA150R4 == 2 | df_18$PA150R4 == 3] <- 0
names(df_18)[names(df_18) == "PA150R4"] <- "Exercise150"

#RFHYPE6 - high BP; 1-with, 0-without, 9 drop
df_18 <- df_18[!(df_18$RFHYPE6 %in% c(9)),]
df_18$RFHYPE6[df_18$RFHYPE6 == 1] <- 0
df_18$RFHYPE6[df_18$RFHYPE6 == 2] <- 1
names(df_18)[names(df_18) == "RFHYPE6"] <- "Hi_BP"

#MICHD - w/ or w/o CHD or MI; 1-with, 0-without
df_18$MICHD[df_18$MICHD == 2] <- 0
names(df_18)[names(df_18) == "MICHD"] <- "Chronic_Heart"

#RACE - race; just drop the entries where the data is missing
df_18 <- df_18[!(df_18$RACE %in% c(9)),]
names(df_18)[names(df_18) == "RACE"] <- "Race"

#SEX - sex; set 2(Female) to be 0
df_18$SEX[df_18$SEX == 2] <- 0
names(df_18)[names(df_18) == "SEX"] <- "Sex"

#AGEG - already ordinal, just change name
names(df_18)[names(df_18) == "AGEG"] <- "Age"

#RFBMI5 - BMI classified as either >=25.0 or <25.0; 9 drop
df_18 <- df_18[!(df_18$RFBMI5 %in% c(9)),]
df_18$RFBMI5[df_18$RFBMI5 == 1] <- 0
df_18$RFBMI5[df_18$RFBMI5 == 2] <- 1
names(df_18)[names(df_18) == "RFBMI5"] <- "Hi_BMI"

#EDUCAG - already ordinal, just need to drop 9
df_18 <- df_18[!(df_18$EDUCAG %in% c(9)),]
names(df_18)[names(df_18) == "EDUCAG"] <- "Education"

#INCOMG1 - already ordinal, just need to drop 9
df_18 <- df_18[!(df_18$INCOMG1 %in% c(9)),]
names(df_18)[names(df_18) == "INCOMG1"] <- "Income"

#SMOKER3 - already ordinal, just need to drop 9
df_18 <- df_18[!(df_18$SMOKER3 %in% c(9)),]
names(df_18)[names(df_18) == "SMOKER3"] <- "Smoker"

#RFDRHV8 - heavy drinker; 0-No, 1-Yes, 9-drop
df_18 <- df_18[!(df_18$RFDRHV8 %in% c(9)),]
df_18$RFDRHV8[df_18$RFDRHV8 == 1] <- 0
df_18$RFDRHV8[df_18$RFDRHV8 == 2] <- 1
names(df_18)[names(df_18) == "RFDRHV8"] <- "Drinker"

print(paste0("post-cleanup dimensions: ",dim(df_18))) # 230049 samples

write.csv(df_18,"2023-brfss-18var.csv")


# Correlation Heatmap for df_18
correlation_matrix <- abs(cor(df_18))

matr_18 <- ggplot(data = as.data.frame(as.table(correlation_matrix)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("grey", "orange", "maroon"))(50), name="Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "black", size=5) +
  theme_minimal() +
  theme(axis.text.x = element_text(size=10, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 10),   # Adjust plot title font size
        legend.text = element_text(size = 10),
        legend.title = element_text(size=10))

matr_18


# Applying threshold of 0.1, we are left with 11 independent variables
drop_01 <- c("Depression","MentalHealth","withHealthInsur","Race","Sex","Smoker","Drinker")
df_11 = df_18[,!(names(df_18) %in% drop_01)]

print(paste0("df_11: ",dim(df_11))) # confirmed, only has 12 cols now
write.csv(df_11,"2023-brfss-11var.csv")


# Correlation Heatmap for df_11
correlation_matrix_11 <- abs(cor(df_11))

matr_11 <- ggplot(data = as.data.frame(as.table(correlation_matrix_11)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("grey", "orange", "maroon"))(50), name="Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "black", size=5) +
  theme_minimal() +
  theme(axis.text.x = element_text(size=10, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 10),   # Adjust plot title font size
        legend.text = element_text(size = 10),
        legend.title = element_text(size=10))

matr_11


# Applying threshold of 0.15, we are left with 7 independent variables
drop_015 <- c("Had_Stroke","PhysHealth","Exercise150","Education")
df_7 = df_11[,!(names(df_11) %in% drop_015)]

print(paste0("df_7: ",dim(df_7))) # confirmed, only has 8 cols now
write.csv(df_7,"2023-brfss-7var.csv")


# Correlation Heatmap for df_7
correlation_matrix_7 <- abs(cor(df_7))

matr_7 <- ggplot(data = as.data.frame(as.table(correlation_matrix_7)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c("grey", "orange", "maroon"))(50), name="Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "black", size=5) +
  theme_minimal() +
  theme(axis.text.x = element_text(size=10, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 10),   # Adjust plot title font size
        legend.text = element_text(size = 10),
        legend.title = element_text(size=10))

matr_7
