
####Reading in an external dataframe.
df <- read.csv("C:/JAGSOM TERM 3/Business Forecasting/Assignment 2/Case 2.csv", 
               stringsAsFactors = FALSE, 
               fileEncoding = "UTF-8")

names(df)
str(df)

####Understanding the dataframe
head(df)
tail(df)
str(df)
summary(df)
class (df$Foot)
class (df$Pos)

install.packages("plyr")
library(plyr)

# Mapping charcter values in columns to numbers but map values does not change the character type
df$Foot <- mapvalues(df$Foot, from = c("L", "R", "B"), to = c(1, 2, 3))
df$Pos <- mapvalues (df$Pos, from = c("OFF","MID"), to =c(0,1))

# Convert character values to numeric values
df$Foot <- as.numeric(df$Foot)
df$Pos <- as.numeric(df$Pos)

print(df)
View(df)
class(df$Foot)
class (df$Pos)


# Bivariate Analysis


#######Scatterplot - Bivariate analysis
# 1 Foot is Player's Dominant foot
plot(df$GBP_M~df$Foot, xlab="Foot", 
     ylab = "GBP_M", main = "Foot vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~Foot,data = df), col = "blue")
#simple linear regression
SLR1 <- lm(GBP_M~Foot,data = df)
summary(SLR1)
anova(SLR1)

#2 Y_5 is Appearance over total no of league games 5 yr prior to transfer
plot(df$GBP_M~df$Y_5, xlab="Y_5", 
     ylab = "GBP_M", main = "Y_5 vs GBP_M",
     col = "black", pch = 16)
cabline(lm(GBP_M~Y_5,data = df), col = "blue")
#simple linear regression
SLR2 <- lm(GBP_M~Y_5,data = df)
summary(SLR2)
anova(SLR2)

#3 Y_4 is Appearance over total no of league games 4 yr prior to transfer
plot(df$GBP_M~df$Y_4, xlab="Y_5", 
     ylab = "GBP_M", main = "Y_4 vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~Y_4,data = df), col = "blue")
#simple linear regression
SLR3 <- lm(GBP_M~Y_4,data = df)
summary(SLR3)
anova(SLR3)

#4 Y_3 is Appearance over total no of league games 3 yr prior to transfer
plot(df$GBP_M~df$Y_3, xlab="Y_3", 
     ylab = "GBP_M", main = "Y_3 vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~Y_3,data = df), col = "blue")
#simple linear regression
SLR4 <- lm(GBP_M~Y_3,data = df)
summary(SLR4)
anova(SLR4)

#5 Y_2 is Appearance over total no of league games 2 yr prior to transfer
plot(df$GBP_M~df$Y_2, xlab="Y_2", 
     ylab = "GBP_M", main = "Y_2 vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~Y_2,data = df), col = "blue")
#simple linear regression
SLR5 <- lm(GBP_M~Y_2,data = df)
summary(SLR5)
anova(SLR5)

#6 Y_1 is Appearance over total no of league games 5 yr prior to transfer
plot(df$GBP_M~df$Y_1, xlab="Y_1", 
     ylab = "GBP_M", main = "Y_1 vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~Y_1,data = df), col = "blue")
#simple linear regression
SLR6 <- lm(GBP_M~Y_1,data = df)
summary(SLR6)
anova(SLR6)

#7 WA is Weighted average utilization ratio in league games over past 5 years
plot(df$GBP_M~df$WA, xlab="WA", 
     ylab = "GBP_M", main = "WA vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~WA,data = df), col = "blue")
#simple linear regression
SLR7 <- lm(GBP_M~WA,data = df)
summary(SLR7)
anova(SLR7)


#8 Goals is Total no. of goals scored in league games over the past 5 years
plot(df$GBP_M~df$Goals, xlab="Goals", 
     ylab = "GBP_M", main = "Goals vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~Goals,data = df), col = "blue")
#simple linear regression
SLR8 <- lm(GBP_M~Goals,data = df)
summary(SLR8)
anova(SLR8)

#9 App is Total no of appearances in league games over past 5 years
plot(df$GBP_M~df$App, xlab="App", 
     ylab = "GBP_M", main = "App vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~App,data = df), col = "blue")
#simple linear regression
SLR9 <- lm(GBP_M~App,data = df)
summary(SLR9)
anova(SLR9)

#10 G/A is Ration of goals to appearances in league games over past 5 years
plot(df$GBP_M~df$GA, xlab="GA", 
     ylab = "GBP_M", main = "GA vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~GA,data = df), col = "blue")
#simple linear regression
SLR10 <- lm(GBP_M~GA,data = df)
summary(SLR10)
anova(SLR10)

#11 Age is Age at time of the transfer
plot(df$GBP_M~df$Age, xlab="Age", 
     ylab = "GBP_M", main = "Age vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~Age,data = df), col = "blue")
#simple linear regression
SLR11 <- lm(GBP_M~Age,data = df)
summary(SLR11)
anova(SLR11)

#12 Height is Height of player
plot(df$GBP_M~df$Height, xlab="Height", 
     ylab = "GBP_M", main = "Height vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~Height,data = df), col = "blue")
#simple linear regression
SLR12 <- lm(GBP_M~Height,data = df)
summary(SLR12)
anova(SLR12)

#13 Pos is Player's Position
plot(df$GBP_M~df$Pos, xlab="Pos", 
     ylab = "GBP_M", main = "Pos vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~Pos,data = df), col = "blue")
#simple linear regression
SLR13 <- lm(GBP_M~Pos,data = df)
summary(SLR13)
anova(SLR13)

#14 CR is club ranking of the team that is selling player in categories wher 1= top eight and 2 = bottom eight
plot(df$GBP_M~df$CR, xlab="CR", 
     ylab = "GBP_M", main = "CR vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~CR,data = df), col = "blue")
#simple linear regression
SLR14 <- lm(GBP_M~CR,data = df)
summary(SLR14)
anova(SLR14)

#15 NR is FIFA ranking of players's national team in categories (1=best can reach semifinal, 2= low chance, 3= very low chance)
plot(df$GBP_M~df$NR, xlab="NR", 
     ylab = "GBP_M", main = "NR vs GBP_M",
     col = "black", pch = 16)
abline(lm(GBP_M~NR,data = df), col = "blue")
#simple linear regression
SLR15 <- lm(GBP_M~NR,data = df)
summary(SLR15)
anova(SLR15)


View(df)

numeric_vars <- df[ , c("GBP_M", "Y_5", "Y_4", "Y_3", "Y_2", "Y_1", "WA", "Goals", "App", "GA", "Age", "Height","Pos","Foot","CR","NR")]

# Compute correlation matrix
correlation_matrix <- cor(numeric_vars)

# Print correlation matrix
print(correlation_matrix)


# Perform multiple linear regression
MLR <- lm(GBP_M ~ Y_5 + Y_4 + Y_3 + Y_2 + Y_1 + WA + Goals + App + GA + Age + Height + Pos + Foot + CR + NR, data = df)

# Print summary of the regression model
summary(MLR)

#Of all IV only Age has p value less than 0.05
Final <- lm(GBP_M ~ Age, data = df)

# Summarize the model
summary(Final)

# Multicollinearity
library(car)
vif(MLR)

vif_df <- as.data.frame(vif(MLR))

# Removing Highly correlated predictor variables (Y_5 , Y_4 , Y_3 , Y_2 , Y_1 , WA , Goals , App , GA)

# Since Years Y_1 to Y_5 are highly correlated , we give combine them by giving appropriate weights. Here higher weights is given to Player having five year performance record will have more sustained performance than lesser years
# Define the weights
weights <- c(0.1, 0.2, 0.3, 0.4, 0.5)

# Calculate the weighted average
df$Weighted_Average_Y <- rowSums(df[, c("Y_1", "Y_2", "Y_3", "Y_4", "Y_5")] * weights, na.rm = TRUE) / sum(weights)

# Now, you can remove the individual Y variables from the data frame
df <- df[, !colnames(df) %in% c("Y_1", "Y_2", "Y_3", "Y_4", "Y_5")]

# Check the summary of the new data frame
summary(df)
#####Multiple linear regression
MLR_new1 <- lm(GBP_M ~ Weighted_Average_Y  + GA + Age + Height + Pos + Foot + CR + NR, data = df)
summary(MLR_new1)
vif(MLR_new1)

MLR_new <- lm(GBP_M ~  GA + Age , data = df)
summary(MLR_new)
vif(MLR_new)

#Calculating GBA_P
Predicted_Market_Value = 61.7334 + GA*27.1870 + Age * (-1.4752)


# Load necessary libraries
library(dplyr)


# Filter offensive players (where Pos = 0)
offensive_players <- df %>%
  filter(Pos == 0)

# Compute predicted market value for offensive players using the regression model
offensive_players$Predicted_Market_Value <- 61.7334 + offensive_players$GA * 27.1870 +  offensive_players$Age * (-1.4752)

#Sort the offensive players by predicted market value in descending order
sorted_offensive_players <- offensive_players %>%
  arrange(desc(Predicted_Market_Value))

# Print the sorted dataframe
print(sorted_offensive_players)


# Find the index of the player with the maximum predicted market value
max_index <- which.max(sorted_offensive_players$Predicted_Market_Value)

# Retrieve the player information with the maximum predicted market value
max_player <- sorted_offensive_players[max_index, ]

# Print the information
print(max_player)

View(df)




