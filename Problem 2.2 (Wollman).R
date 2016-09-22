#Part 1: Plot and examine if there is a statistical difference between the weights of the chicks assigned to different diet treatments on day 0
#organizing data into factors for analysis
ChickWeight = ChickWeight;
data <- ChickWeight;
data <- within(data, {
  Chick <- factor(Chick);
  Diet <- factor(Diet);
  Time <- factor(Time);
}
)

#creating a boxplot for the data differences
day0 <- data[data$Time == 0,]
boxplot(weight~Diet, data = day0, xlab = "Diet", ylab = "Weight", main = "Weights of Chicks on Various Diets, Day 0")

#ANOVA for comparing diets on day 0
anova_day0 = aov(weight~Diet, data = day0)
summary(anova_day0)
TukeyHSD(anova_day0)
#p = 0.346; there is no effect of diet on weight on Day 0.

#Part 2: Plot and examine if there is a statistical difference between the weights of the chicks assigned to different diet treatments at the end of the study (day 21).
day21 <- data[data$Time == 21,]
boxplot(weight~Diet, data = day21, xlab = "Diet", ylab = "Weight", main = "Weights of Chicks on Various Diets, Day 21")

#ANOVA for comparing diets on day 0
anova_day21 = aov(weight~Diet, data = day21)
summary(anova_day21)
TukeyHSD(anova_day21)
#p < 0.01; there is an effect of diet on weight on Day 21.

#Part 3: Is there an effect of diet on chick growth? (repeated measures ANOVA)
summary(aov(weight ~ Diet*Time+Error(Chick), data = data));
#p < 0.001; there is an effect of diet on chick growth.

#Part 4: Plot the effects of diet on chick growth
#setup to substitute colors for each diet type
dietColumn = sub('1', 'firebrick1', data$Diet)
dietColumn = sub('2', 'goldenrod1', dietColumn)
dietColumn = sub('3', 'darkolivegreen1', dietColumn)
dietColumn = sub('4', 'deepskyblue2', dietColumn)

plot(as.numeric(data$Time),
     data$weight,
     xlab = "Time",
     ylab = "Weight",
     main = "The Effects of Diet on Chick Growth",
     col = dietColumn,
     pch = 16
     )

legend("topleft",
       title = "Diet #",
       legend = unique(data$Diet),
       text.colc("firebrick1", "goldenrod1", "darkolivegreen1", "deepskyblue2"),
       pch = 16,
       col = c("firebrick1", "goldenrod1", "darkolivegreen1", "deepskyblue2"))

#Part 5: Using a for loop, plot the growth of each chick in a different color, all on the same plot.
plot(
     data$Time,
     data$weight,
     xlim = c(0, 21),
     ylim = c(0, 400),
     xlab = "Time",
     ylab = "Weight",
     main = "21-Day Per Chick Growth",
     pch = 16)

individualColor = rainbow(50)

for(individual in 1:50){
  chickRow = which(data$Chick == individual)
  lines(x = data$Time[chickRow],
        y = data$weight[chickRow],
        col = individualColor[individual])
}
