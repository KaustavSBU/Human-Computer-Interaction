# Loading dplyr package from the R Library
library(dplyr)

# Loading the PATH of the dataset
PATH <- "I:/Human Computer Interaction/Statistical Analysis/Statistical Analysis/dataset1.csv"

# Reading the CSV file
df <- read.csv(PATH) %>%
mutate(menu = factor(menu, ordered = TRUE))

# Printing the Dataframe along with the distinctive menus
print(glimpse(df))
print(levels(df$menu))

# Printing the different statistical measures
summary(df)

# Loading ggplot2 package from the R Library
library(ggplot2)

# Plotting a BoxPlot to visualize the different statistical measures

ggplot(df, aes(x = menu, y = time, fill = menu)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "red",
              position = position_jitter(0.21)) +
  theme_classic()

# Performing ANOVA test and then summarizing the results
anova_one_way <- aov(time~menu, data = df)
summary(anova_one_way)

# Performing Pairwise t-test
pairwise.t.test(df$time, df$menu, p.adj = "bonf")