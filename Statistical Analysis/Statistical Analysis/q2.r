# Loading dplyr package from the R Library
library(dplyr)

# Loading the PATH of the dataset
PATH <- "I:/Human Computer Interaction/Statistical Analysis/Statistical Analysis/dataset2.csv"

# Reading the CSV file
df <- read.csv(PATH) %>%
  mutate(menu = factor(menu, ordered = TRUE))

# Printing the Dataframe along with the distinctive menus
print(glimpse(df))
print(levels(df$menu))

# Loading the individual variables

tm <- df$time
err <- df$error

# Performing the MANOVA test
manova_test <-manova(cbind(tm, err) ~ menu, data = df)
summary(manova_test)

# Checking which one is differing by using ANOVA
summary.aov(manova_test)

# Printing the different statistical measures
summary(df)

# Loading ggplot2 package from the R Library
library(ggplot2)

# Plotting BoxPlots to visualize the different statistical measures

ggplot(df, aes(x = menu, y = time, fill = menu)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "red",
              position = position_jitter(0.21)) +
  theme_classic()

ggplot(df, aes(x = menu, y = error, fill = menu)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "red",
              position = position_jitter(0.21)) +
  theme_classic()

# Performing Pairwise t-test first on time and then on error
pairwise.t.test(df$time, df$menu, p.adj = "bonf")
pairwise.t.test(df$error, df$menu, p.adj= "bonf")