
# Lab 1: Basic Data Visualizations


library(tidyverse)

data = mpg

# Try it: What other types of plots are there? Try to find several more geom_
# Answer: Line: geom_line()  Histogram: geom_histogram()

library(gapminder)
library(ggplot2)
gapminder
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point()

# Exercise 1
ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy)) + geom_point()
# Does it capture the intuitive relationship you expected?
# Yes, as a car's engine size increases, a car’s fuel efficiency on the highway decreases.

ggplot(data = mpg,
       mapping = aes(x = class, y = drv)) + geom_point()
# What happens if you make a scatterplot of class vs drv ? Why is the plot not useful?
# There is no linear trend between class and drv.

# Exercise 1b
ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy, color=class)) + geom_point()
# What conclusions can we make?
# Smaller cars tend to have high hwy and low displ, while bigger cars tend to have low hwy and high displ.


p + geom_point()
p + geom_smooth()

p + geom_point() + geom_smooth(method = "lm") + scale_x_log10()
# Try it: Describe what the scale_x_log10() does. Why is it a more evenly distributed cloud of points now?
# scale_x_log10() makes the original x values become log10(x), which redistribute data on x-axis more evenly. Then the picture looks more clear and concise.

library(scales)
p + geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::dollar)
# Try it: What does the dollar() call do?
# The dollar() changes the labels of x-axis to dollors.

# How can you find other ways of relabeling the scales when using scale_x_log10() ?
# ???

# Try it: Write the above sentence for the original call aes(x = gdpPercap, y = lifeExp, color = 'yellow')
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = 'yellow'))
p + geom_point() + scale_x_log10()
# Try it: describe in your words what is going on.
# The points in the graph is not yellow.

# How can we tell ggplot to draw yellow points?
# p + geom_point(color="yellow") + scale_x_log10()
p + geom_point(color="yellow") + scale_x_log10()


p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point() + geom_smooth(color = "orange", se = FALSE, size = 8, method = "lm") + scale_x_log10()
# Try it: Write down what all those arguments in geom_smooth(...) do
# geom_smooth(...) generates a linear regression line for the given data and plot the line.

p + geom_point(alpha = 0.3) +
  geom_smooth(method = "gam") +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy", subtitle = "Data Points are country-years", caption = "Source: Gapminder")

library(scales)
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = continent, fill = continent))
p + geom_point()
p + geom_point() + scale_x_log10(labels = dollar)
p + geom_point() + scale_x_log10(labels = dollar) + geom_smooth()
# Try it: What does fill = continent do? What do you think about the match of colors between lines and error bands?
# ？？？

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) + geom_smooth() + scale_x_log10()
# Try it: Notice how the above code leads to a single smooth line, not one per continent. Why?
# Because different continents' data are in the same dataframe, and we do not separate them when making the smooth line.



p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
  geom_smooth(mapping = aes(color = continent, fill = continent)) +
  scale_x_log10() +
  geom_smooth(mapping = aes(color = continent), method = "gam")
# Try it: What is bad about the following example, assuming the graph is the one we want?
# The above codes make regression line for each continent, and this makes the picture look very confusing. So we should set aesthetics at the top level rather than at the individual geometry level.

# Ecercise 2

bank_df <- read.csv("bank.csv")







