library(ggplot2)

# 1.1 x mapped to wt, y mapped to mpg and point geometry used
ggplot(mtcars) + aes(x=wt, y = mpg) + geom_point()

# 1.2 x mapped to wt and histogram geometry is used
ggplot(mtcars) + aes(x=wt) + geom_histogram(bins=10)

# 1.3 x mapped to cyl and bar geometry is used
ggplot(mtcars) + aes(x = factor(cyl)) + geom_bar()

# 1.4 x mapped to cyl, y mapped to mpg and boxplot geometry used
ggplot(mtcars) + aes(x = factor(cyl), y = mpg) + geom_boxplot(fill='green')

# 1.5 x mapped to wt, y mapped to mpg and point and line geometry used
ggplot(mtcars) + aes(x=wt, y = mpg) + geom_point() + geom_line()

# 2. x mapped to wt, y mapped to mpg, and color mapped to am
ggplot(mtcars) + aes(x=wt, y = mpg, col = factor(am)) + geom_point()

# 3. x mapped to wt, y mapped to mpg, color mapped to am, shape mapped to cyl, size
# mapped to vs
ggplot(mtcars) + aes(x=wt, y = mpg, col = factor(am), shape = factor(cyl),
                     size = factor(vs)) + geom_point()

# 4. To assign a constant value to an attribute, put it inside the geom 
ggplot(mtcars) + aes(x=wt, y = mpg, col = factor(am), shape = factor(cyl)) +
                    geom_point(size = 3)

# 5. To change x and y scale use appropriate scale command
ggplot(mtcars) + aes(x=wt, y = mpg, col = factor(am), shape = factor(cyl)) +
  geom_point(size = 3) + scale_x_continuous(limits=c(1,6)) +
  scale_y_continuous(limits=c(5,40))

# 6. Setting legend properties using guide
ggplot(mtcars) + aes(x=wt, y = mpg, col = factor(am), shape = factor(cyl)) +
  geom_point(size = 3) + guides(col = guide_legend('am'), shape=guide_legend('cyl'))

# 7. Setting axis properties using theme
ggplot(mtcars) + aes(x=wt, y = mpg, col = factor(am), shape = factor(cyl)) +
  geom_point(size = 3) + guides(col = guide_legend('am'), shape=guide_legend('cyl')) +
  theme(axis.text=element_text(size = 15), axis.title=element_text(size=20))


## 8. Plotting subset of data in a separate panel: Facetting
ggplot(mtcars) + aes(x=wt, y=mpg) + 
  geom_point(size=3, color = "blue", shape = 17) +
  facet_wrap( ~ cyl)


## Challenge: 
### 1. Plot Day vs Ozone data for airquality dataset
### 2. Use different colors for different months
### 3. Use different panels for different months

