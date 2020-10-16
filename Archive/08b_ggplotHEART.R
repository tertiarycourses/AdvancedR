library(ggplot2)

dahc.lab2=read.csv(file.choose(), header=TRUE) # choose the dahc dataset
dahc.lab2=subset(dahc.lab2,TG<500)

#### you will perform on the heart dataset for practise

# 1a. Scatter plot
ggplot(dahc.lab2) + aes(x=TG, y = TC) +
  geom_point()

# 1b. Scatter plot with colour and different shape
ggplot(dahc.lab2) + aes(x=TG, y = TC) + 
  geom_point(size=4, colour="green", shape = 17)

# defining the size and colour
ggplot(dahc.lab2) + aes(x=TG, y = TC, colour=factor(GENDER)) + 
  geom_point()  # different colour based on vs column

ggplot(dahc.lab2) + aes(x=TG, y = TC, colour=factor(ETHNIC)) + 
  geom_point()


# this column vs values is numeric
ggplot(dahc.lab2) + aes(x=TG, y = TC, colour=HDL) + 
  geom_point()

# now we have a range of values, so we get rid of factor part
ggplot(dahc.lab2) + aes(x=TG, y = TC, colour=HDL, shape=factor(ETHNIC)) + 
  geom_point()


ggplot(dahc.lab2) + aes(x=TG, y = TC, 
                        colour=HDL, 
                        shape=factor(ETHNIC), 
                        size=factor(GENDER)) + 
  geom_point()

# using multiple variables

# 2. Bar Plot
ggplot(dahc.lab2, aes(x = factor(ETHNIC))) + 
  geom_bar(width = 0.5, color = "black", fill = "red")


ggplot(dahc.lab2, aes(x = factor(ETHNIC), fill=factor(GENDER))) + 
  geom_bar(width = 0.5, color = "black")


ggplot(dahc.lab2, aes(x = factor(ETHNIC), fill=factor(GENDER)), position = position_stack(reverse = TRUE)) + 
  geom_bar(width = 0.5, color = "black") +
  coord_flip() +
  theme(legend.position = "top")

# mean.mpg <- tapply(mtcars$mpg, mtcars$cyl, mean)
# mean.mpg
# names(mean.mpg)
# ggplot( mean.mpg, aes(x = factor( cyl), y = mean(mpg))) + 
#   geom_bar(stat = "identity", width = 0.5, color = "black", fill = "red")

# 3. Histogram
# using bin width
ggplot(dahc.lab2, aes(x = TG)) + 
  geom_histogram(binwidth = 3, color = "black", fill = "red")

ggplot(dahc.lab2, aes(x = TG)) + 
  geom_histogram(binwidth = 20, color = "black", fill = "red")

# using number of bins
ggplot(dahc.lab2, aes(x = TG)) + 
  geom_histogram(bins = 10, color = "black", fill = "blue")


# 4. Boxplot
ggplot(dahc.lab2, aes(x = factor(ETHNIC), y = TG)) + 
  geom_boxplot(width = 0.6, color = "black", fill = "red")

# divide by factor, other all the values in one plot
ggplot(dahc.lab2, aes(x = TC, y = TG)) + 
  geom_boxplot(width = 0.9, color = "black", fill = "red")   # we get warning

#df=melt(dahc.lab2, id=c("GENDER","ETHNIC"),measure.vars=c("SBP1","DBP1","HT","WT","TG","TC"))
#ggplot(df, aes(ETHNIC, value, fill=variable)) + 
#   geom_bar(position="dodge")


# 5. scatter plot
ggplot(dahc.lab2) + aes(x=TC, y=TG) + geom_point()

## Setting the size of the points in the scatter plot
ggplot(dahc.lab2) + aes(x=TC, y=TG) + geom_point(size=3)

## Setting the color of the points in the scatter plot
ggplot(dahc.lab2) + aes(x=TC, y=TG) + geom_point(size=3, colour = "blue")

## Setting the shape of the points in the scatter plot
ggplot(dahc.lab2) + aes(x=TC, y=TG) + geom_point(size=3, colour = "blue", shape = 17)

## Changing labels
ggplot(dahc.lab2) + aes(x=TC, y=TG) + geom_point() +
  labs(x = "TC", y = "TG", title = "TC vs TG") +
  theme(axis.text = element_text(colour = "blue"),
        axis.title = element_text(size = rel(1.5), angle = 0),
        plot.title = element_text(size = rel(2.5), colour = "green"))
  
## Changing x and y scale
ggplot(dahc.lab2) + aes(x=TC, y=TG) + geom_point() +
  scale_x_continuous(limits = c(100,350)) +
  scale_y_continuous(limits = c(5,35))


## Grouping data points by variable
ggplot(dahc.lab2) + aes(x=TC, y=TG, colour = HDL ) + 
  geom_point(size=3, shape = 17)

ggplot(dahc.lab2) + aes(x=TC, y=TG, colour = HDL, shape = factor(ETHNIC)) + 
  geom_point(size=2)

ggplot(dahc.lab2) + aes(x=TC, y=TG, colour = HDL, shape = factor(ETHNIC)) + 
  geom_point(size=5) + scale_shape_manual(values = c(4,5,6,7))

#ggplot(dahc.lab2) + aes(x=TC, y=TG, colour = HDL, shape = factor(ETHNIC)) + 
#  geom_point(size=2) + scale_shape_manual(values = c(17,18,19,20)) +
#  scale_colour_brewer(palette = "Set1")

## Adding trend line
ggplot(dahc.lab2) + aes(x=TC, y=TG) + 
  geom_point(size=3, colour = "blue", shape = 17) +
  stat_smooth(method = "lm")

ggplot(dahc.lab2) + aes(x=TC, y=TG) + 
  geom_point(size=3, colour = "blue", shape = 17) +
  stat_smooth(method = "lm", se = FALSE)


## Faceting 1
ggplot(dahc.lab2) + aes(x=TC, y=TG) + 
  geom_point(size=3, colour = "blue", shape = 17) +
  facet_grid(. ~ ETHNIC)  # plot by columns

ggplot(dahc.lab2) + aes(x=TC, y=TG) + 
  geom_point(size=3, colour = "blue", shape = 17) +
  facet_grid( ETHNIC ~ .)   # plot by rows

ggplot(dahc.lab2) + aes(x=TC, y=TG) + 
  geom_point(size=3, colour = "blue", shape = 17) +
  facet_grid(GENDER ~ ETHNIC) #plot interaction btw BloodPscale and Ethnic


ggplot(dahc.lab2) + aes(x=TC, y=TG, color=SBP1) + 
  geom_point() +
  facet_grid(GENDER ~ ETHNIC)

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
  facet_wrap( ~ cyl)+
  ggtitle("testing")


## waterfall chart

# sample data

balance=data.frame(id=seq(1,8), desc=c("Start cash", "Sales","Refunds","Payouts","Losses","Wins","Contracts","End cash"),
                   type=c("net","in","out","out","out","in","in","net"),
                   start=c(0,2000,5400,4300,4200,-2400,1400,2800),
                   end=c(2000,5400,4300,4200,-2400,1400,2800,0),
                   amount=c(2000,3400,-1100,-10,-6600,3800,1400,2800))

ggplot(balance, aes(desc, fill=type)) + 
  geom_rect(aes(x=desc, xmin=id -0.45, xmax=id + 0.45, ymin=end, ymax=start))



## Bubble plot

ggplot(mtcars, aes(carb,gear, size=mpg, label=vs)) + 
  geom_point(colour="red") + 
  geom_text(size=3) + xlab("mtcars CARB") + ylab("mtcars GEAR")

ggplot(mtcars, aes(wt,mpg)) + geom_point(aes(size=qsec))


#### heat maps
ggplot(mtcars, aes(x=wt, y=gear, group=gear)) + geom_tile(aes(fill=mpg)) + 
  scale_fill_gradient(low="white", high="red")

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, group=Species)) + 
  geom_tile(aes(fill=Petal.Length)) + scale_fill_gradient(low="white", high="red")


## Consultant chart

country=c("Australia","Canada","USA","Germany","France","Brazil","China",
          "India","Indonesia","Africa")

dd3=data.frame(var=country, value=sample(10, replace=T))
ggCONSULT=ggplot(dd3, aes(x=factor(var),y=value,fill=factor(var))) + geom_bar(width=1, stat='identity')+
  scale_y_continuous(breaks = 0:10) + coord_polar() + labs(x = "", y = "") + 
  theme(legend.position = "none",axis.text.x =element_blank(), axis.text.y = element_blank(),
  axis.ticks = element_blank()) +
  geom_text(aes(x=var, y=value, label=var))

