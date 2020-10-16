
######################################################################
#             Data Preprocessing (PART 1)
######################################################################

# necessary to 'clean' the data before perfoming machine learning algorithms


################# DEALING WITH OUTLIERS #####################


########## examples > to identify outliers

######### Univariate - ESD method

x=c(rnorm(100), 150)	# x has 10 normal values w mean 0 and the 150 is a outlier

boxplot(x)
x

t=2
m=mean(x)
s=sd(x)

b1=m - s*t
b2=m + s*t

y=ifelse(x >=b1 & x <=b2, 0, 1)   # 1 is an outlier
                                   # we see that 100 is an outlier
table(y)

##### challenge
# import heart dataset and identify outliers as above in the
# chol column using 2 sd

##### Univariate - Boxplot method

boxplot(x)
boxplot.stats(x)


# for(i in 1:ncol(dataset)){
#  print(colnames(dataset)[i])
#  print(boxplot.stats(dataset[,i]))
# }

####### Univariate - use outliers package
library(outliers)

dixon.test(x)  # column by column

# for(i in 1:ncol(dataset)){
#  print(colnames(dataset)[i])
#  print(dixon.test(dataset[,i]))
# }

grubbs.test(x, type=11, two.sided=T)  # type 11 for 2 sides


######### multivariate - use mvoutlier package
library(mvoutlier)

moss # dataset
elements=data.frame(Hg=moss$Hg, Fe=moss$Fe, Al=moss$Al, Ni=moss$Ni)
head(elements)    # moss is a dataset

myout=sign1(elements[ ,1:4], qqcit=0.975)   # get 0 or 1 > clasification if outlier or not

# myout2=pcout(elements[ ,1:4])

plot(moss$Fe, moss$Al, col=myout$wfinal01+2)


##### Challenge

# use mtcars
# perform boxplot stats & dixon test
# perform univariate & multivariate outlier test
