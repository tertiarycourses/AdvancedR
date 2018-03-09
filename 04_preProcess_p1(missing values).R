######################################################################
#             Data Preprocessing (PART 1)
######################################################################

# necessary to 'clean' the data before perfoming machine learning algorithms


################# DEALING WITH MISSING VALUES #####################


#install.packages("mice")
#install.packages("VIM")
library(mice)
library(VIM) 

## Create data set with missing values
set.seed(2) 
miss_mtcars <- mtcars 

some_rows <- sample(1:nrow(miss_mtcars), 7) 
miss_mtcars$drat[some_rows] <- NA

some_rows <- sample(1:nrow(miss_mtcars), 5) 
miss_mtcars$mpg[some_rows] <- NA

some_rows <- sample(1:nrow(miss_mtcars), 5)
miss_mtcars$cyl[some_rows] <- NA

some_rows <- sample(1:nrow(miss_mtcars), 3) 
miss_mtcars$wt[some_rows] <- NA

some_rows <- sample(1:nrow(miss_mtcars), 3) 
miss_mtcars$vs[some_rows] <- NA 

only_automatic <- which(miss_mtcars$am==0) 
some_rows <- sample(only_automatic, 4) 
miss_mtcars$qsec[some_rows] <- NA

nrow(miss_mtcars)

## Visualization of the missing pattern
md.pattern(miss_mtcars) #  Cells with a 1 represent nonmissing data; 0s represent missing data.
mpattern <- md.pattern(miss_mtcars)
sum(as.numeric(row.names(mpattern)), na.rm = TRUE)

aggr(miss_mtcars, numbers=TRUE) # visualize the missing data pattern graphically 


######### Challenge #########

#visualize the weather dataset for NA values

## Dealing with missing data
### Complete case analysis
mean(miss_mtcars$mpg)
mean(miss_mtcars$mpg, na.rm = TRUE)

m1 <- lm(mpg ~ am + wt + qsec, data = miss_mtcars, na.action = na.omit)


### Missing Data Imputation
#### Mean Substitution
mean_sub <- miss_mtcars
mean_sub$qsec[is.na(mean_sub$qsec)] <- mean(mean_sub$qsec, na.rm = TRUE)

### challenge
# replace the NA's in 2 other columns


#### Multiple Imputation
#### replace all columns with median
for(i in 1:ncol(mean_sub)){       # replaces all the NA with col means
  mean_sub[is.na(mean_sub[,i]),i]=median(mean_sub[,i],na.rm=TRUE)
}


# replace miss_mtcars
for(i in 1:ncol(miss_mtcars)){       # replaces all the NA with col means
  miss_mtcars[is.na(miss_mtcars[,i]),i]=median(miss_mtcars[,i],na.rm=TRUE)
}

##################################################################################
#                     replacing NA
##################################################################################

data[is.na(data)]=0  
# replace with zero don't interfere with calculations

###################################################################################
#                     replace Inf values with NA
###################################################################################

is.na(data)=sapply(data, is.infininte)
# replace inf or -inf with NA

##### dealing with NAs
is.na(mydata) # checks if there are any NAs in the data

apply(data,2,function(x) sum(is.na(x))) # check if NA in all the columns

# visualize the NAs
library(mice)
library(VIM) 

aggr(mydata, numbers=TRUE) # visualize the missing data pattern graphically 
# typically we remove columns with NA > 5% (our choice)


#if few NAs that affect out data a little
newdata=na.omit(mydata)  # removes all the data with NA values

# if we want to replace the NAs with zero
data[is.na(mydata)]=0   # replaces all the NA with zero

mydata[] <- lapply(mydata, function(x) ifelse(is.na(x), round(mean(x, na.rm = TRUE),1), x))

# median better choice than mean


