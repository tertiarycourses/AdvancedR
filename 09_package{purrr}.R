######################### purrr package #####################################

library(tidyverse)   # using the purrr package
# map() returns a list or dataframe
# map_lgl() returns a logical vector
# map_int() returns a integer value
# map_dbl() returns a double vector
# map_chr() returns a character vector

# can include na.rm=TRUE argument

### apply functions

map(heart, summary) # find a summary of each column
map_lgl(heart, is.numeric) # find columns that are numeric (return logical)
map_chr(heart, typeof) # find the type of each column (return charaacter)
map_dbl(heart, mean) # find column means
map_dbl(heart, sd) # find column std dev
map_dbl(heart, quantile, probs=c(0.05)) # find 5th percentile

### nesting data
# moves groups into cells as data.frames

n_heart <- heart %>%
  group_by(chest_pain) %>%
  nest()

## udf

# create a model for each chestpain

mod_fun=function(x) lm(chol~ age + trestbps + thalach, data=x)

model_heart=n_heart %>% mutate(model=map(data, mod_fun))
# use "data" to symbolize the data

# find the co-efficients of the models

b_fun <- function(mod)
  coefficients(mod)[[1]]

model_heart %>% transmute(chest_pain,
                     beta = map_dbl(model, b_fun))


### filetering data

pluck(heart,"age")   # get values in "age"

old=function(x){x>50}

keep(heart$age, old)   # keep elements that pass a logical test

discard(heart$age, old)  # remove elements that pass a logical test



library(reshape2)   # use this dataset
weather=airquality

compact(weather)    # remove empty elements


miss=function(x){isTRUE(is.na(x))==FALSE}  # checking if it is NA or not

head_while(weather$Ozone, miss)



### summarize data

every(heart$age, old) # do all elements pass a test

some(heart$age, old) # do some elements pass a test

detect(heart$age, old)   # find first element that pass a test

detect_index(heart$age, old)

### reshaping data

flatten(heart)

transpose(heart)


### transform data

add.2=function(x){x+2}

modify(weather, add.2)   # every element is list is added by 2

modify_at(weather, "Ozone", add.2)  # only Ozone column is modified



high.10=function(x){x>10}

modify_if(heart, is.numeric, add.2) 
# only elements in numeric columns be added by 2




# map to call a UDF
map(data, function(x) lm(chol~ age + trestbps + thalach, data=x))
    
# further functions with map
models=map(cyl, ~lm(mpg ~ wt, data=df))
coef=map(models, coef) # get the coeffecients of each model
map(coef, "wt") # extract the wt coeefecient
map_dbl(coef,2) # pull out the 2nd element
    

    
# using multiple arguments with map
n=list(5,10,20)
mu=list(1,5,10)
sd=list(0.1,1,0.1)
    
# other functions in purrr package
    
# pmap takes a list of arguments as input
pmap(list(n, mu, sd), rnorm) 