###############################################################################
# Exploartory Data Analysis
###############################################################################

# we will explore the heart dataset

heart=read.csv(file.choose())


# load our library
library(DataExplorer)
library(data.table)

## explore our dataset
names(heart)
head(heart)
str(heart)
summary(heart)

## changing our data type
heartDT=data.table(heart)



####################  grouping and frequency

drop_columns(heartDT, "fbs")

## grouping categories for discreate features

group_category(heartDT, "chest_pain" ,0) # view frequency


group_category(heartDT, "chest_pain", 0, "chol")  # view frequency based on another measure
group_category(heartDT, "chest_pain", 0, "age")


##################### plotting 

#discrete features (categorical data)

plot_bar(heartDT)

# continous features (numeric data)

plot_boxplot(heartDT, by="disease")   # disease is the categorical var

# correlation plot

plot_correlation(heartDT)

plot_correlation(heartDT, type="d")  # only discrete features

plot_correlation(heartDT, type="c")  # only continous features

# density plot

plot_density(heartDT)  # only for numerical columns

# histogram

plot_histogram(heartDT) # only for numeric columns

# scatterplot

plot_scatterplot(heartDT,"age")  # using age as y axis

# D3 network graph
# visualize structure of the dataset
plot_str(heartDT, type="radial")


######## spliting data into discrete and continous parts

# will generate 2 data tables for continous and discrete
 output=split_columns(heartDT)

output$discrete

output$continous


##### generate a report

create_report(heartDT, 
              output_file="report.html",
              output_dir="C://Users//user//Desktop")






