library(readr)

mydata=read_csv(file.choose())

read_tsv(file.choose()) #tab seperated values

spec_csv(file.choose())  # will display the class type of each column 
# useful before importing data or checking data
spec_csv(file.choose())   # chose airlines Bigdata


#### read columns

names2=read_csv(file.choose(), col_names=c("age","trestbps","fbs"), 
                na=c("NA","null"))

##### skip columns
heart22=read_tsv(file.choose(), col_names=FALSE,
			col_types=cols(
				X2=col_skip(),
				X3=col_skip(),
				X4=col_skip()
			))

read_csv2(file.choose())  # seperated by colon ;

read_table("filename") # txt files

####### text mining
tm1= read_lines(file.choose()) # returns a vector of strings representing each line in the file
             # reads line by line, not entire corpus, 

tm2= read_file(file.choose()) # reads entire file as one vector, with breaks represented as \n

####### writing files

write_csv(file,"filepath.csv")
write_csv(file,"filepath.csv", append=TRUE)  # append new data at end of old file

write_rds(file,"filepath.rds") #write a file with metadata and all information in rds file
read_rds #reads the rds file

####### convert data
heart33=type_convert(heart, col_types=cols(Girth="d", Height="d", Volume="d"))
# d her stands for double

# parse as factors
heart$disease=parse_factor(heart$chest_pain, levels=c("asympt", "atyp_angina", "non_anginal","type_angina"))
heart$sex=parse_factor(heart$disease, levels=c("male","female"))

# parse as dates
weather=read_csv(file.choose())
weather$date=parse_date(weather$date, format= "%m/%d/%Y")

# parse number  (useful when numbers stored as characters)
debt$amount=parse_number(debt$amount)