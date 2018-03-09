####################################################################################################
# csv file
######################################################################################################
data1 <- read.csv("C://Users//user//Desktop//tertiary//Rprogramming//RdataAnalysis//datasets//weather.csv", header = TRUE)
data1=read.csv(file.choose(), header=TRUE)

### view data
View(data1)
head(data1)
summary(data1)
str(data1)
levels(data1$chest_pain)


write.csv(data1, file = "redata1.csv", quote = FALSE, row.names = FALSE)

data2 <- read.table("data1.csv", header = TRUE, sep = ",")
head(data2)
write.table(data2, file = "redata2.csv", quote = FALSE, row.names = FALSE, sep = ",")

#######################################################################################################
# JSON file
#######################################################################################################
install.packages("jsonlite")
library(jsonlite)

employees.json <- '{
  "employees":[
  {"firstName":"John", "lastName":"Doe"},
  {"firstName":"Anna", "lastName":"Smith"},
  {"firstName":"Peter", "lastName":"Jones"}
  ]
  }'

employee <- fromJSON(employees.json)  # read in the string
employee
names(employee)
employee$employees

jsonIris <- toJSON(iris[1:3,], pretty = TRUE)
jsonIris
fromJSON(jsonIris)

write(jsonIris, file = "iris.json")  # write json data to a file
fromJSON("iris.json")                # read json data from a file

###########################################################################################################
## XML file
###########################################################################################################
employee.xml <- '<employees>
    <employee>
        <firstName>John</firstName> <lastName>Doe</lastName>
    </employee>
    <employee>
        <firstName>Anna</firstName> <lastName>Smith</lastName>
    </employee>
    <employee>
        <firstName>Peter</firstName> <lastName>Jones</lastName>
    </employee>
</employees>'

install.packages("XML")
library(XML) 
employee <- xmlTreeParse(employee.xml)
names(employee)
employee$doc

class(employee)

xmltop <- xmlRoot(employee)
xmltop[2]

empName <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
empName

empNameDf <- data.frame(t(empName), row.names = NULL)
empNameDf


#########################################################################################################
# Reading data from web
#########################################################################################################
browseURL("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine=read.table(url, nrows=5, header = FALSE, sep = ",")

## Challenge: download the boston housing dataset and save it under the name house
boston <- "https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"

###########################################################################################################
# Reading data using API
###########################################################################################################
# Application Programming Interface

URLencode("The Beatles")

create_artist_query_url_lfm <- function(artist_name){ 
  prefix <- "http://ws.audioscrobbler.com/2.0/?method=artist.gettoptags&artist="  
  postfix <- "&api_key=c2e57923a25c03f3d8b317b3c8622b43&format=json"  
  encoded_artist <- URLencode(artist_name)  
  return(paste0(prefix, encoded_artist, postfix)) 
}

create_artist_query_url_lfm("Depeche Mode") 
fromJSON(create_artist_query_url_lfm("Depeche Mode"))

get_tag_vector_lfm <- function(an_artist){  
  artist_url <- create_artist_query_url_lfm(an_artist)  
  json <- fromJSON(artist_url)  
  return(json$toptags$tag$name) 
}

get_tag_vector_lfm("Depeche Mode")

our_artists <- list("Kate Bush", "Peter Tosh", "Radiohead", 
                    "The Smiths", "The Cure", "Black Uhuru") 

our_artists_tags <- lapply(our_artists, get_tag_vector_lfm) 
names(our_artists_tags) <- our_artists
print(our_artists_tags)
