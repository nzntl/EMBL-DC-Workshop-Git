#dataframes
#import
download.file(url= "https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")

library(tidyverse)


surveys <- read_csv("data_raw/portal_data_joined.csv")

head(surveys) 
view(surveys)
str(surveys)
dim(surveys)
nrow(surveys)
ncol(surveys)
tail(surveys)
colnames(surveys)
summary(surveys)

# indexing & subsetting 
surveys[1, 6]

surveys[1, ]
surveys[ ,1]

surveys[c(1,2,3), c(5,6)]
surveys[1:4, 5:9]

surveys[, -1&-5]

surveys$weight

#challange
surveys_200 <- read_csv("data_raw/portal_data_joined.csv")
surveys_200 <- surveys_200[200, ]
surveys_200

summary(surveys_200)

nrow(surveys)
surveys[nrow(surveys)/2, ]

my_list <- list(names = c("nelly", "jan"),
                money = c(1, 2, 3, 4, 5, 6))

my_list [[1]]

str(surveys)

surveys$sex <- factor(surveys$sex)
levels(surveys$sex)
nlevels(surveys$sex)

sex <- factor(c("male", "female", "female", "male"))
sex <- factor(sex, levels= c("male", "female"))

#challange
surveys$taxa <- factor(surveys$taxa)
surveys$genus <- factor(surveys$genus)

# why do both work??
nlevels(genus)
nlevels(surveys$genus)

levels(taxa)
levels(surveys$taxa)

sum(surveys$taxa == "Rabbit")
sum(taxa == "Rabbit")

summary(surveys)
summary(surveys$taxa)


#factor
as.character(sex)

#i dont get it
year_fct <- factor(c(1990, 1983, 1977, 1997))
as.numeric(year_fct)

as.numeric(as.character(year_fct))
as.numeric(levels(year_fct))[year_fct]


#renaming factors
plot(surveys$sex)
summary(surveys$sex)
sex <- surveys$sex
levels(sex)
sex <- addNA(sex)
levels(sex)
levels(sex)[3] <- "undetermined"

plot(sex)

#challange2
levels(sex)[1:2] <- c("female", "male")

sex <- factor(sex, levels = c("undetermined", "female", "male"))
plot(sex)
              