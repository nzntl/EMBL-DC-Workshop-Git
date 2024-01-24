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
