myName <- "Runci Hu"

library(tidyverse)
library(magrittr)
library(readxl)

##Preparation work, r package and reading data. 
strawb <- read_xlsx("/Users/runcihu/Desktop/strawberries-2022oct30-a.xlsx",
                    col_names = TRUE)

##Get the column names and index them, then find unique variable of each column. 
cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]

unique(strawb[1])
unique(strawb[2])
unique(strawb[3])

##Re-arrange the data frame by year and state, separate the column, 
##and clean the data
T <- NULL
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}
drop_cols <- cnames[which(T == 1)]
strawb %<>% select(!all_of(drop_cols))
strawb %<>% arrange(Year, State)

colnames(strawb)
temp1 <- strawb %>% select(`Data Item`) %>% 
  distinct()

strawb2 <- strawb %>% separate(col=`Data Item`, 
                               into = c("Strawberries", "items", "units"), 
                               sep = ",",
                               fill = "right")
strawb3 <- strawb %>% separate(col=`Data Item`,
                               into = c("Strawberries", "type", "items", "units"),
                               sep = ",",
                               fill = "right")
rm(strawb2, strawb3)

strawb %<>% separate(col=`Data Item`,
                     into = c("Strawberries", "type", "items", "units"),
                     sep = ",",
                     fill = "right")

# Q1
# 285CWT=285*100lb

# Q2. Compute a 95% confidence interval for California organic strawberry sales in 2016.
filter_q2 <- filter(strawb, Year == "2016" & 
                      State == "CALIFORNIA" & 
                      Domain == "ORGANIC STATUS")
margin_error <- 231304956*1.96*0.137
lower.bound <- 231304956 - margin_error
upper.bound <- 231304956 + margin_error
print(c(lower.bound, upper.bound))     ##169194949 293414963

# Q3. Compute a 95% confidence interval for California non-organic strawberry sales in 2016.
filter_q3 <- filter(strawb, Year == "2016" & 
                      State == "CALIFORNIA" & 
                      Domain != "ORGANIC STATUS")
# filter_q3 <- filter (filter_q3, Value != "(NA)" & Value != "(D)", Domain != "TOTAL")


# install.packages("gmodels")
# install.packages("Rmisc")
library(gmodels)
library(Rmisc)
CI(as.numeric(filter_q3$Value))     ##upper mean lower NA NA NA 
##Because the date set have NA variable in it, the CI contains NA values, 
##that in mean(x) : NAs introduced by coercion 

# Q4. How many different chemicals are listed?
unique(strawb[10])     ##7
filter_q4 <- filter(strawb, Domain != 'ORGANIC STATUS' & 
                      Domain != 'TOTAL')
unique(filter_q4[11])     ##175
grep("TOTAL", filter_q4$`Domain Category`, ignore.case = T)     ##36

Ans_4 <- 175-36
Ans_4    ##139

# Q5. How many more chemicals have been used in California than in Florida?
FL <- filter(strawb, State == 'FLORIDA' & 
               Domain != 'ORGANIC STATUS' & 
               Domain != 'TOTAL')
CA <- filter(strawb, State == 'CALIFORNIA' & 
               Domain != 'ORGANIC STATUS' & 
               Domain != 'TOTAL')
grep("TOTAL", FL$`Domain Category`, ignore.case = T)     ##16
unique(FL[11])     ##119

grep("TOTAL", CA$`Domain Category`, ignore.case = T)     ##16
unique(CA[11])     ##142

Ans_5 <- (142-16)-(119-16)
Ans_5      #23

