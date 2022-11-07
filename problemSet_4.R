library(magrittr)
library(readr)
library(tidyr)
library(dplyr)

##0
myName <- "Runci Hu"

##1
print_order <- function(x){
  y <- c()  
  max <- max(x)
  min <- min(x)
  for (i in 1:3){
    if (x[i] == max){y[1] = x[i]}
    else if (x[i] == min){y[3] = x[i]}
    else{y[2] = x[i]}
    }
  return(y)
}

##2 
print_string <- function(x) {
  for (i in 1:x) {
    if (i %% 3 ==0 && i %% 5 ==0) {
      print("Unknow")
    }
    else if (i %% 3 ==0) {
      print("Yes")
    }
    else if (i %% 5 ==0) {
      print("No")
    }
    else {
      print(i)
    }
  }
}
print_string(5)

##3
factor <- NULL
calc_sum_of_factor <- function(x) {
  for (i in 1:x) {
    if (x %% i ==0){
    factor <- append(factor, i)
    }
  }
  sum(sapply(factor**2, sum))
}
calc_sum_of_factor(12)

##4
intersect <- NULL
find_intersect <- function(x, y, z){
  for (i in x) {
    if (i %in% y & i %in% z){
      intersect <- append(intersect, i)
    }
  }
  print(intersect)
}

##5
factorial <- 1
factorial_base <- function(x){
  for (i in 1:x) {
    factorial <- factorial * i
  }
  print(factorial)
}

##6 
T <- function(n){
  ans = n*(n+1)/2
  return(ans)
}

perfect_sqr <- function(x){
  sqrt <- sqrt(x)
  
  ifelse(sqrt == trunc(sqrt), return(TRUE),return(FALSE))
}

num_tri_sqr <- function(n) {
  vec <- c()
  logic <- c()
  for( i in 1:n) {
    vec[i] = T(i)
    logic[i] = perfect_sqr(T(i))
  }
  index <- which(logic == TRUE)
  return(vec[index])
}

q6_sum <- sum(num_tri_sqr(1500000))
# n_term <- NULL
# T_n <- function(x){
#   for (i in 1:x) {
#     sum <- i * (i+1)/2
#     n_term <- append(n_term, sum)
#   }
#   return(n_term)
# }
# 
# perfect_sqr <- function(x){
#   for (j in T_n(x)) {
#     if (j ** (1/2) == trunc(j**(1/2))){
#       print("TRUE")
#     }else {
#       print("FALSE")
#     }
#   }
# }
# 
# all_term <- NULL
# num_tri_sqr <- function(x){
#   for (j in T_n(x)) {
#     if (j**(1/2) == trunc(j**(1/2))){
#       all_term <- append(all_term, j)
#     }
#   }
#   return(all_term)
# }
# 
# q6_sum <- sum(num_tri_sqr(1500000))
# q6_sum

##2022 H-1B Employer Data Hub:
##1
h1b_2022 <- read_csv('https://www.uscis.gov/sites/default/files/document/data/h1b_datahubexport-2022.csv')

##2
##Read

##3
na_num <- sum(is.na(h1b_2022))
na_num
h1b_2022a <- h1b_2022 %>% drop_na()
h1b_2022a <- h1b_2022a[h1b_2022a$City !="-",]

##4
df_num <-h1b_2022a %>% 
  group_by(State) %>% 
  summarise('Init App' = sum(`Initial Approval`) + sum(`Initial Denial`),
            'Conti App' = sum(`Continuing Approval` + `Continuing Denial`),
            Approve = sum(`Initial Approval`),
            Denial = sum(`Initial Denial`))
df_num

##5
app_num <- sum(as.integer(df_num$Approve))
den_num <- sum(as.integer(df_num$Denial))

##6
city_num <- h1b_2022a %>% 
  select(3,10) %>% group_by(City) %>% 
  count(`Initial Approval`) %>% 
  arrange(City) %>% 
  transmute(Count=sum(n)) %>% 
  unique()

##7
visa_num <- h1b_2022a %>%
  group_by(NAICS) %>%
  arrange(NAICS) %>%
  count(NAICS) %>%
  transmute(Number=sum(n)) %>%
  unique()
visa_num$Percentage <- round(visa_num$Number *100 / sum(visa_num$Number), digits = 3)

