#loading library
library(dplyr)
library(tidyr)

#load data in RStudio called refine_original.csv
getwd()
setwd("/Users/justinvhuang/Desktop/Springboard")
elec <- read.csv("refine_original.csv")
elec

#clean up brand names, so mispelling and brand name standarized and lowercase 
elec$company <- tolower(elec$company)
elec$company
elec$company <- sub(pattern = ".*\\ps$" , replacement = "philips", x = elec$company)
elec$company
elec$company <-sub(pattern = "^ak.*", replacement = "akzo", x = elec$company)
elec$company
elec$company <- sub(pattern = "^van.*", replacement = "van houten", x = elec$company)
elec$company
elec$company <- sub(pattern = "^uni.*", replacement = "unilever", x = elec$company)
elec

#Separate produce code and number add two new columns product_code and product_number
elec <- separate(elec, "Product.code...number", c("product_code", "product_number"), sep = "-")
elec
#Add product categories p = Smartphone, v = TV x = Laptop, q = Tablelet
elec$product_category <- sub(pattern = "^p$", replacement = "Smartphone", sub("^v$", "TV", sub("^x$", "Laptop", sub("^q$", "Tablet", elec$product_code))))
elec
#Add full address for geocoding create new column full_address concatenates 3 addresses address, city, country seperated by commas
elec <- elec %>% mutate(full_address = paste(address, city, country, sep = ", "))
head(elec)
#create dummy variable for company and product category company_and product_i.e.
#add four binary columns for company_philips, company_akzo, company_van_houten, company_unilever
#product_smartphone, product_tv, product_laptop and product_tablet
elec <- mutate(elec, company_philips = ifelse(company == "philips", 1, 0))
elec <- mutate(elec, company_akzo = ifelse(company =="akzo", 1, 0))
elec <- mutate(elec, company_van_houten = ifelse(company == "van houten", 1, 0))
elec <- mutate(elec, company_unilever = ifelse(company == "unilever", 1, 0))
elec <- mutate(elec, product_smartphone = ifelse(product_category == "Smartphone", 1, 0))
elec <- mutate(elec, product_tv = ifelse(product_category == "TV", 1, 0))
elec <- mutate(elec, product_laptop = ifelse(product_category == "Laptop", 1, 0))
elec <- mutate(elec, product_tablet = ifelse(product_category == "Tablet", 1, 0))
str(elec)

#Submit original data as csv refine_original.csv and refine_clean.csv 

write.csv(elec, "refine_clean.csv")


