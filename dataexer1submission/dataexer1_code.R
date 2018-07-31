library("tidyverse")

refine_original<- read.csv("dataexer1submission/refine_savedasCSV.csv",stringsAsFactors = FALSE)
refine_draft <- refine_original

#correct company names
refine_draft$company[agrep("philips", refine_draft$company, ignore.case = TRUE, value = FALSE, max.distance = 3)] <- "philips"
refine_draft$company[agrep("van houten", refine_draft$company, ignore.case = TRUE, value = FALSE, max.distance = 1)] <- "van houten"
refine_draft$company[agrep("unilever", refine_draft$company, ignore.case = TRUE, value = FALSE, max.distance = 1)] <- "unilever"
refine_draft$company[agrep("akzo", refine_draft$company, ignore.case = TRUE, value = FALSE, max.distance = 1)] <- "akzo"

#2 separate values in Product.code...number
refine_draft <- separate(data=refine_draft, col="Product.code...number", into = c('product_code', 'product_number'), sep = "-", remove=FALSE)

#add product category column
refine_draft <- mutate(refine_draft, 'product_category' = product_code)

#3 convert codes to category names
refine_draft$product_category <-gsub("p", "Smartphone", refine_draft$product_category)
refine_draft$product_category <-gsub("v", "TV", refine_draft$product_category)
refine_draft$product_category <-gsub("x", "Laptop", refine_draft$product_category)
refine_draft$product_category <-gsub("q", "Tablet", refine_draft$product_category)

#4 make full address column
refine_draft <- unite(refine_draft, "full_address", address, city, country, sep = ",")

#create binary dummy variables for company
dummy_philips <- as.numeric(refine_draft$company == "philips")
dummy_akzo <- as.numeric(refine_draft$company == "akzo")
dummy_van_houten <- as.numeric(refine_draft$company == "van houten")
dummy_unilever <- as.numeric(refine_draft$company == "unilever")

#create binary dummy variables for product category
dummy_product_smartphone <- as.numeric(refine_draft$product_category == "Smartphone")
dummy_product_tv <- as.numeric(refine_draft$product_category == "TV")
dummy_product_laptop <- as.numeric(refine_draft$product_category == "Laptop")
dummy_product_tablet <- as.numeric(refine_draft$product_category == "Tablet")

#add dummary variables to the table
refine_draft <- cbind(refine_draft, dummy_philips, dummy_akzo, dummy_van_houten, dummy_unilever, dummy_product_smartphone, dummy_product_tv, dummy_product_laptop, dummy_product_tablet)
refine_draft

refine_clean <- refine_draft
write.csv(refine_clean, "dataexer1submission/refine_clean.csv")
