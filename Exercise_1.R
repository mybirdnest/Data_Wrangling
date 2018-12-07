library(tidyverse)

#0: Load the data in RStudio
refine_original <- read_csv("refine_original.csv")

#1: Clean up brand names
refine_clean <- 
  refine_original %>% 
  mutate(company = tolower(company)) %>%
  mutate(company = ifelse(
    substr(company, 1, 1) == "p" | substr(company, 1, 1) == "f", "philips", company)) %>%
  mutate(company = ifelse(
    substr(company, 1, 1) == "a", "akzo", company)) %>%
  mutate(company = ifelse(
    substr(company, 1, 1) == "u", "unilever", company)) %>%
  mutate(company = ifelse(
    substr(.$company, 1, 1) == "v", "van houten", company))

#2: Separate product code and number
refine_clean <- refine_clean %>% separate('Product code / number', c("product_code", "product_number"), sep = "-")

#3: Add product categories
refine_clean <- refine_clean %>%
  mutate(Product_Category = case_when(
    product_code %in% "p" ~ "Smartphone",
    product_code %in% "v" ~ "TV",
    product_code %in% "x" ~ "Laptop",
    product_code %in% "q" ~ "Tablet")
  )

#4: Add full address for geocoding
refine_clean <- refine_clean %>% unite(`Full Address`, c(address, city, country), sep = ",", remove = TRUE)

#5: Create dummy variables for company and product category
refine_clean <- refine_clean %>%
  mutate(
    company_philips = ifelse(company == "philips", 1, 0),
    company_akzo = ifelse(company == "akzo", 1, 0),
    company_van_houten = ifelse(company == "van houten", 1, 0),
    company_unilever = ifelse(company == "unilever", 1, 0),
    product_smartphone = ifelse(product_code == "p", 1, 0),
    product_tv = ifelse(product_code == "v", 1, 0),
    product_laptop = ifelse(product_code == "x", 1, 0),
    product_tablet = ifelse(product_code == "q", 1, 0)
  )

#6: Submit the project on Github
write_csv(refine_clean, file = "refine_clean.csv", row.names = FALSE)