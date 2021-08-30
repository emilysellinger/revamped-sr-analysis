# Lifespan summary 

# Load data --------------------------------------------------------------------------------------------------
taxonomy <- read.csv(here("data", "taxonomy.csv")) # taxonomic info for each species

stock_info <- read.csv(here("data", "stock_info.csv")) # information for each stock, links to taxonomic data


# Match taxonomic info to current stocks ---------------------------------------------------------------------

# in the taxonomy and stock info datasets, there are some discrepancies in the naming conventions for some stocks
# a - is used instead of . To deal with this, going to replace the . w/ -
filtered_stocks$stock_name <- str_replace_all(filtered_stocks$stock_name, "\\.", "-")

lifespan <- tibble(stock_name = "",
                         scientific_name = "",
                         common_name = "",
                         region = "",
                         tsn = numeric())
# match data
for(x in filtered_stocks$stock_name){
  
  # find row in stock info dataset
  row <- which(stock_info$stockid == x)
  
  lifespan <- lifespan %>%
    add_row(stock_name = x, scientific_name = stock_info[row, "scientificname"], common_name = stock_info[row, "commonname"],
            region = stock_info[row, "region"], tsn = stock_info[row, "tsn"])
}

# Add fishery type data --------------------------------------------------------------------------------------
stock_lifespan <- stock_lifespan %>%
  add_column(fishery_type = "")

for(x in stock_lifespan$tsn){
  row <- which(stock_lifespan$tsn == x)
  row2 <- which(taxonomy$tsn == x)
  
  stock_lifespan[row, "fishery_type"] <- taxonomy[row2,"FisheryType"]
}

# Write lifespan data frame to csv --------------------------------------------------------------------------------
write.csv(lifespan, "lifespan.csv")

# Re-load populated lifespan data ---------------------------------------------------------------------------------
# researched max age data for each of the stocks in the analysis, most from FishBase

lifespan <- read.csv("lifespan.csv")

# Going to switch back to the stock naming conventions that the rest of the data frames have
lifespan$stock_name <- str_replace_all(lifespan$stock_name, "-", "\\.")
