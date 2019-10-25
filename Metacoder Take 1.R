

otu_data <- read.csv("otu.csv") # You might need to change the path to the file
print(otu_data) # You can also enter just `otu_data` to print it

tax_data <- read.csv("taxa1.csv") # You might need to change the path to the file
print(tax_data) # You can also enter just `otu_data` to print it



library(dplyr) # Loads the dplyr package so we can use `left_join`
#tax_data$`OTU ID` <- as.character(tax_data$`OTU ID`) # Must be same type for join to work
#otu_data$OTU_ID <- as.character(otu_data$OTU_ID) # Must be same type for join to work
otu_data <- left_join(otu_data, tax_data,
                      by = c("X" = "SampleID")) # identifies cols with shared IDs
print(otu_data)

tail(colnames(otu_data), n = 10) # `tail` returns the last n elements

sample_data <- read.csv("Tara_oceans_mapping_file.csv", colClasses=c('character', 'character', 'character', 'character'))
                        #col_types = "cccc") # each "c" means a column of "character"
print(sample_data) # You can also enter `sample_data` to print it
str(sample_data)

head(otu_data$taxonomy, 10)

library(taxa)
obj <- parse_tax_data(otu_data,
                      class_cols = "taxonomy", # The column in the input table
                      class_sep = ";") # What each taxon is seperated by
print(obj)

print(obj$data$tax_data)

obj <- parse_tax_data(otu_data,
                      class_cols = "taxonomy",
                      class_sep = ";",
                      class_regex = "(.*)",
                      class_key = c( "name" = "taxon_name"))
print(obj)
head(taxon_names(obj))
obj$data$class_data


names(obj$data) <- "otu_counts"
print(obj)

tree = heat_tree(obj,
          node_size = n_obs,
          node_color = n_obs,
          node_label = taxon_names,
          tree_label = taxon_names)
