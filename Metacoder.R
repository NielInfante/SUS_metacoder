library(tidyverse)
library(metacoder)



otu <- read_csv('Data_Files/otu.csv')
names(otu)[1] <- 'otu_id'
names(otu) <- sapply(names(otu), function(x) gsub('-', '_', as.character(x)))

# Get metadata ready
hmp_samples <- read_csv('Data_Files/Tara_oceans_mapping_file.csv')
names(hmp_samples)[1] <-'sample_id'


med_list <- filter(ungroup(hmp_samples), Province == 'Mediterranean') %>% select(sample_id)
hmp_samples %>% ungroup() %>% group_by(Province) %>% summarize(n())



hmp_samples <- hmp_samples %>% mutate(sample_id = gsub('-','_', sample_id))
hmp_samples <- hmp_samples %>% group_by(Depth)
hmp_samples <- hmp_samples %>% group_by(Province, add=T)
hmp_samples <- hmp_samples %>% group_by(Size_fraction, add=T)


taxa <- read_csv('Data_Files/taxa.csv')
#taxa <- as.data.frame(taxa)
names(taxa)[1] <- 'otu_id'

str(taxa)

taxa <- taxa
taxa$Domain <- paste0("k__", taxa$Domain)
taxa$Phylum <- paste0("p__", taxa$Phylum)
taxa$Class <- paste0("c__", taxa$Class)
taxa$Order <- paste0("o__", taxa$Order)
taxa$Family <- paste0("f__", taxa$Family)
taxa$Genus <- paste0("g__", taxa$Genus)

taxa <- taxa %>% mutate(lineage = paste("r__Root", Domain, Phylum, Class, Order, Family, Genus, sep=";"))
taxa <- taxa %>% select(otu_id, lineage)



otu[1:5, 1:3]


to_mc <- inner_join(taxa, otu)




to_mc[1:5, 1:4]



obj <- parse_tax_data(to_mc,
                      class_cols = "lineage", # the column that contains taxonomic information
                      class_sep = ";", # The character used to separate taxa in the classification
                      class_regex = "^(.+)__(.+)$", # Regex identifying where the data for each taxon is
                      class_key = c(tax_rank = "info", # A key describing each regex capture group
                                    tax_name = "taxon_name"))


print(obj)

obj_med <- filter_obs(obj, data="tax_data", hmp_samples$Province)



obj$data$tax_data <- zero_low_counts(obj, data = "tax_data", min_count = 5)

no_reads <- rowSums(obj$data$tax_data[, hmp_samples$sample_id]) == 0
sum(no_reads)

obj <- filter_obs(obj, data = "tax_data", ! no_reads, drop_taxa = TRUE)
print(obj)

obj$data$tax_data <- calc_obs_props(obj, "tax_data")

obj$data$tax_abund <- calc_taxon_abund(obj, "tax_data",
                                       cols = hmp_samples$sample_id)



obj$data$tax_occ <- calc_n_samples(obj, "tax_abund", groups = hmp_samples$Province)



set.seed(1) # This makes the plot appear the same each time it is run 
heat_tree(obj, 
          node_label = taxon_names,
          node_size = n_obs,
          node_color = Mediterranean, 
          node_size_axis_label = "OTU count",
          node_color_axis_label = "Samples with reads",
          layout = "davidson-harel", # The primary layout algorithm
          initial_layout = "reingold-tilford") # The layout algorithm that initializes node locations





obj$data$diff_table$wilcox_p_value <- p.adjust(obj$data$diff_table$wilcox_p_value, method = "fdr")
range(obj$data$diff_table$wilcox_p_value, finite = TRUE)
obj$data$diff_table$log2_median_ratio[obj$data$diff_table$wilcox_p_value > 0.05] <- 0
obj$data$diff_table <- compare_groups(obj, dataset = "tax_abund",
                                      cols = hmp_samples$sample_id, # What columns of sample data to use
                                      groups = hmp_samples$Depth) # What category each sample is assigned to
print(obj$data$diff_table)
set.seed(1)
heat_tree_matrix(obj,
                 dataset = "diff_table",
                 node_size = n_obs, # n_obs is a function that calculates, in this case, the number of OTUs per taxon
                 node_label = taxon_names,
                 node_color = log2_median_ratio, # A column from obj$data$diff_table
                 node_color_range = diverging_palette(), # The built-in palette for diverging data
                 node_color_trans = "linear", # The default is scaled by circle area
                 node_color_interval = c(-3, 3), # The range of log2_median_ratio to display
                 edge_color_interval = c(-3, 3), # The range of log2_median_ratio to display
                 node_size_axis_label = "Number of OTUs",
                 node_color_axis_label = "Log2 ratio median proportions",
                 layout = "davidson-harel", # The primary layout algorithm
                 initial_layout = "reingold-tilford") # The layout algorithm that initializes node locations



#                 output_file = "differential_heat_tree.pdf") # Saves the plot as a pdf file



