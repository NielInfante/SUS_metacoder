




otu <- read_csv('Data_Files/otu.csv')
names(otu)[1] <- 'otu_id'
names(otu) <- sapply(names(otu), function(x) gsub('-', '_', as.character(x)))

# Get metadata ready
hmp_samples <- read_csv('Data_Files/Tara_oceans_mapping_file.csv')
names(hmp_samples)[1] <-'sample_id'
hmp_samples <- hmp_samples %>% mutate(sample_id = gsub('-','_', sample_id))
hmp_samples <- hmp_samples %>% group_by(Depth)
hmp_samples <- hmp_samples %>% group_by(Province, add=T)
hmp_samples <- hmp_samples %>% group_by(Size_fraction, add=T)


taxa <- read_csv('Data_Files/taxa.csv')
#taxa <- as.data.frame(taxa)
names(taxa)[1] <- 'otu_id'

str(taxa)

taxa <- taxa
head(t2)
taxa$Domain <- paste0("k__", taxa$Domain)
taxa$Phylum <- paste0("p__", taxa$Phylum)
taxa$Class <- paste0("c__", taxa$Class)
taxa$Order <- paste0("o__", taxa$Order)
taxa$Family <- paste0("f__", taxa$Family)
taxa$Genus <- paste0("g__", taxa$Genus)

taxa <- taxa %>% mutate(lineage = paste("r__Root", Domain, Phylum, Class, Order, Family, Genus, sep=";"))
taxa <- taxa %>% Sselect(otu_id, lineage)



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








