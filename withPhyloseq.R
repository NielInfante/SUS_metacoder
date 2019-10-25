

install.packages('metacoder')

library(metacoder)


# Read Data
otu <- read_csv('Data_Files/otu.csv')
taxa <- read_csv('Data_Files/taxa.csv')
meta <- read_csv('Data_Files/Tara_oceans_mapping_file.csv')
tree <- ape::read.tree('Data_Files/tree.tre')

# Get OTU ready
otu <- as.data.frame(otu)
nm <- names(otu)
names(otu) <- sapply(nm, function(x) gsub('-', '_', as.character(x)))
row.names(otu) <- otu$X1
otu$X1 <- NULL
otu <- otu_table(otu, taxa_are_rows = T)

# Get Taxonomy ready
taxonomy <- as.data.frame(taxa)
row.names(taxonomy) <- taxonomy$X1
taxonomy$X1 <- NULL
taxonomy <- as.matrix(taxonomy)
taxonomy <- tax_table(taxonomy)

# Get metadata ready
meta <- as.data.frame(meta)
meta$SampleID <- sapply(meta$SampleID, function(x) gsub('-', '_', as.character(x)))
row.names(meta) <- meta$SampleID
meta <- sample_data(meta)

head(meta)


ps2 <- phyloseq(otu, taxonomy, meta)



obj <- parse_phyloseq(ps2, class_regex = "(.*)")

print(obj)



obj$data$tax_data <- zero_low_counts(obj, data = "tax_data", min_count = 5)

no_reads <- rowSums(obj$data$tax_data[, hmp_samples$sample_id]) == 0
sum(no_reads)

heat_tree(obj)



head(ps2@tax_table)

data("GlobalPatterns")



heat_tree_matrix(obj,
                 dataset = "diff_table",
                 node_size = n_obs, # n_obs is a function that calculates, in this case, the number of OTUs per taxon
                 node_label = taxon_names,
                 node_color = log2_median_ratio, # A column from `obj$data$diff_table`
                 node_color_range = diverging_palette(), # The built-in palette for diverging data
                 node_color_trans = "linear", # The default is scaled by circle area
                 node_color_interval = c(-3, 3), # The range of `log2_median_ratio` to display
                 edge_color_interval = c(-3, 3), # The range of `log2_median_ratio` to display
                 node_size_axis_label = "Number of OTUs",
                 node_color_axis_label = "Log2 ratio median proportions",
                 layout = "davidson-harel", # The primary layout algorithm
                 initial_layout = "reingold-tilford", # The layout algorithm that initializes node locations
                 output_file = "differential_heat_tree.pdf") # Saves the plot as a pdf file



heat_tree(obj)



plot_one <- function(my_col_name) {
  heat_tree(obj,
            node_color = obj$data$my_table[[my_col_name]],
            output_file = paste0("plot for sample ", my_col_name, ".pdf"),
            ...) # other commands
}
my_plots <- lapply(my_sample_names, plot_one)

plot_one('TARA_031_SRF_0.22-1.6')



heat_tree(obj, node_color = obj$



            
            
            heat_tree(obj, 
                      node_label = obj$taxon_names(),
                      node_size = obj$n_obs(),
                      node_color = obj$data$sample_data$sample_id, 
                      node_size_axis_label = "OTU count",
                      node_color_axis_label = "Samples with reads",
                      layout = "davidson-harel", # The primary layout algorithm
                      initial_layout = "reingold-tilford") # The layout algorithm that initializes node locations            
            
            
obj          %>%
            filter_taxa(taxon_names == "Archaea", subtaxa = TRUE) %>%
    heat_tree(node_size=obj$n_obs('TARA_023_DCM_0.22_1.6'))      

length(obj$n_obs())
