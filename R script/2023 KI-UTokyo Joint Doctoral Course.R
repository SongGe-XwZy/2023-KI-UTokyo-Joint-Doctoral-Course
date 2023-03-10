#Task 1 - Literature
# Altered cervicovaginal microbiota in premenopausal ovarian cancer patients PMID: 34856363

# What is the medically relevant insight from the article?

# The medically relevant insight from the article is that the cervicovaginal microbiota in premenopausal 
# ovarian cancer patients is frequently a diversified community and similar to those in healthy subjects at postmenopausal 
# ages. The diverse microbiota was associated with the major histotypes of epithelial ovarian cancer, including serous ovarian 
# cancer and ovarian clear cell cancer. The study implies the potential of a cervicovaginal microbiome biomarker in screening 
# ovarian cancer in premenopausal women. However, further research is needed to confirm the findings and determine the clinical 
# utility of cervicovaginal microbiota as a biomarker for ovarian cancer diagnosis.

# Which genomics technology/ technologies were used?

# The study used 16S rRNA amplicon sequencing to determine the composition of the cervicovaginal microbial 
# community at the genus level. 

# List and explain at least three questions/ hypotheses you can think of that extend the analysis presented in the paper.

# 1. Can the cervicovaginal microbiota be used as a biomarker for ovarian cancer diagnosis in postmenopausal women?
#  This question would extend the analysis presented in the paper by investigating whether the cervicovaginal microbiota can be used as a biomarker for ovarian cancer diagnosis in postmenopausal women, given that the study only focused on premenopausal women.

# 2. How does the cervicovaginal microbiota change over time in women with ovarian cancer?
#  This question would extend the analysis presented in the paper by investigating how the cervicovaginal microbiota changes over time in women with ovarian cancer, which could provide insights into disease progression and potential therapeutic targets.

# 3. What is the role of specific bacterial species or strains in ovarian cancer development?
#  This question would extend the analysis presented in the paper by investigating the role of specific bacterial species or strains in ovarian cancer development, which could provide insights into disease mechanisms and potential therapeutic targets.

#Test 4 - R basic operations
library(tidyr)
#1

sqrt(100) %>% print()# Calculate the result and print the result

#2

log2(32) %>% print()# Calculate the result and print the result

#3

sum(1:1000) %>% print()# Calculate the sum of these numbers and print the result

#4

even_numbers <- seq(from = 2, to = 1000, by = 2)# Generate a sequence of even numbers from 2 to 1000
sum_of_even_numbers <- sum(even_numbers)# Calculate the sum of these numbers
print(sum_of_even_numbers)# Print the result

#5

n_genes <- 100# Genes number
pairwise_comparisons <- n_genes * (n_genes - 1) / 2# Calculate the number of pairwise comparisons
print(pairwise_comparisons)# Print the result

#6

n_genes <- 100# Genes number
arrangements_in_triples <- choose(n_genes, 3)# Calculate the number of ways to arrange 100 genes in triples
print(arrangements_in_triples)# Print the result

#Task 5 - Using R example datasets

#1

data(CO2)

#2

help(CO2)

#3

Quebec <- subset(CO2, CO2$Type == 'Quebec')# Take the subset of the CO2 data set that 'Type' is Quebec
Quebec$uptake %>% mean() %>% print()# Calculate the mean and print the result
Quebec$uptake %>% median() %>% print()# Calculate the median and print the result

Mississippi <- subset(CO2, CO2$Type == 'Mississippi')# Take the subset of the CO2 data set that 'Type' is Mississippi
Mississippi$uptake %>% mean() %>% print()# Calculate the mean and print the result
Mississippi$uptake %>% median() %>% print()# Calculate the median and print the result

#Task 6 - R Functions

#1

mean_median_ratio <- function(x) {      # Define a function to calculate the mean/median ratio of a vector
  x_mean <- mean(x)                     # Calculate the mean of the input vector
  x_median <- median(x)                 # Calculate the median of the input vector
  ratio <- x_mean / x_median            # Calculate the ratio of the mean and median
  return(ratio)                         # Return the ratio as the output of the function
}


#2

mean_without_extremes <- function(x) {                    # Define a function to calculate the mean of a vector after ignoring the lowest and highest values
  x_without_extremes <- x[-which.min(x)][-which.max(x)]   # Remove the lowest and highest values from the input vector
  mean_x <- mean(x_without_extremes)                      # Calculate the mean of the remaining values
  return(mean_x)                                          # Return the mean as the output of the function
}

#3

# Pipes in R allow you to chain together multiple operations by sending the output 
# of one function as input to the next function, making code more readable and concise.
# Piping is useful for performing a series of transformations on a dataset. However, pipes should 
# be used with caution when working with large datasets or when there is a need to save 
# intermediate results, as piping can result in unnecessary copying of data and increased memory usage. 
# Additionally, pipes may not be the best choice for complex or nested operations where it is difficult 
# to maintain clarity and readability.

#4

#The apply-family of functions in R (apply, lapply, sapply, etc.) are useful for iterating over arrays 
# (e.g., matrices, data frames) and applying a function to each element, row, or column. This can save 
# time and reduce code duplication, especially when working with large datasets or performing repetitive 
# operations. The apply-family functions are also versatile and can be used with user-defined functions 
# or functions from other packages, allowing for flexible data manipulation and analysis.

#Task 7 - Basic visualization with R

#1
#a. Histograms
library(ggplot2)
library(gridExtra)

# load the data
data <- read.csv("magic_guys.csv")

# create histogram using hist function
par(mfrow=c(1,2)) # display plots side by side
hist(data[data$species=="jedi",]$length, main="jedi", xlab="Height", breaks=10)
hist(data[data$species=="sith",]$length, main="sith", xlab="Height", breaks=10)

# create histograms using ggplot2 package
ggplot(data, aes(x=length, fill=species)) + 
  geom_histogram(binwidth=5, color="black", alpha=0.5) +
  facet_grid(.~species) +
  labs(x="Height", y="Count") +
  theme_bw()

ggplot(data, aes(x=length, fill=species)) + 
  geom_histogram(binwidth=2, color="black", alpha=0.5) +
  facet_grid(.~species) +
  labs(x="Height", y="Count") +
  theme_bw()

#b. Boxplots

ggplot(data, aes(x=species, y=length, fill=species)) +
  geom_boxplot() +
  labs(x="Species", y="Height") +
  theme_bw()

#c. save

# PNG: Use PNG format for images that will be displayed on the web or in a document.
# PDF: Use PDF format for high-quality prints, especially when the plot contains text.
# SVG: Use SVG format for scalable vector graphics that can be edited in vector graphics software.

# save histograms as PNG, PDF, and SVG
ggsave("histograms.png", width=7, height=4, dpi=300)
ggsave("histograms.pdf", width=7, height=4)
ggsave("histograms.svg", width=7, height=4)

# save boxplots as PNG, PDF, and SVG
ggsave("boxplots.png", width=5, height=5, dpi=300)
ggsave("boxplots.pdf", width=5, height=5)
ggsave("boxplots.svg", width=5, height=5)

#2
#load the data
microarray <- read.table("/Users/cuiyang/Desktop/microarray_data.tab", header = TRUE, sep = "\t")

#a
dim(microarray)# determine size

#b
# Count the missing values per gene
missing_vals <- apply(microarray, 2, function(x) sum(is.na(x)))

# Create a data frame with gene names and missing value counts
missing_df <- data.frame(genes = colnames(microarray), missing_vals = missing_vals)

# Create a bar plot of missing value counts
ggplot(missing_df, aes(x = genes, y = missing_vals)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Gene") +
  ylab("Number of missing values")

#c
# calculate percentage missing values per gene
pct_missing <- apply(microarray, 2, function(x) mean(is.na(x))*100)

# find genes with more than X% missing values
x <- 10 # set X to 10%
genes_10 <- names(pct_missing[pct_missing > x])

x <- 20 # set X to 20%
genes_20 <- names(pct_missing[pct_missing > x])

x <- 50 # set X to 50%
genes_50 <- names(pct_missing[pct_missing > x])

#d
# replace missing values by mean
microarray_imputed <- apply(microarray, 2, function(x) ifelse(is.na(x), mean(x, na.rm=TRUE), x))

#3

# create histograms using ggplot2 package
ggplot(CO2, aes(x=uptake, fill=Type)) + 
  geom_histogram(binwidth=5, color="black", alpha=0.5) +
  facet_grid(.~Type) +
  labs(x="counts", y="uptake") +
  theme_bw()
#I saw the distribution of this data set

#Task 8
library(tidyverse)
library(tidybiology)
#1
#a
chromosome %>%
  summary(mean_variations = mean(variations),
            median_variations = median(variations),
            max_variations = max(variations),
            mean_protein_coding_genes = mean(protein_coding_genes),
            median_protein_coding_genes = median(protein_coding_genes),
            max_protein_coding_genes = max(protein_coding_genes),
            mean_miRNAs = mean(miRNAs),
            median_miRNAs = median(miRNAs),
            max_miRNAs = max(miRNAs))
#b
ggplot(chromosome, aes(x = length_mm)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(x = "Chromosome Size", y = "Frequency", title = "Distribution of Chromosome Sizes")

#c
#protein coding genes&length
ggplot(chromosome, aes(x = protein_codinggenes, y = length_mm)) +
  geom_point(aes(size = protein_codinggenes), alpha = 0.6, color = "blue") +
  scale_size_continuous(range = c(2, 8)) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  labs(x = "protein_coding_genes",
       y = "length_mm",
       title = "protein_coding_genes vs length_mm",
       subtitle = "With protein_coding_genes count represented by point size") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1)) 

#mirna&length
ggplot(chromosome, aes(x = mi_rna, y = length_mm)) +
  geom_point(aes(size = mi_rna), alpha = 0.6, color = "blue") +
  scale_size_continuous(range = c(2, 8)) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  labs(x = "mi_rna",
       y = "length_mm",
       title = "mi_rna vs length_mm",
       subtitle = "With miRNA count represented by point size") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1))

#d
proteins %>%
  summary(mean_length = mean(length),
            median_length = median(length),
            max_length = max(length),
            mean_mass = mean(mass),
            median_mass = median(mass),
            max_mass = max(mass))

ggplot(proteins, aes(x = length, y = mass)) +
  geom_point(aes(size = length), alpha = 0.6, color = "blue") +
  scale_size_continuous(range = c(2, 8)) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  labs(x = "length",
       y = "mass",
       title = "length vs mass") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black", size = 1))
