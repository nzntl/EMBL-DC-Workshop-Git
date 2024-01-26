library(tidyverse)

trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")
sample_info <- read_csv("data_rnaseq/sample_info.csv")

pca_matrix <- trans_cts %>% 
  column_to_rownames("gene") %>% 
  as.matrix() %>% 
  t()

sample_pca <- prcomp(pca_matrix)
class(sample_pca)
summary(sample_pca)
str(sample_pca)

pca_matrix[1:10, 1:5]

as_tibble(pca_matrix)
as_tibble(pca_matrix, rownames="sample")

pc_eigenvalues <- sample_pca$sdev^2

pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)),
                         variance = pc_eigenvalues) %>% 
  mutate(pct = variance/sum(variance)*100) %>% 
  mutate(pct_cum = cumsum(pct))

# pareto
pc_eigenvalues %>% 
  ggplot(aes(x=PC))+
  geom_col(aes(y = pct))+
  geom_line(aes(y= pct_cum, group = 1))+
  geom_point(aes(y = pct_cum))+
  #geom_hline(yintercept= 90)+
  labs(x= "principal component", y= "fraction variance explained")

pc_scores <- sample_pca$x %>% 
  as_tibble(rownames = "sample")

pc_scores %>% 
  ggplot((aes(x=PC1, y=PC2)))+
  geom_point()

pca_plot <- pc_scores %>% 
  full_join(sample_info, by="sample") %>% 
  ggplot(aes(x=PC1, y=PC2, 
              color = factor(minute),
              shape = strain))+
  geom_point()

pc_scores %>% 
  full_join(sample_info, by="sample") %>% 
  ggplot(aes(x=PC3, y=PC2, 
             color = factor(minute),
             shape = strain))+
  geom_point()

pc_loadings <- sample_pca$rotation %>% 
  as_tibble(rownames = "gene")  

  
top_genes <- pc_loadings %>% 
  select(gene, PC1, PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
  group_by(PC) %>% 
  arrange(desc(abs(loading))) %>% 
  slice(1:10) %>% 
  pull(gene) %>%
  unique()
  
top_loadings <- pc_loadings %>% 
  filter(gene %in% top_genes)

loadings_plot <- ggplot(data= top_loadings)+
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2),
                arrow=arrow(length = unit(0.1, "in")),
                color="brown")+
  geom_text(aes(x=PC1, y=PC2, label=gene),
            nudge_y=0.005, size=3)+
  scale_x_continuous(expand=c(0.02, 0.02))
  
library(patchwork)
((pca_plot|pca_plot|pca_plot)/ loadings_plot)+
  plot_annotation(tag_levels = "A")

library(ggfortify)
autoplot(sample_pca)

autoplot(sample_pca, 
         data=sample_info %>% mutate(minute = as.factor(minute)), 
         colour="minute", 
         shape="strain")

autoplot(sample_pca, 
         data=sample_info, 
         colour="minute", 
         shape="strain")

library(broom)
tidy(sample_pca, matrix="eigenvalues")
tidy(sample_pca, matrix="loadings")


# differential expression result
test_result <- read_csv("data_rnaseq/test_result.csv")
#gene name
# basemean -> normalized expression leevel of a gene
#log2foldchange column -> amount of change between 2 conditions
#lfcSE -> standard error associated to log2foldchange value
# stat column -> statistics value computed as log2foldchange/lfcSE compared to standard normal distribution 
# p value -> integer, associated with the change 
# p adjust -> p value corrected for multiple hypothesis testing
#  comparison -> 

# MA plot
test_result %>% 
  ggplot(aes(x= log10(baseMean), y= log2FoldChange))+
  geom_point(alpha= 0.1)+
  facet_wrap(facets = vars(comparison))

ma_plot <- test_result %>% 
  mutate(sig = ifelse(padj < 0.01, log2FoldChange, NA)) %>% 
  ggplot(aes(x= log10(baseMean), y= log2FoldChange))+
  geom_point(alpha= 0.1)+
  geom_point(aes(y=sig), color="tomato")+
  geom_hline(yintercept = 0, color = "dodgerblue")+
  facet_wrap(facets = vars(comparison))

(ma_plot | pca_plot)


# visualizin expression trends
# 1. get candidate gene (aka padj <0.01

candidate_gene <-  test_result %>% 
  filter(padj<0.01) %>% 
  pull(gene) %>%  # aka test_results[, "gene"] aka test_results$gene
  unique()

#1. trans_cts in long format
trans_cts_long <-  trans_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3, names_to = "sample", 
               values_to = "cts") %>% 
  full_join(sample_info, by = "sample")

# 2. filter trans_cts_long for candidate genes and compute mean expression value for each gene in each timepoint and each genotype
trans_cts_mean <- trans_cts_long %>%
  filter(gene %in% candidate_gene) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts = mean (cts), nrep = n()) %>% 
  ungroup()
  
# plot trends
trans_cts_mean %>% 
  ggplot(aes(x = minute, y = mean_cts))+
  geom_line(aes(group = gene), alpha = 0.3)+
  facet_grid(rows = vars(strain))

#scaling data to improve visualization
trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_gene) %>% 
  group_by(gene) %>%
  mutate(cts_scaled = (cts - mean(cts)) / sd(cts)) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts_scaled = mean(cts_scaled), 
            nrep = n())

trans_cts_mean %>% 
  ggplot(aes(x = minute, y= mean_cts_scaled))+
  geom_line(aes(group = gene), alpha =.3)+
  geom_hline(yintercept=0, color = "brown", linetype = "dashed")+
  facet_grid(rows = vars(strain))+
  scale_x_continuous(breaks = unique (trans_cts_mean$minute))
  #scale_x_continuous(breaks = c(0, 15, 30, 60, 120, 180))
  

#clustering
#1 matrix of counts
hclust_matrix <-  trans_cts %>% 
  select(-gene) %>% 
  as.matrix()

  rownames(hclust_matrix) <- trans_cts$gene
hclust_matrix <- hclust_matrix[candidate_gene, ]

hclust_matrix <- hclust_matrix %>% 
  t() %>% 
  scale() %>% 
  t()

gene_dist <- dist(hclust_matrix)

##hierarchical clustering

gene_hclust <- hclust(gene_dist, method = "complete")

plot(gene_hclust, labels = F)
abline(h=10, col = "brown", lwd = 2)

#cluster basend on the number i want
cutree(gene_hclust, k=8)

gene_cluster <- cutree(gene_hclust, k=8) %>% 
  enframe() %>% 
  rename(gene = name, cluster = value)

trans_cts_cluster <- trans_cts_mean %>%
  inner_join(gene_cluster, by = "gene")

trans_cts_cluster %>% 
  ggplot(aes(x=minute, y=mean_cts_scaled))+
  geom_line(aes(group=gene))+
  facet_grid(cols = vars(cluster), rows = vars(strain))+
  scale_x_continuous(breaks = unique (trans_cts_mean$minute))

library(ComplexHeatmap)

Heatmap(hclust_matrix, show_row_names = F)

