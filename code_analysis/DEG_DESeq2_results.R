#adjusted pval and fold change threshold to 'define' DEG
adj_pval_cutoff = 0.1
filter_counts = 5
threshold = 0.9  # 90% of the subject
LFC_threshold = 0.2
pvalue_threshold = 0.05


temp <- list.files(path = 'data_clean',pattern = "*_counts.csv")
count_list <- lapply(temp, function(x) read_csv(glue('data_clean/{x}')))
names(count_list) <- temp %>% str_sub(1,-12)
count_list %>% map(head)


get_DESeq_dds <- function(countdata, filter_counts = 10, threshold = 0.9)
{
  countdata %>% colnames()  -> x
  x[x != "ensgene"] -> to_filter
  
  id_status <- behav %>% 
    filter(sample_id %in% to_filter) %>% 
    select(subjectID,status,sample_id)  %>% 
    mutate(status = factor(status, levels = c("Alpha","Subdominant","Subordinate")))
  
  coldata = data.frame(row.names = id_status$sample_id, 
                       status = id_status$status)
  
  # tidy count data format  
  d.raw <- as.matrix(countdata %>% select(-ensgene))
  rownames(d.raw) <- countdata$ensgene
  
  
  which(complete.cases(d.raw) == F) -> x
  d.noNA <- d.raw[-x,]
  
  print(nrow(d.noNA))
  
  countData <- d.noNA[rowSums(d.noNA > filter_counts) > round((length(df)-1)*threshold), ]
  dds <- DESeqDataSetFromMatrix(countData = countData, 
                                colData = coldata, 
                                design = ~status)
  dds_deseq <- DESeq(dds)
  return(dds_deseq)
}


# DESeq_list <- count_list %>% map(~get_DESeq_dds(.,filter_counts = filter_counts, threshold = threshold))
# 
# saveRDS(DESeq_list, "results_RDS/DESeq_list.RDS")

DESeq_list <- readRDS("results_RDS/DESeq_list.RDS")


get_DEG_results <- function(dds, LFC_threshold = 0.2, pvalue_threshold = 0.05) # I won't set padj yet 
{
  temp1 <- results(dds, contrast=c("status","Alpha","Subdominant")) %>% 
    as.data.frame() %>% 
    rownames_to_column('ensgene') %>% 
    as_tibble() %>% 
    select(ensgene, log2FoldChange, pvalue, padj) %>% 
    mutate(contrast = "Alpha - Subdominant") %>% 
    filter(abs(log2FoldChange)>LFC_threshold) %>% 
    filter(pvalue <=pvalue_threshold) #I will get to padj later 
  
  temp2 <- results(dds, contrast=c("status","Alpha","Subordinate")) %>% 
    as.data.frame() %>% 
    rownames_to_column('ensgene') %>% 
    as_tibble() %>% 
    select(ensgene, log2FoldChange, pvalue, padj) %>% 
    mutate(contrast = "Alpha - Subordinate") %>% 
    filter(abs(log2FoldChange)>LFC_threshold) %>% 
    filter(pvalue <=pvalue_threshold) 
  
  temp3 <- results(dds, contrast=c("status","Subdominant", "Subordinate")) %>% 
    as.data.frame() %>% 
    rownames_to_column('ensgene') %>% 
    as_tibble() %>% 
    select(ensgene, log2FoldChange, pvalue, padj) %>% 
    mutate(contrast = "Subdominant - Subordinate") %>% 
    filter(abs(log2FoldChange)>LFC_threshold) %>% 
    filter(pvalue <=pvalue_threshold) 
  
  rbind(temp1, temp2, temp3) %>% 
    arrange(desc(abs(log2FoldChange))) -> result
  
  return(result)
}


DEG_results_list <- DESeq_list %>% map(get_DEG_results) # threshold set as default values 

DEG_results_df <- DEG_results_list %>% map2_df(.,names(.), ~mutate(.x,region = .y))


DEG_results_final <- DEG_results_df %>% #random but I love tibble!!!! 
  left_join(grcm38 %>%  select(ensgene, symbol, chr, biotype, description)) %>% 
  unique()




table(DEG_results_final$chr)

DEG_results_final %>% 
  filter(abs(log2FoldChange) > 1) %>% View()


DEG_results_final %>% 
  filter(chr == "X")


DEG_results_final %>% 
  filter(chr == "Y")


DEG_results_final %>% 
  filter(symbol == "Sst")


DEG_results_final %>% 
  filter(symbol == "Ttr")
