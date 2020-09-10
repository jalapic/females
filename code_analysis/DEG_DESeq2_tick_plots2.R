
get_LFC_results <- function(dds, LFC_threshold = 0.2, pvalue_threshold = 0.05) # I won't set padj yet 
{
  temp1 <- results(dds, contrast=c("status","Alpha","Subdominant")) %>% 
    as.data.frame() %>% 
    rownames_to_column('ensgene') %>% 
    as_tibble() %>% 
    select(ensgene, log2FoldChange, pvalue, padj) %>% 
    mutate(contrast = "Alpha - Subdominant") 
  
  temp2 <- results(dds, contrast=c("status","Alpha","Subordinate")) %>% 
    as.data.frame() %>% 
    rownames_to_column('ensgene') %>% 
    as_tibble() %>% 
    select(ensgene, log2FoldChange, pvalue, padj) %>% 
    mutate(contrast = "Alpha - Subordinate") 
  
  temp3 <- results(dds, contrast=c("status","Subdominant", "Subordinate")) %>% 
    as.data.frame() %>% 
    rownames_to_column('ensgene') %>% 
    as_tibble() %>% 
    select(ensgene, log2FoldChange, pvalue, padj) %>% 
    mutate(contrast = "Subdominant - Subordinate") 
  
  rbind(temp1, temp2, temp3) %>% 
    arrange(desc(abs(log2FoldChange))) -> result
  
  return(result)
}

LFC_results_list <- DESeq_list %>% map(get_LFC_results) # threshold set as default values 

LFC_results_df <- LFC_results_list %>% map2_df(.,names(.), ~mutate(.x,region = .y))




#alright explored enough now make figure 

number_the_genes <- function(df){

  result <- df %>% 
    left_join(LFC_results_df %>% 
                rename(real_LFC = log2FoldChange)) %>% 
    rename(LFC_with_NAs = log2FoldChange) %>% 
    rename(log2FoldChange = real_LFC) %>% 
    select(ensgene, log2FoldChange,contrast) %>%  
    spread(contrast, log2FoldChange, fill = 0) %>% 
    arrange(`Alpha - Subdominant`,`Alpha - Subordinate`, `Subdominant - Subordinate`) %>%
    mutate(x_var = row_number()) %>% 
    gather(contrast,log2FoldChange,2:4) %>% 
    mutate(contrast = factor(contrast, 
                             levels = rev(c('Alpha - Subdominant','Alpha - Subordinate', 'Subdominant - Subordinate')))) 
  
  return(result)
    
}

DEG_results_final %>% 
  split(.$region) %>% 
  .$CA1 -> df

df %>% 
  filter(is.na(log2FoldChange))

DEG_results_final %>% 
  split(.$region) %>% 
  map(number_the_genes) %>% 
  map2_df(.,names(.), ~mutate(.x, region = .y)) -> DEG_result_for_plot





table(DEG_results_final$region) %>% 
  as.data.frame() %>% 
  rename(region = Var1) %>% 
  mutate(facet_it = glue('{region} ({Freq})')) %>% 
  select(region, facet_it) -> freq1


table(DEG_results_final$region, DEG_results_final$contrast) %>% 
  as.data.frame() %>% 
  rename(region = Var1,
         contrast = Var2) %>% 
  mutate(each_region = glue('({Freq})')) %>% 
  left_join(freq1) %>% 
  select(facet_it, contrast, each_region) -> freq2




DEG_result_for_plot %>% 
  filter(is.na(log2FoldChange))




DEG_result_for_plot %>% 
  mutate(contrast = factor(contrast, 
                           levels = rev(c('Alpha - Subdominant','Alpha - Subordinate', 'Subdominant - Subordinate')))) %>% 
  left_join(freq1) %>%  
  ggplot(aes(x_var, contrast, fill = log2FoldChange))+
  geom_tile(color = NA, size = 10)+
  facet_grid(facet_it ~., switch = "y")+
  labs(x = "",
       y = "",
       fill = "Log2 Fold Change")+
  scale_fill_gradient2(low="purple4", mid="white", high="gold", #colors in the scale
                       midpoint=0,    #same midpoint for plots (mean of the range)
                       limits=c(-2, 2),#same limits for plots
                       breaks=seq(-2,2,1),   #breaks in the scale bar
                       na.value = 'white')+
  theme(legend.position = "bottom", 
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        strip.background = element_blank(),
        # plot.background =  element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent",colour = NA),
        strip.text.y.left = element_text(angle = 0, size = 12))
  

table(DEG_results_final$region, DEG_results_final$contrast) %>% 
  as.data.frame() %>% 
  rename(region = Var1,
         contrast = Var2) %>% 
  mutate(each_region = glue('({Freq})')) %>% 
  select(region, contrast, each_region) %>% 
  left_join(freq1) -> freq2

dat_text <- data.frame(
  label = c("4 cylinders", "6 cylinders", "8 cylinders"),
  cyl   = c(4, 6, 8)
)
# https://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2