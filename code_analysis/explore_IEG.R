ieg_gene_list <- c('Nr4a2', 'Fos','Egr4','Junb','Nr4a1','Dusp6','Ier5',
                   'Egr1', 'Homer1', 'Fosb','Fosl2','Egr2','Dusp1','Arc', )
#https://www.pnas.org/content/early/2019/10/18/1913658116
housekeeping_gene_list <- c('Bdnf')
my_gene_list <- c(ieg_gene_list , housekeeping_gene_list)

my_gene_list <- c('Bdnf','Arc','Nr4a2', 'Fos','Egr4','Junb','Homer1', 'Fosb','Fosl2')

grcm38 %>% 
  filter(symbol %in% my_gene_list) -> goi

goi %>% dplyr::arrange(symbol)
goi$ensgene -> goi_ensgene
goi_ensgene %in% names(dds)

goi %>% filter(ensgene %in% names(dds))



tcounts <- t(log2((counts(dds[goi_ensgene, ], replaced=FALSE)+.5))) %>%
  merge(colData(dds), ., by="row.names") %>%
  gather(gene, expression, (ncol(.)-length(goi_ensgene)+1):ncol(.))


tcounts %>%
  head %>% 
  knitr::kable()

tcounts %>%
  mutate(ensgene = gene) %>% 
  dplyr::select(status,ensgene,expression,Row.names,region) %>% 
  as.data.frame() %>% 
  left_join(.,goi %>% as.data.frame()) ->tcountsx


ggplot(tcountsx, aes(status, expression,color=status,fill= status)) + 
  geom_boxjitter(outlier.color = NA, jitter.shape = 21, jitter.color = NA, 
                 alpha = 0.3,
                 jitter.height = 0.02, jitter.width = 0.07, errorbar.draw = TRUE,
                 position = position_dodge(0.85)) +
  facet_grid(symbol ~ region, scales="free_y",switch = "y") + 
  labs(x="Social status", 
       y="Expression (log normalized counts)")+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  # scale_color_viridis(discrete = T,option = "C")+
  # scale_fill_viridis(discrete = T,option = "C")+
  theme_bw()+
  theme(legend.position = "none") -> p

dev.off()
print(p)
ggsave(p, filename = "results_figures/IEG_region_status_housekeeping_new.png",height = 18,width = 12)



# heatmap ====================================================
ieg_gene_list <- c('Nr4a2', 'Fos','Egr4','Junb','Nr4a1','Dusp6','Ier5',
                   'Egr1', 'Homer1', 'Fosb','Fosl2','Dusp1')
#https://www.pnas.org/content/early/2019/10/18/1913658116


my_gene_list <-ieg_gene_list 

grcm38 %>% 
  filter(symbol %in% my_gene_list) %>% 
  select(ensgene) %>% 
  unlist -> goi_ensgene

tcountsx <- t(log2((counts(dds[goi_ensgene, ], replaced=FALSE)+.5))) %>%
  merge(colData(dds), ., by="row.names") %>%
  gather(gene, expression, (ncol(.)-length(goi_ensgene)+1):ncol(.)) %>% 
  rename(ensgene = gene) %>% 
  left_join(.,grcm38 %>% 
              filter(symbol %in% my_gene_list) %>% 
              select(ensgene,symbol)) %>%
  mutate(symbol = factor(symbol, 
                         ordered =  TRUE,
                         levels = c("Fos","Fosl2","Dusp1", "Ier5","Nr4a1", "Homer1", "Nr4a2","Egr4",
                                    "Dusp6","Junb","Egr1","Fosb")))

table(tcountsx$region)


tcountsx %>% 
  dplyr::select(symbol,Row.names,expression) %>% 
  spread(.,Row.names,expression) -> corx


row.names(corx) <- corx$symbol
corx<- corx[,-1]
corx %>% as.matrix() %>% t() -> cormat

cor(cormat, method = "pearson") -> res
diag(res) <- NA
pheatmap(res,main = "All individuals") -> all

my_status = c("Alpha", "Subordinate")
my_region = "CA1"


plot_my_pheatmap <- function(my_status = "Alpha",
                             my_region = "CA1"){
  
  tcountsx %>% 
    dplyr::filter(status %in% my_status & region == my_region) %>% 
    dplyr::select(symbol,Row.names,expression) %>% 
    tidyr::spread(.,Row.names,expression) -> corx
  
  row.names(corx) <- corx$symbol
  corx<- corx[,-1]
  corx %>% as.matrix() %>% t() -> cormat
  
  cor(cormat, method = "pearson") -> res
  diag(res) <- NA
  
  ggplotify::as.ggplot(pheatmap(res,
                                # breaks = my_break,
                                # cluster_rows=FALSE, cluster_cols=FALSE,
                                treeheight_row = 0, treeheight_col = 0,
                                main = glue('{my_status}: {my_region}'))) -> temp
  
  return(temp)
  
}



my_break = seq(-1,1,length = 100)
plot_list <- list()
plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Alpha",my_region = "CA1")
plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Subdominant",my_region = "CA1")
plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Subordinate",my_region = "CA1")
plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Alpha",my_region = "MEA")
plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Subdominant",my_region = "MEA")
plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Subordinate",my_region = "MEA")

plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Alpha",my_region = "mPOA")
plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Subdominant",my_region = "mPOA")
plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Subordinate",my_region = "mPOA")

plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Alpha",my_region = "NaccCore")
plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Subdominant",my_region = "NaccCore")
plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Subordinate",my_region = "NaccCore")

plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Alpha",my_region = "VMH")
plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Subdominant",my_region = "VMH")
plot_list[[length(plot_list)+1]] <- plot_my_pheatmap(my_status = "Subordinate",my_region = "VMH")




dev.off()
png("results_figures/goi_heatmap_diffscale_no_trees.png", width = 12, height = 18, units = 'in', res = 100)
g <- grid.arrange(arrangeGrob(grobs= plot_list, ncol = 3))
dev.off()

g<-do.call(grid.arrange,plot_list)
ggsave("g2.pdf",g)

