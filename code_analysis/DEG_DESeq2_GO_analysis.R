data(geneList, package="DOSE")

DEG_results_df %>% 
  left_join(grcm38 %>% dplyr::select(ensgene, entrez)) %>% 
  mutate(entrez = as.character(entrez)) %>% 
  dplyr::select(region,entrez) %>% 
  filter(!is.na(entrez)) %>% 
  split(.$region) -> go_list

myregion = "MEA"

ggo <- enrichGO(gene = go_list[[myregion]]$entrez ,
                    OrgDb = org.Mm.eg.db::org.Mm.eg.db,
                    keyType = "ENTREZID",
                    ont = "BP",
                    readable = T,
                    pAdjustMethod = "BH",
                    pvalueCutoff  = 0.05,
                    qvalueCutoff  = 0.10)


dev.off()

png(glue("results_figures/enrichGO_{myregion}.png"), width = 8, height = 4, units = 'in', res = 300)
dotplot(ggo, orderBy = "Count")+
  ggtitle(glue("{myregion}"))+
  theme(plot.title = element_text(size = 18))+
  scale_color_viridis()

dev.off()

# Merge all regions =================================================




go_clusters <- lapply(go_list, function(x) x$entrez %>%  as.character)



x=compareCluster(go_clusters, fun="enrichGO", 
                 pvalueCutoff=0.05, 
                 pAdjustMethod="BH", 
                 OrgDb=org.Mm.eg.db::org.Mm.eg.db,
                 ont="BP",
                 readable=T)

dev.off()

png("results_figures/enrichGO_compareClusters.png", width = 10, height = 9, units = 'in', res = 300)
dotplot(x, showCategory=10, includeAll=FALSE)+
  scale_color_viridis()
dev.off()

