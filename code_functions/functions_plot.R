# data = dds90
# 
# 
# plot_each_gene <- function(data, gene_symbol = c('Egr1','Egr4')){
#   
#   my_ensgene = grcm38 %>% 
#     filter(symbol %in% gene_symbol) %>% 
#     select(ensgene) %>% 
#     unlist
#   data %>% 
#     filter(ensgene %in% my_ensgene) %>% 
#     pivot_longer(-ensgene,names_to = 'sample_id',values_to = 'count') -> x
#     
#   x %>% 
#     ggplot()
# 
#   
#   str(my)
#   print(p)
# }
