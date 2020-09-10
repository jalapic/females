#### body weight

bw<-read.csv("raw_data/bodyweight_female.csv",stringsAsFactors = F)
bw$cohortx <- LETTERS[bw$cohort-87+1]
bw$Mouse.ID <- bw$mouse

head(bw)

rankdf <- do.call('rbind',Map(cbind, lapply(m.isi,cbind), cohort=LETTERS[1:8]))
colnames(rankdf)<-c("Mouse.ID","cohortx")
rankdf <- as.data.frame(rankdf)
rankdf$rank <- 1:12
rankdf$Mouse.ID <- as.numeric(as.character(rankdf$Mouse.ID))

head(rankdf)

bw <- rankdf %>% full_join(bw)
bw$bwdiff <- bw$end -bw$start

head(bw)

ggplot(bw, aes(x=rank,y=start)) + geom_point() + facet_wrap(~cohort, ncol=2)
ggplot(bw, aes(x=rank,y=end)) + geom_point() + facet_wrap(~cohort, ncol=2)
ggplot(bw, aes(x=rank,y=bwdiff)) + geom_point() + facet_wrap(~cohort, ncol=2)


# Correlation tests

split(bw, bw$cohortx) %>%
  map(function(x) cor.test(x$start,x$rank,method="spearman")[[4]]) %>%
  unlist() -> rhos.start

split(bw, bw$cohortx) %>%
  map(function(x) cor.test(x$start,x$rank,method="spearman")$p.value) %>%
  unlist() -> pval.start

split(bw, bw$cohortx) %>%
  map(function(x) cor.test(x$end,x$rank,method="spearman")[[4]]) %>%
  unlist() -> rhos.end

split(bw, bw$cohortx) %>%
  map(function(x) cor.test(x$end,x$rank,method="spearman")$p.value) %>%
  unlist() -> pval.end

split(bw, bw$cohortx) %>%
  map(function(x) cor.test(x$bwdiff,x$rank,method="spearman")[[4]]) %>%
  unlist() -> rhos.diff

split(bw, bw$cohortx) %>%
  map(function(x) cor.test(x$bwdiff,x$rank,method="spearman")$p.value) %>%
  unlist() -> pval.diff


rhos.start
pval.start

rhos.end
pval.end

rhos.diff
pval.diff

median(rhos.start)
median(rhos.end)
median(rhos.diff)
