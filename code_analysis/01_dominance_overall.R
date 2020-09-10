
### Dominance Analysis.

library(compete)
library(igraph)
library(steepness)

# 0. Set seed for this analysis
set.seed(1001)

# 1. Create Expanded Dataframes of Each event.
df.wl <- lapply(df.l, function(x) expandrows(x[c(2,4,3)]))

# 2. Make Win-Loss Sociomatrices
m.wl <- lapply(df.wl, wlmatrix)

# 3. Make Binary Sociomatrices
m.di <- lapply(df.wl, dimatrix)

# 4. Calculate modifed h'
m.dv <- lapply(m.wl,devries)
m.dv.p <- lapply(m.dv, function(x) x$`h-modified`) %>% unlist
m.dv.pval <- lapply(m.dv, function(x) x$`p-value`) %>% unlist

# 5. Calculate directional consistency
m.dc <- lapply(m.wl,dc_test)
m.dc.p <- lapply(m.dc, function(x) x$DC) %>% unlist
m.dc.pval <- lapply(m.dc, function(x) x$`DC.pvalue`) %>% unlist


# 6. Steepness
m.st.p <- lapply(m.wl, steepness::getStp) %>% unlist
m.st.pval <- lapply(m.wl, getStp.pval) %>% unlist


# 7. Triangle transitivity
m.tt <- lapply(m.di,ttri_test)
m.tt.p <- lapply(m.tt, function(x) x$ttri) %>% unlist
m.tt.pval <- lapply(m.tt, function(x) x$pval) %>% unlist



# 8. Despotism
m.d <- lapply(m.wl,despotism)
m.d.val <- lapply(m.d, function(x) x[[1]]) %>% unlist
m.d.val


# 9. Gini-coefficients 
gcw <- lapply(m.wl, function(x) ineq::Gini(rowSums(x))) %>% unlist() #GC Wins
gcl <- lapply(m.wl, function(x) ineq::Gini(colSums(x))) %>% unlist() #GC Losses




### Save all results data to results folder

## Store Matrices
matrices <- list(m.wl, m.di)
saveRDS(matrices, "results/matrices.RDS")


## Hierarchy Results
data.frame(
     'cohort' = LETTERS[1:8],
     'hvalues' = m.dv.p,
     'dc' = m.dc.p,
     'steepness' = m.st.p,
     'ttri' = m.tt.p,
     'despotism' = m.d.val/100,
     'gini.win' = gcw,
     'gini.lose'= gcl,
     'hvalue.pval' = m.dv.pval,
     'dc.pval' = m.dc.pval,
     'steep.pval' = m.st.pval,
     'ttri.pval' = m.tt.pval
) -> resultsdf

#round
resultsdf[,2:8]  <- round(resultsdf[,2:8],2)
resultsdf[,9:12]  <- round(resultsdf[,9:12],3)

saveRDS(resultsdf, "results/resultsdf.RDS")



