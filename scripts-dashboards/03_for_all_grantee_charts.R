# for all grantee charts
all_grantees <- list()
df_all <- df %>% replace(is.na(.), 0)
all_grantees <- lapply(ps_cols, make_chart_all, data = df_all)
names(all_grantees) <- ps_cols

table_by_grantee_no_kbl <- lapply(grantees, make_table, data = df, targets = df_targets)
names(table_by_grantee_no_kbl) <- grantees
targets_for_table_by_grantee <- str_subset(colnames(table_by_grantee_no_kbl[[1]]), "\nY")


master_table <- do.call(rbind.data.frame, table_by_grantee_no_kbl)
master_table_grantees <- rep(grantees, each=6) %>% cbind(master_table) %>% rename(`Grantee Name` = 1)
rownames(master_table_grantees) <- NULL
columns_in_master <- ncol(master_table)

# create charts for each grantee for each metric
for (i in seq_along(ps_cols)){
  col <- ps_cols[i]
  column <- enquo(col)
  label <- paste0("charts_", ps_cols[i])
  set_of_charts <- lapply(grantees, make_chart, data = df, metric = ps_cols[i])
  names(set_of_charts) <- grantees
  assign(label, set_of_charts)
}
