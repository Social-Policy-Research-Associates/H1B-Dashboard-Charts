# standardize table
kbl_table <- function(x){
  x <- x %>%
    mutate(across(where(~is.numeric(.)), function(x) {prettyNum(x, big.mark = ",")}))
  
  kbl(x, escape = F) %>%
    kable_paper() %>% 
    kable_styling(bootstrap_options = c("striped", "hover"), 
                  position = "left", 
                  full_width = TRUE,
                  font_size = 14)
}

# chart for viewing one grantee at a time for one metric
make_chart <- function(data, one_grantee, metric) {
  a_col <- enquo(metric)
  subtitle <- recode(metric, !!!setNames(df_variables$ow_variable, df_variables$renamed_variables_lower))
  
  data %>%
    filter(grantee %in% one_grantee) %>%
    mutate(qtr_end = as.Date(qtr_end, "%m.%d.%Y")) %>% 
    ggplot(aes(x=qtr_end, y=!!sym(metric))) +
    geom_line(color = "#7E909A")+
    geom_point(size = 4, color = "#0F7173") + 
    geom_label(aes(label = prettyNum(!!sym(metric), big.mark = ',')), 
               vjust = -0.75, 
               inherit.aes = TRUE) +
    scale_x_date(
      date_labels = "%m.%d.%Y",
                 breaks = unique(df$qtr_end),
                 expand = expansion(mult = 0.2)) +
    scale_y_continuous(expand = expansion(mult = c(0.1, .3))) +
    labs(
      title = one_grantee,
      subtitle = subtitle,
      x = "Quarter End",
      y = "Participants") + 
    theme_minimal() +
    theme(text = element_text(size = 12),
          legend.position = "none",panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(family = "sans",
                                     size = 10, 
                                     hjust = .5, 
                                     vjust = .5))
}

# chart for viewing all grantees for one metric
make_chart_all <- function(data, metric) {
  
  # # TEST VALUES
  # data <- df
  # metric <- "tps_ps"
  
  ps_cols <- c("tps_ps", "train_bt", "train_ct", "train_cred_coc", "empl_ee", "total_iwa")
  a_col <- enquo(metric)
  subtitle <- recode(metric, !!!setNames(df_variables$ow_variable, df_variables$renamed_variables_lower))
  
  data2 <- data %>%
    group_by(qtr_end) %>%
    mutate(
      across(all_of(ps_cols), ~sum(.))
    ) %>% 
    ungroup() %>%
    mutate(
      qtr_end = as.Date(qtr_end, "%m.%d.%Y")
    )
  
  data2 %>% 
    ggplot(aes(x=qtr_end, y=!!sym(metric))) +
    geom_line(linewidth = 1.5, color = "#7E909A")+
    geom_point(size = 4, color = "#0F7173") + 
    geom_label(aes(label = prettyNum(!!sym(metric), big.mark = ',')), 
               vjust = -0.75, 
               inherit.aes = TRUE,
               size = 4) +
    scale_x_date(date_labels = "%m.%d.%Y",
                 breaks = unique(data2$qtr_end),
                 expand = expansion(mult = 0.2)) +
    scale_y_continuous(expand = expansion(mult = c(0.1, .3))) +
    #expand_limits(y = c(0, max(df$tps_ps) + 100)) +
    labs(
      title = "All Grantees",
      subtitle = subtitle,
      x = "Quarter End",
      y = "Participants") + 
    theme_minimal(base_size = 12) +
    theme(text = element_text(size = 12),
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(family = "sans",
                                     size = 10,
                                     hjust = .5,
                                     vjust = .5))
}

# table for viewing one grantee with all metrics and targets
make_table <- function(data, one_grantee, targets) {
  
  # VALUES FOR TESTING
  # data <- df
  # one_grantee <- grantees[12]
  # targets <- df_targets
  
  targets_table <- targets %>% 
    filter(grantee %in% one_grantee) %>%
    pivot_longer(
      cols = starts_with("target"),
      names_prefix = "target_",
      names_to = c("year", "metric"),
      names_sep = "_",
      values_to = "target"
    ) %>%
    # select(-grant_no) %>% 
    pivot_wider(
      id_cols = c(grantee, metric),
      names_from = "year",
      names_prefix = "03.31.",
      values_from = c("target"),
      values_fn = max
    )
  
  num_quarters <- unique(data$qtr_enddate) %>% length()
  last_quarter <- max(as.Date(data$qtr_end, "%m.%d.%Y"))
  
  
  
  last_target <- targets_table %>% 
    select(-1, -2) %>% 
    colnames() %>%
    lapply(., mdy) %>%
    lapply(., tibble) %>%
    lapply(., 
           function(x) x %>% 
             mutate(
               days = difftime(
                 last_quarter, 
                 `<date>`, 
                 units = "days"
               )
             )
    ) %>%
    keep(function(x) x$days > -365) %>%
    lapply(., function(x) x %>% select(`<date>`)) %>% 
    map_df(as_tibble)
  
  current_year_target <- subset(
    targets_table, 
    select = format(
      last_target$`<date>`, 
      "%m.%d.%Y")) %>%
    mutate(`Current Year Target` = rowSums(across(everything()))) %>%
    select(last_col())
  
  better_targets_table <- targets_table %>% 
    rename_with(., ~ str_c(.x, "\nY", 1:4, " Target"), 
                .cols = !c(1:2))
  
  total_target <- better_targets_table %>% 
    mutate(across(all_of(c(3:6)), ~replace(., is.na(.), 0))) %>% 
    rowwise() %>% 
    mutate(total = sum(across(all_of(3:6)))) %>%
    select(2, total)
  
  target_fctr <- total_target %>% 
    mutate(metric = as.factor(metric)) %>% 
    select(metric)
  assign("target_fctr", target_fctr, envir = .GlobalEnv)
  
  data %>% 
    filter(grantee %in% one_grantee) %>%
    pivot_longer(cols = all_of(ps_cols), names_to = "Performance Metric", values_to = "Value") %>%
    mutate(qtr_end = as.Date(qtr_end, "%m.%d.%Y")) %>% 
    arrange(qtr_end) %>% 
    mutate(`Performance Metric` = str_replace_all(`Performance Metric`, setNames(df_variables$ow_variable, df_variables$renamed_variables_lower)),
           # qtr_end = format(qtr_enddate, "%m.%d.%Y")
    ) %>% 
    pivot_wider(id_cols = `Performance Metric`,
                names_from = qtr_enddate,
                values_from = c(`Value`)) %>%
    cbind(better_targets_table) %>% 
    select(-grantee, -metric) %>%
    mutate(`lastyear` = .[[as.numeric(num_quarters)+1]]) %>%
    cbind(current_year_target) %>% 
    mutate(`% of Current Year Target*` = lastyear/`Current Year Target`,
           `% of Current Year Target*` = scales::percent(`% of Current Year Target*`, accuracy = 1)) %>% 
    cbind(total_target) %>%
    relocate(`Total Target` = total, .before = `% of Current Year Target*`) %>%
    mutate(`% of Total Target` = lastyear/`Total Target`,
           `% of Total Target` = scales::percent(`% of Total Target`)) %>%
    select(-lastyear, -`Current Year Target`, -metric)
}

make_kbl_table <- function(data){
  num_quarters <- unique(df$qtr_end) %>% length()
  kbl_table(data) %>% 
    add_header_above(c(" " = 1, 
                       "Reporting Quarters" = as.numeric(num_quarters), 
                       "Targets" = 5,
                       "Progress to Targets" = 2))}

# Chart for By Grantee Progress to Current Year's Target
chart_by_current_target <- function(perf_metric, title_label) {
  ggplot(perf_metric,
         aes(`Grantee Name`, `% of Cumulative Target`)) +
    geom_bar(aes(fill = color_bars), stat = "identity", color = "black") +
    geom_text(aes(label = `% of Cumulative Target*`),
              vjust = -1.0) +
    aes(x = fct_inorder(`Grantee Name`)) +
    scale_x_discrete(labels = function(x) str_wrap (x, width = 20)) +
    scale_fill_identity() +
    scale_y_discrete(expand = expansion(mult = c(0, .3))) +
    labs(
      title = title_label,
      subtitle = "Cumulative Target Achieved",
      x = "Grantee Name",
      y = "% of Cumulative Target*") +
    theme_minimal(base_size = 12) +
    theme(text = element_text(size = 12),
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(family = "sans",
                                     # angle = 90,
                                     size = 10,
                                     # hjust = 0.5,
                                     # vjust = 10
          )) +
    ggpubr::rotate_x_text()
}