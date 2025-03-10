# load data

df <- read.csv(here::here("data-ready", "combined_QPR_ow_data.csv"))
df_targets_raw <- readxl::read_excel(here::here("data-ready","Targets.xlsx"),
                                     sheet = "Targets")
df_grant_years_raw <- readxl::read_excel(here::here("data-ready", "Reporting Quarters.xlsx"))
df_variables <- read.csv(here::here("data-ready", "wips_variables.csv"))

# DEFINITION FOR THRESHOLDS ---------------------------------
threshold_guide <- tibble(
  quarter = c("06.30", "09.30", "12.31", "03.31"),
  green_t = c(10, 10, 10, 10),
  green_b = c(.25, .50, .75, .99),
  yellow_t = c(.24, .49, .74, .98),
  yellow_b = c(0, .25, .40, .75),
  red_t = c(.24, .24, .39, .74)
) %>% mutate(
  nice_green_b = scales::percent(green_b),
  nice_yellow_t = scales::percent(yellow_t, accuracy = 0.1),
  nice_yellow_b = scales::percent(yellow_b),
  nice_red_t = scales::percent(red_t, accuracy = 0.1)
)

# performance metrics column
ps_cols <- c("tps_ps", "train_bt", "train_ct", "train_cred_coc", "empl_ee", "total_iwa")
# create factors for target variables
short_target_fctr <- factor(c("ps", "bt", "ct", "coc", "ee", "iwa"), levels = c("ps", "bt", "ct", "coc", "ee", "iwa"))
# tibble it together
ps_cols_fctr <- tibble(
  renamed = ps_cols,
  short = short_target_fctr
)

# grant year table
df_grant_years <- df_grant_years_raw %>%
  mutate(`Reporting Quarters` = format(`Reporting Quarters`, "%m.%d.%Y"),
         `Target Date` = format(`Target Date`, "%m.%d.%Y"),
         `Target Date` = if_else(Year %in% c(1:4), str_c(`Target Date`, "<br>Y", Year, " Target"), str_c(`Target Date`, "<br>", Year, " Target"))) %>% 
  select(-`Target Date`) %>%
  group_by(Year) %>% 
  mutate(`Reporting Quarters` = paste0(`Reporting Quarters`, collapse = "|"),
         count_of_cols = n()) %>%
  distinct()
# pivot_longer(cols = c(`Target Date`, `Reporting Quarters`), names_to = "Reporting Date", values_drop_na = TRUE)



# pull out dates from data to be used for filtering
dates <- df_grant_years_raw
df_dates <- df %>% 
  clean_names() %>%
  select(qtr_enddate) %>% 
  distinct() %>% 
  mutate(qtr_end = mdy(qtr_enddate)) %>% 
  arrange(qtr_end)
df_dates_first <- head(df_dates, n = 1)
df_dates_last <- tail(df_dates, n = 1)

# import variable names
# df_variables <- readxl::read_excel(here::here("data-raw","wips_variables.xlsx"))
# Rename columns to shorter names based on spreadsheet
# colnames(df) <-
#   dplyr::recode(
#     colnames(df),
#     !!!setNames(df_variables$renamed_variables, df_variables$wips_variable)
#   )

# clean names with janitor and convert date columns to characters
# df <- df %>%
#   clean_names() %>%
#   mutate(qtr_end = mdy(qtr_enddate))

# join the variables names to the factored one
ps_columns_fctr <- ps_cols_fctr %>% 
  left_join(y = df_variables, by = c("renamed" = "renamed_variables_lower"))

# pull out unique list of all grantee names
grantees <- unique(df$grantee)

# remove spaces and dashes from target data
df_targets <- df_targets_raw %>% 
  mutate(`Grant Number` = str_remove_all(`Grant Number`, pattern = "\\s|\\-")) %>% 
  left_join(., y = df, by = c("Grant Number" = "grant_no")) %>% 
  select(
    `Grant Number`,
    `Grantee`,
    contains("Target")) %>%
  distinct() %>% 
  clean_names()

all_targets_table <- df_targets %>% 
  pivot_longer(
    cols = starts_with("target"),
    names_prefix = "target_",
    names_to = c("year", "metric"),
    names_sep = "_",
    values_to = "target"
  ) %>%
  mutate(target = replace(target, is.na(target), 0)) %>% 
  select(-grantee, -grant_number) %>%
  group_by(metric, year) %>% 
  summarise(target = sum(target)) %>% 
  pivot_wider(
    id_cols = c(metric),
    names_from = "year",
    names_prefix = "03.31.",
    values_from = c("target"),
    values_fn = max
  ) %>% 
  ungroup() %>% 
  left_join(ps_cols_fctr, by = c("metric" = "short"), keep = TRUE, suffix = c(".x", ".y")) %>%
  arrange(short) %>%
  select(-metric, -short) %>%
  select(
    metric = renamed,
    everything())

num_quarters <- unique(df$qtr_end) %>% length()
last_quarter <- max(as.Date(df$qtr_end, "%m.%d.%Y"))
current_reporting_year <- df_grant_years_raw[df_grant_years_raw$`Reporting Quarters` == last_quarter, ]
previous_reporting_year <- df_grant_years |> ungroup() |> mutate(previous_year = lag(Year)) |> filter(Year == current_reporting_year$Year) |> pull(previous_year)
previous_reporting_quarter <- df_grant_years_raw[df_grant_years_raw$Year == previous_reporting_year, ] |> select(3) |> slice_head() |> pull() |> format("%m.%d.%Y")



all_last_target <- all_targets_table %>% 
  select(-1) %>%
  colnames() %>% 
  lapply(., mdy) %>%
  lapply(., tibble) %>%
  lapply(., function(x) x %>% mutate(days = difftime(last_quarter, `<date>`, units = "days"))) %>% 
  keep(function(x) x$days > -365) %>%
  lapply(., function(x) x %>% select(`<date>`)) %>% 
  map_df(as_tibble)

all_current_year_target <- subset(all_targets_table, select = format(all_last_target$`<date>`, "%m.%d.%Y")) %>%
  rowwise() %>% 
  mutate(`Current Year Target` = sum(across(everything()))) %>% 
  select(tail(names(.), 1))

all_previous_year_target <- subset(
  all_targets_table, 
  select = format(
    all_last_target$`<date>`, 
    "%m.%d.%Y")) |> 
  select(-last_col()) |> 
  mutate(`Previous Year Target` = rowSums(across(everything()))) %>%
  select(last_col())


all_better_targets_table <- all_targets_table %>% 
  rename_with(., ~ str_c(.x, "<br>Y", 1:4, " Target"), .cols = !c(1))

all_total_target <- all_better_targets_table %>% 
  mutate(across(any_of(starts_with("03")), ~replace(., is.na(.), 0))) %>% 
  rowwise() %>% 
  mutate(total = sum(across(any_of(starts_with("03"))))) %>%
  select(1, total)

for_make_table_all <- df %>%
  select(qtr_end, all_of(ps_cols)) %>%
  mutate(across(all_of(ps_cols), ~replace(., is.na(.), 0))) %>%
  group_by(qtr_end) %>% 
  summarise(across(.cols = all_of(ps_cols), sum)) %>%
  pivot_longer(cols = all_of(ps_cols), names_to = "Performance Metric", values_to = "Value") %>%
  mutate(qtr_end = as.Date(qtr_end, "%m.%d.%Y")) %>%
  arrange(qtr_end) %>%
  mutate(`Performance Metric` = str_replace_all(`Performance Metric`, 
                                                setNames(df_variables$ow_variable,
                                                         df_variables$renamed_variables_lower)),
         qtr_end = format(qtr_end, "%m.%d.%Y")) %>% 
  pivot_wider(id_cols = `Performance Metric`,
              names_from = qtr_end,
              values_from = c(`Value`)) %>%
  cbind(all_better_targets_table) %>%
  select(-metric) %>%
  mutate(`lastyear` = .[[as.numeric(num_quarters)+1]]) %>%
  cbind(all_current_year_target) %>%
  cbind(all_previous_year_target) |> 
  mutate(`% of Current Year Target*` = lastyear/`Current Year Target`,
         `% of Current Year Target*` = scales::percent(`% of Current Year Target*`, accuracy = 1)) %>%
  mutate(`% of Previous Year Target` := !!rlang::sym(previous_reporting_quarter)/`Previous Year Target`) |> 
  mutate(previous_year_target_met = case_when(
    `% of Previous Year Target` >= 1 ~ 1,
    TRUE ~ 0
  )) |> 
  cbind(all_total_target) %>%
  relocate(`Total Target` = total, .before = `% of Current Year Target*`) %>%
  mutate(`% of Total Target` = lastyear/`Total Target`,
         `% of Total Target` = scales::percent(`% of Total Target`)) %>%
  select(-lastyear, -`Current Year Target`, -metric) |> 
  mutate(is_6_30_qtr = case_when(
    format(last_quarter, "%m.%d") == "06.30" ~ 1,
    TRUE ~ 0
  ))

# VALUES FOR DROPDOWNS-----------------------------

choices <- setNames(object = c(1:length(grantees)), grantees)
choices_rev <- setNames(names(choices), choices)
metric_choices <- setNames(object = ps_cols, recode(ps_cols, !!!setNames(df_variables$ow_variable, df_variables$renamed_variables_lower)))





# OTHER STUFF -------------------------
addl_cols <- tibble(dontforget = c("", "|Y1", "|Y1|Y2", "|Y1|Y2|Y3", "|Y1|Y2|Y3|Y4"))

