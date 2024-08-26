library(tidyverse)
library(readxl)
library(here)
library(janitor)

# functions --------------------------------------------------------------------

fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

# import data -------------------------------------------------------------

filenames <- list.files("data-raw", pattern="H-1B RH QPR Data", full.names=TRUE)
df_variables <- readxl::read_excel(here::here("data-raw","wips_variables.xlsx")) %>% 
  mutate(no_total = tolower(str_remove_all(wips_variable, "Total$"))) %>% 
  mutate(renamed_variables_lower = tolower(renamed_variables))

ldf <- lapply(filenames, readxl::read_excel, sheet = "Grant To Date")

# data processing ---------------------------------------------------------
names(ldf) <- str_extract(filenames, "[0-9]{2}.[0-9]{2}.[0-9]{4}") %>% as.character()
all_cols <- lapply(ldf, colnames) %>% unlist() %>% as_tibble() %>% arrange(value)
ldf <- lapply(ldf, function(df) {rename_with(df, ~str_remove_all(.x, pattern = "\\s|\\-|\\/|\\'"))})
ldf <- lapply(ldf, function(df) {rename_with(df, ~str_remove_all(.x, pattern = "(Total)$"))})
ldf <- lapply(ldf, function(df) {
  rename_with(df, ~str_replace_all(.x, pattern = "Education", replacement = "Edu")) %>% 
    rename_with(., ~str_replace_all(.x, pattern = "Edu", replacement = "Education"))
})
ldf <- lapply(ldf, function(df) {rename_with(df, ~str_extract(.x, pattern = "^(CompletedEducationJobTrainingProgram)"), .cols = starts_with("CompletedEducationJobTrainingProgram"))})
ldf <- lapply(ldf, function(df) {rename_with(df, tolower)})

ldf <- lapply(ldf, function(df) {df %>%  select(df_variables$no_total)})

# adding an QPR end date column to use for plotting in dashboards 
# from the file name
list_names <- c(names(ldf))
df <- do.call(rbind, Map(cbind, ldf, QTR_END = list_names)) %>%
  relocate(QTR_END)
rownames(df) <- NULL

df <- df %>%
  mutate(QTR_ENDDATE = QTR_END) %>%
  relocate(QTR_ENDDATE, .after = QTR_END)

df <- df %>%
  mutate(
    YR = fct_case_when(
      QTR_END %in% c("12.31.2021", "03.31.2022") ~ "YR1",
      QTR_END %in% c("06.30.2022", "09.30.2022", "12.31.2022", "03.31.2023") ~ "YR2",
      QTR_END %in% c("06.30.2023", "09.30.2023", "12.31.2023", "03.31.2024") ~ "YR3",
      QTR_END %in% c("06.30.2024", "09.30.2024", "12.31.2024", "03.31.2025") ~ "YR4",
      TRUE ~ as.character(NA)
    )
  ) %>%
  relocate(YR)

colnames(df) <-
  dplyr::recode(
    colnames(df),
    !!!setNames(df_variables$renamed_variables_lower, df_variables$no_total)
  )

df <- df %>% 
  clean_names() |> 
  filter(
    grantee != ""
  )

# export ------------------------------------------------------------------

output_path <- here::here("data-ready")
write.csv(df, paste(output_path, "combined_QPR_rh_data.csv", sep = "/"),
          row.names = FALSE,
          na = "")

write.csv(df_variables, paste(output_path, "wips_variables.csv", sep = "/"),
          row.names = FALSE,
          na = "")

