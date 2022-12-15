# this takes mutliple copies of the All Users CSV file and compares them, looking for errors in the last login date
# SSO all users is downloaded from https://signin.aucklandcouncil.govt.nz/ofis/pages/config/_stats.aspx

library(tidyverse)
library(lubridate)
library(janitor)
library(fs)


csv_files <- fs::dir_ls("data/all-users-past/", regexp = "\\.csv$") |>
  map_dfr(read_csv, .id = "source")
  # keep only some columns
  
csv_files_new <- csv_files |> 
  # keep only some columns
  select(1,3,5) |>
  # convert to R date
  mutate(sso_last_login_date = ymd(substr(LastLogin,1,8))) |>
  # drop LastLogin
  select(1,2,4) |>
  # delete path from ID
  mutate(across('source', str_replace, 'data/all-users-past/AllUsers-', ''))


csv_files_wide <- csv_files_new |>
  # make wide
  pivot_wider(id_cols = Email, names_from = source, values_from = sso_last_login_date)
  