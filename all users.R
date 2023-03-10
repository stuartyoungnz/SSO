# this takes a single CSV file containing all records, 
# downloaded from https://signin.aucklandcouncil.govt.nz/ofis/pages/config/_stats.aspx
# and determines various reports

library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(ggthemes)

# read from CSV
allraw <- read.csv("AllUsers.csv") |>
# identify and remove bad rows
  mutate(test = substr(CreatedDate,1,2)) |>
  filter(test =="20")

# drop unneeded columns
all <-  select(allraw, 2:4) |>
# get the created date
  mutate(created_date = ymd(substr(CreatedDate,1,8))) |>
# get the creation year
  mutate(created_year = as.numeric(substr(CreatedDate,1,4))) |>
# get the creation month
  mutate(created_month = month(created_date)) |>
# get the creation month / year
  mutate(created_month_year = ym(substr(CreatedDate,1,6))) |>
# get the last login date
  mutate(last_login_date = ymd(substr(LastLogin,1,8))) |>
# get the last login year
  mutate(last_login_year = as.numeric(substr(LastLogin,1,4))) |>
# get the last login month
  mutate(last_login_month = month(last_login_date)) |>
# get the last login month / year
  mutate(last_login_month_year = ym(substr(LastLogin,1,6))) |>
# remove outlier  
  filter(created_month_year != "2016-01-01")

# get max date
max_date <- summarise(all, max_date = max(created_date))


# ======== determine active users ========
# get date 6 or 12 months prior to today's date
date_to_check <- Sys.Date() %m-% months(6)
date_to_check2 <- Sys.Date() %m-% months(12)

# check for dates after that date.
# Does not handle NA correctly
all <- all |>
  mutate(active_user = ifelse(created_date >= date_to_check, "yes", ifelse(last_login_date >= date_to_check, "yes", "no"))) |>
  relocate(active_user, .before = created_year) |>
  mutate(active_user2 = ifelse(created_date >= date_to_check2, "yes", ifelse(last_login_date >= date_to_check2, "yes", "no"))) |>
  relocate(active_user, .before = created_year)

# summarise all accounts and add total row 
creation_year <- group_by(all, created_year ) |>
  count() |>
  summarise(count = sum(n)) |>
  adorn_totals("row")

# summarise active users and add total row
creation_year_active <- all |>
  filter(active_user == "yes") |>
  group_by(created_year) |>
  count() |>
  summarise(active = sum(n)) |>
  adorn_totals("row")

# summarise active users and add total row
creation_year_active2 <- all |>
  filter(active_user2 == "yes") |>
  group_by(created_year) |>
  count() |>
  summarise(active = sum(n)) |>
  adorn_totals("row")

# join all and active - 6 months
creation_year_all_with_active <- 
  left_join(creation_year,creation_year_active, key=created_year) |>
  mutate(percent_active = (active/count), percent_active = scales::percent(percent_active))

view(creation_year_all_with_active)

# join all and active - 12 months
creation_year_all_with_active2 <- 
  left_join(creation_year,creation_year_active2, key=created_year) |>
  mutate(percent_active = (active/count), percent_active = scales::percent(percent_active))

view(creation_year_all_with_active2)

# join all and active - 6 and 12 months
creation_year_active <- creation_year_active |>
  rename('active_6_months' = active)
creation_year_active2 <- creation_year_active2 |>
  rename('active_12_months' = active)
creation_year_all_with_active3 <- 
  left_join(creation_year,creation_year_active, key=created_year)
creation_year_all_with_active3 <- 
  left_join(creation_year_all_with_active3,creation_year_active2, key=created_year) 

creation_year_all_with_active3 <- creation_year_all_with_active3 |>
  mutate(percent_active_6 = (active_6_months/count), percent_active_6 = scales::percent(percent_active_6)) |>
  mutate(percent_active_12 = (active_12_months/count), percent_active_12 = scales::percent(percent_active_12)) |>
  relocate(percent_active_6, .before = active_12_months)

view(creation_year_all_with_active3)


# summarise and add total row
login_year_by_creation_year <- group_by(all, last_login_year, created_year ) |>
  count() |>
  summarise(Count = sum(n)) |>
  adorn_totals("row")


creation_year_month <- group_by(all, created_year,created_month ) |>
  count()  |>
  summarise(Count = sum(n))

creation_year_month2 <- group_by(all, created_month_year ) |>
  count()  |>
  summarise(Count = sum(n))

# plot by month
ggplot(creation_year_month2, aes(created_month_year, Count)) +
  labs(y = "Number SSO accounts created", x = "Month") +
  scale_y_continuous(
    limits = c(0, 15000),
    expand = c(0, 0),
    breaks = seq(0, 15000, 5000),
    minor_breaks = seq(0, 15000, 1000)
  ) +
  scale_x_date(
    date_breaks = "1 year", date_labels = "%Y",
    date_minor_breaks = "1 month"
  ) +
  ggtitle("Monthly SSO accounts created") + 
#  theme_economist() + scale_colour_economist() +
  theme_minimal() +
  geom_line() + 
  geom_point()

# plot by month
ggplot(creation_year_month2, aes(created_month_year, Count)) +
  labs(y = "Number SSO accounts created", x = "Month") +
  scale_y_continuous(
    limits = c(0, 15000),
    expand = c(0, 0),
    breaks = seq(0, 15000, 5000),
    minor_breaks = seq(0, 15000, 1000)
  ) +
  scale_x_date(
    date_breaks = "1 year", date_labels = "%Y",
    date_minor_breaks = "1 month"
  ) +
  ggtitle("Monthly SSO accounts created") + 
  theme_minimal() +
  geom_area()

