# this takes a single CSV file containing all records, 
# downloaded from https://signin.aucklandcouncil.govt.nz/ofis/pages/config/_stats.aspx
# and determines various reports

library(tidyverse)
library(lubridate)

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

# summarise
creation_year <- group_by(all, created_year ) |>
  count() |>
  summarise(Count = sum(n))

creation_year <- rbind(data.frame(creation_year, created_year='Total', t(colSums(creation_year[, -1]))))

creation_year_new <- creation_year %>%
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'Total')))


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
  geom_line() + 
  geom_point()
