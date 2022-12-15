# this takes CSV files from different sources and compares them, 
# SSO all users is downloaded from https://signin.aucklandcouncil.govt.nz/ofis/pages/config/_stats.aspx


# ======== load libraries ========

library(tidyverse)
library(lubridate)
library(janitor)


# ======== read all data and assemble file ========

# read all SSO users CSV
all_sso <- read.csv("AllUsers.csv") |>
  # identify and remove bad rows
  mutate(test = substr(CreatedDate,1,2)) |>
  filter(test =="20") |>
  # drop unneeded columns
  select(2:4) |>
  # get the created date
  mutate(sso_created_date = ymd(substr(CreatedDate,1,8))) |>
  # get the last login date
  mutate(sso_last_login_date = ymd(substr(LastLogin,1,8))) |>
  # rename email column
  rename(email = Email) |>
  # drop unneeded columns
  select(1,4:5)


# read myAUCKLAND rates CSV
ma_rates <- read.csv("data/myAUCKLAND-rates-start.csv") |>
  # trim cols
  select(1:3) |>
  # remove rows with duplicate email addresses
  distinct(email, .keep_all = TRUE) |>
  # convert dates to R dates
  mutate(ma_rates_created_date = dmy(substr(Datetime,1,10))) |>
  # rename the permission column
  rename(ma_rates_permission = Permission) |>
  # drop Datetime
  select(1:2,4)

# add myAUCKLAND rates data to all SSO
all_sso <- left_join(all_sso,ma_rates, key=email)

# read myAUCKLAND dogs CSV
ma_dogs <- read.csv("data/myAUCKLAND-dogs-start.csv") |>
  # trim cols
  select(1,4:5) |>
  # rename email column
  rename(email = Owner.Email) |>
  # convert dates to R dates
  mutate(ma_dogs_created_date = dmy(substr(Created.Date,1,10))) |>
  # rename the permission column
  rename(ma_dogs_update_type = Update.Type) |>
  # remove rows with duplicate email addresses
  distinct(email, .keep_all = TRUE) |>
  # drop Datetime
  select(2:4)

# add myAUCKLAND dogs data to all SSO
all_sso <- left_join(all_sso,ma_dogs, key=email)

# read accommodation booking CSV
accomm_booking <- read.csv("data/accomm-booking-start.csv") |>
  # rename columns
  rename(email = Customer.Email.Address) |>
  rename(accomm_booking_status = Booking.Status) |>
  # convert dates to R dates
  mutate(accomm_booking_created_date = dmy(substr(Booking.Creation.Calendar.Date.Dt,1,10))) |>
  # remove rows with duplicate email addresses
  distinct(email, .keep_all = TRUE) |>
  # drop Datetime
  select(1,3:4)
  
# add accommodation booking data to all SSO
all_sso <- left_join(all_sso,accomm_booking, key=email)

# read library room booking
libr_room_booking <- read.csv("U:/CityWide/Permanent/EBS Data Extract/SphereLibaryBookingsExtract.csv") |>
  select(BookingStartDate,BookingStatus,ProfileEmail) |>
  rename(email = ProfileEmail) |>
  rename(libr_room_booking_status = BookingStatus) |>
  mutate(libr_room_booking_created_date = dmy(BookingStartDate)) |>
  distinct(email, .keep_all = TRUE) |>
  # drop Datetime
  select(2:4)

# add library room booking data to all SSO
all_sso <- left_join(all_sso,libr_room_booking, key=email)


# read venue hire booking
venue_csv_files <- fs::dir_ls("data/venue-hire/", regexp = "\\.csv$") |>
  map_dfr(read_csv)
  
venue_hire_booking <- venue_csv_files |>
  select(7,30) |>
  rename(email = 'Profile Email') |>
  distinct(email, .keep_all = TRUE) |>
  rename(venue_hire_booking_status = 'Booking Status')


# add venue hire booking data to all SSO
all_sso <- left_join(all_sso,venue_hire_booking, key=email)

# ======== read all data and assemble file ========
