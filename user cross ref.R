# this takes CSV files from different sources and compares them, 
# SSO all users is downloaded from https://signin.aucklandcouncil.govt.nz/ofis/pages/config/_stats.aspx


# ======== load libraries ========

library(tidyverse)
library(lubridate)
library(janitor)
library(scales)


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


# read Sphere user account
sphere_account_raw <- read.csv("U:/CityWide/Permanent/EBS Data Extract/SphereUsersExtract.csv")

sphere_account <- sphere_account_raw |>
  select(CustomerNumber,ProfileEmail,LastAuthenticatedDate,CreatedDate) |>
  rename(email = ProfileEmail) |>
  rename(sphere_customer_id = CustomerNumber) |>
  mutate(sphere_account_last_login_date = dmy(substr(LastAuthenticatedDate,1,10))) |>
  mutate(sphere_account_created_date = dmy(CreatedDate)) |>
  distinct(email, .keep_all = TRUE) |>
  # drop Datetime (x2)
  select(1,2,5,6)

# add Sphere account to all SSO
all_sso <- left_join(all_sso,sphere_account, key=email)

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


# read sports booking (Sphere)
casual_sports_booking <- read.csv("U:/CityWide/Permanent/EBS Data Extract/SphereSportsCasualExtract.csv") |>
  select(DateCreated,RegistrationEmail) |>
  rename(email = RegistrationEmail) |>
  mutate(sports_sphere_booking_created_date = dmy(DateCreated)) |>
  distinct(email, .keep_all = TRUE) |>
  # drop Datetime
  select(2:3)
seasonal_sports_booking <- read.csv("U:/CityWide/Permanent/EBS Data Extract/SphereSportsSeasonalExtract.csv") |>
  select(DateCreated,PrimaryEmailAddress) |>
  rename(email = PrimaryEmailAddress) |>
  mutate(sports_sphere_booking_created_date = dmy(DateCreated)) |>
  distinct(email, .keep_all = TRUE) |>
  # drop Datetime
  select(2:3)
sports_booking_sphere <- bind_rows(casual_sports_booking,seasonal_sports_booking)


# add sports sphere booking data to all SSO
all_sso <- left_join(all_sso,sports_booking_sphere, key=email)

# read Hybris orders
hybris_orders_csv_files <- fs::dir_ls("data/hybris-orders/", regexp = "\\.csv$") |>
  map_dfr(read_csv) |>
  select(3,7,8) |>
  rename(email = 'Email') |>
  distinct(email, .keep_all = TRUE) |>
  rename(hybris_order_date = 'Hybris Form Lodged Calendar Date Dt') |>
  rename(hybris_order_type = 'Class Code')


# add hybris orders data to all SSO
all_sso <- left_join(all_sso,hybris_orders_csv_files, key=email)

# ======== determine active users ========

# get date 6 months prior to today's date
date_to_check <- Sys.Date() %m-% months(6)

# check for dates after that date. 
# Does not handle NA correctly
all_sso <- all_sso |>
  mutate(active_user = ifelse(sso_created_date >= date_to_check, "yes", ifelse(sso_last_login_date >= date_to_check, "yes", "no"))) |>
  relocate(active_user, .before = ma_rates_permission)


# ======== analyse ========

# count each one
sphere_account_count <- all_sso |>
  drop_na(sphere_customer_id) |>
  count()
venue_hire_count <- all_sso |>
  filter(venue_hire_booking_status == 'Confirmed') |>
  count()
libr_room_booking_count <- all_sso |>
  drop_na(libr_room_booking_status) |>
  count()
sports_sphere_booking_count <- all_sso |>
  drop_na(sports_sphere_booking_created_date) |>
  count()
accomm_booking_count <- all_sso |>
  drop_na(accomm_booking_status) |>
  count()
ma_dogs_count <- all_sso |>
  filter(ma_dogs_update_type == 'New') |>
  count()
ma_rates_count <- all_sso |>
  drop_na(ma_rates_permission) |>
  count()
bc_count <- all_sso |>
  filter(hybris_order_type == 'BC') |>
  count()
rc_count <- all_sso |>
  filter(hybris_order_type == 'RC') |>
  count()
licence_count <- all_sso |>
  filter(hybris_order_type == 'Licence') |>
  count()
bwof_count <- all_sso |>
  filter(hybris_order_type == 'BWOF Renewal') |>
  count()


# count active for each one
sphere_account_active_count <- all_sso |>
  drop_na(sphere_customer_id) |>
  filter(active_user == "yes") |>
  count()
venue_hire_active_count <- all_sso |>
  filter(venue_hire_booking_status == 'Confirmed') |>
  filter(active_user == "yes") |>
  count()
libr_room_booking_active_count <- all_sso |>
  drop_na(libr_room_booking_status) |>
  filter(active_user == "yes") |>
  count()
sports_sphere_booking_active_count <- all_sso |>
  drop_na(sports_sphere_booking_created_date) |>
  filter(active_user == "yes") |>
  count()
accomm_booking_active_count <- all_sso |>
  drop_na(accomm_booking_status) |>
  filter(active_user == "yes") |>
  count()
ma_dogs_active_count <- all_sso |>
  filter(ma_dogs_update_type == 'New') |>
  filter(active_user == "yes") |>
  count()
ma_rates_active_count <- all_sso |>
  drop_na(ma_rates_permission) |>
  filter(active_user == "yes") |>
  count()
bc_active_count <- all_sso |>
  filter(hybris_order_type == 'BC') |>
  filter(active_user == "yes") |>
  count()
rc_active_count <- all_sso |>
  filter(hybris_order_type == 'RC') |>
  filter(active_user == "yes") |>
  count()
licence_active_count <- all_sso |>
  filter(hybris_order_type == 'Licence') |>
  filter(active_user == "yes") |>
  count()
bwof_active_count <- all_sso |>
  filter(hybris_order_type == 'BWOF Renewal') |>
  filter(active_user == "yes") |>
  count()



# assemble data frame for totals and active totals
product_with_active <- data.frame(
  product = c("myAUCKLAND Dogs",
              "myAUCKLAND Rates",
              "Accomm booking",
              "Venue hire",
              "Library room booking",
              "Sports field booking (OLD)",
              "BC orders",
              "RC orders",
              "Licence orders",
              "BWOF renewals"
              ),
  total = c(ma_dogs_count$n[1],
            ma_rates_count$n[1],
            accomm_booking_count$n[1],
            venue_hire_count$n[1],
            libr_room_booking_count$n[1],
            sports_sphere_booking_count$n[1],
            bc_count$n[1],
            rc_count$n[1],
            licence_count$n[1],
            bwof_count$n[1]
            ),
  active = c(ma_dogs_active_count$n[1],
             ma_rates_active_count$n[1],
             accomm_booking_active_count$n[1],
             venue_hire_active_count$n[1],
             libr_room_booking_active_count$n[1],
             sports_sphere_booking_active_count$n[1],
             bc_active_count$n[1],
             rc_active_count$n[1],
             licence_active_count$n[1],
             bwof_active_count$n[1]
             )
) |>
  mutate(percent_active = (active/total), percent_active = scales::percent(percent_active))

# intersection of each one
ma_rates_intersect <- all_sso |>
  drop_na(ma_rates_permission)
ma_rates_intersect_dogs <- ma_rates_intersect |>
  filter(ma_dogs_update_type == 'New') |>
  count()
ma_rates_intersect_accomm <- ma_rates_intersect |>
  drop_na(accomm_booking_status) |>
  count()
ma_rates_intersect_venue <- ma_rates_intersect |>
  drop_na(venue_hire_booking_status) |>
  count()
ma_rates_intersect_library <- ma_rates_intersect |>
  drop_na(libr_room_booking_status) |>
  count()
ma_rates_intersect_sports <- ma_rates_intersect |>
  drop_na(sports_sphere_booking_created_date) |>
  count()





