# this takes CSV files from different sources and compares them, 
# SSO all users is downloaded from https://signin.aucklandcouncil.govt.nz/ofis/pages/config/_stats.aspx


# ======== load libraries ========

library(tidyverse)
library(lubridate)
library(janitor)
library(scales)


# ======== read all data and assemble main data frame ========

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

# get max date SSO
max_date_sso <- summarise(all_sso, max_date = max(sso_created_date))


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

# get max date myAUCKLAND rates
max_date_ma_rates <- summarise(ma_rates, max_date = max(ma_rates_created_date))

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

# get max date myAUCKLAND rates
max_date_ma_dogs <- summarise(ma_dogs, max_date = max(ma_dogs_created_date))

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

# get max date accommodation booking
max_date_accomm_booking <- summarise(accomm_booking, max_date = max(accomm_booking_created_date))


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
  mutate(libr_room_booking_date = dmy(BookingStartDate)) |>
  distinct(email, .keep_all = TRUE) |>
  # drop Datetime
  select(2:4)

# add library room booking data to all SSO
all_sso <- left_join(all_sso,libr_room_booking, key=email)

# get max date library room booking
max_date_libr_room_booking <- summarise(libr_room_booking, max_date = max(libr_room_booking_date))


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

# get max date venue hire booking
max_date_venue_hire <- "Unknown"


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

# get max date sports field booking (Sphere)
max_date_sports_sphere_booking <- summarise(sports_booking_sphere, max_date = max(sports_sphere_booking_created_date))


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

# get max date hybris orders
max_date_hybris_orders <- summarise(hybris_orders_csv_files, max_date = max(hybris_order_date))


# ======== determine active users ========

# get date 6 months prior to today's date
date_to_check <- Sys.Date() %m-% months(6)

# check for dates after that date. 
# Does not handle NA correctly
all_sso <- all_sso |>
  mutate(active_user = ifelse(sso_created_date >= date_to_check, "yes", ifelse(sso_last_login_date >= date_to_check, "yes", "no"))) |>
  relocate(active_user, .before = ma_rates_permission)


# ======== analyse 1. counts / totals ========

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

# ======== analyse 2. check the max dates ========

max_date_sso
max_date_ma_rates
max_date_ma_dogs
max_date_accomm_booking
max_date_venue_hire
max_date_libr_room_booking
max_date_sports_sphere_booking
max_date_hybris_orders


# ======== analyse 3. intersection of each one ========

# intersection of rates with others

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
ma_rates_intersect_bc <- ma_rates_intersect |>
  filter(hybris_order_type == 'BC') |>
  count()
ma_rates_intersect_rc <- ma_rates_intersect |>
  filter(hybris_order_type == 'RC') |>
  count()
ma_rates_intersect_licence <- ma_rates_intersect |>
  filter(hybris_order_type == 'Licence') |>
  count()
ma_rates_intersect_bwof <- ma_rates_intersect |>
  filter(hybris_order_type == 'BWOF Renewal') |>
  count()
ma_rates_intersect_none <- ma_rates_intersect |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_type)) |>
  count()


ma_rates_intersect_results <- data.frame(
  product = c("myAUCKLAND Dogs",
              "Accomm booking",
              "Venue hire",
              "Library room booking",
              "Sports field booking (OLD)",
              "BC orders",
              "RC orders",
              "Licence orders",
              "BWOF renewals",
              "None (myAUCKLAND rates is the only thing)"
  ),
  total = c(ma_rates_intersect_dogs$n[1],
            ma_rates_intersect_accomm$n[1],
            ma_rates_intersect_venue$n[1],
            ma_rates_intersect_library$n[1],
            ma_rates_intersect_sports$n[1],
            ma_rates_intersect_bc$n[1],
            ma_rates_intersect_rc$n[1],
            ma_rates_intersect_licence$n[1],
            ma_rates_intersect_bwof$n[1],
            ma_rates_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/ma_rates_count$n[1]), percent_users = scales::percent(percent_users))


# intersection of dogs with others

ma_dogs_intersect <- all_sso |>
  filter(ma_dogs_update_type == 'New')
ma_dogs_intersect_rates <- ma_dogs_intersect |>
  drop_na(ma_rates_permission) |>
  count()
ma_dogs_intersect_accomm <- ma_dogs_intersect |>
  drop_na(accomm_booking_status) |>
  count()
ma_dogs_intersect_venue <- ma_dogs_intersect |>
  drop_na(venue_hire_booking_status) |>
  count()
ma_dogs_intersect_library <- ma_dogs_intersect |>
  drop_na(libr_room_booking_status) |>
  count()
ma_dogs_intersect_sports <- ma_dogs_intersect |>
  drop_na(sports_sphere_booking_created_date) |>
  count()
ma_dogs_intersect_bc <- ma_dogs_intersect |>
  filter(hybris_order_type == 'BC') |>
  count()
ma_dogs_intersect_rc <- ma_dogs_intersect |>
  filter(hybris_order_type == 'RC') |>
  count()
ma_dogs_intersect_licence <- ma_dogs_intersect |>
  filter(hybris_order_type == 'Licence') |>
  count()
ma_dogs_intersect_bwof <- ma_dogs_intersect |>
  filter(hybris_order_type == 'BWOF Renewal') |>
  count()
ma_dogs_intersect_none <- ma_dogs_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_type)) |>
  count()


ma_dogs_intersect_results <- data.frame(
  product = c("myAUCKLAND Rates",
              "Accomm booking",
              "Venue hire",
              "Library room booking",
              "Sports field booking (OLD)",
              "BC orders",
              "RC orders",
              "Licence orders",
              "BWOF renewals",
              "None (myAUCKLAND Dogs is the only thing)"
  ),
  total = c(ma_dogs_intersect_rates$n[1],
            ma_dogs_intersect_accomm$n[1],
            ma_dogs_intersect_venue$n[1],
            ma_dogs_intersect_library$n[1],
            ma_dogs_intersect_sports$n[1],
            ma_dogs_intersect_bc$n[1],
            ma_dogs_intersect_rc$n[1],
            ma_dogs_intersect_licence$n[1],
            ma_dogs_intersect_bwof$n[1],
            ma_dogs_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/ma_dogs_count$n[1]), percent_users = scales::percent(percent_users))


# intersection of venue hire with others

venue_hire_intersect <- all_sso |>
  drop_na(venue_hire_booking_status)
venue_hire_intersect_rates <- venue_hire_intersect |>
  drop_na(ma_rates_permission) |>
  count()
venue_hire_intersect_dogs <- venue_hire_intersect |>
  filter(ma_dogs_update_type == 'New') |>
  count()
venue_hire_intersect_accomm <- venue_hire_intersect |>
  drop_na(accomm_booking_status) |>
  count()
venue_hire_intersect_library <- venue_hire_intersect |>
  drop_na(libr_room_booking_status) |>
  count()
venue_hire_intersect_sports <- venue_hire_intersect |>
  drop_na(sports_sphere_booking_created_date) |>
  count()
venue_hire_intersect_bc <- venue_hire_intersect |>
  filter(hybris_order_type == 'BC') |>
  count()
venue_hire_intersect_rc <- venue_hire_intersect |>
  filter(hybris_order_type == 'RC') |>
  count()
venue_hire_intersect_licence <- venue_hire_intersect |>
  filter(hybris_order_type == 'Licence') |>
  count()
venue_hire_intersect_bwof <- venue_hire_intersect |>
  filter(hybris_order_type == 'BWOF Renewal') |>
  count()
venue_hire_intersect_none <- venue_hire_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_type)) |>
  count()


venue_hire_intersect_results <- data.frame(
  product = c("myAUCKLAND Rates",
              "myAUCKLAND Dogs",
              "Accomm booking",
              "Library room booking",
              "Sports field booking (OLD)",
              "BC orders",
              "RC orders",
              "Licence orders",
              "BWOF renewals",
              "None (Venue hire is the only thing)"
  ),
  total = c(venue_hire_intersect_rates$n[1],
            venue_hire_intersect_dogs$n[1],
            venue_hire_intersect_accomm$n[1],
            venue_hire_intersect_library$n[1],
            venue_hire_intersect_sports$n[1],
            venue_hire_intersect_bc$n[1],
            venue_hire_intersect_rc$n[1],
            venue_hire_intersect_licence$n[1],
            venue_hire_intersect_bwof$n[1],
            venue_hire_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/venue_hire_count$n[1]), percent_users = scales::percent(percent_users))


# intersection of accomm booking with others

accomm_booking_intersect <- all_sso |>
  drop_na(accomm_booking_status)
accomm_booking_intersect_rates <- accomm_booking_intersect |>
  drop_na(ma_rates_permission) |>
  count()
accomm_booking_intersect_dogs <- accomm_booking_intersect |>
  filter(ma_dogs_update_type == 'New') |>
  count()
accomm_booking_intersect_venues <- accomm_booking_intersect |>
  drop_na(venue_hire_booking_status) |>
  count()
accomm_booking_intersect_library <- accomm_booking_intersect |>
  drop_na(libr_room_booking_status) |>
  count()
accomm_booking_intersect_sports <- accomm_booking_intersect |>
  drop_na(sports_sphere_booking_created_date) |>
  count()
accomm_booking_intersect_bc <- accomm_booking_intersect |>
  filter(hybris_order_type == 'BC') |>
  count()
accomm_booking_intersect_rc <- accomm_booking_intersect |>
  filter(hybris_order_type == 'RC') |>
  count()
accomm_booking_intersect_licence <- accomm_booking_intersect |>
  filter(hybris_order_type == 'Licence') |>
  count()
accomm_booking_intersect_bwof <- accomm_booking_intersect |>
  filter(hybris_order_type == 'BWOF Renewal') |>
  count()
accomm_booking_intersect_none <- accomm_booking_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_type)) |>
  count()


accomm_booking_intersect_results <- data.frame(
  product = c("myAUCKLAND Rates",
              "myAUCKLAND Dogs",
              "Venue hire",
              "Library room booking",
              "Sports field booking (OLD)",
              "BC orders",
              "RC orders",
              "Licence orders",
              "BWOF renewals",
              "None (Accomm booking is the only thing)"
  ),
  total = c(accomm_booking_intersect_rates$n[1],
            accomm_booking_intersect_dogs$n[1],
            accomm_booking_intersect_venues$n[1],
            accomm_booking_intersect_library$n[1],
            accomm_booking_intersect_sports$n[1],
            accomm_booking_intersect_bc$n[1],
            accomm_booking_intersect_rc$n[1],
            accomm_booking_intersect_licence$n[1],
            accomm_booking_intersect_bwof$n[1],
            accomm_booking_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/accomm_booking_count$n[1]), percent_users = scales::percent(percent_users))


# intersection of library room booking with others

libr_room_booking_intersect <- all_sso |>
  drop_na(libr_room_booking_status)
libr_room_booking_intersect_rates <- libr_room_booking_intersect |>
  drop_na(ma_rates_permission) |>
  count()
libr_room_booking_intersect_dogs <- libr_room_booking_intersect |>
  filter(ma_dogs_update_type == 'New') |>
  count()
libr_room_booking_intersect_venues <- libr_room_booking_intersect |>
  drop_na(venue_hire_booking_status) |>
  count()
libr_room_booking_intersect_accomm <- libr_room_booking_intersect |>
  drop_na(accomm_booking_status) |>
  count()
libr_room_booking_intersect_sports <- libr_room_booking_intersect |>
  drop_na(sports_sphere_booking_created_date) |>
  count()
libr_room_booking_intersect_bc <- libr_room_booking_intersect |>
  filter(hybris_order_type == 'BC') |>
  count()
libr_room_booking_intersect_rc <- libr_room_booking_intersect |>
  filter(hybris_order_type == 'RC') |>
  count()
libr_room_booking_intersect_licence <- libr_room_booking_intersect |>
  filter(hybris_order_type == 'Licence') |>
  count()
libr_room_booking_intersect_bwof <- libr_room_booking_intersect |>
  filter(hybris_order_type == 'BWOF Renewal') |>
  count()
libr_room_booking_intersect_none <- libr_room_booking_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_type)) |>
  count()


libr_room_booking_intersect_results <- data.frame(
  product = c("myAUCKLAND Rates",
              "myAUCKLAND Dogs",
              "Venue hire",
              "Accomm booking",
              "Sports field booking (OLD)",
              "BC orders",
              "RC orders",
              "Licence orders",
              "BWOF renewals",
              "None (Library room booking is the only thing)"
  ),
  total = c(libr_room_booking_intersect_rates$n[1],
            libr_room_booking_intersect_dogs$n[1],
            libr_room_booking_intersect_venues$n[1],
            libr_room_booking_intersect_accomm$n[1],
            libr_room_booking_intersect_sports$n[1],
            libr_room_booking_intersect_bc$n[1],
            libr_room_booking_intersect_rc$n[1],
            libr_room_booking_intersect_licence$n[1],
            libr_room_booking_intersect_bwof$n[1],
            libr_room_booking_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/libr_room_booking_count$n[1]), percent_users = scales::percent(percent_users))



# intersection of sports field booking with others

sports_booking_intersect <- all_sso |>
  drop_na(sports_sphere_booking_created_date)
sports_booking_intersect_rates <- sports_booking_intersect |>
  drop_na(ma_rates_permission) |>
  count()
sports_booking_intersect_dogs <- sports_booking_intersect |>
  filter(ma_dogs_update_type == 'New') |>
  count()
sports_booking_intersect_venues <- sports_booking_intersect |>
  drop_na(venue_hire_booking_status) |>
  count()
sports_booking_intersect_accomm <- sports_booking_intersect |>
  drop_na(accomm_booking_status) |>
  count()
sports_booking_intersect_library <- sports_booking_intersect |>
  drop_na(libr_room_booking_status) |>
  count()
sports_booking_intersect_bc <- sports_booking_intersect |>
  filter(hybris_order_type == 'BC') |>
  count()
sports_booking_intersect_rc <- sports_booking_intersect |>
  filter(hybris_order_type == 'RC') |>
  count()
sports_booking_intersect_licence <- sports_booking_intersect |>
  filter(hybris_order_type == 'Licence') |>
  count()
sports_booking_intersect_bwof <- sports_booking_intersect |>
  filter(hybris_order_type == 'BWOF Renewal') |>
  count()
sports_booking_intersect_none <- sports_booking_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(hybris_order_type)) |>
  count()


sports_booking_intersect_results <- data.frame(
  product = c("myAUCKLAND Rates",
              "myAUCKLAND Dogs",
              "Venue hire",
              "Accomm booking",
              "Library room booking",
              "BC orders",
              "RC orders",
              "Licence orders",
              "BWOF renewals",
              "None (Sports field booking is the only thing)"
  ),
  total = c(sports_booking_intersect_rates$n[1],
            sports_booking_intersect_dogs$n[1],
            sports_booking_intersect_venues$n[1],
            sports_booking_intersect_accomm$n[1],
            sports_booking_intersect_library$n[1],
            sports_booking_intersect_bc$n[1],
            sports_booking_intersect_rc$n[1],
            sports_booking_intersect_licence$n[1],
            sports_booking_intersect_bwof$n[1],
            sports_booking_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/sports_sphere_booking_count$n[1]), percent_users = scales::percent(percent_users))