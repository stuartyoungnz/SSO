# this takes CSV files from different sources and compares them, 
# SSO all users is downloaded from https://signin.aucklandcouncil.govt.nz/ofis/pages/config/_stats.aspx


# ======== load libraries ========

library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(readxl)

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


# read Hybris orders and process
hybris_orders_csv_files <- fs::dir_ls("data/hybris-orders/", regexp = "\\.csv$") |>
  map_dfr(read_csv) |>
  select(3,7,8) |>
  rename(email = 'Email') |>
  rename(hybris_order_type = 'Class Code') |>
  rename(hybris_order_date = 'Hybris Form Lodged Calendar Date Dt') |>
  mutate(across('hybris_order_type', str_replace, 'BC-Streamline', 'BC'))

# split Hybris orders
hybris_orders_bc <- hybris_orders_csv_files |>
  filter(hybris_order_type == 'BC') |>
  rename(hybris_order_bc = hybris_order_type) |>
  rename(hybris_order_date_bc = hybris_order_date) |>
  distinct(email, .keep_all = TRUE)
hybris_orders_rc <- hybris_orders_csv_files |>
  filter(hybris_order_type == 'RC') |>
  rename(hybris_order_rc = hybris_order_type) |>
  rename(hybris_order_date_rc = hybris_order_date) |>
  distinct(email, .keep_all = TRUE)
hybris_orders_licence <- hybris_orders_csv_files |>
  filter(hybris_order_type == 'Licence') |>
  rename(hybris_order_licence = hybris_order_type) |>
  rename(hybris_order_date_licence = hybris_order_date) |>
  distinct(email, .keep_all = TRUE)
hybris_orders_bwof <- hybris_orders_csv_files |>
  filter(hybris_order_type == 'BWOF Renewal') |>
  rename(hybris_order_bwof = hybris_order_type) |>
  rename(hybris_order_date_bwof = hybris_order_date) |>
  distinct(email, .keep_all = TRUE)

# add BC orders data to all SSO
all_sso <- left_join(all_sso,hybris_orders_bc, key=email)
# add RC orders data to all SSO
all_sso <- left_join(all_sso,hybris_orders_rc, key=email)
# add Licence orders data to all SSO
all_sso <- left_join(all_sso,hybris_orders_licence, key=email)
# add BWOF orders data to all SSO
all_sso <- left_join(all_sso,hybris_orders_bwof, key=email)

# get max date hybris orders
max_date_hybris_orders <- summarise(hybris_orders_csv_files, max_date = max(hybris_order_date))


# read solicitors CSV
solicitors <- read_xlsx("data/Solicitor Users List 2022.xlsx") |>
  # rename columns
  rename(email = p_uid) |>
  # convert dates to R dates
  mutate(solicitors_created_date = ymd(substr(createdTS,1,10))) |>
  # remove rows with duplicate email addresses
  distinct(email, .keep_all = TRUE) |>
  # drop Datetime
  select(3,4)

# add solicitors data to all SSO
all_sso <- left_join(all_sso,solicitors, key=email)

# get max date solicitors
max_date_solicitors <- summarise(solicitors, max_date = max(solicitors_created_date))


# ======== determine active users ========

# get date 6 months prior to today's date
date_to_check <- Sys.Date() %m-% months(6)

# check for dates after that date. 
# Does not handle NA correctly
all_sso <- all_sso |>
  mutate(active_user = ifelse(sso_created_date >= date_to_check, "yes", ifelse(sso_last_login_date >= date_to_check, "yes", "no"))) |>
  relocate(active_user, .before = ma_rates_permission)


# ======== determine total services ========

all_sso <- all_sso |>
  mutate (total_services = 
            ifelse(!is.na(ma_rates_created_date), 1,0) +
            ifelse(!is.na(ma_dogs_created_date), 1,0) +
            ifelse(!is.na(accomm_booking_created_date), 1,0) +
            ifelse(!is.na(libr_room_booking_date), 1,0) +
            ifelse(!is.na(venue_hire_booking_status), 1,0) +
            ifelse(!is.na(sports_sphere_booking_created_date), 1,0) +
            ifelse(!is.na(hybris_order_bc), 1,0) +
            ifelse(!is.na(hybris_order_rc), 1,0) +
            ifelse(!is.na(hybris_order_licence), 1,0) +
            ifelse(!is.na(hybris_order_bwof), 1,0) +
            ifelse(!is.na(solicitors_created_date), 1,0)
          )

view(all_sso)


# ======== analyse 1. counts / totals ========

# count distribution of number of products

count_number_products <- all_sso |>
  group_by(total_services) |>
  count()
view(count_number_products)

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
  filter(hybris_order_bc == 'BC') |>
  count()
rc_count <- all_sso |>
  filter(hybris_order_rc == 'RC') |>
  count()
licence_count <- all_sso |>
  filter(hybris_order_licence == 'Licence') |>
  count()
bwof_count <- all_sso |>
  filter(hybris_order_bwof == 'BWOF Renewal') |>
  count()
solicitors_count <- all_sso |>
  drop_na(solicitors_created_date) |>
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
  filter(hybris_order_bc == 'BC') |>
  filter(active_user == "yes") |>
  count()
rc_active_count <- all_sso |>
  filter(hybris_order_rc == 'RC') |>
  filter(active_user == "yes") |>
  count()
licence_active_count <- all_sso |>
  filter(hybris_order_licence == 'Licence') |>
  filter(active_user == "yes") |>
  count()
bwof_active_count <- all_sso |>
  filter(hybris_order_bwof == 'BWOF Renewal') |>
  filter(active_user == "yes") |>
  count()
solicitors_active_count <- all_sso |>
  drop_na(solicitors_created_date) |>
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
              "BWOF renewals",
              "Solicitors rates statement"
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
            bwof_count$n[1],
            solicitors_count$n[1]
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
             bwof_active_count$n[1],
             solicitors_active_count$n[1]
             )
) |>
  mutate(percent_active = (active/total), percent_active = scales::percent(percent_active))

view(product_with_active)

# ======== analyse 2. check the max dates ========

max_date_sso
max_date_ma_rates
max_date_ma_dogs
max_date_accomm_booking
max_date_venue_hire
max_date_libr_room_booking
max_date_sports_sphere_booking
max_date_hybris_orders
max_date_solicitors

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
  filter(hybris_order_bc == 'BC') |>
  count()
ma_rates_intersect_rc <- ma_rates_intersect |>
  filter(hybris_order_rc == 'RC') |>
  count()
ma_rates_intersect_licence <- ma_rates_intersect |>
  filter(hybris_order_licence == 'Licence') |>
  count()
ma_rates_intersect_bwof <- ma_rates_intersect |>
  filter(hybris_order_bwof == 'BWOF Renewal') |>
  count()
ma_rates_intersect_solicitors <- ma_rates_intersect |>
  drop_na(solicitors_created_date) |>
  count()
ma_rates_intersect_none <- ma_rates_intersect |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_bc)) |>
  filter(is.na(hybris_order_rc)) |>
  filter(is.na(hybris_order_licence)) |>
  filter(is.na(hybris_order_bwof)) |>
  filter(is.na(solicitors_created_date)) |>
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
              "Solicitors rates account",
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
            ma_rates_intersect_solicitors$n[1],
            ma_rates_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/ma_rates_count$n[1]), percent_users = scales::percent(percent_users))

view(ma_rates_intersect_results)


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
  filter(hybris_order_bc == 'BC') |>
  count()
ma_dogs_intersect_rc <- ma_dogs_intersect |>
  filter(hybris_order_rc == 'RC') |>
  count()
ma_dogs_intersect_licence <- ma_dogs_intersect |>
  filter(hybris_order_licence == 'Licence') |>
  count()
ma_dogs_intersect_bwof <- ma_dogs_intersect |>
  filter(hybris_order_bwof == 'BWOF Renewal') |>
  count()
ma_dogs_intersect_solicitors <- ma_dogs_intersect |>
  drop_na(solicitors_created_date) |>
  count()
ma_dogs_intersect_none <- ma_dogs_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_bc)) |>
  filter(is.na(hybris_order_rc)) |>
  filter(is.na(hybris_order_licence)) |>
  filter(is.na(hybris_order_bwof)) |>
  filter(is.na(solicitors_created_date)) |>
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
              "Solicitors rates account",
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
            ma_dogs_intersect_solicitors$n[1],
            ma_dogs_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/ma_dogs_count$n[1]), percent_users = scales::percent(percent_users))

view(ma_dogs_intersect_results)


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
  filter(hybris_order_bc == 'BC') |>
  count()
venue_hire_intersect_rc <- venue_hire_intersect |>
  filter(hybris_order_rc == 'RC') |>
  count()
venue_hire_intersect_licence <- venue_hire_intersect |>
  filter(hybris_order_licence == 'Licence') |>
  count()
venue_hire_intersect_bwof <- venue_hire_intersect |>
  filter(hybris_order_bwof == 'BWOF Renewal') |>
  count()
venue_hire_intersect_solicitors <- venue_hire_intersect |>
  drop_na(solicitors_created_date) |>
  count()
venue_hire_intersect_none <- venue_hire_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_bc)) |>
  filter(is.na(hybris_order_rc)) |>
  filter(is.na(hybris_order_licence)) |>
  filter(is.na(hybris_order_bwof)) |>
  filter(is.na(solicitors_created_date)) |>
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
              "Solicitors rates account",
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
            venue_hire_intersect_solicitors$n[1],
            venue_hire_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/venue_hire_count$n[1]), percent_users = scales::percent(percent_users))

view(venue_hire_intersect_results)


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
  filter(hybris_order_bc == 'BC') |>
  count()
accomm_booking_intersect_rc <- accomm_booking_intersect |>
  filter(hybris_order_rc == 'RC') |>
  count()
accomm_booking_intersect_licence <- accomm_booking_intersect |>
  filter(hybris_order_licence == 'Licence') |>
  count()
accomm_booking_intersect_bwof <- accomm_booking_intersect |>
  filter(hybris_order_bwof == 'BWOF Renewal') |>
  count()
accomm_booking_intersect_solicitors <- accomm_booking_intersect |>
  drop_na(solicitors_created_date) |>
  count()
accomm_booking_intersect_none <- accomm_booking_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_bc)) |>
  filter(is.na(hybris_order_rc)) |>
  filter(is.na(hybris_order_licence)) |>
  filter(is.na(hybris_order_bwof)) |>
  filter(is.na(solicitors_created_date)) |>
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
              "Solicitors rates account",
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
            accomm_booking_intersect_solicitors$n[1],
            accomm_booking_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/accomm_booking_count$n[1]), percent_users = scales::percent(percent_users))

view(accomm_booking_intersect_results)


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
  filter(hybris_order_bc == 'BC') |>
  count()
libr_room_booking_intersect_rc <- libr_room_booking_intersect |>
  filter(hybris_order_rc == 'RC') |>
  count()
libr_room_booking_intersect_licence <- libr_room_booking_intersect |>
  filter(hybris_order_licence == 'Licence') |>
  count()
libr_room_booking_intersect_bwof <- libr_room_booking_intersect |>
  filter(hybris_order_bwof == 'BWOF Renewal') |>
  count()
libr_room_booking_intersect_solicitors <- libr_room_booking_intersect |>
  drop_na(solicitors_created_date) |>
  count()
libr_room_booking_intersect_none <- libr_room_booking_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_bc)) |>
  filter(is.na(hybris_order_rc)) |>
  filter(is.na(hybris_order_licence)) |>
  filter(is.na(hybris_order_bwof)) |>
  filter(is.na(solicitors_created_date)) |>
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
              "Solicitors rates account",
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
            libr_room_booking_intersect_solicitors$n[1],
            libr_room_booking_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/libr_room_booking_count$n[1]), percent_users = scales::percent(percent_users))

view(libr_room_booking_intersect_results)


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
  filter(hybris_order_bc == 'BC') |>
  count()
sports_booking_intersect_rc <- sports_booking_intersect |>
  filter(hybris_order_rc == 'RC') |>
  count()
sports_booking_intersect_licence <- sports_booking_intersect |>
  filter(hybris_order_licence == 'Licence') |>
  count()
sports_booking_intersect_bwof <- sports_booking_intersect |>
  filter(hybris_order_bwof == 'BWOF Renewal') |>
  count()
sports_booking_intersect_solicitors <- sports_booking_intersect |>
  drop_na(solicitors_created_date) |>
  count()
sports_booking_intersect_none <- sports_booking_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(hybris_order_bc)) |>
  filter(is.na(hybris_order_rc)) |>
  filter(is.na(hybris_order_licence)) |>
  filter(is.na(hybris_order_bwof)) |>
  filter(is.na(solicitors_created_date)) |>
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
              "Solicitors rates account",
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
            sports_booking_intersect_solicitors$n[1],
            sports_booking_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/sports_sphere_booking_count$n[1]), percent_users = scales::percent(percent_users))

view(sports_booking_intersect_results)


# intersection of BC orders with others

bc_orders_intersect <- all_sso |>
  filter(hybris_order_bc == 'BC')
bc_orders_intersect_rates <- bc_orders_intersect |>
  drop_na(ma_rates_permission) |>
  count()
bc_orders_intersect_dogs <- bc_orders_intersect |>
  filter(ma_dogs_update_type == 'New') |>
  count()
bc_orders_intersect_venues <- bc_orders_intersect |>
  drop_na(venue_hire_booking_status) |>
  count()
bc_orders_intersect_accomm <- bc_orders_intersect |>
  drop_na(accomm_booking_status) |>
  count()
bc_orders_intersect_library <- bc_orders_intersect |>
  drop_na(libr_room_booking_status) |>
  count()
bc_orders_intersect_sports <- bc_orders_intersect |>
  drop_na(sports_sphere_booking_created_date) |>
  count()
bc_orders_intersect_rc <- bc_orders_intersect |>
  filter(hybris_order_rc == 'RC') |>
  count()
bc_orders_intersect_licence <- bc_orders_intersect |>
  filter(hybris_order_licence == 'Licence') |>
  count()
bc_orders_intersect_bwof <- bc_orders_intersect |>
  filter(hybris_order_bwof == 'BWOF Renewal') |>
  count()
bc_orders_intersect_solicitors <- bc_orders_intersect |>
  drop_na(solicitors_created_date) |>
  count()
bc_orders_intersect_none <- bc_orders_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_rc)) |>
  filter(is.na(hybris_order_licence)) |>
  filter(is.na(hybris_order_bwof)) |>
  filter(is.na(solicitors_created_date)) |>
  count()


bc_orders_intersect_results <- data.frame(
  product = c("myAUCKLAND Rates",
              "myAUCKLAND Dogs",
              "Venue hire",
              "Accomm booking",
              "Library room booking",
              "Sports field booking (OLD)",
              "RC orders",
              "Licence orders",
              "BWOF renewals",
              "Solicitors rates account",
              "None (BC orders is the only thing)"
  ),
  total = c(bc_orders_intersect_rates$n[1],
            bc_orders_intersect_dogs$n[1],
            bc_orders_intersect_venues$n[1],
            bc_orders_intersect_accomm$n[1],
            bc_orders_intersect_library$n[1],
            bc_orders_intersect_sports$n[1],
            bc_orders_intersect_rc$n[1],
            bc_orders_intersect_licence$n[1],
            bc_orders_intersect_bwof$n[1],
            bc_orders_intersect_solicitors$n[1],
            bc_orders_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/bc_count$n[1]), percent_users = scales::percent(percent_users))

view(bc_orders_intersect_results)


# intersection of RC orders with others

rc_orders_intersect <- all_sso |>
  filter(hybris_order_rc == 'RC')
rc_orders_intersect_rates <- rc_orders_intersect |>
  drop_na(ma_rates_permission) |>
  count()
rc_orders_intersect_dogs <- rc_orders_intersect |>
  filter(ma_dogs_update_type == 'New') |>
  count()
rc_orders_intersect_venues <- rc_orders_intersect |>
  drop_na(venue_hire_booking_status) |>
  count()
rc_orders_intersect_accomm <- rc_orders_intersect |>
  drop_na(accomm_booking_status) |>
  count()
rc_orders_intersect_library <- rc_orders_intersect |>
  drop_na(libr_room_booking_status) |>
  count()
rc_orders_intersect_sports <- rc_orders_intersect |>
  drop_na(sports_sphere_booking_created_date) |>
  count()
rc_orders_intersect_bc <- rc_orders_intersect |>
  filter(hybris_order_bc == 'BC') |>
  count()
rc_orders_intersect_licence <- rc_orders_intersect |>
  filter(hybris_order_licence == 'Licence') |>
  count()
rc_orders_intersect_bwof <- rc_orders_intersect |>
  filter(hybris_order_bwof == 'BWOF Renewal') |>
  count()
rc_orders_intersect_solicitors <- rc_orders_intersect |>
  drop_na(solicitors_created_date) |>
  count()
rc_orders_intersect_none <- rc_orders_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_bc)) |>
  filter(is.na(hybris_order_licence)) |>
  filter(is.na(hybris_order_bwof)) |>
  filter(is.na(solicitors_created_date)) |>
  count()


rc_orders_intersect_results <- data.frame(
  product = c("myAUCKLAND Rates",
              "myAUCKLAND Dogs",
              "Venue hire",
              "Accomm booking",
              "Library room booking",
              "Sports field booking (OLD)",
              "BC orders",
              "Licence orders",
              "BWOF renewals",
              "Solicitors rates account",
              "None (RC orders is the only thing)"
  ),
  total = c(rc_orders_intersect_rates$n[1],
            rc_orders_intersect_dogs$n[1],
            rc_orders_intersect_venues$n[1],
            rc_orders_intersect_accomm$n[1],
            rc_orders_intersect_library$n[1],
            rc_orders_intersect_sports$n[1],
            rc_orders_intersect_bc$n[1],
            rc_orders_intersect_licence$n[1],
            rc_orders_intersect_bwof$n[1],
            rc_orders_intersect_solicitors$n[1],
            rc_orders_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/rc_count$n[1]), percent_users = scales::percent(percent_users))

view(rc_orders_intersect_results)


# intersection of Licence orders with others

licence_orders_intersect <- all_sso |>
  filter(hybris_order_licence == 'Licence')
licence_orders_intersect_rates <- licence_orders_intersect |>
  drop_na(ma_rates_permission) |>
  count()
licence_orders_intersect_dogs <- licence_orders_intersect |>
  filter(ma_dogs_update_type == 'New') |>
  count()
licence_orders_intersect_venues <- licence_orders_intersect |>
  drop_na(venue_hire_booking_status) |>
  count()
licence_orders_intersect_accomm <- licence_orders_intersect |>
  drop_na(accomm_booking_status) |>
  count()
licence_orders_intersect_library <- licence_orders_intersect |>
  drop_na(libr_room_booking_status) |>
  count()
licence_orders_intersect_sports <- licence_orders_intersect |>
  drop_na(sports_sphere_booking_created_date) |>
  count()
licence_orders_intersect_bc <- licence_orders_intersect |>
  filter(hybris_order_bc == 'BC') |>
  count()
licence_orders_intersect_rc <- licence_orders_intersect |>
  filter(hybris_order_rc == 'RC') |>
  count()
licence_orders_intersect_bwof <- licence_orders_intersect |>
  filter(hybris_order_bwof == 'BWOF Renewal') |>
  count()
licence_orders_intersect_solicitors <- licence_orders_intersect |>
  drop_na(solicitors_created_date) |>
  count()
licence_orders_intersect_none <- licence_orders_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_bc)) |>
  filter(is.na(hybris_order_rc)) |>
  filter(is.na(hybris_order_bwof)) |>
  filter(is.na(solicitors_created_date)) |>
  count()


licence_orders_intersect_results <- data.frame(
  product = c("myAUCKLAND Rates",
              "myAUCKLAND Dogs",
              "Venue hire",
              "Accomm booking",
              "Library room booking",
              "Sports field booking (OLD)",
              "BC orders",
              "RC orders",
              "BWOF renewals",
              "Solicitors rates account",
              "None (Licence orders is the only thing)"
  ),
  total = c(licence_orders_intersect_rates$n[1],
            licence_orders_intersect_dogs$n[1],
            licence_orders_intersect_venues$n[1],
            licence_orders_intersect_accomm$n[1],
            licence_orders_intersect_library$n[1],
            licence_orders_intersect_sports$n[1],
            licence_orders_intersect_bc$n[1],
            licence_orders_intersect_rc$n[1],
            licence_orders_intersect_bwof$n[1],
            licence_orders_intersect_solicitors$n[1],
            licence_orders_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/licence_count$n[1]), percent_users = scales::percent(percent_users))

view(licence_orders_intersect_results)


# intersection of BWOF orders with others

bwof_orders_intersect <- all_sso |>
  filter(hybris_order_bwof == 'BWOF Renewal')
bwof_orders_intersect_rates <- bwof_orders_intersect |>
  drop_na(ma_rates_permission) |>
  count()
bwof_orders_intersect_dogs <- bwof_orders_intersect |>
  filter(ma_dogs_update_type == 'New') |>
  count()
bwof_orders_intersect_venues <- bwof_orders_intersect |>
  drop_na(venue_hire_booking_status) |>
  count()
bwof_orders_intersect_accomm <- bwof_orders_intersect |>
  drop_na(accomm_booking_status) |>
  count()
bwof_orders_intersect_library <- bwof_orders_intersect |>
  drop_na(libr_room_booking_status) |>
  count()
bwof_orders_intersect_sports <- bwof_orders_intersect |>
  drop_na(sports_sphere_booking_created_date) |>
  count()
bwof_orders_intersect_bc <- bwof_orders_intersect |>
  filter(hybris_order_bc == 'BC') |>
  count()
bwof_orders_intersect_rc <- bwof_orders_intersect |>
  filter(hybris_order_rc == 'RC') |>
  count()
bwof_orders_intersect_licence <- bwof_orders_intersect |>
  filter(hybris_order_licence == 'Licence') |>
  count()
bwof_orders_intersect_solicitors <- bwof_orders_intersect |>
  drop_na(solicitors_created_date) |>
  count()
bwof_orders_intersect_none <- bwof_orders_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_bc)) |>
  filter(is.na(hybris_order_rc)) |>
  filter(is.na(hybris_order_licence)) |>
  filter(is.na(solicitors_created_date)) |>
  count()


bwof_orders_intersect_results <- data.frame(
  product = c("myAUCKLAND Rates",
              "myAUCKLAND Dogs",
              "Venue hire",
              "Accomm booking",
              "Library room booking",
              "Sports field booking (OLD)",
              "BC orders",
              "RC orders",
              "Licence orders",
              "Solicitors rates account",
              "None (BWOF renewals is the only thing)"
  ),
  total = c(bwof_orders_intersect_rates$n[1],
            bwof_orders_intersect_dogs$n[1],
            bwof_orders_intersect_venues$n[1],
            bwof_orders_intersect_accomm$n[1],
            bwof_orders_intersect_library$n[1],
            bwof_orders_intersect_sports$n[1],
            bwof_orders_intersect_bc$n[1],
            bwof_orders_intersect_rc$n[1],
            bwof_orders_intersect_licence$n[1],
            bwof_orders_intersect_solicitors$n[1],
            bwof_orders_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/bwof_count$n[1]), percent_users = scales::percent(percent_users))

view(bwof_orders_intersect_results)


# intersection of Solicitors rates account with others

solicitors_intersect <- all_sso |>
  drop_na(solicitors_created_date)
solicitors_intersect_rates <- solicitors_intersect |>
  drop_na(ma_rates_permission) |>
  count()
solicitors_intersect_dogs <- solicitors_intersect |>
  filter(ma_dogs_update_type == 'New') |>
  count()
solicitors_intersect_venues <- solicitors_intersect |>
  drop_na(venue_hire_booking_status) |>
  count()
solicitors_intersect_accomm <- solicitors_intersect |>
  drop_na(accomm_booking_status) |>
  count()
solicitors_intersect_library <- solicitors_intersect |>
  drop_na(libr_room_booking_status) |>
  count()
solicitors_intersect_sports <- solicitors_intersect |>
  drop_na(sports_sphere_booking_created_date) |>
  count()
solicitors_intersect_bc <- solicitors_intersect |>
  filter(hybris_order_bc == 'BC') |>
  count()
solicitors_intersect_rc <- solicitors_intersect |>
  filter(hybris_order_rc == 'RC') |>
  count()
solicitors_intersect_licence <- solicitors_intersect |>
  filter(hybris_order_licence == 'Licence') |>
  count()
solicitors_intersect_bwof <- solicitors_intersect |>
  filter(hybris_order_bwof == 'BWOF Renewal') |>
  count()
solicitors_intersect_none <- solicitors_intersect |>
  filter(is.na(ma_rates_permission)) |>
  filter(is.na(ma_dogs_update_type)) |>
  filter(is.na(venue_hire_booking_status)) |>
  filter(is.na(accomm_booking_status)) |>
  filter(is.na(libr_room_booking_status)) |>
  filter(is.na(sports_sphere_booking_created_date)) |>
  filter(is.na(hybris_order_bc)) |>
  filter(is.na(hybris_order_rc)) |>
  filter(is.na(hybris_order_licence)) |>
  filter(is.na(hybris_order_bwof)) |>
  count()


solicitors_intersect_results <- data.frame(
  product = c("myAUCKLAND Rates",
              "myAUCKLAND Dogs",
              "Venue hire",
              "Accomm booking",
              "Library room booking",
              "Sports field booking (OLD)",
              "BC orders",
              "RC orders",
              "Licence orders",
              "BWOF renewals",
              "None (Solicitors rates account is the only thing)"
  ),
  total = c(solicitors_intersect_rates$n[1],
            solicitors_intersect_dogs$n[1],
            solicitors_intersect_venues$n[1],
            solicitors_intersect_accomm$n[1],
            solicitors_intersect_library$n[1],
            solicitors_intersect_sports$n[1],
            solicitors_intersect_bc$n[1],
            solicitors_intersect_rc$n[1],
            solicitors_intersect_licence$n[1],
            solicitors_intersect_bwof$n[1],
            solicitors_intersect_none$n[1]
  )
) |>
  mutate(percent_users = (total/solicitors_count$n[1]), percent_users = scales::percent(percent_users))

view(solicitors_intersect_results)

# ========= table of they only do one thing =======

none_intersect_results <- data.frame(
  product = c("myAUCKLAND Rates",
              "myAUCKLAND Dogs",
              "Venue hire",
              "Accomm booking",
              "Library room booking",
              "Sports field booking (OLD)",
              "BC orders",
              "RC orders",
              "Licence orders",
              "BWOF renewals",
              "Solicitors rates account"
  ),
  no_others = c(ma_rates_intersect_none$n[1],
            ma_dogs_intersect_none$n[1],
            venue_hire_intersect_none$n[1],
            accomm_booking_intersect_none$n[1],
            libr_room_booking_intersect_none$n[1],
            sports_booking_intersect_none$n[1],
            bc_orders_intersect_none$n[1],
            rc_orders_intersect_none$n[1],
            licence_orders_intersect_none$n[1],
            bwof_orders_intersect_none$n[1],
            solicitors_intersect_none$n[1]
  ),
  total = c(ma_rates_count$n[1],
            ma_dogs_count$n[1],
            venue_hire_count$n[1],
            accomm_booking_count$n[1],
            libr_room_booking_count$n[1],
            sports_sphere_booking_count$n[1],
            bc_count$n[1],
            rc_count$n[1],
            licence_count$n[1],
            bwof_count$n[1],
            solicitors_count$n[1]
  )
) |>
  mutate(percent_users = (no_others/total), percent_users = scales::percent(percent_users))

view(none_intersect_results)




