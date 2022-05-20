library(RSelenium)
library(rvest)
library(dplyr)

### Create Index File ----------------------------------------------------------
# Use selenium to drive to results page
driver <- rsDriver(browser = "firefox") 
remote_driver <- driver[["client"]]
remote_driver$navigate("https://ncchildcaresearch.dhhs.state.nc.us/search.asp")
button <- remote_driver$findElement("css selector", "#submit_mult")
button$clickElement()

# Grab facility names and ids
links <- remote_driver$findElements("css selector", 
                                    ".table-responsive .table a")
link_text <- unlist(sapply(links, function(x) {
  x$getElementAttribute("href")
  }
))
name_text <- unlist(sapply(links, function(x) {
  x$getElementText()
  }
))

# Subset and save
facilities <- data.frame(fname = name_text,
                         fid = link_text)
facilities <- facilities[which(grepl("Facility_ID=", facilities$fname),]
fwrite(facilities, file = paste0("data_raw/index", Sys.Date(), ".csv"))

download_pages <- function(id, loc = paste0(getwd(), "/data_raw")) {
  #directory setup (if necessary)
  setwd(loc)
  date <- Sys.Date()
  if (!dir.exists(date)) {
    dir.create(date)
  }
  if (!dir.exists(paste0(date, "/", id))) {
    dir.create(paste0(date, "/", id))
  }
  #download pages
  subpages <- c("Main_info", "Permits", "Owners", "Visits", "Actions")
  stub <- "https://ncchildcaresearch.dhhs.state.nc.us/"
  sapply(subpages, function(x) {
    download.file(paste0(stub, x, "/.asp?Facility_ID=", id),  
                  destfile = paste0(date, "/", id, "/", x, ".htm"))
  })
  
}

### Parsers
parse_pages <- function(id, date = "recent") {
  #choose date
  if (date == "recent") {
    d1 <- sort(list.dirs("data_raw"), decreasing = TRUE)[1]
  } else {
    d1 <- date
  }
  #main page
  main_table <- read_html(paste0(id, "/", "Main_info.htm")) %>%
    html_nodes("font td") %>%
    html_text()
  if (grepl("Basic Information", main_table[1])) {
    f_address = main_table[5]
    f_email = main_table[7]
    f_website = main_table[9]
    progtype = main_table[11]
    subsidize = main_table[13]
    if (grepl("does not require", main_table[15], ignore.case = TRUE) | 
        grepl("home", progtype, ignore.case = TRUE)) {
      sani_date <- sani_class <- sani_score <-  NA
    } else {
      sani_date <- main_table[17]
      sani_class <- main_table[19]
      sani_score <- main_table[21]
    }
  }
  f_phone <- read_html(paste0(id, "/", "Main_info.htm")) %>%
    html_nodes("center font") %>%
    html_text()
  f_phone <- f_phone[which(grepl("Ph:", f_phone))]
  f_phone <- gsub("[^0-9]", "", gsub("^.+Ph:", "", f_phone))
  main_info <- cbind(id, f_address, f_phone, f_email, f_website, progtype, 
                     subsidize, sanitation)
  
  #license page
  lic_table <- read_html(paste0(id, "/", "Permits.htm")) %>%
    html_nodes("font td") %>%
    html_text()
  if (grepl("Current License Details", lic_table[1])) {
    positions <- c(3, 5, 7, 10, 12, 14, 16, 19, 21, 23, 25)
    vars <- c("lic_type", "lic_effdate", "ages","star_std_prog", "star_std_edu",
    "star_quality", "star_total", "cap_shift1", "cap_shift2", "cap_shift2", 
    "lic_restrict")
    for (i in 1:length(vars)) {
      assign(vars[i], lic_table[positions[i]])
    }
  }
}


# # search results
# https://ncchildcaresearch.dhhs.state.nc.us/search.asp
# 
# ".table-responsive .table table td"
# #drop first 3
# c1 = school name + link
# c2 = address (street \n city_zip \phone)
# c3 = star ranking
# 
# # school detail
# table = "font td"
# tabs <- c("Main_info", "Permits", "Owners", "Visits", "Actions")
# https://ncchildcaresearch.dhhs.state.nc.us/Main_info.asp?Facility_ID=92003306
# https://ncchildcaresearch.dhhs.state.nc.us/Permits.asp?Facility_ID=92003306
# https://ncchildcaresearch.dhhs.state.nc.us/Owners.asp?Facility_ID=92003306
# https://ncchildcaresearch.dhhs.state.nc.us/Visits.asp?Facility_ID=92003306
# https://ncchildcaresearch.dhhs.state.nc.us/Actions.asp?Facility_ID=92003306
# #button5  for visit report
