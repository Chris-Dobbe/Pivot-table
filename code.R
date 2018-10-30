
############################### Standard Test/Ghost Test Lead Conversion #########################################

# Code used to create pivot tables for standard test/ghost tests for email programs

# Project Name: ???

# Data source: Marketo List Data

############################################################################################

# begin by setting the working directory for the files
setwd("C:/Users/cdobbe/Documents/Analysis/Program Reports/AB Tests/MRR5330 Charts Enhancement/data exports")
#Load the packages and libraries, or d/l any using install.packages("???")
 
library(plyr)
library(tidyverse)
library(pivottabler)
library(lubridate)


# This is intended to be used with 2 separate lead lists. Load data first, manipulation is in sections

# Load both data sets. object names are default. Group names are assigned within each manipulation section
red <- read_csv("trial_send_test.csv")
blu <- read_csv("trial_not_send_test.csv")
set_date <- ymd("2018-04-24")

red[is.na(red)] <- 0
blu[is.na(blu)] <- 0

# Now assign the group names to each of the data sets that you loaded

red <- add_column(red, group = "test", .before = 1)
blu <- add_column(blu, group = "ghost", .before = 1)

# Then join the two together into one data set we can work with & do some data type manipulation

green <- plyr::rbind.fill(red, blu)

# Update column names to make them easier to work with

colnames(green) <- c("group", "Id", "full.name", "email.address", "created.at", "account.role", "paid.license.limit",
                     "industry", "fit.rating", "android.app.login.count", "ios.app.login.count", "num.employees", "last.mobile.login",
                     "product", "monthly.plan.rate.usd", "payment.start.date", "owner.role.at.import", "sales.owner.job.title",
                     "trial.start.date", "trial.end.date", "test.buckets", "is.trial.restart", "web.form.date", "unsubscribed",
                     "last.login", "login.count", "sheet.count", "event.log.count", "template.sheets", "report.count", "card.view.count",
                     "community.member.id", "sharing.count", "dashboard.count", "used.cell.linking", "used.branded.workspace",
                     "used.change.view", "used.drive.attachment", "used.reminder")

# Sometimes product names are introduced with errors, so run a quick check here
green$product <- as.factor(green$product)
#str(green$product)


# The joining and data manipulation process converted all variables to character, to converting some back is needed
green$login.count <- as.numeric(green$login.count)
green$event.log.count <- as.numeric(green$event.log.count)
green$sheet.count <- as.numeric(green$sheet.count)
green$template.sheets <- as.numeric(green$template.sheets)
green$report.count <- as.numeric(green$report.count)
green$sharing.count <- as.numeric(green$sharing.count)
green$dashboard.count <- as.numeric(green$dashboard.count)
green$android.app.login.count <- as.numeric(green$android.app.login.count)
green$ios.app.login.count <- as.numeric(green$ios.app.login.count)

# Now there are certain fields that needed to be converted to date types for comparison
green$last.login <- mdy_hm(green$last.login)
green$last.mobile.login <- mdy_hm(green$last.mobile.login)
green$web.form.date <- mdy_hm(green$web.form.date)

# Exporting Any data sets with filters ------------------------------------

##### After data is prepared, export to CSV if needed for analysis outside of R

# Can export any of the existing objects, or create new objects to export to CSV
write_csv(object_name, "name_of_export_file.csv")

trigger_eligible <- green %>% filter(trigger.eligible == 'Got Trigger')
write_csv(trigger_eligible, "trigger_eligible_data.csv")

hvt_data<- green %>% filter(hvt == 'hvt')
write_csv(hvt_data, "hvt_data.csv")

id_triggers <- green %>% select(Id, first_sight, ten_webpages, strong_lead, team_trial, five_hun_actions,
                                first_report, six_sheets, five_hun_employees, hvt, enter_hvt, not_hvt)
write_csv(id_triggers, "ID_with_triggers.csv")

write_csv(green, "Full sales trigger data.csv")
        

# Manipulate the new complete data frame prior to summarizing ---------------------------------------

# First  we need to append our Calculated fields
green <- add_column(green, account_payor = "")
green$account_payor <- ifelse(green$monthly.plan.rate.usd > 0, "Yes", "No")

green <- add_column(green, shared = "")
green$shared <- ifelse(green$sharing.count > 0, "Yes", "No")

green <- add_column(green, created_sheet = "")
green$created_sheet <- ifelse(green$sheet.count > 0, "Yes", "No")

green <- add_column(green, created_template = "")
green$created_template <- ifelse(green$template.sheets > 0, "Yes", "No")

green <- add_column(green, created_report = "")
green$created_report <- ifelse(green$report.count > 0, "Yes", "No")

green <- add_column(green, card_view = "")
green$card_view <- ifelse(green$card.view.count > 0, "Yes", "No")

green <- add_column(green, community_member = "")
green$community_member <- ifelse(green$community.member.id > 0, "Yes", "No")

green <- add_column(green, dashboard_use = "")
green$dashboard_use <- ifelse(green$dashboard.count > 0, "Yes", "No")

green$num.employees <- as.numeric(green$num.employees)
green$employee_count <- cut(green$num.employees , breaks=c(-Inf, 1, 49, 499, Inf),
                                                               labels=c("empty", "1-49", "50-499", "500+"))
replace_empty <- "empty"
green$employee_count[is.na(green$employee_count)] <- replace_empty

green <- add_column(green, mobile_app_use = "")
green$mobile_app_use <- ifelse((green$android.app.login.count + green$ios.app.login.count) > 0, "Yes", "No")

green <- add_column(green, login_since_date = "")
green$login_since_date <- ifelse(green$last.login > set_date, "Yes", "No")

green <- add_column(green, mobile_login_since_date = "")
green$mobile_login_since_date <- ifelse(green$last.mobile.login > set_date, "Yes", "No")

green <- add_column(green, form_since_date = "")
green$form_since_date <- ifelse(green$web.form.date > set_date, "Yes", "No")

#for any missing dates in the referenced columns, NA's are set in new calculated columns, so replacing NA with No
missing_dates <- "No"
green$mobile_login_since_date[is.na(green$mobile_login_since_date)] <- missing_dates
green$login_since_date[is.na(green$login_since_date)] <- missing_dates
green$form_since_date[is.na(green$form_since_date)] <- missing_dates


# The joining and data manipulation process converted all variables to character, to converting some back is needed
green$login.count <- as.numeric(green$login.count)
green$event.log.count <- as.numeric(green$event.log.count)
green$sheet.count <- as.numeric(green$sheet.count)
green$template.sheets <- as.numeric(green$template.sheets)
green$report.count <- as.numeric(green$report.count)
green$sharing.count <- as.numeric(green$sharing.count)
green$card.view.count <- as.numeric(green$card.view.count)
green$community.member.id <- as.numeric(green$community.member.id)
green$dashboard.count <- as.numeric(green$dashboard.count)
green$android.app.login.count <- as.numeric(green$android.app.login.count)
green$ios.app.login.count <- as.numeric(green$ios.app.login.count)


##### _-_-_ Sections for Pivoting and summarizng the data!!!!! _-_-_ #####

# Pivoting by Group ONLY!!! -------------------------------------------------------------

# First pivot table pulls product by group and counts the occurrences
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("product")
pt$addRowDataGroups("group")
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()

# Product & Owner & Payer (This accounts for all of the product table variations)
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("product")
pt$addColumnDataGroups("account.role", addTotal = FALSE)
pt$addColumnDataGroups("account_payor", addTotal = FALSE) 
pt$addRowDataGroups("group")
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()

#Unsubcsribed
qhpvt(green, "group", "unsubscribed", "n()")

# Total MRR
qhpvt(green, "group", "group", "sum(monthly.plan.rate.usd, na.rm=TRUE)")

# Significance test for MRR
wilcox.test(monthly.plan.rate.usd ~ group, green)

# Average Deal Size (MRR for Payer accounts)
qhpvt(green, "group", "account_payor", "mean(monthly.plan.rate.usd, na.rm=TRUE)", format="%.2f")

# Average Paid License Limit
qhpvt(green, "group", "group", "mean(paid.license.limit, na.rm=TRUE)", format="%.1f")

# Trial Restarts
qhpvt(green, "group", "group", "sum(is.trial.restart, na.rm=TRUE)", format="%.f")

# Mobile App Logins (iOS or Android)
qhpvt(green, "group", "mobile_app_use", "n()")

# App Login Since Certain Date
qhpvt(green, "group", "login_since_date", "n()")

# Mobile Login Since Certain Date
qhpvt(green, "group", "mobile_login_since_date", "n()")

# Web Form Since Certain Date
qhpvt(green, "group", "form_since_date", "n()")

# Average activity count for variables
pt <- PivotTable$new()
pt$addData(green)
pt$addRowDataGroups("group")
pt$defineCalculation(calculationName="AvgLoginCount", caption="Avg Login Count",summariseExpression="mean(login.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgEventLogCount", caption="Avg Event Log Count",summariseExpression="mean(event.log.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgEmployeeCount", caption="Avg Employee Count",summariseExpression="mean(num.employees, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgSheetCount", caption="Avg Sheet Count",summariseExpression="mean(sheet.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgTemplateCount", caption="Avg Template Count",summariseExpression="mean(template.sheets, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgReportCount", caption="Avg Report Count",summariseExpression="mean(report.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgSharingCount", caption="Avg Sharing Count",summariseExpression="mean(sharing.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgDashboardCount", caption="Avg Dashboard Count",summariseExpression="mean(dashboard.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgAndroidLoginCount", caption="Avg Android Login",summariseExpression="mean(android.app.login.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgiOSLoginCount", caption="Avg iOS Logint",summariseExpression="mean(ios.app.login.count, na.rm=TRUE)", format="%2f")
pt$renderPivot()

# Count of additional occurrences of activities (loops through values and outputs tables in Console

actions.vec <- c("shared", "created_sheet","created_template", "created_report", "card_view", 
                 "community_member", "dashboard_use", "used.cell.linking", "used.branded.workspace", "used.change.view",
                 "used.drive.attachment", "used.reminder")

sapply(actions.vec, function(z){
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addColumnDataGroups(z, addTotal = FALSE)
  pt$defineCalculation(calculationName = "Count", summariseExpression="n()")
  pt$evaluatePivot()
  pt
})


# Pivoting by Employee Count!!! -------------------------------------------

# Employee Count Buckets
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("employee_count", addTotal = FALSE)
pt$addRowDataGroups("group")
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()

#Product by group and employee group
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("product")
pt$addRowDataGroups("group")
pt$addRowDataGroups("employee_count", addTotal = FALSE)
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()

# Product & Owner & Payer (This accounts for all of the product table variations)
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("product")
pt$addColumnDataGroups("account.role", addTotal = FALSE)
pt$addColumnDataGroups("account_payor", addTotal = FALSE) 
pt$addRowDataGroups("group")
pt$addRowDataGroups("employee_count", addTotal = FALSE)
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()

# Unsubscribe by group & Employee Count Buckets
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("unsubscribed")
pt$addRowDataGroups("group")
pt$addRowDataGroups("employee_count", addTotal = FALSE)
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()

# Total MRR by Employee size
qhpvt(green, "employee_count", "group", "sum(monthly.plan.rate.usd, na.rm=TRUE)")

# Average Deal Size (MRR for Payor Accounts)
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("account_payor")
pt$addRowDataGroups("group")
pt$addRowDataGroups("employee_count", addTotal = FALSE)
pt$defineCalculation(calculationName = "ProductCount", caption="Average Deal Size", summariseExpression="mean(monthly.plan.rate.usd, na.rm=TRUE)", format="%.2f")
pt$renderPivot()

# Average Paid License Limit
qhpvt(green, "employee_count", "group", "mean(paid.license.limit, na.rm=TRUE)", format="%.1f")

# Trial Restarts
qhpvt(green, "employee_count", "group", "sum(is.trial.restart, na.rm=TRUE)", format="%.f")

# Mobile App Logins (iOS or Android)
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("mobile_app_use")
pt$addRowDataGroups("group")
pt$addRowDataGroups("employee_count", addTotal = FALSE)
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()

# App Login Since certain date
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("login_since_date")
pt$addRowDataGroups("group")
pt$addRowDataGroups("employee_count", addTotal = FALSE)
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()

# Mobile Login Since certain date
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("mobile_login_since_date")
pt$addRowDataGroups("group")
pt$addRowDataGroups("employee_count", addTotal = FALSE)
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()

# Filled out web form since date
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("form_since_date")
pt$addRowDataGroups("group")
pt$addRowDataGroups("employee_count", addTotal = FALSE)
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()

# Average activity count for variables
pt <- PivotTable$new()
pt$addData(green)
pt$addRowDataGroups("group")
pt$addRowDataGroups("employee_count", addTotal = FALSE)
pt$defineCalculation(calculationName="AvgLoginCount", caption="Avg Login Count",summariseExpression="mean(login.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgEventLogCount", caption="Avg Event Log Count",summariseExpression="mean(event.log.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgEmployeeCount", caption="Avg Employee Count",summariseExpression="mean(num.employees, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgSheetCount", caption="Avg Sheet Count",summariseExpression="mean(sheet.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgTemplateCount", caption="Avg Template Count",summariseExpression="mean(template.sheets, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgReportCount", caption="Avg Report Count",summariseExpression="mean(report.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgSharingCount", caption="Avg Sharing Count",summariseExpression="mean(sharing.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgDashboardCount", caption="Avg Dashboard Count",summariseExpression="mean(dashboard.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgAndroidLoginCount", caption="Avg Android Login",summariseExpression="mean(android.app.login.count, na.rm=TRUE)", format="%.2f")
pt$defineCalculation(calculationName="AvgiOSLoginCount", caption="Avg iOS Logint",summariseExpression="mean(ios.app.login.count, na.rm=TRUE)", format="%2f")
pt$renderPivot()

# Count of additional occurrences of activities (has to be done 1 at a time)

actions.vec <- c("shared", "created_sheet","created_template", "created_report", "card_view", 
                 "community_member", "dashboard_use", "used.cell.linking", "used.branded.workspace", "used.change.view",
                 "used.drive.attachment", "used.reminder")

sapply(actions.vec, function(z){
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addRowDataGroups("employee_count", addTotal = FALSE)
  pt$addColumnDataGroups(z, addTotal = FALSE)
  pt$defineCalculation(calculationName = "Count", summariseExpression="n()")
  pt$evaluatePivot()
  pt
})


# Group + Filter: Define filter value  ---------------------------------------


### First create your filter variable and values for use in the functions
filter.variable <- "login_since_date"
filter.value <- "Yes"
actions.vec <- c("shared", "created_sheet","created_template", "created_report", "card_view", 
                  "community_member", "dashboard_use", "used.cell.linking", "used.branded.workspace", "used.change.view",
                  "used.drive.attachment", "used.reminder")
# Function pivot table: product x group and filter 
# Note: initially we just create the functions, we then call them later on to create pivot tables
product.pivot <- function(x, y)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addColumnDataGroups("product")
  pt$addRowDataGroups("group")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
  pt$renderPivot()
}

# Product & Owner & Payer (This accounts for all of the product table variations)
payer.role.pivot <- function(x, y)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addColumnDataGroups("product")
  pt$addColumnDataGroups("account.role", addTotal = FALSE)
  pt$addColumnDataGroups("account_payor", addTotal = FALSE) 
  pt$addRowDataGroups("group")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
  pt$renderPivot()
}

#Unsubcsribed
unsub.pivot <- function(x, y)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addColumnDataGroups("unsubscribed")
  pt$addRowDataGroups("group")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
  pt$renderPivot()
}

# Total MRR
mrr.pivot <- function(x, y)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$defineCalculation(calculationName = "Total.MRR", summariseExpression="sum(monthly.plan.rate.usd)")
  pt$renderPivot()
}

# Average Deal Size
deal.size.pivot <- function(x, y)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addRowDataGroups("account_payor")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$defineCalculation(calculationName = "average.deal.size", summariseExpression="mean(monthly.plan.rate.usd)", format="%.2f")
  pt$renderPivot()
}

# Average Paid License Limit
paid.license.pivot <- function(x, y)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$defineCalculation(calculationName = "average.license.limit", summariseExpression="mean(paid.license.limit)", format="%.2f")
  pt$renderPivot()
}

# Trial Restarts
trial.restart.pivot <- function(x, y)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$defineCalculation(calculationName = "trial.restart", summariseExpression="sum(is.trial.restart)", format="%.2f")
  pt$renderPivot()
}

# Mobile App Logins
mobile.app.login.pivot <- function(x, y)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addRowDataGroups("mobile_app_use")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$defineCalculation(calculationName = "mobile.app.login", summariseExpression="n()")
  pt$renderPivot()
}

# App login since certain date
app.login.date.pivot <- function(x, y)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addRowDataGroups("login_since_date")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$defineCalculation(calculationName = "app.login.date", summariseExpression="n()")
  pt$renderPivot()
}

# Mobile Login Since Date
mobile.login.date.pivot <- function(x, y)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addRowDataGroups("mobile_login_since_date")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$defineCalculation(calculationName = "mobile.login.date", summariseExpression="n()")
  pt$renderPivot()
}

# web form Since Date
form.date.pivot <- function(x, y)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addRowDataGroups("form_since_date")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$defineCalculation(calculationName = "form.date", summariseExpression="n()")
  pt$renderPivot()
}


# Average activity count for variables
activity.pivot <- function(x, y)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$defineCalculation(calculationName="AvgLoginCount", caption="Avg Login Count",summariseExpression="mean(login.count, na.rm=TRUE)", format="%.2f")
  pt$defineCalculation(calculationName="AvgEventLogCount", caption="Avg Event Log Count",summariseExpression="mean(event.log.count, na.rm=TRUE)", format="%.2f")
  pt$defineCalculation(calculationName="AvgEmployeeCount", caption="Avg Employee Count",summariseExpression="mean(num.employees, na.rm=TRUE)", format="%.2f")
  pt$defineCalculation(calculationName="AvgSheetCount", caption="Avg Sheet Count",summariseExpression="mean(sheet.count, na.rm=TRUE)", format="%.2f")
  pt$defineCalculation(calculationName="AvgTemplateCount", caption="Avg Template Count",summariseExpression="mean(template.sheets, na.rm=TRUE)", format="%.2f")
  pt$defineCalculation(calculationName="AvgReportCount", caption="Avg Report Count",summariseExpression="mean(report.count, na.rm=TRUE)", format="%.2f")
  pt$defineCalculation(calculationName="AvgSharingCount", caption="Avg Sharing Count",summariseExpression="mean(sharing.count, na.rm=TRUE)", format="%.2f")
  pt$defineCalculation(calculationName="AvgDashboardCount", caption="Avg Dashboard Count",summariseExpression="mean(dashboard.count, na.rm=TRUE)", format="%.2f")
  pt$defineCalculation(calculationName="AvgAndroidLoginCount", caption="Avg Android Login",summariseExpression="mean(android.app.login.count, na.rm=TRUE)", format="%.2f")
  pt$defineCalculation(calculationName="AvgiOSLoginCount", caption="Avg iOS Logint",summariseExpression="mean(ios.app.login.count, na.rm=TRUE)", format="%.2f")
  pt$renderPivot()
}

# Count of additional occurrences of activities (has to be done 1 at a time)

activity.occur.pivot <- function(x, y, z)
{
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addRowDataGroups(x, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(y))
  pt$addColumnDataGroups(z, addTotal = FALSE)
  pt$defineCalculation(calculationName = "Count", summariseExpression="n()")
  pt$renderPivot()
}

### Group + Filter: Run functions, create pivot tables ##########
product.pivot(filter.variable, filter.value)
payer.role.pivot(filter.variable, filter.value)
unsub.pivot(filter.variable, filter.value)
mrr.pivot(filter.variable, filter.value)
deal.size.pivot(filter.variable, filter.value)
paid.license.pivot(filter.variable, filter.value)
trial.restart.pivot(filter.variable, filter.value)
mobile.app.login.pivot(filter.variable, filter.value)
app.login.date.pivot(filter.variable, filter.value)
mobile.login.date.pivot(filter.variable, filter.value)
form.date.pivot(filter.variable, filter.value)
activity.pivot(filter.variable, filter.value)
sapply(actions.vec, function(z){
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addRowDataGroups(filter.variable, addTotal = FALSE, fromData = FALSE, explicitListOfValues = list(filter.value))
  pt$addColumnDataGroups(z, addTotal = FALSE)
  pt$defineCalculation(calculationName = "Count", summariseExpression="n()")
  pt$evaluatePivot()
  pt
})

### END OF CODE ###
