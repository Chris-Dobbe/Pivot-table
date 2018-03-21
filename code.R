
############################### Merging lists, adding values, and pivoting to summarize #########################################

# Project Owner:

# Project Name:  

# Data source: 

############################################################################################

# begin by setting the working directory for the files
setwd("C:\\Users\\")
#Load the packages and libraries, or d/l any using install.packages("???")

library(plyr)
library(tidyverse)
library(pivottabler)
library(openxlsx)

# This is intended to be used with 2 separate lead lists. Load data first, manipulation is in sections

# Load both data sets. object names are default. Group names are assigned within each manipulation section
red <- read_csv("Sales_80_Gets_Trig_Trial_Alerts_ALL_Q2_3.csv")
blu <- read_csv("Sales_20_Not_Trigger_ALL_Q2_3.csv")

# Then join the two together into one data set we can work with & do some data type manipulation

green <- plyr::rbind.fill(red, blu)

# Update column names to make them easier to work with

colnames(green) <- c("group", "Id", "full.name", "email.address", "created.at", "account.role", "person.status",
                     "lead.status.at.import", "fit.rating", "dashboard.count", "trigger.eligible", "num.employees", "account.health",
                     "product", "monthly.plan.rate.usd", "payment.start.date", "owner.role.at.import", "sales.owner.job.title",
                     "trial.start.date", "trial.end.date", "test.buckets", "is.ever.well.qualified", "is.strong.lead", "unsubscribed",
                     "last.login", "login.count", "sheet.count", "event.log.count", "template.sheets", "report.count", "card.view.count",
                     "community.member.id", "sharing.count")

# Sometimes product names are introduced with errors, so run a quick check here
green$product <- as.factor(green$product)
summary(green$product)

### If everything looks good, skip ahead to the 'Manipulate' section
### Otherwise code below can be used to replace whatever values are needed

#green$product <- replace(green$product, green$product == "free", "Free")


### Identifying Users that were eligible for specific Triggers --------------

### First need to load all of the lists for the triggers

trig1 <- read_csv("Sales_01_Created_1st_Sight_V2.csv")
trig2 <- read_csv("Sales_02_Visited_10_web_pages_V2.csv")
trig3 <- read_csv("Sales_03_Strong_lead_Fit_Rating_A_V2.csv")
trig4 <- read_csv("Sales_04_Team_Trial_Owners_V2.csv")
trig5 <- read_csv("Sales_05_Over_500_Actions_V2.csv")
trig6 <- read_csv("Sales_06_Created_1st_Report.csv")
trig7 <- read_csv("Sales_07_Created_6_sheets.csv")
trig8 <- read_csv("Sales_08_500_Employees.csv")
hvt <- read_csv("Sales_HVT_Trigger_V2.csv")
enter_hvt <- read_csv("Sales_HVT_Entered_as_HVT.csv")
not_hvt <- read_csv("Sales_HVT_NOT_Entered_as_HVT.csv")

### With lists loaded, we need to add a column with their ID to append for ID matches

trig1$trig1_sights <- rep("first_sight",nrow(trig1))
trig2$trig2_webpages <- rep("ten_webpages",nrow(trig2))
trig3$trig3_stronglead <- rep("strong_lead",nrow(trig3))
trig4$trig4_teamtrial <- rep("team_trial",nrow(trig4))
trig5$trig5_500actions <- rep("five_hun_actions",nrow(trig5))
trig6$trig6_1streport <- rep("first_report",nrow(trig6))
trig7$trig7_6sheets <- rep("six_sheets",nrow(trig7))
trig8$trig8_500employees <- rep("five_hun_employees",nrow(trig8))
hvt$hvt_status <- rep("hvt",nrow(hvt))
enter_hvt$enter_hvt <- rep("enter_hvt",nrow(enter_hvt))
not_hvt$not_hvt <- rep("not_enter_hvt",nrow(not_hvt))

### Now we can begin appending each of the trigger lists to the all_leads full lead list
green <- merge(green, trig1, by = "Id", all.x = TRUE)
names(green) <- names(green)
green[is.na(green)] <- "No"
colnames(green)[34] <- "first_sight"

green <- merge(green, trig2, by = "Id", all.x = TRUE)
names(green) <- names(green)
green[is.na(green)] <- "No"
colnames(green)[35] <- "ten_webpages"

green <- merge(green, trig3, by = "Id", all.x = TRUE)
names(green) <- names(green)
green[is.na(green)] <- "No"
colnames(green)[36] <- "strong_lead"

green <- merge(green, trig4, by = "Id", all.x = TRUE)
names(green) <- names(green)
green[is.na(green)] <- "No"
colnames(green)[37] <- "team_trial"

green <- merge(green, trig5, by = "Id", all.x = TRUE)
names(green) <- names(green)
green[is.na(green)] <- "No"
colnames(green)[38] <- "five_hun_actions"

green <- merge(green, trig6, by = "Id", all.x = TRUE)
names(green) <- names(green)
green[is.na(green)] <- "No"
colnames(green)[39] <- "first_report"

green <- merge(green, trig7, by = "Id", all.x = TRUE)
names(green) <- names(green)
green[is.na(green)] <- "No"
colnames(green)[40] <- "six_sheets"

green <- merge(green, trig8, by = "Id", all.x = TRUE)
names(green) <- names(green)
green[is.na(green)] <- "No"
colnames(green)[41] <- "five_hun_employees"

green <- merge(green, hvt, by = "Id", all.x = TRUE)
names(green) <- names(green)
green[is.na(green)] <- "No"
colnames(green)[42] <- "hvt"

green <- merge(green, enter_hvt, by = "Id", all.x = TRUE)
names(green) <- names(green)
green[is.na(green)] <- "No"
colnames(green)[43] <- "enter_hvt"

green <- merge(green, not_hvt, by = "Id", all.x = TRUE)
names(green) <- names(green)
green[is.na(green)] <- "No"
colnames(green)[44] <- "not_hvt"

# The joining and data manipulation process converted all variables to character, to converting some back is needed
green$login.count <- as.numeric(green$login.count)
green$event.log.count <- as.numeric(green$event.log.count)
green$sheet.count <- as.numeric(green$sheet.count)
green$template.sheets <- as.numeric(green$template.sheets)
green$report.count <- as.numeric(green$report.count)
green$sharing.count <- as.numeric(green$sharing.count)


### Exporting Any data sets with filters ------------------------------------

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
        

### Manipulate the new complete data frame prior to summarizing ---------------------------------------

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

green$num.employees <- as.numeric(green$num.employees)
green$employee_count <- cut(green$num.employees , breaks=c(-Inf, 1, 49, 499, Inf),
                                                               labels=c("empty", "1-49", "50-499", "500+"))
replace_empty <- "empty"
green$employee_count[is.na(green$employee_count)] <- replace_empty

green <- add_column(green, dashboard_user = "")
green$dashboard_user <- ifelse(green$dashboard.count > 0, "Yes", "No")

#### ___________ Pivoting Section ________________ #####

# Group: Pivot by Group only -------------------------------------------------------------

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
pt$renderPivot()

# Count of additional occurrences of activities (loops through values and outputs tables in Console

actions.vec <- c("account_payor", "shared", "created_sheet","created_template", "created_report", "card_view", "community_member")

sapply(actions.vec, function(z){
  pt <- PivotTable$new()
  pt$addData(green)
  pt$addRowDataGroups("group")
  pt$addColumnDataGroups(z, addTotal = FALSE)
  pt$defineCalculation(calculationName = "Count", summariseExpression="n()")
  pt$evaluatePivot()
  pt
})

# Group + Employee Count!!! -------------------------------------------

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
qhpvt(green, "group", "employee_count", "sum(monthly.plan.rate.usd, na.rm=TRUE)")

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
pt$renderPivot()

# Count of additional occurrences of activities (has to be done 1 at a time)

actions.vec <- c("account_payor", "shared", "created_sheet","created_template", "created_report", "card_view", "community_member")

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
filter.variable <- "hvt"
filter.value <- "hvt"
actions.vec <- c("account_payor", "shared", "created_sheet","created_template", "created_report", "card_view", "community_member")

### Group + Filter: Create Functions to be Run ----------------------------------------------

# Function pivot table: product x group and filter
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

### GRoup + Filter: Run functions, create pivot tables ##########
product.pivot(filter.variable, filter.value)
payer.role.pivot(filter.variable, filter.value)
unsub.pivot(filter.variable, filter.value)
mrr.pivot(filter.variable, filter.value)
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
