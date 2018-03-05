
############################### Project Name  #########################################

# Description of project

# Project Name: 

# Data source: 

############################################################################################

# begin by setting the working directory for the files
setwd("???_Directory_???")

#Load the packages and libraries, or d/l any using install.packages("???")

library(plyr)
library(tidyverse)
library(pivottabler)
# the pivottaabler package vignette is at the URL below for refrence
# https://cran.r-project.org/web/packages/pivottabler/vignettes/v01-introduction.html

# This is intended to be used with 2 separate lead lists. Load data first, manipulation is in sections

# Load both data sets. object names are default. Group names are assigned within each manipulation section
red <- read_csv("???.csv")
blu <- read_csv("???.csv")

# Now assign the group names to each of the data sets that you loaded

red <- add_column(red, group = "Monthly", .before = 1)
blu <- add_column(blu, group = "Quarterly", .before = 1)

# Then join the two together into one data set we can work with

green <- plyr::rbind.fill(red, blu)

# Update column names to make them easier to work with

colnames(green) <- c("group", "id", "full.name", "email.address", "created.at", "account.role", "person.status",
                     "lead.status.at.import", "fit.rating", "job.title", "title.rank", "num.eployees", "account.health",
                     "product", "monthly.plan.rate.usd", "lead.source.at.import", "owner.role.at.import", "sales.owner.job.title",
                     "trial.start.date", "trial.end.date", "test.buckets", "is.ever.well.qualified", "is.strong.lead", "unsubscribed",
                     "last.login", "login.count", "sheet.count", "event.log.count", "template.sheets", "report.count", "card.view.count",
                     "community.member.id", "sharing.count", "lead.status")

# Sometimes variables are introduced with errors, so run a quick check here and replace them if needed
summary(green$product)

# If everything looks good, skip ahead to the 'Manipulate' section, otherwise code below can be used to replace
green$product <- replace(green$product, green$product == "free", "Free")

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

green$employee_count <- cut(green$num.eployees , breaks=c(-Inf, 1, 49, 499, Inf),
                                                               labels=c("empty", "1-49", "50-499", "500+"))
replace_empty <- "empty"
green$employee_count[is.na(green$employee_count)] <- replace_empty

# Pivoting!!! -------------------------------------------------------------

#these pivot tables are only rendered in the Viewer at this point in time

# First pivot table pulls product by group and counts the occurrences
pt <- PivotTable$new() #invokes the creation of a new pivot table
pt$addData(green)  #declare what data frame you're going to be using data from
pt$addColumnDataGroups("product")  #declare the variable that will generate the column headers
pt$addRowDataGroups("group")  #declare the variable that will generate the row headers
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")  #important piece is the calculation being done "n()"
pt$renderPivot()  #command to display the resulting pivot table in the viewer
pt  #this will show the plain text version in the console

# Product & Owner & Payer (This accounts for all of the product table variations)
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("product")
pt$addColumnDataGroups("account.role", addTotal = FALSE)
pt$addColumnDataGroups("account_payor", addTotal = FALSE) 
pt$addRowDataGroups("group")
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()

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

# Employee Count Buckets
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("employee_count")
pt$addRowDataGroups("group")
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()

#Product by group and employee group
pt <- PivotTable$new()
pt$addData(green)
pt$addColumnDataGroups("product")
pt$addRowDataGroups("group")
pt$addRowDataGroups("employee_count")
pt$defineCalculation(calculationName = "ProductCount", summariseExpression="n()")
pt$renderPivot()


