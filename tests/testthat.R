# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(redipDuediligence)

test_check("redipDuediligence")


library(dplyr)
library(readxl)
library(writexl)

df <- readxl::read_xlsx("data//Due Diligence Feedback.xlsx",
                        sheet = "Sheet1")




xlColNames <- c("Request #",
                "Project name",
                "Project description",
                "Request amount",
                "Organization name",
                "City",
                "Organization Type",
                "Risk Level (Low, Medium, High)",
                "Project Value (Low, Medium, High)",
                "Other funding sources",
                "Notes or additional info",
                "Recommendation (Approve/Deny)",
                "Feedback from")

projectNumber = "Request #"
riskLevel = "Risk Level (Low, Medium, High)"
valueLevel = "Project Value (Low, Medium, High)"
otherFundingSources = "Other funding sources"
additionalInfo = "Notes or additional info"
recommendation = "Recommendation (Approve/Deny)"
feedbackBy = "Feedback from"
sepStr = "\n"


df <- df[!is.na(df[projectNumber]), , drop = FALSE]

tt <- complieDataFrame(df)

tt <- bind_rows(tt)

writexl::write_xlsx(tt, path = "data//trial.xlsx")


dd <- Map(function(x , y) try(readDueDiligenceExcel(x,y)), files, dueDiligenceFiles)

df <- bind_rows(dd)

files[unlist(lapply(files, file.exists))]



