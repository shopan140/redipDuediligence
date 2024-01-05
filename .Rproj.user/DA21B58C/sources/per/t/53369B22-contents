
projectNumber = "Request #"
riskLevel = "Risk Level (Low, Medium, High)"
valueLevel = "Project Value (Low, Medium, High)"
otherFundingSources = "Other funding sources"
additionalInfo = "Notes or additional info"
recommendation = "Recommendation (Approve/Deny)"
feedbackBy = "Feedback from"
sepStr = "\n"

fileLocation <-  "data//"
fileName <-  "REDIP Intake 2 - Due Diligence - "
fileExtension <- ".xlsx"
sheetName = "REDIP Intake 2"


dueDiligenceFiles <- c("AGRI",
                       "CITZ",
                       "Education",
                       "EMBC",
                       "EMLI",
                       "ENV (CLEAN BC)",
                       "ENV (Parks)",
                       "ENV (RSTBC",
                       "Forests",
                       "Housing",
                       "JEDI (Major Investments)",
                       "JEDI (Manufacturing)",
                       "JEDI (MJF)",
                       "JEDI (PNP Program)",
                       "JEDI (REIP)",
                       "JEDI (Small Business)",
                       "MIRR",
                       "MOTI",
                       "MUNI",
                       "PSFS",
                       "PSSG",
                       "TACS (Arts & Culture)",
                       "TACS (Heritage)",
                       "TACS (Mountain Resort)",
                       "TACS (Sport)",
                       "TACS (Tourism)",
                       "WLRS")



makefileName <- function(){
  unlist(lapply(dueDiligenceFiles, function(x) paste0(fileLocation, fileName, x, fileExtension)))

}


baseString <- paste0("Evaluator: ", "<evaluator>", sepStr,
                     "Risk Level: ", "<risk>", sepStr,
                     "Project Value: ", "<projectVal>", sepStr,
                     "Other funding sources: ", "<otherSources>", sepStr,
                     "Notes: ", "<notes>", sepStr,
                     "Recommendation: ", "<recomm>", sepStr)
replaceString <- function (df, colName, strToReplace, baseString){

    val <- df[1, colName, drop = TRUE]
    if(is.na(val)) return (gsub(strToReplace, "NA", baseString))
    outstr <- gsub(strToReplace, val, baseString)
    outstr
}

makeEvaluationStr <- function(df){
  text1 <- replaceString(df, feedbackBy, "<evaluator>", baseString)
  text2 <- replaceString(df, riskLevel, "<risk>", text1)
  text3 <- replaceString(df, valueLevel, "<projectVal>", text2)
  text4 <- replaceString(df, otherFundingSources, "<otherSources>", text3)
  text5 <- replaceString(df, additionalInfo, "<notes>", text4)
  text6 <- replaceString(df, recommendation, "<recomm>", text5)

  text6

}

compileSingleProject <- function(projectid, df){
  df <- df[df[projectNumber] == projectid, , drop = FALSE]
  outstr = ""
  for (i in 1:nrow(df)){
    outstr = paste0(outstr, makeEvaluationStr(df[i, , drop = FALSE]))
    cat(projectid, ": ", outstr)
  }

  out <- data.frame(projectid, outstr)
  names(out) = c("Project Number", "Evlauation")
  out
}

compileDataFrame <- function(df){
  projectList <- unique(df[, projectNumber, drop = TRUE])

  lapply(projectList, compileSingleProject, df)
}


readDueDiligenceExcel <- function(filePath, reviewer){
      if (file.exists(filePath)){
        df <- readxl::read_xlsx(filePath, sheet = sheetName, skip = 1)
        df <- df[, c(projectNumber, riskLevel, valueLevel, otherFundingSources, additionalInfo, recommendation)]
        df[, feedbackBy] <- reviewer
        return (df)
      }
}


compileDueDiligence <- function(outputFile){
  files <- makefileName()
  fileExists <- files[unlist(lapply(files, file.exists))]
  filesNotExists <- files[!unlist(lapply(files, file.exists))]

  unlist(lapply(filesNotExists, function(x) cat("File not exist: ",x, "\n")))

  df <- Map(readDueDiligenceExcel, files, dueDiligenceFiles)
  df <- dplyr::bind_rows(dd)
  df <- df[!is.na(df[projectNumber]), , drop = FALSE]
  temp <- compileDataFrame(df)
  temp <- dplyr::bind_rows(temp)
  writexl::write_xlsx(temp, outputFile)
  temp
}

