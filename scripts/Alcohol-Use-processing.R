library(dplyr)
library(datapkg)
library(tidyr)

##################################################################
#
# Processing Script for Alcohol Use
# Created by Jenna Daly
# On 08/23/2018
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
raw_data <- dir(path_to_raw, recursive=T, pattern = "xlsx") 

#read in entire xls file (all sheets)
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

#Get all sheets for all data sets
for (i in 1:length(raw_data)) {
  mysheets <- read_excel_allsheets(paste0(path_to_raw, "/", raw_data[i]))
  for (j in 1:length(mysheets)) {
    sheet_name <- names(mysheets[j])
    sheet_file <- mysheets[[j]]
    assign(sheet_name, sheet_file)
  }
}

#Find all "Tables"
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
table_dfs <- grep("Table", dfs, value=T)

#Bring together all legacy Alcohol Tables
legacy_table_dfs <- table_dfs[!grepl("Substate", table_dfs)]
all_legacy_data <- data.frame(stringsAsFactors = F)
for (i in 1:length(legacy_table_dfs)) {
  current_table <- get(legacy_table_dfs[i])
  if (ncol(current_table) < 12) {
    colnames(current_table) <- c("Year", "Table Number", "Substate Region", "12-17 (Estimate)",
                                 "12-17 (95% Confidence Interval)", "18-25 (Estimate)", "18-25 (95% Confidence Interval)", 
                                 "26+ (Estimate)", "26+ (95% Confidence Interval)", "18+ (Estimate)")
  } else {
    colnames(current_table) <- c("Year", "Table Number", "Substate Region", "12-17 (Estimate)",
                                 "12-17 (95% Confidence Interval)", "18-25 (Estimate)", "18-25 (95% Confidence Interval)", 
                                 "26+ (Estimate)", "26+ (95% Confidence Interval)", "18+ (Estimate)",
                                 "18+ (95% Confidence Interval)", "Table Description")
  }
  descriptions <- c("Alcohol Dependence in the Past Year",         #16
                    "Alcohol Dependence in Past Year",         #16
                    "Alcohol Dependence or Abuse in the Past Year", #15
                    "Alcohol Dependence or Abuse in Past Year", #15
                    "Alcohol Use in the Past Month", #9
                    "Alcohol Use in Past Month", #9
                    "Binge Alcohol Use in the Past Month", #10
                    "Binge Alcohol Use in Past Month")  #10
  current_table <- current_table[grepl(paste(descriptions, collapse = "|"), current_table$`Table Description`),]
  all_legacy_data <- rbind(all_legacy_data, current_table)
}

#separate CI columns into lower and upper 
all_legacy_data_sep <- all_legacy_data %>% 
  separate("12-17 (95% Confidence Interval)", c("12-17 MOE Lower", "12-17 MOE Upper"), sep = "-") %>% 
  separate("18-25 (95% Confidence Interval)", c("18-25 MOE Lower", "18-25 MOE Upper"), sep = "-") %>% 
  separate("26+ (95% Confidence Interval)", c("26+ MOE Lower", "26+ MOE Upper"), sep = "-") %>% 
  separate("18+ (95% Confidence Interval)", c("18+ MOE Lower", "18+ MOE Upper"), sep = "-")

#Process new data (2012-2014, 2014-2016)
new_table_dfs <- table_dfs[grepl("Substate", table_dfs)]
new_table_dfs <- new_table_dfs[grepl(paste(c(" 6", " 9", " 10", " 15", " 16"), collapse = "|"), new_table_dfs)]

all_new_data <- data.frame(stringsAsFactors = F)
for (i in 1:length(new_table_dfs)) {
  current_table <- get(new_table_dfs[i])
  description <- colnames(current_table)[1]
  current_table$`Table Description` <- description
  if (ncol(current_table) < 16) {
    colnames(current_table) <- c("Order", "Table Number", "Substate Region", "18-25 (Estimate)", 
                               "18-25 MOE Lower" , "18-25 MOE Upper", "26+ (Estimate)", 
                               "26+ MOE Lower", "26+ MOE Upper", "18+ (Estimate)",
                               "18+ MOE Lower", "18+ MOE Upper", "Table Description")
   current_table$"12-17 (Estimate)" <- NA
   current_table$"12-17 MOE Lower" <- NA
   current_table$"12-17 MOE Upper" <- NA
  } else {
    colnames(current_table) <- c("Order", "Table Number", "Substate Region", "12-17 (Estimate)", 
                               "12-17 MOE Lower", "12-17 MOE Upper", "18-25 (Estimate)", 
                               "18-25 MOE Lower" , "18-25 MOE Upper", "26+ (Estimate)", 
                               "26+ MOE Lower", "26+ MOE Upper", "18+ (Estimate)", 
                               "18+ MOE Lower", "18+ MOE Upper", "Table Description")
  }
  get_year <- as.numeric(substr((unlist(gsub("[^0-9]", "", unlist(new_table_dfs[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year+2)
  current_table$Year <- get_year
  all_new_data <- rbind(all_new_data, current_table)
}

all_new_data$Order <- NULL

all_new_data$`12-17 (Estimate)` <- as.numeric(all_new_data$`12-17 (Estimate)`) * 100
all_new_data$`12-17 MOE Lower` <- as.numeric(all_new_data$`12-17 MOE Lower`) * 100   
all_new_data$`12-17 MOE Upper` <- as.numeric(all_new_data$`12-17 MOE Upper`) * 100   
all_new_data$`18-25 (Estimate)` <- as.numeric(all_new_data$`18-25 (Estimate)`) * 100 
all_new_data$`18-25 MOE Lower` <- as.numeric(all_new_data$`18-25 MOE Lower`) * 100   
all_new_data$`18-25 MOE Upper` <- as.numeric(all_new_data$`18-25 MOE Upper`) * 100   
all_new_data$`26+ (Estimate)` <- as.numeric(all_new_data$`26+ (Estimate)`) * 100   
all_new_data$`26+ MOE Lower` <- as.numeric(all_new_data$`26+ MOE Lower`) * 100     
all_new_data$`26+ MOE Upper` <- as.numeric(all_new_data$`26+ MOE Upper`) * 100     
all_new_data$`18+ (Estimate)` <- as.numeric(all_new_data$`18+ (Estimate)`) * 100   
all_new_data$`18+ MOE Lower` <- as.numeric(all_new_data$`18+ MOE Lower`) * 100     
all_new_data$`18+ MOE Upper` <- as.numeric(all_new_data$`18+ MOE Upper`) * 100

#combine all years
all_data <- rbind(all_legacy_data_sep, all_new_data)

#select connecticut rows
ct_rows <- which(grepl("Connecticut", all_data$`Substate Region`))
add1 <- ct_rows + 1
add2 <- ct_rows + 2
add3 <- ct_rows + 3
add4 <- ct_rows + 4
add5 <- ct_rows + 5

all_ct_rows <- c(ct_rows, add1, add2, add3, add4, add5)

#add rows for Northeast and United States
us_rows <- which(grepl("^Total United States$", all_data$`Substate Region`))
add1 <- us_rows + 1 #northeast
us_ne_rows <- c(us_rows, add1)

all_rows <- c(all_ct_rows, us_ne_rows)

test <- all_data[all_rows,]

#append "Region" to CT substate regions
regions <- c("Eastern", "North Central", "Northwestern", "South Central", "Southwest")
region_rows <- which(grepl(paste(regions, collapse = "|"), test$`Substate Region`))
test$`Substate Region`[region_rows] <- gsub("$", " Region", test$`Substate Region`[region_rows])
#rename US rows
test$`Substate Region` <- gsub("Total ", "", test$`Substate Region`)

#assign US MOE for 2004-2006 to zero
test[test$`Substate Region` == "United States" & is.na(test)] <- 0

#Calculate MOE from upper and lower bounds
test[] <- lapply(test, gsub, pattern = "\\(", replacement = "")
test[] <- lapply(test, gsub, pattern = "\\)", replacement = "")

test <- test %>% 
  mutate(`12-17 MOE` = (((as.numeric(`12-17 MOE Upper`)-as.numeric(`12-17 MOE Lower`))/2)/1.96)*1.645, 
         `18-25 MOE` = (((as.numeric(`18-25 MOE Upper`)-as.numeric(`18-25 MOE Lower`))/2)/1.96)*1.645, 
         `26+ MOE` = (((as.numeric(`26+ MOE Upper`)-as.numeric(`26+ MOE Lower`))/2)/1.96)*1.645, 
         `18+ MOE` = (((as.numeric(`18+ MOE Upper`)-as.numeric(`18+ MOE Lower`))/2)/1.96)*1.645) 
        
test <- test %>% 
  select(`Substate Region`, Year, 
         `12-17 (Estimate)`, `12-17 MOE`, 
         `18-25 (Estimate)`, `18-25 MOE`, 
         `26+ (Estimate)`, `26+ MOE`, 
         `18+ (Estimate)`, `18+ MOE`, 
         `Table Description`)

test <- test[grepl("Alcohol", test$`Table Description`),]

#recode description column
test$`Alcohol Use` <- NA
test$`Alcohol Use`[grep("Abuse", test$`Table Description`)] <- "Alcohol Dependence or Abuse in the Past Year"
test$`Alcohol Use`[grep("Dependence in", test$`Table Description`)] <- "Alcohol Dependence in the Past Year"
test$`Alcohol Use`[grep("Alcohol Use", test$`Table Description`)] <- "Alcohol Use in the Past Month"
test$`Alcohol Use`[grep("Disorder", test$`Table Description`)] <- "Alcohol Dependence or Abuse in the Past Year" #Diagnostic and Statistical Manual of Mental Disorders (DSM-IV).
test$`Alcohol Use`[grep("Binge", test$`Table Description`)] <- "Binge Alcohol Use in the Past Month"

#backfill alcohol use (2014-2016 missing 2 tables)
backfill_use <- expand.grid(
  `Substate Region` = unique(test$`Substate Region`),
  `Year` = unique(test$Year),
  `Alcohol Use` = unique(test$`Alcohol Use`)
)

test_merge <- merge(test, backfill_use, by = c("Substate Region", "Year", "Alcohol Use"), all.y=T)

#wide to long format
all_data_long <- gather(test_merge, Variable, Value, 4:11, factor_key=F)

#set backfilled cells to -6666 to denote missing
all_data_long$Value[is.na(all_data_long$`Table Description`)] <- -6666

#assign age range
all_data_long$`Age Range` <- NA
all_data_long$`Age Range`[grep("12-17", all_data_long$Variable)] <- "12-17"
all_data_long$`Age Range`[grep("26+", all_data_long$Variable)] <- "Over 25"
all_data_long$`Age Range`[grep("18+", all_data_long$Variable)] <- "Over 17"
all_data_long$`Age Range`[grep("18-25", all_data_long$Variable)] <- "18-25"

#assign variable
all_data_long$Variable[grep("Estimate", all_data_long$Variable)] <- "Alcohol Use"
all_data_long$Variable[grep("MOE", all_data_long$Variable)] <- "Margins of Error"

#assign measure type
all_data_long$`Measure Type` <- "Percent"

#Select and sort columns
all_data_final <- all_data_long %>% 
  select(`Substate Region`, Year, `Age Range`, `Alcohol Use`, `Measure Type`, Variable, Value) %>% 
  rename(Region = `Substate Region`) %>% 
  arrange(Region, Year, `Age Range`, `Alcohol Use`, Variable)

all_data_final$Value <- round(as.numeric(all_data_final$Value), 2)

# Write to File
write.table(
  all_data_final,
  file.path(getwd(), "data", "alcohol-use-2016.csv"),
  sep = ",",
  na = "-9999",
  row.names = F
)

