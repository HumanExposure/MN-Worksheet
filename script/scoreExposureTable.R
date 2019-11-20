#Minnesota Chemical Exposure Assessment Worksheet Scoring
#Created by: Jonathan Taylor Wall
#Created on: 24-Oct-2019
#Last Updated: 28-Oct-2019

#Convert character string to logical: https://stackoverflow.com/questions/11614473/converting-a-character-type-to-a-logical
library(readxl); library(dplyr); library(stringr)
################################################################################################
#Handle command-line arguments (if they exist)
args <- commandArgs(trailingOnly=TRUE)
message("\nArg 1: ", args[1], "\nArg 2: ", args[2])
################################################################################################
#Helper Methods
getKeyValues <- function(rubric) {#Create new scoring sheet from input dataframe
  output = list()
  for(var in rubric$varName[!is.na(rubric$varName)]){
    index = which(rubric$varName == var)
    temp = list(key = as.list(unlist(strsplit(as.character(rubric$levels[index]), split=";")[[1]])),
                value = as.list(unlist(strsplit(as.character(rubric$scores[index]), split=";")[[1]])))
    output = append(output, list(var=temp))
  }
  names(output) = rubric$varName[!is.na(rubric$varName)]
  return(output)
}

convertScores <- function(df, chem, inputs, keyValues){#Use input scoreSheet key-values to convert input scores
  for(input in names(inputs)){
    levels = keyValues[[which(names(keyValues) == input)]]
    x = inputs[which(names(inputs) == input)]
    if(!is.na(x)){#skip missing input keyValues
      for(i in 1:length(levels$key)){
        if(eval(parse(text=levels$key[i]))){
          df[which(df$chemical == chem),input] = as.numeric(levels$value[i])
        }
      }
    }
  }
  return(df)
}

dfToUpper <- function(df){#Convert all dataframe character types to uppercase
  return(df %>% lapply(function(v){ifelse(is.character(v), return(toupper(v)), return(v))}) %>%
           as.data.frame(stringsAsFactors=FALSE))
}
################################################################################################
#Load and Prep Inputs
message("Prepping Inputs and Score Sheets")
scoreSheet <- ifelse(!is.na(args[1]), args[1],"input/tablescoring.xlsx")
inputData <- ifelse(!is.na(args[2]), args[2], "input/toyScoreData.xlsx")
unadjustSheet <- read_xlsx(scoreSheet, sheet="unadjusted")
values <- getKeyValues(unadjustSheet)
adjustSheet <- read_xlsx(scoreSheet, sheet="adjusted")
adjustValues <- getKeyValues(adjustSheet) #Create new scoring sheet from adjustSheet
rawinputs <- read_xlsx(inputData, sheet="unadjusted") %>% dfToUpper()
adjustInputs <- read_xlsx(inputData, sheet="adjusted") %>% dfToUpper()
##################################################################################################################
#Creating final scores output table with varNames, TableScores, and Overall Scores
tables <- unique(unadjustSheet$Table)
varNames <- c("chemical", unadjustSheet$varName[!is.na(unadjustSheet$varName)], paste0("Table",tables,"Score"))
finalScores <- setNames(data.frame(matrix(ncol=length(varNames),
                                          nrow=nrow(rawinputs))),varNames) %>%
  mutate(chemical = rawinputs$chemical,
         AdjustedScore = NA,
         UnadjustedScore = NA)
for(chem in unique(rawinputs$chemical)){
  chemInputs <- filter(rawinputs, chemical==chem) %>% select(-chemical)
  for(table in unique(unadjustSheet$Table)){
    inputs <- chemInputs[names(chemInputs) %in% unadjustSheet$varName[unadjustSheet$Table==table]]
    finalScores <- convertScores(finalScores, chem, inputs, values)
    tableScore <- finalScores %>%
      filter(chemical == chem) %>%
      select(names(inputs))
    i <- list(row=which(finalScores$chemical == chem),col=paste0("Table",table,"Score"))
    if(table == 4) finalScores[i$row,i$col] <- as.numeric(max(unlist(tableScore[1,]), na.rm=TRUE))
    else finalScores[i$row,i$col] <- as.numeric(median(unlist(tableScore[1,]), na.rm=TRUE))
  }
  message("Calculating Unadjusted score for: ", chem)
  tableScores <- finalScores %>%
    filter(chemical == chem) %>%
    select(paste0("Table",tables,"Score"))
  finalScores[which(finalScores$chemical == chem),"UnadjustedScore"] <- mean(unlist(tableScores[1,]))
}
message("Done...Unadjusted scores calculated...")
#########################################################################################################
for(chem in adjustInputs$chemical){
  inputs <- filter(adjustInputs, chemical==chem) %>% select(-chemical)#Filter to chemical inputs
  adjustInputs <- convertScores(adjustInputs, chem, inputs, adjustValues)
  message("Calculating Adjusted Score for: ", chem, "...")
  for(chem in adjustInputs$chemical){
    adjustScores <- list()
    for(fx in unique(adjustSheet$fx)){
      vars <- adjustSheet$varName[adjustSheet$fx == fx]
      temp <- adjustInputs[which(adjustInputs$chemical == chem), vars] %>%
        lapply(as.numeric) %>% 
        as.data.frame()
      if(!all(is.na(temp))){#Only do calculations if group has non-missing values
        if(str_detect(fx, "max")){#General max pairs
          adjustScores <- append(adjustScores, max(temp, na.rm = TRUE))
        } else if(str_detect(fx, "mn")){#Special Pairs of MN and Non-MN variables 
          if(!is.na(temp[1])) adjustScores <- append(adjustScores, temp[1])
          else adjustScores <- append(adjustScores, max(temp, na.rm = TRUE))
        } else {#Default to summing the values
          adjustScores <- append(adjustScores, rowSums(temp, na.rm = TRUE))
        }
      }
    }
    index <- which(finalScores$chemical==chem)
    adjustScores <- append(adjustScores, finalScores$UnadjustedScore[index])
    finalScores$AdjustedScore[index] <- sum(unlist(adjustScores))
  }
}
message("Done...Adjusted scores calculated...Exporting Exposure Scores CSV Output...")
write.csv(finalScores, file = "output/ExposureTableScoresOutput.csv", row.names=FALSE)