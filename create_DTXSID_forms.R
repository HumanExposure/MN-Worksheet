#Jonathan Taylor Wall
#Created 10-21-19
#Script to render multiple forms by DTXSID
#Write multi-sheet xlsx file
#Function to create a multi-sheet .xlsx file
#
#Libraries:
#     require(purrr)
#     require(xlsx)
#
# Args: 
#     data - list of dataframes to write to the xlsx sheets
#     sheetNames - list of names for the xlsx sheets
#     filePath - path/name for file to save the data to (if not null)
#     
# Returns: A message specifying if the file was created
writePagedXLSX <- function(data=list(), sheetNames=list(), filePath=NULL){
  if(!is.null(filePath)){ 
    cat("\nCreating file: ", filePath)
    if(!file.exists(dirname(filePath))) dir.create(dirname(filePath))
    write.xlsx(data[1], file = filePath, sheetName=sheetNames[[1]], row.names=FALSE)
    for(i in 2:length(data)) write.xlsx(data[i], file = filePath, sheetName=sheetNames[[i]], append=T, row.names=F)
  }
  return(message("\nCreated File: ", file.exists(filePath)))
}

#Example: https://www.r-bloggers.com/many-reports-from-1-rmarkdown-file/
args = commandArgs(trailingOnly=TRUE) #Pull command-line arguments (if they exist)
if(!length(args)) args[1] = "input/inputDTXSIDList3.csv"
require(rmarkdown); require(dplyr)
source("script/DTXSID_media_detected.R") #Function to pull data from database by DTXSID
chemList = read.csv(args[1], stringsAsFactors = FALSE) #Input list of DTXSID's
groups = chemList$Group %>% unique()
chemList = chemList %>% group_by(Group) %>% group_split()
if(!file.exists("output/DTXSID Reports")){ message("Creating Output Directory for DTXSID Reports..."); dir.create("output/DTXSID Reports") }
for(i in 1:length(chemList)){#Output documents by groups (e.g. PUCs)
  d = chemList[[i]]$DTXSID #Get list of DTXSID
  inputData = get_DTXSID_Media_Detected(page_size=500, d=d, overwrite = F) #Pulling data
  name = groups[i] #Get group name
  message("Creating Report Set ", i, " of ", length(chemList), " for group: ", name)
  if(is.null(name)){ name = "Ungrouped" } else if (is.na(name) | !str_length(name)){ name = "Ungrouped" }
  #Making a separate folder for each sub-list of chemical reports
  if(!file.exists(paste0("output/DTXSID Reports/",name))) dir.create(paste0("output/DTXSID Reports/",name))
  writePagedXLSX(data=list(inputData$data, 
                           data.frame(DTXSID=unlist(inputData$missingDTX$DTXSID), Count=inputData$missingDTX$Count), 
                           data.frame(DTXSID=unlist(inputData$noPucDTX$DTXSID), Count=inputData$noPucDTX$Count)), 
                 sheetNames=list("data", "missingDTX", "noPucDTX"), filePath=paste0("output/DTXSID Reports/",name,"/productData_",Sys.Date(),".xlsx"))
  for(i in 1:length(d)){
    file = paste0("output/DTXSID Reports/",name,"/", d[i], "_",Sys.Date(),".docx")
    message("Creating output doc: ", i, " of ", length(d), " : ", file)
    render("script/Minnesota_Form_Template.Rmd", output_file=paste0("../", file), params=list(dtxsid=d[i], data=inputData))
  }
}