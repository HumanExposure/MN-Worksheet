#DTXSID_source_media_detect Function
#Jonathan Taylor Wall
#Created Sep-27-2019
#Last Updated Sep-27-2019

require(tidyverse); require(DBI); require(readxl); require(sjPlot)
require(magrittr); require(jsonlite); require(xlsx)
#set.seed(500) #Set seed for sampling API rest points

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
#     emptyDFColNames - list of column names for empty dataframes within data
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

#Factorum API Product Data by DTXSID
# Function to pull product data from the Factotum API for a given DTXSID
# 
#Libraries:
#     require(jsonlite)
#     require(purrr)
#
# Args: 
#     page_size - integer value for how many entries the API pulls per page (max 500)
#     dtxsid - DTXSID value in the form of 'DTXSID#'
#     filePath - path/name for file to save the data to (if not null)
#     overwrite - logical switch to determine whether to pull from an existing filePath, or pull raw data and save to filePath     
#     
# Returns: A dataframe of chemical data
getProductData <- function(page_size=100, dtxsid="", filePath=NULL, overwrite=FALSE){
  dtxsid = unlist(dtxsid)
  productDat = data.frame(id = integer())
  missingDTX = list(Count = 0, DTXSID = list(NA))
  noPucDTX = list(Count = 0, DTXSID = list(NA))
  if(!is.null(filePath)){#Check if there's a file with the data already - Don't pull new data
    fileExists = file.exists(filePath)
    if(fileExists & !overwrite){ 
      message("\nFile already exists and overwrite set to FALSE...pulling data from file")
      return(list(data=read_xlsx(filePath, sheet="data"),
                  missingDTX=read_xlsx(filePath, sheet="missingDTX"),
                  noPucDTX=read_xlsx(filePath, sheet="noPucDTX")))
    }
  }
  for(i in 1:length(dtxsid)){
    url = paste0("http://api.factotum.epa.gov/products/?page_size=", page_size, "&chemical=", dtxsid[i])
    pages = list()
    npage = fromJSON(url, flatten=TRUE)$paging$pages
    cat("\nAttempting to pull ", dtxsid[i], ": ", i, " of ", length(dtxsid), " with ", npage, " pages...")
    if(npage*page_size > 2500){ message("\n\n\n", dtxsid[i]," has too many records for the current pull method...Skipping for now!\n\n\n"); next }
    for(j in 1:npage){
      #Randomly wait between API requests for 10 seconds 10% of the time
      #if(sample(1:100, 1) > 90){ cat("\nResting 10 seconds between requests..."); Sys.sleep(10) }
      Sys.sleep(0.25) #Sleep so no more than 4 requests each second
      cat("\nTrying URL: ", paste0(url, "&page=", j)); data = fromJSON(paste0(url, "&page=", j), flatten=TRUE)  
      cat("\nRetrieving page ", j); pages[[j]] = data$data #Select the data column
    }
    if(!(is_empty(data$data) & is_empty(pages[[1]]))) pages = rbind_pages(pages) #Bind non-empty data pages
    if(!is.data.frame(pages)){
      cat("\n", dtxsid[i], " not found in Factotum...moving on...")
      if(any(is.na(missingDTX$DTXSID))) missingDTX$DTXSID <- missingDTX$DTXSID[!is.na(missingDTX$DTXSID)]
      missingDTX$Count = missingDTX$Count + 1
      missingDTX$DTXSID = append(missingDTX$DTXSID, dtxsid[i]) #Append missing DTXSID
      next #Skip empty list -- DTXSID not in factotum
    }
    pages = unnest(pages, chemicals, names_sep="_") #Unnesting chemicals column
    if(!("puc.id" %in% names(pages))){ #Include if the DTXSID has a puc assigned
      cat("\nNo PUC assigned to ", dtxsid[i], "...moving on...\n")
      if(any(is.na(noPucDTX$DTXSID))) noPucDTX$DTXSID <- noPucDTX$DTXSID[!is.na(noPucDTX$DTXSID)]
      noPucDTX$Count = noPucDTX$Count + 1
      noPucDTX$DTXSID = append(noPucDTX$DTXSID, dtxsid[i]) #Append noPuc DTXSID
      next #Skip empty list -- DTXSID without PUC
    } 
    productDat = full_join(productDat, pages) #Full join in case mixing PUC assigned and unassigned DTXSID's
  }
  writePagedXLSX(data=list(productDat, data.frame(DTXSID=unlist(missingDTX$DTXSID), Count=missingDTX$Count), data.frame(DTXSID=unlist(noPucDTX$DTXSID), Count=noPucDTX$Count)), 
                 sheetNames=list("data", "missingDTX", "noPucDTX"), filePath=filePath)  
  return(list(data=productDat, missingDTX=missingDTX, noPucDTX=noPucDTX))
}

#Factorum API True Chemical Data
# Function to pull True Chemical CAS and Name data from the Factotum API for all DTXSIDs in the database
# 
#Libraries:
#     require(jsonlite)
#     require(purrr)
#
# Args: 
#     page_size - integer value for how many entries the API pulls per page (max 500)
#     filePath - path/name for file to save the data to (if not null)
#     overwrite - logical switch to determine whether to pull from an existing filePath, or pull raw data and save to filePath     
#
# Returns: A dataframe of chemical data
getTrueChem <- function(page_size=100, filePath=NULL, overwrite=FALSE){
  if(!is.null(filePath)){
    fileExists = file.exists(filePath)
    if(fileExists & !overwrite){ 
      message("File already exists and overwrite set to FALSE...pulling data from file")
      return(read.csv(filePath, stringsAsFactors = FALSE))
    }
  }
  url = paste0("http://api.factotum.epa.gov/truechemicals/?page_size=", page_size)
  pages = list(); npage = fromJSON(url, flatten=TRUE)$paging$pages
  for(i in 1:npage){
    #Randomly wait between API requests for 10 seconds 10% of the time
    #if(sample(1:100, 1) > 90){ cat("\nResting 10 seconds between requests..."); Sys.sleep(10) }
    Sys.sleep(0.25) #Sleep so no more than 4 requests each second
    cat("\nTrying URL: ", paste0(url, "&page=", i)); data = fromJSON(paste0(url, "&page=", i), flatten=TRUE)  
    cat("\nRetrieving page ", i); pages[[i]] = data$data 
  }
  if(!(is_empty(data$data) & is_empty(pages[[1]]))) pages = rbind_pages(pages) #Bind non-empty data pages
  pages <- rename(pages, DTXSID=sid, CAS = true_cas, chemName = true_chemname)
  if(!is.null(filePath)) write.csv(pages, file = filePath, row.names=FALSE, na="NA")
  return(pages)
}

#Main Method
get_DTXSID_Media_Detected <- function(d, page_size=100, overwrite=F){
  ########################################################################################
  #Pull batch files of mapped CASRN to DTXSID
  #Creating map list of DTXSID to CASRN from CHEMTOX batch output
  cat("\nPulling CASRN/DTXSID data to map files\n")
  #batchFiles = list.files("input/DTXSID_batch", pattern="^batch", full.names = TRUE)
  #batches = read_excel(unlist(batchFiles[1]), na = "-")
  #for(i in 2:length(batchFiles)){
  #  batches = rbind(batches, read_excel(unlist(batchFiles[i]), na = "-"))
  #}
  #batches = select(batches, CAS = INPUT, DTXSID)
  batches = getTrueChem(page_size=page_size, filePath = "input/batchOverall.csv", overwrite = overwrite)
  #write.csv(batches, file = "input/batchOverall.csv",row.names=FALSE, na="NA")
  ########################################################################################
  path = getwd()
  cat("\n",path)
  
  #Unlisting so SQL will parse
  cat("\nPreparing Input DTXSID values")
  d = unlist(d)
  
  #Connect to database
  cat("\nAttempting to Connect to Database")
  con =  dbConnect(RMySQL::MySQL(), 
                        username = Sys.getenv("tesla_user"),
                        password = Sys.getenv("tesla_pass"),
                        host = "tesla.epa.gov", 
                        port = 3306, 
                        dbname = "prod_samples"
                    )
  con2 = dbConnect(RMySQL::MySQL(),
                    username = Sys.getenv("tesla_user"),
                    password = Sys.getenv("tesla_pass"),
                    host = "tesla.epa.gov", 
                    port = 3306, 
                    dbname = "dev_consumer_product"
                    )
  #Pull Data tables
  cat("\nPulling Tables")
  #Adding prefixes to help differentiate identical fields between tables
  #prod_samples
  ha = tbl(con, "harmonized_aggregate") %>% rename_all(function(x) paste0("ha.", x))
  f = tbl(con, "files") %>% rename_all(function(x) paste0("f.", x))
  m = tbl(con, "media") %>% rename_all(function(x) paste0("m.", x))
  s = tbl(con, "substances") %>% rename_all(function(x) paste0("s.", x))
  src = tbl(con, "source") %>% rename_all(function(x) paste0("src.", x))
  
  #dev_consumer (prodCat, prod, and ing not needed if using API approach)
  cs = tbl(con2, "chemical_substances") %>% rename_all(function(x) paste0("cs.", x))
  #prodCat = tbl(con2, "sheds_product_categories")%>% rename_all(function(x) paste0("prodCat.", x))
  #prod = tbl(con2, "products") %>% rename_all(function(x) paste0("prod.", x))
  #ing = tbl(con2, "ingredients")%>% rename_all(function(x) paste0("ing.", x))
  fus = tbl(con2, "functional_uses")%>% rename_all(function(x) paste0("fus.", x))

#############################################################################################  
  #Save list of unique harmonized media
  mediaList = select(m, m.harmonized_medium) %>% 
    distinct() %>%
    collect() %>%
    mutate(media.category = "uncategorized")%T>%{#Important T-operator
      colnames(.)[1] = "harmonized_medium" #Rename harmonized_medium column
    }
    
  for(i in 1:nrow(mediaList)){
    media = mediaList$harmonized_medium[i]
    if(media %in% c("sediment", "soil", "indoor dust", "soil, or outdoor settled dust")){
      newCategory = "soil"
    } else if(media %in% c("surface water", "drinking water", "groundwater", "wastewater (influent, effluent)")){
      newCategory = "water"
    } else if(media %in% c("ambient air", "indoor air", "personal air")){
      newCategory = "air"
    } else if(media %in% c("wildlife (aquatic invertebrate)", "wildlife (aquatic vertebrates/mammals)", "wildlife (birds)",
                           "wildlife (fish)","wildlife (terrestrial invertebrates/worms)","wildlife (terrestrial vertebrates)",
                           "other-ecological", "vegetation", "fish")
              ){
      newCategory = "ecoBiota"
    } else if(media %in% c("breast milk", "human (other tissues or fluids)", "human blood (whole/serum/plasma)", "skin wipes", "urine")){
      newCategory = "biomonitoring"
    }
    mediaList$media.category[i] = newCategory
    newCategory = "uncategorized"
  }
    
    write.csv(mediaList, file = "input/mediaList.csv", row.names=FALSE)
########################################################################################
  #Pulls unique cas values to manually send to batch DTXSID mapping
  cat("\nUpdating CASRN output for manual CHEMTOX Mapping")
  casUnique = fus %>%
    left_join(cs, by=c("fus.fk_chemical_substances_id"="cs.id")) %>%
    select(cs.cas) %>%
    distinct() %>% 
    na.omit() %>%
    collect() %>%
    write.csv(file = "input/dev_consumer_product_cas.csv",row.names=FALSE)
############################################################################################### 
  #Original tbl2 used until added CPDat and CPCat columns
  # tbl2 = fus %>%
  #   left_join(cs, by=c("fk_chemical_substances_id"="id")) %>%
  #   select(cas, reported_functional_use) %>% 
  #   collect() %>%
  #   mutate(cas = sub("^[0]+", "", cas)) %>% #Remove leading 0 from cas
  #   left_join(batches, by=c("cas"="INPUT"))
    #head(tbl2)
  
  #Using only dev_consumer data --> switched to API version
  # tbl2 = prodCat %>%
  #   left_join(prod, by=c("prodCat.id"="prod.fk_sheds_product_categories_id")) %>%
  #   left_join(ing, by=c("prod.id"="ing.fk_products_id")) %>%
  #   left_join(cs, by=c("ing.fk_chemical_substances_id"="cs.id")) %>%
  #   left_join(fus, by=c("ing.fk_chemical_substances_id"="fus.fk_chemical_substances_id")) %>%
  #   select(cs.cas, prod.product_name, prodCat.general_category, prodCat.product_type, ing.minimum_weight_fraction, ing.maximum_weight_fraction,
  #          prodCat.description, fus.reported_functional_use) %>%
  #   collect() %>%
  #   mutate(cs.cas = sub("^[0]+", "", cs.cas)) %>% #remove leading 0
  #   left_join(batches, by=c("cs.cas"="INPUT"))
  
  #Using API to Pull PUC information
factotumDat <- getProductData(page_size=page_size, dtxsid=d, filePath = "input/cache/productDat.xlsx", overwrite = overwrite)
# factotumDat = data.frame(id= as.integer()) #Empty dataframe to add data to
# missingDTX = list(Count = 0, DTXSID = list())
# noPucDTX = list(Count = 0, DTXSID = list())
#   for(i in 1:length(d)){
#   #for(sid in d){
#     cat("\nAttempting to pull ", d[i], ": ", i, " of ", length(d))
#     temp = getProductData(page_size=page_size, dtxsid=d[i])
#     if(!is.data.frame(temp)){
#       cat("\n", d[i], " not found in Factotum...moving on...")
#       missingDTX$Count = missingDTX$Count + 1
#       missingDTX$DTXSID = append(missingDTX$DTXSID, d[i]) #Append missing DTXSID
#       next #Skip empty list -- DTXSID not in factotum
#     }
#     temp = unnest(temp, chemicals, names_sep="_") #Unnesting chemicals column
#     if(!("puc.id" %in% names(temp))){ #Include if the DTXSID has a puc assigned
#       cat("\nNo PUC assigned to ", d[i], "\n")
#       noPucDTX$Count = noPucDTX$Count + 1
#       noPucDTX$DTXSID = append(noPucDTX$DTXSID, d[i]) #Append noPuc DTXSID
#       temp = mutate(temp, chemicals_sid=as.character(NA), puc.name=as.character(NA), puc.gen_cat=as.character(NA), 
#                     puc.id=as.integer(NA), puc.prod_type=as.character(NA), puc.description=as.character(NA)) #Add placeholder columns in case where all input DTXSID's are unassigned
#     } 
#     factotumDat = full_join(factotumDat, temp) #Full join in case mixing PUC assigned and unassigned DTXSID's
#   }
cat("\n Pulled all available DTXSIDs...")
factotumDat$data = select(factotumDat$data, name, chemicals_sid, puc.name, puc.gen_cat, puc.id, puc.prod_type, 
                      puc.description, chemicals_min_weight_fraction, chemicals_max_weight_fraction)
  
  tbl2 = left_join(fus, cs, by=c("fus.fk_chemical_substances_id"="cs.id")) %>%
    select(cs.cas, fus.reported_functional_use) %>%
    collect() %>%
    mutate(cs.cas = sub("^[0]+", "", cs.cas)) %>% #remove leading 0
    left_join(batches, by=c("cs.cas"="CAS")) %>%
    left_join(factotumDat$data, by=c("DTXSID"="chemicals_sid")) %>%
    select(DTXSID, prod.product_name = name, prodCat.general_category = puc.gen_cat, #Selecting and renaming columns
           prodCat.product_type = puc.prod_type, ing.minimum_weight_fraction = chemicals_min_weight_fraction, 
           ing.maximum_weight_fraction = chemicals_max_weight_fraction, 
           fus.reported_functional_use, prodCat.description = puc.description)
  
  #Join prod_samples tables, select columns, filter by DTXSID list, and summarize the data tables by detected
  #Combine with tbl2 by DTXSID and export
  cat("\nJoining and Summarizing Tables")
  tbl1 = ha %>%
    left_join(f, by=c("ha.files_fileid" = "f.fileid")) %>%
    left_join(m, by=c("ha.media_idmedia" = "m.idmedia")) %>%
    left_join(s, by=c("ha.substances_idsubstance"="s.idsubstance")) %>%
    left_join(src, by=c("f.source_sourceid"="src.sourceid")) %>% 
    filter(s.DTXSID %in% d) %>%
    collect() %>% #ALWAYS COLLECT AFTER FILTERING DATA INTO SMALLER BITS!
    left_join(tbl2, by=c("s.DTXSID"="DTXSID")) %>%
    select(s.DTXSID, #f.n_records, #Probably don't need f.n_records
           src.name, prod.product_name, prodCat.general_category, prodCat.product_type, ing.minimum_weight_fraction, 
           ing.maximum_weight_fraction, fus.reported_functional_use, m.harmonized_medium, ha.reported_species, ha.detected, prodCat.description) %>%
    group_by(s.DTXSID, #f.n_records, #Probably don't need f.n_records
             src.name, prod.product_name, prodCat.general_category, prodCat.product_type, ing.minimum_weight_fraction, 
             ing.maximum_weight_fraction, fus.reported_functional_use, m.harmonized_medium, ha.reported_species, prodCat.description) %>%
    summarize(ha.detected = sum(!is.na(ha.detected))) %>%
    ungroup() %T>%{ #Important T-operator
      colnames(.) = sub('.*\\.', '', colnames(.)) #Remove Prefixes
    }
  
  #cat("\nRendering and Saving Output Tables\n")
  #tab_df(tbl1, title = "Chemical Detection by DTXSID", use.viewer = TRUE, 
  #       sort.column = 1, file =paste0(path, "/output/detectedAPITable.html"))
  
  #head(tbl1)
  #Disconnect database
  cat("\nDisconnecting from database"); dbDisconnect(con); dbDisconnect(con2); cat("\nDone.\n")
  return(list(data=tbl1, missingDTX=factotumDat$missingDTX, noPucDTX=factotumDat$noPucDTX))
}

#Testing single DTXSID to filter by
#test = get_DTXSID_Media_Detected("DTXSID9020827")

#Testing list of DTXSID to filter by
#test2 = get_DTXSID_Media_Detected(c("DTXSID9020827","DTXSID0024000"))
#test3 = get_DTXSID_Media_Detected(list("DTXSID9020827","DTXSID0024000"))
# test = get_DTXSID_Media_Detected(d=list("DTXSID1020560","DTXSID4020458","DTXSID1020855",
#                                        "DTXSID6022341", "DTXSID2021995", "DTXSID4022020"),
#                                   page_size=500, overwrite=T)

# test = get_DTXSID_Media_Detected(d=list("DTXSID7034410", "DTXSID5029631", "DTXSID7051216", 
#                                         "DTXSID6023947", "DTXSID3036238"),
#                                   page_size=500, overwrite=T)