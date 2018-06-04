

#Define paths:
O_PATH <- file.path("/Users/Dani/ownCloud/publico/fur_patterns/datos/") #output and SpotEgg results path
P_PATH <- file.path("/Users/Dani/Dropbox/tablas_lince/lynxpardinus/Ex_situ/") #pedigree path
G_PATH <- file.path("/Users/Dani/Dropbox/tablas_lince/lynxpardinus/") #general_info path
F_PATH <- file.path("/Users/Dani/ownCloud/publico/fur_patterns/fotos_pelaje_cautividad/acebuche_raw_Mesas/Todas") #photos path

#Load libraries:
library(readr)
library(dplyr)
library(tidyr)

#Load data frame with IDs:
original_db <- read_tsv(paste0(O_PATH,"BASE_DATOS_SPOTEGG_56v2.txt"))
original_db
a_priori_info <- data_frame("studbook_id"=unique(sort(original_db$StudBookID)))
#a_priori_info <- data_frame("studbook_id"=c(3,12,17,47,90,100,115,168,169,196,197,226)) #Old version of individual IDs with only 12 individuals.

#Load Exchange and keep valuable information from there:
setwd(P_PATH)

lineage <- read_tsv("LINEAGE_180308.TXT",skip=2,col_names=T)
lineage
lineage$id <- as.numeric(lineage$id)
colnames(lineage)[11] <- "lineage_name"

exchange_odd <- read_csv2("script_input_EXCHANGE_180308.CSV", col_names=T)[-c(1),][c(T,F),]
exchange_odd

even_col_names <- unlist(read_csv2("script_input_EXCHANGE_180308.CSV")[c(1),c(1:7)],use.names=F)
even_col_names

exchange_even <- read_csv2("script_input_EXCHANGE_180308.CSV", col_names=even_col_names)[-c(1:2),][c(F,T),]
exchange_even
colnames(exchange_odd)[1] <- "ID"

exchange_joined <- left_join(exchange_odd, exchange_even, by=c("ID"="ID"))
exchange_joined
exchange_joined$ID <- as.numeric(exchange_joined$ID)

studbook_joined <- left_join(exchange_joined, lineage, by=c("ID"="id"))
studbook_joined
studbook_joined <- studbook_joined[-nrow(studbook_joined),]

studbook_dataframe <- filter(studbook_joined,ID%in%a_priori_info$studbook_id) %>% select(ID,LocalID,lineage_name,Sire,Dam,Sex,Location,DOB)
studbook_dataframe$lineage_name <- toupper(studbook_dataframe$lineage_name)
studbook_dataframe$lineage_name <- gsub('\\s+', '', studbook_dataframe$lineage_name)
studbook_dataframe$lineage_name <- gsub('-', '', studbook_dataframe$lineage_name)
jaipur_row <- studbook_dataframe[which(studbook_dataframe$lineage_name=="JAIPUR"),]
studbook_dataframe[which(studbook_dataframe$lineage_name=="JAIPUR"),2] <- "JAIPURadulto"
jaipur_row[,2] <- "JAIPURcachorro"
studbook_dataframe <- rbind(studbook_dataframe,jaipur_row)

studbook_dataframe_A <- studbook_dataframe %>% select(1,2,4,5,6,7,8)
colnames(studbook_dataframe_A)[2] <- "Name"
studbook_dataframe_B <- studbook_dataframe %>% select(1,3,4,5,6,7,8)
colnames(studbook_dataframe_B)[2] <- "Name"
studbook_dataframe_combined <- rbind(studbook_dataframe_A,studbook_dataframe_B)
studbook_dataframe_combined <- studbook_dataframe_combined %>% distinct()
studbook_dataframe_combined$DOB <- substr(studbook_dataframe_combined$DOB,3,8)
print.data.frame(studbook_dataframe_combined)

#Read picture names and extract valuable information from there:
setwd(F_PATH)
pictures_list <- list.files(F_PATH, recursive=F)
pictures_list
picture_dataframe <- data_frame("name"=character(0),"picture_date"=numeric(0))
for (row in 1:nrow(studbook_dataframe)) {
  if (studbook_dataframe[row,3]=="JAIPUR") {
    current_name <- c(unlist(studbook_dataframe[row,2],use.names=F))
    print(current_name)
    picture_row <- strsplit(paste(gsub(".*[/]([^.]+)[.].*", "\\1", pictures_list[grep(paste(current_name,collapse="|"),pictures_list)]),collapse="_"),"_")[[1]][c(1:2)]
    picture_dataframe[row,] <- picture_row
  } else {
    current_name <- c(unlist(studbook_dataframe[row,2:3],use.names=F))
    print(current_name)
    picture_row <- strsplit(paste(gsub(".*[/]([^.]+)[.].*", "\\1", pictures_list[grep(paste(current_name,collapse="|"),pictures_list)]),collapse="_"),"_")[[1]][c(1:2)]
    picture_dataframe[row,] <- picture_row
  }
}
picture_dataframe
picture_dataframe$picture_date <- paste0(substr(picture_dataframe$picture_date,5,6),substr(picture_dataframe$picture_date,3,4),substr(picture_dataframe$picture_date,1,2))
picture_dataframe

#Load SpotEgg outputs and extract valuable information from there:
setwd(paste0(O_PATH,"resultados_12_linces/"))
resultados_12_linces <- list.files(getwd(), recursive=T)
resultados_12_linces[grep("ALL_EGG_FEATURES_REPORT.csv",resultados_12_linces)]
spotegg_list <- list()
for (i in 1:length(resultados_12_linces[grep("ALL_EGG_FEATURES_REPORT.csv",resultados_12_linces)])) {
  spotegg_all_features <- read.csv2(paste0(getwd(),"/",resultados_12_linces[grep("ALL_EGG_FEATURES_REPORT.csv",resultados_12_linces)][i]))
  spotegg_list[[i]] <- spotegg_all_features
}
spotegg_joined <- rbind(as.data.frame(spotegg_list[[1]],stringsAsFactors=FALSE),as.data.frame(spotegg_list[[2]],stringsAsFactors=FALSE),as.data.frame(spotegg_list[[3]],stringsAsFactors=FALSE),stringsAsFactors=FALSE)
spotegg_dataframe <- spotegg_joined %>% select(FileName,EGG_ID,Area,NumSpots,TotAreaSpots,AvgSpotSize,AvgExcentricity,FractalDim,Per_vs_Area)
spotegg_dataframe$FileName <- as.character(spotegg_dataframe$FileName)
spotegg_dataframe <- spotegg_dataframe %>% mutate(Name=gsub("^(.*?)_.*", "\\1", spotegg_dataframe$FileName),Picture_date=gsub("^[^_]*_([^_]+)_.*", "\\1", spotegg_dataframe$FileName),
                                                  Picture_order=gsub("(.*_){2}(\\d+)_.+", "\\2", spotegg_dataframe$FileName),Picture_flank=gsub("^(?:[^_]*_){3}([^_]*)_(.+)$", "\\1", spotegg_dataframe$FileName),
                                                  Pattern_a_priori=gsub(".dng","",gsub("^(?:[^_]*_){4}", "\\1", spotegg_dataframe$FileName)))
pictures_path <- list.files(F_PATH, recursive=T)[grep(paste(spotegg_dataframe$Name,collapse="|"),basename(list.files(F_PATH, recursive=T)))]
pictures_location <- numeric(0)
for (i in 1:nrow(spotegg_dataframe)) {
  extract_location <- toupper(strsplit(pictures_path[grepl(gsub("_[^_]*$","\\1",spotegg_dataframe[i,1]),pictures_path)],"_")[[1]][1])
  pictures_location <- c(pictures_location,extract_location)
}
spotegg_dataframe$Pictures_location <- pictures_location
spotegg_dataframe$Picture_date <- paste0("20",substr(spotegg_dataframe$Picture_date,5,6),substr(spotegg_dataframe$Picture_date,3,4),substr(spotegg_dataframe$Picture_date,1,2))
spotegg_dataframe

#Now the SpotEgg outputs have been combined into a single data frame by Lidia:
spotegg_dataframe <- read_tsv(paste0(O_PATH,"BASE_DATOS_SPOTEGG_56v2.txt"))
spotegg_dataframe$FECHA <- paste0(substr(spotegg_dataframe$FECHA,5,6),substr(spotegg_dataframe$FECHA,3,4),substr(spotegg_dataframe$FECHA,1,2))
spotegg_dataframe

#Load the

#Load the latest general_info to extract ancestry information:
setwd(G_PATH)
ancestry_dataframe <- read_csv2("lp_gralinfo_20180309.csv") %>% select(id_studbook,ancestry) %>% rename(ID=id_studbook,Ancestry=ancestry)
ancestry_dataframe

#Join together all information coming from the studbook, the SpotEgg outputs, and the ancestry, and generate an output:
complete_dataframe <- left_join(right_join(studbook_dataframe_combined,spotegg_dataframe,by=c("Name"="NOMBRE")),ancestry_dataframe,by=c("ID"="ID"))
complete_dataframe$FECHA <- as.numeric(complete_dataframe$FECHA)
complete_dataframe <- complete_dataframe %>% rename(Current_location=Location,Birth_date=DOB,ROI=EGG_ID) %>% mutate(Age=round(round(strptime(FECHA,format="%y%m%d")-strptime(Birth_date,format="%y%m%d"))/365,2)) %>% select(1,2,3,4,5,41,6,7,9,42,c(10:40)) %>% arrange(ID,FECHA)
print.data.frame(complete_dataframe)

write_csv(complete_dataframe,paste0(O_PATH,"complete_dataframe_55_linces.csv"))



# setwd(F_PATH)
# pictures_list <- list.files(F_PATH, recursive=T)
# acebuche_dng_list <- pictures_list[grep("Acebuche",pictures_list)][grep(".dng",pictures_list[grep("Acebuche",pictures_list)])]
# picture_names <- character(0)
# for (i in 1:length(acebuche_dng_list)) {
#   name <- strsplit(paste(gsub(".*[/]([^.]+)[.].*", "\\1", acebuche_dng_list[i]),collapse="_"),"_")[[1]][c(1)]
#   picture_names <- c(picture_names,name)
# }
# picture_names_df <- data.frame("names"=unique(sort(picture_names)))
# picture_names_df$names <- as.character(picture_names_df$names)
# left_join(picture_names_df,lineage,by=c("names"="lineage_name"))



#Something else I ran for Simon (ignore):

lynx_picture <- sort(c(12,47,10,3,17,72,226,65,57,78,90,100,6,115,131,282,168,185,169,186,196,227,197,224,210,272,270,255,254,244,245,253,243,306,305,287,304,307,366,381,332,358,331,490,398,420,435,419,384,421,431,399))
length(lynx_picture)

setwd(P_PATH)

lineage <- read_tsv("LINEAGE_180308.TXT",skip=2,col_names=T)
lineage
lineage$id <- as.numeric(lineage$id)
lineage_complete <- lineage %>% mutate(epillepsy_aff=ifelse(EPILLEPSY=="Epilepsia",1,ifelse(age<=0.2,NA,0)),
                                       cryptorchidism_aff=ifelse(CRYPTORCHIDISM=="Criptorquidia",1,ifelse(sex==1 | age>=1,0,NA)),
                                       fur=ifelse(id %in% lynx_picture,1,NA)) %>%
  select(c(1:11,19,20,21,14:18))
write_csv(lineage_complete,"LINEAGE_280308_modified_with_fur.csv")


