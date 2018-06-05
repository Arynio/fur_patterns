
library(readr)

full_df <- read_tsv("/Users/Dani/ownCloud/publico/fur_patterns/relatedness_analyses/PMx_relatedness_matrix_v2.txt")[-1,-2]  #read PMx matrix (as data frame) without the extra column
full_df
full_matrix <- as.matrix(full_df[,-1]) #turn the data frame into a matrix and remove the first column with the IDs
rownames(full_matrix) <- full_df$UniqueID #save the IDs as row names
class(full_matrix) <- "numeric"
full_matrix


original_db <- read_tsv("/Users/Dani/ownCloud/publico/fur_patterns/datos/BASE_DATOS_SPOTEGG_56v2.txt") #read the table with the IDs that we're using
original_db
a_priori_info <- data_frame("studbook_id"=unique(sort(original_db$StudBookID))) #extract those IDs
a_priori_info

subset_matrix <- full_matrix[unlist(a_priori_info,use.names=F),unlist(a_priori_info,use.names=F)] #filter the matrix with the IDs in order to subset it
subset_matrix
subset_matrix_wout_null <- subset_matrix[-which(rowSums(subset_matrix)<1),-which(colSums(subset_matrix)<1)] #remove individuals without data (they will only have r=0.5 with themselves)
subset_matrix_wout_null

write.table(subset_matrix_wout_null, file="/Users/Dani/ownCloud/publico/fur_patterns/relatedness_analyses/acebuche_relatedness_w_names.txt", row.names=T, col.names=T)
write.table(subset_matrix_wout_null, file="/Users/Dani/ownCloud/publico/fur_patterns/relatedness_analyses/acebuche_relatedness_wout_names.txt", row.names=F, col.names=F)