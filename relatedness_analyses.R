
library(readr)

full_matrix <- read_tsv("/Users/Dani/ownCloud/publico/fur_patterns/relatedness_analyses/PMx_relatedness_matrix.txt")  !!!remove row and column with names 
full_matrix
as.matrix(full_matrix) !!!convert to matrix

original_db <- read_tsv("/Users/Dani/ownCloud/publico/fur_patterns/datos/BASE_DATOS_SPOTEGG_56v2.txt")
original_db
a_priori_info <- data_frame("studbook_id"=unique(sort(original_db$StudBookID)))
a_priori_info

full_matrix[a_priori_info,a_priori_info] !!!subset
