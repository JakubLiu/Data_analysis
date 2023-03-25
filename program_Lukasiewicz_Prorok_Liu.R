
# Jakub P., Kacper L., QBA LIU ------------------------------------------------------------------------------------------------------------------------------


# notes________________________________________________________________________________________________

# whenever possible, objects where transformed to matrices for faster performance
# the introduction of new variables was minimized to enhance performance
# sometimes an possibly unnecessary variable was introduced to enhance code readability
# please change the file paths
# before every execution please execute the gc() command in the terminal

library(data.table)

SNP_csv <- fread("C:/Users/Lenovo/Desktop/STUDIA/BIOINFORMATYKA/SEMESTR_V/ANALIZA_DANYCH/projekt/pliki/SNPs_X.csv")
TRAITS_csv <- fread("C:/Users/Lenovo/Desktop/STUDIA/BIOINFORMATYKA/SEMESTR_V/ANALIZA_DANYCH/projekt/pliki/Values_Y.csv")

# changing the csv files to matrices__________________________________________________________________________________________

SNP <- as.matrix(SNP_csv)
TRAITS <- as.matrix(TRAITS_csv)

# applying model in parallel________________________________________________________________________________

library(parallel)    

# definiowanie liczby wątków
copies_of_r <- detectCores() - 1

# tworzenie clastra
cl <- makeCluster(copies_of_r)

# exportowanie danych do clastra
clusterExport(cl=cl, c('TRAITS', 'SNP'))

full_model <- parApply(cl ,TRAITS[,2:7],2,function(x) apply(SNP[,2:ncol(SNP)], 2, function(y) summary(lm(x~y))))


# converting the output to file_____________________________________________________________________

unlisted_full <- unlist(full_model, recursive = F, use.names = T)

custom_function <- function(iter){
  holder_variable_1 <- unlist(iter, recursive = F, use.names = T)[4]
  holder_variable_2 <- unlist(holder_variable_1, recursive = F, use.names = T)
  return(holder_variable_2)
}

# apply() was used instead pod parLapply(), due to the fact that exporting the variables to the cluster...
# ... was very time consuming

output_matrix <- as.matrix(lapply(unlisted_full, function(iter) custom_function(iter)))

write.table(output_matrix, file="C:/Users/Lenovo/Desktop/output_file.txt", row.names=TRUE, col.names=TRUE)



