#"C:/Users/52552/Alexito/ejemplos_R/clasifica_delitos/ejemplo_delitos03.R"

corpus<-openxlsx::read.xlsx("~/clasifica_delitos/delitos01.xlsx",sheet=3)
delitos01<-openxlsx::read.xlsx("~/delitos01.xlsx",sheet=2)
library(tm)
library(stringr)
library(foreign)
##################################################################################
# raw_test <- read.table("D:/Varios/Text_mining/palabras_asp.txt",              # TXT data file indicated as string or full path to the file
#                        header = FALSE,    # Whether to display the header (TRUE) or not (FALSE)
#                        sep = "",          # Separator of the columns of the file
#                        dec = ".") # Importa como data frame
# sorted_words  <- as.table(as.matrix(raw_test)) # Convierte data frame a table
sorted_words <- as.data.frame(read.csv("~/sorted_words.csv"))
sorted_words1 <- sorted_words[,2] 
#-------------------------------------------------------------------------------
correct <- function(word) {
  edit_dist <- adist(word, sorted_words1)
  min_edit_dist <- min(edit_dist, 2)
  proposals_by_prob <- c(sorted_words1[ edit_dist <= min(edit_dist, 2)])
  proposals_by_prob <- c(proposals_by_prob, word)
  proposals_by_prob[1]
}

#------------------------------------------------------------------------------
palabras_trans <- c("torton", "troca", "blazer", "cheyen", "auto", "clonaron", "laptop", "metro", "hostiga", "hostigar", "vandalizaron",
                    "existia", "robo")

#quitar <- c(stopwords("es"))
quitar <- c(stopwords("es")[-16])

simplifica <- function(x){
  y <- strsplit(x, " ")
  xx<-list()
  for(i in 1:sum(table(y))){
    
   if(y[[1]][i]%in%palabras_trans){
         xx[[i]] <- y[[1]][i]
       }else{
         xx[[i]] <- correct(y[[1]][i])  
       }                    
  }

for(i in 1:length(xx)){
 if(any(xx[[i]]%in%quitar)){
   xx[[i]]<-""
 }else{xx[[i]]<-xx[[i]] }
}
  
  texto_col = as.character(unlist(xx))
  texto_col = data.frame(texto_col)
  
  
  if(sum(table(texto_col))==1){
    numero<-xx[[1]]
  }else{
    numero<-texto_col[1,1]
    for(i in 2:sum(table(texto_col)))
      numero<-paste(numero,texto_col[i,1],sep = " ")
  }
  numero <- str_replace_all(numero,"[\\s]+", " ")
  numero <- str_trim(numero)
  return(numero)
}

corpus$DEL_MOCH <- lapply(corpus$DEL_X, simplifica)##

reclasificadora <- function(tabla){
delitos01 <- tabla  
delitos01$DEL_MOCH <- lapply(delitos01$DEL_X,simplifica)

for(i in 1:dim(delitos01)[1])
{  
 if(length(corpus[grep(delitos01$DEL_MOCH[i], corpus$DEL_MOCH), c(1)])==1) 
 {
 delitos01$DEL_MOCH <- str_trim(delitos01$DEL_MOCH, 'left')
 delitos01$del_reclas[i] <- corpus[grep(delitos01$DEL_MOCH[i], corpus$DEL_MOCH), 1]
 delitos01$del_reclasX[i] <- corpus[grep(delitos01$DEL_MOCH[i], corpus$DEL_MOCH), 2]
 }else{
 delitos01$del_reclas[i] <-"REVISAR"
 delitos01$del_reclasX[i]<-"REVISAR"}
}

delitos01$CONCORDANCIA <- ifelse(delitos01$del_reclas==delitos01$ID_DEL,":)", "REVISAR")
delitos01 <- delitos01[ , -3]
return(delitos01)
}
#####################################################################################

aa <- reclasificadora(delitos01)
openxlsx::write.xlsx(aa, "C:/Users/52552/Alexito/ejemplos_R/clasifica_delitos/resultado.xlsx", overwrite = T)

#save.image("D:/Varios/Text_mining/experimento.Rdata")
#write.csv(sorted_words, "D:/Varios/Text_mining/sorted_words.csv")
