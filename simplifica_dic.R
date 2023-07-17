#Corrige y acomoda los enunciados que se tienen de la clasificación.
#La idea es tener el diccionario en una forma procesada para agilizar la revisión
#y la asignación de códigos por parte del programita
library(tm);library(utils)
palabras_trans <- c("torton", "troca", "blazer", "cheyen", "auto", "clonaron", "laptop", "metro", "hostiga", "hostigar", "vandalizaron",
                    "existia", "robo")

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

#---- Aquí meteré o trataré de agregar casos----
# Caso de ver si se asigna bien un código acorde a la descripción
library(svDialogs)            # Para usar el paquete

del_name <- dlgInput(message = "Ingrese delito: ")$res
del_num  <- dlgInput(message = "Ingrese código de delito: ")$res

del_num  <- as.integer(del_num) # convert character into integer


df <- data.frame("ID_DEL" = del_num, "DEL_X" = del_name)
aa <- reclasificadora(df)

if(aa[,4] == "REVISAR" ){
print(paste("Hola, la descripción '", del_name, 
            "' no corresponde a ningún delito registrado"))
}else{
  print(paste("Este es el resultado de la revisión:"))
  print(aa)
}


#Caso de verificar si una descripción está en el diccionario base
#Si no aparece se pregunta se sugiere agregar
comprueba <- function(){

del_name <- dlgInput(message = "Ingrese delito: ")$res

#del_name <- "pishing"

del_name1 <- simplifica(del_name)

if(length(corpus[grep(del_name1, corpus$DEL_MOCH), c(1)])==1){
 print( paste("La descripción corresponde a ID_DEL:",
              corpus[grep(del_name1, corpus$DEL_MOCH), c(1)], 
              "|| Que identifica: ", corpus[grep(del_name1, corpus$DEL_MOCH), c(2)] ))
}else{
  print("La descripción NO está en el diccionario base. Se sugiere agregarla o revisar la descripción.")
}#corpus$DEL_MOCH <- lapply(corpus$DEL_X, simplifica)##
}
