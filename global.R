# VARIABLES GLOBALES


# INPUT: PAGE_ID string  

# Cargar libreras
# library(devtools)   # install.packages("devtools")  
# library(RCurl)      # install.packages("RCurl")
library(Rfacebook)  # install.packages("Rfacebook")
# library(jsonlite)   # install.packages("jsonlite")
# library(rjson)      # install.packages("rjson")
library(RJSONIO)    # install.packages("RJSONIO")
library(rmongodb)   # install_github("mongosoup/rmongodb")
library(plyr)       # install.packages("plyr")
# library(readxl)     # install.packages("readxl")
library(sqldf)      # install.packages("sqldf"))
library(reshape2)   # install.packages("reshape2"))
library(ggplot2)     # install.packages("ggplot2"))

# extended TOKEN
tokenFB <- "EAAEPeHll0o0BAMirr5MXo4b202bTPykdo2iOFNX24RhYHMRDx6C94JzPjOMZBKBO2V9473uQF2oqV3Gs8ZAqBEz3x8Wq8X0EqigKK4ZBfoD94rD3AaEnfZBQk9GzhhnowysVRAZA1E5WWaGfv8X5qWKz9MJeN2JMZD"

# This new long-lived access token will expire on June 24th, 2017:
# tokenFB <- "EAAEPeHll0o0BADaUY5EYNm5Iv21JOQJV98t6LCKLDPUhePZApcm87ZBM9Ad3OoUIc2MFhB2qaHwcNCfcdeze7vmeax4KWZCc8ECD2nhJtjhZB7LcYN8jTE7VXOq5yN67ZAARt4yBc9kqoOwvcpy0OZAof2RdF8GAEZD"

# Mongo database and collection
ns <- "FACEBOOK.PAGES"

# Format Facebook Date Function
format.facebook.date<- function(datestring) {
  date<- as.POSIXct(datestring,
                    format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT") }


getToday <- Sys.Date()

# Arrancar mongod y robomongo
# Comprobar que esta corriendo el mongo local



###################################
#### Delete Mongo Facebook Page ###
###################################

deletemongo <- function(url){
  
  mongo <- mongo.create()  # mongo local
  # mongo <- mongo.create("34.208.146.222:27017")  # <--- poner ip de amazon web services 
  
  # comprobación conexión mongo
  print(mongo.is.connected(mongo))
  
  lista_url <- unlist(strsplit(url,"/"))
  pag_id <- lista_url[length(lista_url)]
  
  
  # find_pag <- '{"page_id" : "140216402663925"}'
  find_pag = paste0("{",'"',"page_id",'"',":", '"',pag_id,'"',"}")
  
  # ?Exite ya en la base de datos?
  pag_bson <-  mongo.bson.from.JSON(find_pag)
  
  QueryPag <- mongo.find.one(mongo, ns,pag_bson)   # bson
  
  
  DeletePag <- mongo.remove(mongo, ns,pag_bson)   # bson
  
  if  (is.null(QueryPag) == FALSE && DeletePag == TRUE){
    e = "Registro eliminado"
  }else{
    e = "Registro no encontrado"
    
  }
  
  return(e)
  mongo.disconnect(mongo)
}




addDates <- function(pag){ 
  
  
  # Columnas de fechas 
  pag$datetime<- format.facebook.date(pag$created_time)
  
  pag$month <- format(pag$datetime, "%Y-%m")
  pag$monthname <- format(pag$datetime,"%B") 
  pag$YMDay <- format(pag$datetime,"%Y-%m-%d") 
  pag$day <- format(pag$datetime,"%d-%m") 
  
  pag$hour <- format(pag$datetime,"%H")
  pag$hour  <- sapply(pag$hour, as.numeric)
  
  
  # # franja horaria / time slot
  i = 1
  for (i in 1:length(pag$hour)){
    if (pag$hour[i] <  8) {
      pag$timeslot[i] <- "00-08"
      
    } else {
      if (pag$hour[i] < 16 ){
        pag$timeslot[i] <- "08-16"
      }else{pag$timeslot[i] <- "16-00"}
    }
  } # end for franaj horaria
  
  # pag$weekday <- weekdays(as.Date(pag$date))
  
  pag$weekdayN <- format(pag$datetime,format="%u")
  
  # dia de la semana
  i = 1
  for (i in 1:length(pag$weekday)){
    pag$weekday[i] <- switch(pag$weekdayN[i], "1" = "lunes", "2" = "martes", "3" = "miercoles","4"= "jueves",
                             "5" = "viernes","6" = "sabado","7" = "domingo")
  }
  
  return(pag)
}



####################################
#### Get Info from Facebook Page ###
####################################

getInfoPage <- function(url) {

  # Conexion a mongoDB (la instncia local o de amazon debe estar activa)
  # mongo <- mongo.create("34.208.146.222:27017")  # mongo local
  mongo <- mongo.create()  # <--- poner ip de amazon web services 
  
  # comprobación conexión mongo
  mongo.is.connected(mongo)
  
  lista_url <- unlist(strsplit(url,"/"))
  pag_id <- lista_url[length(lista_url)]
  
  #getToday <- Sys.Date()
  
  # find_pag <- '{"page_id" : "140216402663925"}'
  find_pag = paste0("{",'"',"page_id",'"',":", '"',pag_id,'"',',','"',"date",'"',":", '"',getToday,'"',"}")
  
  # ?Exite ya en la base de datos?
  pag_bson <-  mongo.bson.from.JSON(find_pag)
  QueryPag <- mongo.find.one(mongo, ns,pag_bson)   # bson
  is.null(QueryPag)
  
  
  if (is.null(QueryPag) == TRUE){
    
    # TRUE --> no existe el registro o no está actualizado
    
    # Extraccion de datos de Facebook de los ultimos 2 meses
    
    #getToday <- Sys.Date()
    l_month <- Sys.Date()-60
    
    
    # feed = FALSE
    pag <- getPage(pag_id,token = tokenFB, n=500, since = l_month, until = getToday, 
                   feed = FALSE, verbose= TRUE, reactions = TRUE)
    
    pag_name <- pag$from_name[1]
    
  # añadir columnas de fechas aqui 
    
    pag <- addDates(pag)
    
    # pasar nulos a ceros
    pag[is.na(pag)] <- 0
    
    # Tabla 
    pag <- pag[,c("id", "likes_count", "from_name","type","comments_count","shares_count",
                  "love_count","haha_count","wow_count","sad_count","angry_count","created_time","datetime",
                  "month","day","YMDay","hour","timeslot","weekday","weekdayN")]
    

    ### dataframe TO JSON 
    ## Row based data frame encoding 
    datapage <- sapply(seq(nrow(pag[1])), function(y){list(pag[y,])}, simplify = TRUE)
    jsonPag <- RJSONIO::toJSON(list(page_id = pag_id, name = pag_name, date = as.character.POSIXt(getToday) , data=datapage), pretty = TRUE)
    
    
    # insertar nuevo registro  
    bs <- mongo.bson.from.JSON(jsonPag)
    mongo.insert(mongo, ns, b = bs)
    
    result_data <- pag
    # dameid <- pag_id
    # damename <- pag_name
    
  }else{
    # FALSE --> existe el documento y está actualizado
    
    
    print(paste0("ID ",pag_id," existe en la base de datos"))
    # bson
    result_find <- mongo.bson.to.Robject(QueryPag)
    
    # extraer datos de mongodb
    result_data <- do.call("rbind.fill", lapply(result_find$data, as.data.frame))
    
  }
  
  mongo.disconnect(mongo)
  
  return(result_data)
  
}

###############
### getName ###
###############

getName <- function(result_data) {
 name <- result_data$from_name[1]
 return(name)
}

###############
### getName ###
###############

updateDate <- function(url){
   # mongo <- mongo.create("34.208.146.222:27017")
  mongo <- mongo.create()
  
  lista_url <- unlist(strsplit(url,"/"))
  pag_id <- lista_url[length(lista_url)]
  
  
  # find_pag <- '{"page_id" : "140216402663925"}'
  find_pag = paste0("{",'"',"page_id",'"',":", '"',pag_id,'"',"}")
  
  # ?Exite ya en la base de datos?
  pag_bson <-  mongo.bson.from.JSON(find_pag)
  QueryPag <- mongo.find.one(mongo, ns,pag_bson)   # bson
  result_find <- mongo.bson.to.Robject(QueryPag)
  upDate <- result_find$date
  return(upDate)
  
  mongo.disconnect(mongo)
  
}


######################
### SQL dataframes ###
######################

result15 <- function(result_data) { 
              month15 <- Sys.Date()-15
              date15<- format.facebook.date(month15)
              date15P <- as.double.POSIXlt(date15)
              result_data15 <- result_data[result_data[,"datetime"]> date15P,]
}

result30 <- function(result_data) { 
              month30 <- Sys.Date()-30
              date30<- format.facebook.date(month30)
              date30P <- as.double.POSIXlt(date30)
              result_data30 <- result_data[result_data[,"datetime"]> date30P,]

}


##################
### SUM TOTALS ###
##################


sqlTotals <- function(result_data) { 
        totals <- sqldf("select from_name, sum(likes_count) as likes, sum(comments_count) as comments,
                                            sum(shares_count) as shares, sum(love_count) as love, sum(haha_count) as haha,
                                            sum(wow_count) as wow, sum(sad_count) as sad, sum(angry_count) as angry
                                            from result_data group by from_name")
        return(totals)

}



######################
### GROUP BY TYPES ###
######################


sqlTypes <- function(result_data){
        totbytype <- sqldf("select type, sum(likes_count) as likes, sum(comments_count) as comments,
                                               sum(shares_count) as shares, sum(love_count) as love, sum(haha_count) as haha,
                                               sum(wow_count) as wow, sum(sad_count) as sad, sum(angry_count) as angry
                                               from result_data group by type")
        return(totbytype)
}



#########################
### GROUP BY WEEK DAY ###
#########################


sqlweekday <- function(result_data){

            byweekday <- sqldf("select from_name, weekday, weekdayN, sum(likes_count) as likes, sum(comments_count) as comments,
                                            sum(shares_count) as shares, sum(love_count) as love, sum(haha_count) as haha,
                                            sum(wow_count) as wow, sum(sad_count) as sad, sum(angry_count) as angry
                                            from result_data group by weekdayN order by weekdayN")

            return(byweekday)
}





sqlbyday <- function(result_data){
            byday <- sqldf("select from_name, YMDay, sum(likes_count) as likes, sum(comments_count) as comments,
                           sum(shares_count) as shares, sum(love_count) as love, sum(haha_count) as haha,
                           sum(wow_count) as wow, sum(sad_count) as sad, sum(angry_count) as angry
                           from result_data group by YMDay order by YMDay")
            return(byday)
}


dfReactions <- function(totals){
            totreactions <- t(totals[,c("love","haha","wow","sad","angry")])
            totreactionsdf <- as.data.frame(totreactions)
            rownames(totreactionsdf) <- c("love","haha","wow","sad","angry")
            # type_labels <- paste( totbytype$type, totbytype$totlikes, sep="\n")
            return(totreactionsdf)
}
