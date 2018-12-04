library(pingr)
library(devtools)
library(mailR)

# Parametros del smtp
from <- "from@domain.com"
to <- c("to1@gmail.com","to2@domain.com")

smtp <- list(host.name = "smtp.domain.com", port = "25",
             user.name = "from@domain.com", passwd = "password", tls = T)

minutos <- 1 # Frecuencia (en minutos) de chequeo
error_num <- 5 # Iteracciones NOKs para que salte la alerta de KO

# Creamos dataframe con los datos de las adsls
adsls <- data.frame(providers = c('movistar','vodafone'),
                    ips = c('123.123.123.123', '124.124.124.124'),
                    status = c(0,0), # Tiempo de respuesta del ping
                    counter = c(0,0), # Contador de fallos de ping
                    avisos = c(F,F), # Si se ha enviado email de alerta
                    stringsAsFactors = FALSE)

##################### INICIO DEL SCRIPT ######################
# A por el bucle infinito
while_counter <- 1
while(T){
  print(paste("Bucle ",while_counter," - ",Sys.time()),sep="")
  
  # Lanzamos los ping
  for(i in 1:length(adsls$ips)){
    print(paste('Ping a ',adsls$providers[i],' por la ip ',adsls$ips[i],sep=''))
    adsls$status[i] <- ping_port(adsls$ips[i], port = 80, continuous = F, timeout = 1, count = 1)
  }
  
  # Incrementamos los contadores de error, en caso de error
  if(sum(is.na(adsls$status))>0){
    adsls$counter[is.na(adsls$status)] <- adsls$counter[is.na(adsls$status)] + 1
  }else{
    adsls$counter <- 0
  }
  
  # Validamos si hay x errores o mas, y enviamos email
  # Descartamos si se ha enviado ya
  if(sum(adsls$counter >= error_num)>0 && !adsls$avisos[adsls$counter >= error_num]){
    subject <- paste("[IT ALERT] > ADSL ISSUE > ",adsls$providers[adsls$counter >= error_num],
                 " without connection from last ",adsls$counter[adsls$counter >= error_num]," minutes"
                 ,sep="")
    if(length(subject)>1){
      subject <- "[IT ALERT] > ADSL ISSUE > Problems with the ISP providers"  
    }
    
    send.mail(from = from, to = to, subject = subject, body = " ",
              smtp = smtp, encoding = "iso-8859-1", authenticate = T, send = T)
    
    
    print(paste("There is an issue with the ADSL ",adsls$providers[adsls$counter >= error_num],
                ", it is down for the last ",adsls$counter[adsls$counter >= error_num]," minutes"
                ,sep=""))
    adsls$avisos[adsls$counter >= error_num] <- T
  }
  
  # Validamos si la conexion se ha restablecido
  if (sum(adsls$avisos) > 0 && adsls$counter[adsls$avisos] == 0){
    subject <- paste("[IT ALERT] > ADSL SOLVED > ",adsls$providers[adsls$counter >= error_num],
                 " is OK again",sep="")
    
    if(length(subject)>1){
      subject <- "[IT ALERT] > ADSL SOLVED > ADSLs are OK again"  
    }
    
    send.mail(from = from, to = to, subject = subject, body = " ",
              smtp = smtp, encoding = "iso-8859-1", authenticate = T, send = T)
    
    print(paste("ADSL restablised: ",adsls$providers[adsls$avisos],
                sep=""))
    adsls$avisos[adsls$avisos] <- F
  }
  
  print(adsls)
  print("------------------------------------------")
  
  
  # Esperamos 1 minuto y volvemos a hacer el chequeo
  Sys.sleep(minutos * 60)
  while_counter <- while_counter + 1
}
