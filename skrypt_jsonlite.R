#pakiety
library(httr)
library(jsonlite)
library(readxl)

#Wczytaj wspó³rzêdne przebiegu ramion z pliku fileInput1 i fileInput2
getpoints_arm0 <- read_excel(fileInput1) 
getpoints_arm0 <- read_excel(fileInput2)


#Pobierz dane API z URL w formacie JSON
res = GET("http://api.przykladowe_KM")

#Przetwórz dane z formatu JSON na data.frame o nazwie dane
dane = fromJSON(rawToChar(res$content), simplifyDataFrame = T) 

#https://db.rstudio.com/best-practices/run-queries-safely/

#Przypisz zmienne z kolumn z tabeli dane
min_x = data$min_x	 	     #pobieramy kolumnê min_x ze zbioru dane            
max_x = data$max_x	       #pobieramy kolumnê max_x ze zbioru dane                                            
min_y = data$min_y		     #pobieramy kolumnê min_y ze zbioru dane                                             
max_y = data$max_y         #pobieramy kolumnê max_y ze zbioru dane       

#Przypisz etykiety nazw 
nrisk_arm0 <- data$nrisk_arm0 
nrisk_arm1 <- data$nrisk_arm1

time_risk =  data$time_risk

label_arm0 = data$label_arm0
label_arm1 = data$label_arm1

###Etap 1 Rekonstrukcja danych

#argumenty stanowi¹ zdefiniowane wczeœniej obiekty

preprocess_arm0  =  preprocess(getpoints_arm0,           #obiekt z fileInput
                               trisk = time_risk,
                               nrisk = nrisk_arm0,       #pacjenci w zagro¿eniu
                               totalpts = nrisk_arm0[1], #³¹czna liczba pacjentów (1 element z wektora nrisk)
                               maxy = max_y)             #wartoœæ maksymalna osi y

preprocess_arm1 = preprocess(getpoints_arm1,
                             trisk = time_risk,
                             nrisk = nrisk_arm1,
                             totalpts = nrisk_arm1[1],
                             maxy = max_y)


###Utworzenie danych ipd (dane wejœciowe do modeli prze¿ycia)
getIPD_arm0 <-   getIPD(preprocess_arm0, #obiekt jako wynik funkcji preprocess
                        armID = 1)       #indeks ramienia (argument obowi¹zkowy 1 lub 2)
getIPD_arm1 <-  getIPD(preprocess_arm1, 
                       armID = 2)

#³¹czenie zbiorów IPD z obiektu - wyniku funkcji getIPD)
ipd <- rbind( 
  data.frame(getIPD_arm0$IPD, "arm" = "0", "group" = label_arm0), #dodanie etykiety z nazw¹ grupy
  data.frame(getIPD_arm1$IPD, "arm" = "1", "group" = label_arm1))

### Tworzenie modelu prze¿ycia

fit_km <- survfit(Surv(time, status) ~ arm, data = ipd)

### Tabela z median¹ prze¿ycia
km_table  <- surv_median(fit_km) #tworzenie obiektu
km_table$median <- round(km_table$median,1) #zaokr¹glanie wyniku
km_table$lower <- round(km_table$lower,1)
km_table$upper <- round(km_table$upper,1)
colnames(km_table) <- c("Grupa", "Mediana", "LCI 0.95", "UCI 0.95") #zmiana nazw kolumn

#Konwersja data.frame na JSON
km_table_json <- toJSON(km_table)

### Wykres prze¿ycia KM
km_plot <- ggsurvplot(fit_km,
                      data=ipd, 
                      pval = TRUE,
                      pval.method = T,
                      conf.int = TRUE,
                      conf.int.style = "step",
                      ggtheme = theme_pubclean(),
                      surv.median.line = "hv")

#ekstrakcja danych (wspó³rzêdnych wyrkesu) do formatu JSON
km_plot_json <- km_plot$data.survplot %>%
  toJSON()
