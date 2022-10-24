---
title: "R for journalister"
author: "Christian Lura"
date: "UiB, oktober 2022"

output: html_document
---



![](bilder/r.jpg)

### Tirsdag 25.10
* Hva er R?
* Derfor bruker jeg R
* Installere R (og Rstudio)
* R-syntaks
* Laste inn data i R, behandle og visualisere dem
* Oppgaver: Vi lager et datasett i plenum og demonstrerer en del funksjoner.

### Onsdag 26.10
* elever i grunnskolen
* Areal vs. befolkning
* Valg 21: Et skript for å beregne mandater til Stortinget
* Egne oppgaver

### Torsdag

* rvest og skrape nettsider: Wikipedia
* Yr: Lage et temperaturvarsel oppdatert i sanntid
* Facebook-nettverk: Lag en venneanalyse
* Egne oppgaver

***

### Hva er R?

* Objektorientert programspråk for statistisk analyse
* Verktøy for å innhente, analysere og publisere data
* Gratis
* Utvikles hver eneste dag
* [> 18.000 pakker](https://cran.r-project.org/web/packages/)
* Forståelig syntaks (etter hvert)
* Mye gratis hjelp og inspirasjon å få!

***

### Når velger jeg å bruke R?
1. Når det er eneste måte
2. Når det sparer tid
3. Når jeg skal løse en repeterende (kjedelig) oppgave
4. Når jeg vil ha full kontroll over mine data
5. Når jeg vil lære mer R
6. Når jeg vil ha det gøy

### Når bruker jeg *ikke* R?
* Når det er kjappere å la være

##### Men i starten vil det *alltid* være kjappere å la være...
### => Punkt 5 og 6 er viktigst!

***

### De to-tre viktigste grunnene til å programmere

> Skriptet ditt er for dine data det notatblokken er for din artikkel.

> Skriptet ditt er for dine data det kakeoppskriften er for din kake.

> Du har *full* kontroll, ref. pkt. 4.

***

### R-syntaks
* Objekt: Noe datainnhold som har fått et navn
* <- : Putt inn noe i et objekt
* Vektor: Flere objekter/verdier samlet 
* c() : Slå sammen verdier til en vektor
* str() : Sjekk objektet
* () : Parenteser rundt argumentene til funksjonen
* [] : Hent noe fra en vektor
* {} : Krølleparentes rundt funksjoner og if-uttrykk
* character() : Vektoren er tekst
* integer() : Vektoren er hele tall
* numeric() : Vektoren er desimaltall
* logical() : TRUE eller FALSE
* data.frame() : Rs form for Excel-tabell. Hver kolonne er en vektor
* ifelse() : Hvis noe er oppfylt, skal noe skje. Ellers skjer noe annet
* function() : Denne koden kan brukes på flere verdier
* sapply() : Kjør funksjon på flere verdier
* gsub() : Bytt ut noe tekst
* strsplit() : Del opp tekst
* paste() : Lim sammen tekst og objekter
* subset() : Filtrer data.frame

***

### Øvelse 1! Last inn data: Elever i grunnskolen

```r
#Øvelse 1: Elever i grunnskolen

install.packages("ggplot2")
```

```
## Warning: package 'ggplot2' is in use and will not be installed
```

```r
install.packages("writexl")
```

```
## Warning: package 'writexl' is in use and will not be installed
```

```r
setwd("kurs_uib/2022")
```

```
## Error in setwd("kurs_uib/2022"): cannot change working directory
```

```r
library(ggplot2)
library(writexl)

elever <- read.csv("data/elever.csv", sep=";")
str(elever)
```

```
## 'data.frame':	724 obs. of  7 variables:
##  $ region                                 : chr  "3001 Halden" "3002 Moss" "3003 Sarpsborg" "3004 Fredrikstad" ...
##  $ Elever.per.kommunal.skole..antall..2015: chr  "." "." "." "." ...
##  $ Elever.per.kommunal.skole..antall..2016: chr  "." "." "." "." ...
##  $ Elever.per.kommunal.skole..antall..2017: chr  "." "." "." "." ...
##  $ Elever.per.kommunal.skole..antall..2018: chr  "." "." "." "." ...
##  $ Elever.per.kommunal.skole..antall..2019: chr  "." "." "." "." ...
##  $ Elever.per.kommunal.skole..antall..2020: chr  "318.2" "309.5" "330.9" "303.2" ...
```

```r
#Rydd opp i dataene
fjern <- c(2:6)

#fjern

elever <- elever[-fjern]
names(elever) <- c("region", "antall_2020")

head(elever)
```

```
##             region antall_2020
## 1      3001 Halden       318.2
## 2        3002 Moss       309.5
## 3   3003 Sarpsborg       330.9
## 4 3004 Fredrikstad       303.2
## 5     3005 Drammen       368.3
## 6   3006 Kongsberg       247.9
```

```r
fjern.tomme <- which(elever$antall_2020 == ".")
elever <- elever[-fjern.tomme,]

#Ta vekk kommunenummer i navnet
elever$region <- substr(elever$region,6,nchar(elever$region))

elever$antall_2020 <- as.numeric(elever$antall_2020)

ggplot(elever, aes(x=region, y=antall_2020)) + geom_point()
```

![plot of chunk Elever i grunnskolen](figure/Elever i grunnskolen-1.png)

```r
elever <- elever[order(elever$antall_2020),]

#https://www.nrk.no/innlandet/800-elever-og-ansatte-ved-glommasvingen-skole-i-sor-odal-settes-i-karantene-1.15424260

elever$gruppe <- cut(elever$antall_2020, breaks = c(0,100,200,300,400,Inf))

#Lag en grafisk fremstilling
ggplot(elever, aes(x=region, y=antall_2020, color=gruppe)) + geom_point()
```

![plot of chunk Elever i grunnskolen](figure/Elever i grunnskolen-2.png)

```r
ggplot(elever, aes(x=region, y=antall_2020, color=gruppe)) + geom_point() +
  geom_text(aes(label=ifelse(antall_2020 > 400,as.character(region),"")),hjust=1.1,vjust=0) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  ggtitle("Antall elever på norske grunnskoler \npr. kommune (2020)")
```

![plot of chunk Elever i grunnskolen](figure/Elever i grunnskolen-3.png)

```r
write_xlsx(elever, "data/elever.xlsx")
```

***

### Øvelse 2! Last inn data: Areal vs befolkning


```r
##Areal vs befolkning

install.packages("readxl")
```

```
## Warning: package 'readxl' is in use and will not be installed
```

```r
library(ggplot2)
library(readxl)

options(scipen = 999) #Ta vekk vitenskapsnotasjon

kommuner <- read_xlsx("data/kommuner.xlsx")
names(kommuner) <- c("komnr", "navn", "admsenter", "fylke", "befolkning", "areal", "språk", 
                     "ordfører", "parti")
kommuner$språk <- gsub("Neutral", "Nøytral", kommuner$språk)
kommuner$befolkning <- gsub(",", "", kommuner$befolkning)
str(kommuner)
```

```
## tibble [356 × 9] (S3: tbl_df/tbl/data.frame)
##  $ komnr     : chr [1:356] "0301" "1101" "1103" "1106" ...
##  $ navn      : chr [1:356] "Oslo" "Eigersund" "Stavanger" "Haugesund" ...
##  $ admsenter : chr [1:356] "Oslo" "Egersund" "Stavanger" "Haugesund" ...
##  $ fylke     : chr [1:356] "Oslo" "Rogaland" "Rogaland" "Rogaland" ...
##  $ befolkning: chr [1:356] "673469" "14898" "141186" "37167" ...
##  $ areal     : chr [1:356] "454.03" "431.66" "262.52" "72.72" ...
##  $ språk     : chr [1:356] "Nøytral" "Bokmål" "Bokmål" "Bokmål" ...
##  $ ordfører  : chr [1:356] "Marianne Borgen" "Leif Erik Egaas" "Kari Nessa Nordtun" "Petter Steen jr" ...
##  $ parti     : chr [1:356] "SV" "H" "Ap" "H" ...
```

```r
kommuner$befolkning <- as.integer(kommuner$befolkning)
kommuner$areal <- as.numeric(gsub(",", "", kommuner$areal))
str(kommuner)
```

```
## tibble [356 × 9] (S3: tbl_df/tbl/data.frame)
##  $ komnr     : chr [1:356] "0301" "1101" "1103" "1106" ...
##  $ navn      : chr [1:356] "Oslo" "Eigersund" "Stavanger" "Haugesund" ...
##  $ admsenter : chr [1:356] "Oslo" "Egersund" "Stavanger" "Haugesund" ...
##  $ fylke     : chr [1:356] "Oslo" "Rogaland" "Rogaland" "Rogaland" ...
##  $ befolkning: int [1:356] 673469 14898 141186 37167 76328 3331 3237 2826 18762 19042 ...
##  $ areal     : num [1:356] 454 431.7 262.5 72.7 304.5 ...
##  $ språk     : chr [1:356] "Nøytral" "Bokmål" "Bokmål" "Bokmål" ...
##  $ ordfører  : chr [1:356] "Marianne Borgen" "Leif Erik Egaas" "Kari Nessa Nordtun" "Petter Steen jr" ...
##  $ parti     : chr [1:356] "SV" "H" "Ap" "H" ...
```

```r
#Legg til kolonne med befolkningstetthet
kommuner$tetthet <- kommuner$befolkning / kommuner$areal

gg <- ggplot(kommuner, aes(x=areal, y=befolkning)) +
  geom_point(aes(col=fylke, size=tetthet)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0,9750)) + 
  ylim(c(0,675000)) +
  labs(subtitle = "Areal vs befolkning",
       y="Befolkning",
       x="Areal",
       title="Scatterplot")

theme_set(theme_bw())

plot(gg)
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![plot of chunk Areal vs befolkning](figure/Areal vs befolkning-1.png)

```r
#Ta vekk Oslo og kommuner med størst areal
minste <- subset(kommuner, kommuner$areal < 500)
minste <- subset(minste, minste$navn != "Oslo")

ggminste <- ggplot(minste, aes(x=areal, y=befolkning)) +
  geom_point(aes(col=fylke, size=tetthet)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0,500)) + 
  ylim(c(0,300000)) +
  labs(subtitle = "Areal vs befolkning, kommuner < 500km^2, unntatt Oslo",
       y="Befolkning",
       x="Areal",
       title="Scatterplot")

plot(ggminste)
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![plot of chunk Areal vs befolkning](figure/Areal vs befolkning-2.png)

***

### Øvelse 3! Valg: Mandatberegning

I Norge brukes [Sainte-Laguës modifiserte metode](https://no.wikipedia.org/wiki/Sainte-Lagu%C3%ABs_metode) for å beregne hvor mange mandater hvert parti skal ha.

* Mål: Regne ut antall distriktsmandater selv.
  + Forstå utregningen og vårt demokratiske system.
  + Frigjøre oss fra ekspertene.
  + Fastslå hvor mange stemmer som skiller kandidatene.
  + Lage egne prognoser neste gang.


```r
##Beregne stortingsmandater

#install.packages(c("tidyr", "dplyr", "writexl"))
library(tidyr)
library(dplyr)
library(writexl)

#Laste inn valglister fra Valgdirektoratet
lister <- read.csv("data/lister21.csv", fileEncoding = "UTF-8")
str(lister)
```

```
## 'data.frame':	5174 obs. of  13 variables:
##  $ election_path: chr  "210001.01.01.000001" "210001.01.01.000001" "210001.01.01.000001" "210001.01.01.000001" ...
##  $ valg         : chr  "Stortingsvalget 2021" "Stortingsvalget 2021" "Stortingsvalget 2021" "Stortingsvalget 2021" ...
##  $ valgdistrikt : chr  "Østfold" "Østfold" "Østfold" "Østfold" ...
##  $ partikode    : chr  "AAN" "AAN" "AAN" "AAN" ...
##  $ partinavn    : chr  "Alliansen - Alternativ for Norge" "Alliansen - Alternativ for Norge" "Alliansen - Alternativ for Norge" "Alliansen - Alternativ for Norge" ...
##  $ display_order: int  2 2 2 2 2 2 2 2 2 2 ...
##  $ kandidatnr   : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ navn         : chr  "Hans Jørgen Lysglimt Johansen" "Bjørn Inge Johansen" "Jarle Johansen" "Kaspar Johan Birkeland" ...
##  $ bosted       : chr  "Oslo" "Orkland" "Giske" "Ålesund" ...
##  $ stilling     : chr  "" "" "" "" ...
##  $ fødselsdato  : chr  "13.09.1971" "03.08.1970" "14.11.1960" "18.06.1951" ...
##  $ alder        : int  50 51 60 70 53 57 36 49 71 47 ...
##  $ kjønn        : chr  "Mann" "Mann" "Mann" "Mann" ...
```

```r
fylkesfordeling <- read.csv("data/partifordeling.csv", sep=";", fileEncoding = "UTF-8")

distriktsres <- fylkesfordeling[c(1,2,7,8,9,10,13)]

names(distriktsres) <- c("fnr", "fylke", "kode", "parti", "pros","ant_velgere", "stemmer")
distriktsres$pros <- gsub(",", "\\.", distriktsres$pros)
distriktsres$pros <- as.numeric(distriktsres$pros)

delingstall <- seq(1,129,2)
delingstall
```

```
##  [1]   1   3   5   7   9  11  13  15  17  19  21  23  25  27  29  31  33  35  37
## [20]  39  41  43  45  47  49  51  53  55  57  59  61  63  65  67  69  71  73  75
## [39]  77  79  81  83  85  87  89  91  93  95  97  99 101 103 105 107 109 111 113
## [58] 115 117 119 121 123 125 127 129
```

```r
length(delingstall)
```

```
## [1] 65
```

```r
delingstall[1] <- 1.4

#Antall distriktsmandater pr.fylke
ant.dman <- data.frame(fnr = c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20),
                       man = c(8,18,19,6,5,7,6,5,3,5,13,15,3,7,9,4,8,5,4))

#Lag funksjon som beregner distriktsmandater for hvert fylke

get.fylkesres <- function(f) {
m <- ant.dman[f,2] #Antall mandater i fylke f

fylkesres <- subset(distriktsres, fnr==ant.dman[f,1]) #Hvilke resultater har vi for fylke f?

#Funksjon som regner ut st.Laguë for et bestemt delingstall
stlague <- function(i){
  del <- fylkesres$stemmer/delingstall[i]
  fylkesres <<- cbind(fylkesres, del)
}

fylkesres <- lapply(1:m, function(i) stlague(i))

fylkesres <- as.data.frame(fylkesres[[m]])
dels <- paste0("del",1:m)
dels <- c("fnr", "fylke","kode", "parti", "pros", "ant_velgere", "stemmer", dels)

names(fylkesres) <- dels
head(fylkesres)
fylkesres <- pivot_longer(fylkesres, cols = starts_with("del")) %>%
  arrange(-value)

fylkesres$name <- gsub("del", "", fylkesres$name)
fylkesres$name <- as.integer(fylkesres$name)
fylkesres <- fylkesres[1:m,]

#Finne politikernavn
get.pol <- function(j) {
  lister %>% dplyr::filter(valgdistrikt == as.character(fylkesres[j,2]),
                    partikode == as.character(fylkesres[j,3]),
                    kandidatnr == as.integer(fylkesres[j,8])) %>% 
                    select(navn)
}

pols <- data.frame(unlist(sapply(1:m, function(j) get.pol(j))))
names(pols) <- "kandidat"
fylkesres <- cbind(fylkesres, pols) 

return(fylkesres)
}

#Beregne alle distriktsmandater og lagre dem som Excel-fil
library(writexl)
alle <- lapply(1:19, function(x) get.fylkesres(x))
alle <- do.call("rbind", alle)

write_xlsx(alle, "data/distriktsmandater_2021.xlsx")
```

***

### Øvelse 4! Skrape Wikipedia-tabeller

```r
library(rvest)

url <- "https://no.wikipedia.org/wiki/Norge"

xpath <- as.character('//*[@id="mw-content-text"]/div/table[3]')
tabell <- url %>% read_html() %>% html_nodes(xpath=xpath) %>% html_table()
tabell
```

```
## [[1]]
## # A tibble: 20 × 4
##    ``        Tettsted              Folkemengde `Areal, km²`
##    <chr>     <chr>                 <chr>       <chr>       
##  1 1 av 983  Oslo                  1 043 168   270,45      
##  2 2 av 983  Bergen                265 470     90,3        
##  3 3 av 983  Stavanger/Sandnes     229 911     80,09       
##  4 4 av 983  Trondheim             191 771     58,61       
##  5 5 av 983  Fredrikstad/Sarpsborg 117 663     59,95       
##  6 6 av 983  Drammen               110 236     47,34       
##  7 7 av 983  Porsgrunn/Skien       94 102      53,83       
##  8 8 av 983  Kristiansand          64 913      25,08       
##  9 9 av 983  Ålesund               54 399      28,53       
## 10 10 av 983 Tønsberg              53 818      26,23       
## 11 11 av 983 Moss                  47 725      22,13       
## 12 13 av 983 Haugesund             45 436      21,26       
## 13 12 av 983 Sandefjord            45 520      24,19       
## 14 14 av 983 Bodø                  42 351      14,84       
## 15 15 av 983 Tromsø                40 979      13,65       
## 16 16 av 983 Arendal               37 861      27,28       
## 17 17 av 983 Hamar                 28 535      13,95       
## 18 19 av 983 Halden                25 887      14,23       
## 19 18 av 983 Larvik                26 731      15,04       
## 20 20 av 983 Askøy                 23 952      15,04
```

```r
#Kan vi lage en generisk funksjon? 
get.table <- function(url, nr) {
  xpath <- paste0('//*[@id="mw-content-text"]/div/table[', nr, ']', collapse = "")
tabell <- url %>% read_html() %>% html_nodes(xpath=xpath) %>% html_table()
tabell
}

#Kan vi hente ut flere på en gang?
sider <- c("https://no.wikipedia.org/wiki/USA", "https://no.wikipedia.org/wiki/Norge","https://no.wikipedia.org/wiki/Tyskland")

sider
```

```
## [1] "https://no.wikipedia.org/wiki/USA"     
## [2] "https://no.wikipedia.org/wiki/Norge"   
## [3] "https://no.wikipedia.org/wiki/Tyskland"
```

```r
lapply(1:length(sider), function(x) get.table(sider[x],2))
```

```
## [[1]]
## [[1]][[1]]
## # A tibble: 9 × 2
##   Rase                                      Prosent                       
##   <chr>                                     <chr>                         
## 1 kaukasoide-amerikanere                    72,4 (ikke-spanske hvite 63,7)
## 2 Afroamerikanere                           12,6                          
## 3 Asiatisk                                  4,8                           
## 4 Innfødte fra Hawaii og stillehavsøyene    0,9                           
## 5 Amerikanske innfødte og innfødte i Alaska 0,9                           
## 6 Andre                                     6,2                           
## 7 Multietniske (to eller flere raser)       2,9                           
## 8 Latinamerikanere (uansett rase)           16,3                          
## 9 Ikke-latinamerikansk (uansett rase)       83,7                          
## 
## 
## [[2]]
## [[2]][[1]]
## # A tibble: 6 × 6
##   `Storby (tettsted)`   `Vinter  (des-feb)` `Vår(mar-mai)` `Sommer(jun-aug)`
##   <chr>                 <chr>               <chr>          <chr>            
## 1 Oslo                  1 °C                14 °C          21 °C            
## 2 Bergen                4 °C                11 °C          19 °C            
## 3 Stavanger/Sandnes     3 °C                11 °C          18 °C            
## 4 Trondheim             0 °C                9 °C           18 °C            
## 5 Fredrikstad/Sarpsborg 0 °C                11 °C          21 °C            
## 6 Drammen               –1 °C               12 °C          21 °C            
## # … with 2 more variables: `Høst(sep-nov)` <chr>, `Nedbør(per år)` <chr>
## 
## 
## [[3]]
## [[3]][[1]]
## # A tibble: 17 × 8
##    Delstat   `Innbyggere[mi…` `Katolikker[mi…` `Protestanter[…` `Andre[million…`
##    <chr>     <chr>            <chr>            <chr>            <chr>           
##  1 Baden-Wü… 10,75            4,12             3,69             2,95            
##  2 Bayern    12,52            7,32             2,74             2,45            
##  3 Berlin    3,42             0,32             0,74             2,37            
##  4 Brandenb… 2,54             0,08             0,50             1,97            
##  5 Bremen    0,66             0,06             0,29             0,29            
##  6 Hamburg   1,77             0,18             0,58             1,01            
##  7 Hessen    6,07             1,57             2,53             1,97            
##  8 Mecklenb… 1,68             0,06             0,31             1,31            
##  9 Niedersa… 7,97             1,43             4,18             2,37            
## 10 Nordrhei… 18,00            7,81             5,11             5,08            
## 11 Rheinlan… 4,05             1,90             1,30             0,85            
## 12 Saarland  1,04             0,68             0,20             0,15            
## 13 Sachsen   4,22             0,15             0,90             3,21            
## 14 Sachsen-… 2,41             0,09             0,45             1,87            
## 15 Schleswi… 2,84             0,17             1,62             1,05            
## 16 Thüringen 2,29             0,19             0,60             1,51            
## 17 Totalt    82,22            25,49            24,83            31,90           
## # … with 3 more variables: `Katolikker[%]` <chr>, `Protestanter[%]` <chr>,
## #   `Andre[%]` <chr>
```


### Øvelse 5! Temperaturvarsel i sanntid

Mål: Lage et skript som henter temperaturdata fra Yr, lagrer dem.


```r
library(jsonlite)
library(ggplot2)
library(lubridate)
library(googlesheets4)

#Hente data
yrurl <- "https://api.met.no/weatherapi/locationforecast/2.0/complete?altitude=0&lat=60.39299&lon=5.32415"
yr <- fromJSON(yrurl)

#Ta en kikk
#head(yr)
str(yr)
```

```
## List of 3
##  $ type      : chr "Feature"
##  $ geometry  :List of 2
##   ..$ type       : chr "Point"
##   ..$ coordinates: num [1:3] 5.32 60.39 0
##  $ properties:List of 2
##   ..$ meta      :List of 2
##   .. ..$ updated_at: chr "2022-10-24T10:35:50Z"
##   .. ..$ units     :List of 24
##   .. .. ..$ air_pressure_at_sea_level    : chr "hPa"
##   .. .. ..$ air_temperature              : chr "celsius"
##   .. .. ..$ air_temperature_max          : chr "celsius"
##   .. .. ..$ air_temperature_min          : chr "celsius"
##   .. .. ..$ air_temperature_percentile_10: chr "celsius"
##   .. .. ..$ air_temperature_percentile_90: chr "celsius"
##   .. .. ..$ cloud_area_fraction          : chr "%"
##   .. .. ..$ cloud_area_fraction_high     : chr "%"
##   .. .. ..$ cloud_area_fraction_low      : chr "%"
##   .. .. ..$ cloud_area_fraction_medium   : chr "%"
##   .. .. ..$ dew_point_temperature        : chr "celsius"
##   .. .. ..$ fog_area_fraction            : chr "%"
##   .. .. ..$ precipitation_amount         : chr "mm"
##   .. .. ..$ precipitation_amount_max     : chr "mm"
##   .. .. ..$ precipitation_amount_min     : chr "mm"
##   .. .. ..$ probability_of_precipitation : chr "%"
##   .. .. ..$ probability_of_thunder       : chr "%"
##   .. .. ..$ relative_humidity            : chr "%"
##   .. .. ..$ ultraviolet_index_clear_sky  : chr "1"
##   .. .. ..$ wind_from_direction          : chr "degrees"
##   .. .. ..$ wind_speed                   : chr "m/s"
##   .. .. ..$ wind_speed_of_gust           : chr "m/s"
##   .. .. ..$ wind_speed_percentile_10     : chr "m/s"
##   .. .. ..$ wind_speed_percentile_90     : chr "m/s"
##   ..$ timeseries:'data.frame':	84 obs. of  2 variables:
##   .. ..$ time: chr [1:84] "2022-10-24T11:00:00Z" "2022-10-24T12:00:00Z" "2022-10-24T13:00:00Z" "2022-10-24T14:00:00Z" ...
##   .. ..$ data:'data.frame':	84 obs. of  4 variables:
##   .. .. ..$ instant      :'data.frame':	84 obs. of  1 variable:
##   .. .. .. ..$ details:'data.frame':	84 obs. of  17 variables:
##   .. .. .. .. ..$ air_pressure_at_sea_level    : num [1:84] 1000 1000 999 999 999 ...
##   .. .. .. .. ..$ air_temperature              : num [1:84] 8.2 8.7 9 8.9 9 9 8.4 8.3 8.4 8.5 ...
##   .. .. .. .. ..$ air_temperature_percentile_10: num [1:84] 7.4 7.5 7.5 7.8 7.8 7.8 7.4 7.4 7.2 7.2 ...
##   .. .. .. .. ..$ air_temperature_percentile_90: num [1:84] 9.1 9.8 10 10.1 10.4 10.5 10.3 10 9.8 9.9 ...
##   .. .. .. .. ..$ cloud_area_fraction          : num [1:84] 100 99.9 100 99.9 100 100 97.6 90.6 90.9 96.2 ...
##   .. .. .. .. ..$ cloud_area_fraction_high     : num [1:84] 92 95.3 30.6 5.1 81.9 91.7 62.9 59.3 72.5 63.7 ...
##   .. .. .. .. ..$ cloud_area_fraction_low      : num [1:84] 50.6 65.9 75.5 82.3 70.5 57.1 49.3 36.1 51.7 65.3 ...
##   .. .. .. .. ..$ cloud_area_fraction_medium   : num [1:84] 99.3 99.1 99.9 99.9 99.6 99.3 81.7 66.9 65.5 76.8 ...
##   .. .. .. .. ..$ dew_point_temperature        : num [1:84] 6.7 5.5 6.5 6.5 6.3 6.1 6.1 4.7 4.4 4.5 ...
##   .. .. .. .. ..$ fog_area_fraction            : num [1:84] 0 0 0 0 0 0 0 0 0 0 ...
##   .. .. .. .. ..$ relative_humidity            : num [1:84] 70.6 66.7 69.3 72.6 72.5 71.6 75.5 72.1 70.7 70.5 ...
##   .. .. .. .. ..$ ultraviolet_index_clear_sky  : num [1:84] 0.7 0.7 0.5 0.3 0.1 0 0 0 0 0 ...
##   .. .. .. .. ..$ wind_from_direction          : num [1:84] 92.3 83.2 92.8 66.3 83.4 ...
##   .. .. .. .. ..$ wind_speed                   : num [1:84] 0.8 2.2 0.9 0.9 0.9 0.6 0.1 1.7 2 1.7 ...
##   .. .. .. .. ..$ wind_speed_of_gust           : num [1:84] 4.9 4 4 1.9 2 2 1.1 2.8 3.5 3.5 ...
##   .. .. .. .. ..$ wind_speed_percentile_10     : num [1:84] 0.3 0.2 0.2 0.6 1.1 0.5 0.4 1.1 0.3 0.7 ...
##   .. .. .. .. ..$ wind_speed_percentile_90     : num [1:84] 3.2 4.2 2.8 2.9 3.1 3.1 3 2.7 3 2.8 ...
##   .. .. ..$ next_12_hours:'data.frame':	84 obs. of  2 variables:
##   .. .. .. ..$ summary:'data.frame':	84 obs. of  2 variables:
##   .. .. .. .. ..$ symbol_code      : chr [1:84] "cloudy" "cloudy" "cloudy" "cloudy" ...
##   .. .. .. .. ..$ symbol_confidence: chr [1:84] "somewhat certain" "somewhat certain" "somewhat certain" "somewhat certain" ...
##   .. .. .. ..$ details:'data.frame':	84 obs. of  1 variable:
##   .. .. .. .. ..$ probability_of_precipitation: num [1:84] 20.6 21 20.5 21 21.6 24 26.5 26.8 30.2 36.7 ...
##   .. .. ..$ next_1_hours :'data.frame':	84 obs. of  2 variables:
##   .. .. .. ..$ summary:'data.frame':	84 obs. of  1 variable:
##   .. .. .. .. ..$ symbol_code: chr [1:84] "cloudy" "cloudy" "cloudy" "cloudy" ...
##   .. .. .. ..$ details:'data.frame':	84 obs. of  5 variables:
##   .. .. .. .. ..$ precipitation_amount        : num [1:84] 0 0 0 0 0 0 0 0 0 0 ...
##   .. .. .. .. ..$ precipitation_amount_max    : num [1:84] 0 0 0 0 0 0 0.2 0.2 0 0 ...
##   .. .. .. .. ..$ precipitation_amount_min    : num [1:84] 0 0 0 0 0 0 0 0 0 0 ...
##   .. .. .. .. ..$ probability_of_precipitation: num [1:84] 4.2 8 11.7 6.2 8.2 8.6 13.4 16.1 6 3.9 ...
##   .. .. .. .. ..$ probability_of_thunder      : num [1:84] 0.3 0.3 0.3 0.3 0.4 0.5 0.2 0.2 0.2 0.2 ...
##   .. .. ..$ next_6_hours :'data.frame':	84 obs. of  2 variables:
##   .. .. .. ..$ summary:'data.frame':	84 obs. of  1 variable:
##   .. .. .. .. ..$ symbol_code: chr [1:84] "cloudy" "cloudy" "cloudy" "cloudy" ...
##   .. .. .. ..$ details:'data.frame':	84 obs. of  6 variables:
##   .. .. .. .. ..$ air_temperature_max         : num [1:84] 9 9 9 9 9 8.5 8.5 8.5 8.5 8.5 ...
##   .. .. .. .. ..$ air_temperature_min         : num [1:84] 8.4 8.3 8.3 8.3 8 8 8 8 8 8 ...
##   .. .. .. .. ..$ precipitation_amount        : num [1:84] 0 0 0 0 0 0 0 0 0 0 ...
##   .. .. .. .. ..$ precipitation_amount_max    : num [1:84] 0 0 0.6 0.6 0.6 0.6 0.6 0 0 0 ...
##   .. .. .. .. ..$ precipitation_amount_min    : num [1:84] 0 0 0 0 0 0 0 0 0 0 ...
##   .. .. .. .. ..$ probability_of_precipitation: num [1:84] 4.1 8.8 11.7 12.4 12 12.2 11.8 8.2 6.2 5.7 ...
```

```r
#Fikse tidsformatet
tid <- yr$properties$timeseries$time
tid <- gsub("Z", "", tid)
tid <- gsub("T", " ", tid)
tid <- ymd_hms(tid, tz=Sys.timezone())
tid <- tid + 2*60*60

#Finne temperaturene
temp <- yr$properties$timeseries$data$instant$details$air_temperature

#Lag data frame (=regneark)
tempbergen <- data.frame(tid=tid, temp=temp)
#head(tempbergen)

#Lag et plot
tempplot <- ggplot(tempbergen, aes(x=tid, y=temp)) + geom_point() +
  xlab("dato") + ylab("temperatur") +
  labs(title = "Temperaturvarsel for Bergen") 

tempplot
```

![plot of chunk Yr-varsel](figure/Yr-varsel-1.png)

```r
tempplot + geom_smooth()
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![plot of chunk Yr-varsel](figure/Yr-varsel-2.png)

```r
#Lagre dataene med unikt filnavn
dir.create("data/temp/")
```

```
## Warning in dir.create("data/temp/"): 'data\temp' already exists
```

```r
filnavn <- paste0("data/temp/yrbergen_",Sys.time(),".csv")
filnavn <- gsub(" ", "_", filnavn)
filnavn <- gsub(":", "-", filnavn)
write.csv(tempbergen, filnavn, row.names = FALSE)
```

### Øvelse 6! Analyse av Facebook-likes (hvis tid)

[Brennpunkt: Millioneventyret (januar 2021)](https://tv.nrk.no/serie/brennpunkt/2021/MDDP11000121)

![](bilder/milleven.jpg)

***

Facebook er vanskelig å skrape etter [Cambridge Analytica-skandalen.](https://www.nytimes.com/2018/04/04/us/politics/cambridge-analytica-scandal-fallout.html)

* Mål: Lage en venneoversikt basert på bilde-likes. 


```r
library(writexl)

fb <- function(x) {
  path <- "data\\fbbilder"
  mappe <- dir(path)
  nytt.bilde <- paste0(path, "\\", mappe[grep(".jpg", mappe)])
  
  fiks.tegn <- function(i) {
    tekst <- ny.tekst[,i]
    tekst <- gsub("Ø", "?", tekst)
    tekst <- gsub("æ", "?", tekst)
    tekst <- gsub("ø", "?", tekst)
    tekst <- gsub("å", "?", tekst)
    tekst <- gsub("ö", "?", tekst)
    tekst <- gsub("ä", "?", tekst)
    tekst <- gsub("?.", "?", tekst)
    return(tekst)
  }
  
  if (file.exists(nytt.bilde) == TRUE){
    ny.tekst <- read.delim(paste0(path, "\\", "tekst.txt"), header=FALSE)
    ny.tekst[,1] <- fiks.tegn(1)
    
    navn <- ny.tekst[2,]
    navn <- strsplit(navn, ",")[[1]][1]
    
    dato <- ny.tekst[3,]
    
    #Lag ny mappe hvis navn ikke eksisterer
    dir.create(file.path(path, navn), showWarnings = FALSE)
    
    #gi nytt navn til bilde
    ny.mappe <- paste0("data\\fbbilder\\",navn)
    filer <- dir(ny.mappe)
    index <- length(grep(".jpg", filer)) + 1
    nytt.bildenavn <- paste0("data\\fbbilder\\",navn, "_", index, ".jpg")
    nytt.bildenavn2 <- paste0(navn, "_", index, ".jpg")
    file.rename(nytt.bilde, nytt.bildenavn)
    
    #Flytt bildet til ny mappe
    file.copy(from = nytt.bildenavn,
              to   = ny.mappe)
    file.remove(nytt.bildenavn)
    
    #Lag mappe til kommentarer
    kommentarmappe <- paste0(navn, "_kommentarer")
    dir.create(file.path(ny.mappe, kommentarmappe), showWarnings = FALSE)
    
    #Hente ut tekst fra fil
    start <- nrow(ny.tekst) - x + 1
    slutt <- nrow(ny.tekst)
    likes <- ny.tekst[start:slutt,]
    
    df <- data.frame(fil=nytt.bildenavn2, likes = likes, dato = dato)
    
    ##Sjekk om data allerede eksisterer. Hvis ikke, lagre data
    filnavn <- paste0("data\\fbbilder\\",navn,"\\",navn,".csv")
    filnavnexcel <- paste0("data\\fbbilder\\",navn,"\\",navn,".xlsx")
    
    if (file.exists(filnavn) == TRUE){
      alle.data <- read.csv(filnavn)} else {
        alle.data <- data.frame(fil=character(), likes=character())
      }
    
    alle.data <- rbind(alle.data, df)
    write.csv(alle.data, filnavn, row.names = FALSE)
    write_xlsx(alle.data, filnavnexcel)
    
    ##Lag liste over de som liker mest
    freq <- as.data.frame(table(alle.data[,2]))
    freq <- freq[order(freq[,2], decreasing=TRUE),]
    names(freq) <- c("navn", "antall")
    freqfile <- paste0("data\\fbbilder\\",navn,"\\",navn,"_freq.xlsx")
    write_xlsx(freq, freqfile)
    
    #Lagre fil av kommentarer
    kommentarfilnavn <- paste0("data\\fbbilder\\",navn,"\\",kommentarmappe, "\\",navn, "_", index, ".txt")
    write.table(ny.tekst, kommentarfilnavn, sep="", row.names = FALSE, col.names = FALSE, fileEncoding = "UTF-8")
  } else {
    print("bildefil mangler")
  }
}
```


### Lage nettverk av Facebook-data


```r
#Lage nettverk av to navn fra Facebook-basen
library(readxl)
library(networkD3)
library(htmlwidgets)
```

```
## 
## Attaching package: 'htmlwidgets'
```

```
## The following object is masked from 'package:networkD3':
## 
##     JS
```

```r
path <- "C:\\Users\\n633164\\Documents\\R\\kurs_uib\\2021\\data\\"
dmapper <- dir(paste0(path, "fbbilder"))
dmapper <- dmapper[-grep("tekst.txt", dmapper)]
ant.mapper <- length(dmapper)


duo <- function(x,y) {
  pers1 <- dmapper[x]
  pers2 <- dmapper[y]
  
  #Finne felles kjente
  fil1 <- paste0(path, "fbbilder\\", pers1,"\\",pers1, "_freq.xlsx")
  fil2 <- paste0(path, "fbbilder\\", pers2,"\\",pers2, "_freq.xlsx")
  
  if (file.exists(fil1) & file.exists(fil2)){
    data1 <- read_xlsx(fil1)
    data2 <- read_xlsx(fil2)
    felles <- intersect(data1$navn, data2$navn)
    
    
    #Fjern skit
    fjern1 <- grep("Kommentarer", felles)
    fjern2 <- grep("Skriv en kommentar", felles)
    fjern3 <- grep("4 år", felles)
    fjern4 <- grep("5 år", felles)
    fjern5 <- grep("6 år", felles)
    fjern6 <- grep("1 år", felles)
    fjern7 <- grep("Se opprinnelig tekst", felles)
    fjern8 <- grep("Oversett alle kommentarer", felles)
    fjern9 <- grep("2 år", felles)
    fjern10 <- grep("3 år", felles)
    fjern <- c(fjern1,fjern2,fjern3,fjern4,fjern5,fjern6,fjern7,fjern8,fjern9,fjern10)
    felles <- if (length(fjern) > 0) {felles[-fjern]} else {felles}
    
    #Fiks bokstaver
    felles <- gsub("ÃƒËœ", "Å", felles)
    felles <- gsub("ÃƒÂ¸", "ø", felles)
    felles <- gsub("ÃƒÂ¸", "ø", felles)
    felles <- gsub("ÃƒÂ¥", "å", felles)
    felles <- gsub("ÃƒÂ¦", "æ", felles)
    felles <- gsub("Ãƒâ€¦", "Ã…", felles)
    
    #Sjekk om pers1 kjenner pers2
    vennesjekk <- grepl(pers1, data2[,1], fixed=TRUE)
    
    #Hvis det er ingen felles venner, avbryt.
    if (length(felles) == 0) {
      print(paste0(pers1, " og ", pers2, " har ingen felles venner."))
      break} else {
        #Lage nettverk
        df1 <- data.frame(person = pers1, bekjent = felles)
        df2 <- data.frame(person = pers2, bekjent = felles)
        df <- rbind(df1,df2)
        
        #Hvis pers1 og pers2 er venner, legg til node
        if(vennesjekk == TRUE) {
          venner <- data.frame(person=pers1, bekjent=pers2)
          df <- rbind(df, venner)
        }
        
        
        names(df) <- c("from", "to")
        nw <- simpleNetwork(df, height="200px", width="200px",        
                            Source = 1,                 # column number of source
                            Target = 2,                 # column number of target
                            linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                            charge = -2000,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                            fontSize = 16,               # size of the node names
                            fontFamily = "serif",       # font og node names
                            linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                            nodeColour = "#99098a",     # colour of nodes, MUST be a common colour for the whole graph
                            opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                            zoom = T                    # Can you zoom on the figure?
        )
        
        filnavn <- paste0("C:\\Users\\n633164\\Documents\\R\\kurs_uib\\2021\\nettverk\\",pers1,"-", pers2,".html")
        saveWidget(nw, file=filnavn)
      }} else {
        df <- "Mangler fil"
      }
  return(df)
}
```

***
