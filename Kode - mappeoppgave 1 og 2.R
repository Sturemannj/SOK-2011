```{r echo=FALSE}
suppressPackageStartupMessages(library(WDI))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(countrycode))
```

```{r echo=TRUE, warning=FALSE}
# 1. BNP per innbyggere (alle ??r) og initial niv?? p?? BNP per innbyggere. WDI-variabel =  "NY.GDP.PCAP.PP.KD". 
# Velg start??r = 2000 og slutt??r = 2019
df_gdp0<-WDI(
  country = "all",
  indicator = c('gdppc'="NY.GDP.PCAP.PP.KD"),  
  start = 2000,
  end = 2019,
  extra = TRUE, # det ?? sette "extra = TRUE" f??rer til at vi laster inn ekstra informasjon som vi kan benytte seinere (f.eks. variabelen "region")
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_gdp <- subset(df_gdp0, select = c(country, region, income, iso2c, iso3c, year, gdppc) ) %>%  arrange(iso3c, year) # velg ut relevante variabler
df_gdp <-  df_gdp %>% mutate_all(na_if,"") # Vi ??nsker ?? ta vekk land som ikke har en iso3c kode. Dessverre er manglende observasjoner for "iso3c" (landkode) kodet som "blanks" isteden for "missing". Denne koden korrigerer dette.
df_gdp <- df_gdp[complete.cases( df_gdp$gdppc, df_gdp$iso3c),] # Ta vekk observasjoner som mangler data p?? gdppc og iso3c. 
df_gdp = df_gdp  %>%  
  mutate(year = as.numeric(year)) # Se til at year er en numerisk variabel. 

# Noen land har flere observasjoner for samme ??r (f.eks afghanistan ??r 2010). Vi ??nsker ?? ha ??n observasjon per land og ??r. 
df_gdp <- df_gdp[!duplicated(df_gdp[c("iso3c", "year", max("gdppc"))]), ]  %>%  arrange(iso3c, year) # Ta vekk duplikater for land og ??r, behold observasjonen med st??rst gdppc (denne regelen kan diskuteres)

# Lag et datasett med Y0 (niv?? p?? BNP per innbyggere i ??r 2000)
df_gdp2000  <- df_gdp %>%  arrange(iso3c, year) %>% group_by(iso3c) %>% #Behold den f??rste observasjonen for BNP per innbyggere (Y0)
  slice(1) %>%
  ungroup()
df_gdp2000 = subset(df_gdp2000, select = -c(year) ) # Slett un??dvendige variabler
df_gdp2000 <-   plyr:: rename(df_gdp2000,c("gdppc" = "gdppc0")) # Gi variabeln et nytt navn slik at vi kan identifisere den i datasetet. 

df_gdp <- left_join(df_gdp,df_gdp2000, by=c("country", "iso2c", "iso3c", "region", "income")) # Sett sammen data for BNP per innbygger alle ??r, med BNP per innbygger ??r 2000.
```

```{r echo=TRUE, warning=FALSE}
# 2. Humankapital (gjennomsnittlig antall ??r i skole blant befolkningen eldre enn 15 ??r). WDI-variabel = BAR.SCHL.15UP 
df_educ0<-WDI(
  country = "all",
  indicator = c('educ'="BAR.SCHL.15UP"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_educ <- subset(df_educ0, select = c(country, region, income, iso2c, iso3c, year, educ) ) %>%  arrange(iso3c, year) #Behold n??dvendige variabler
df_educ <- df_educ[complete.cases(df_educ$educ),] %>%  arrange(iso3c, year) # Slett observasjoner med manglende data

df_educ = df_educ %>%  
  arrange(iso3c, year) %>%  # Sorter etter Iso-kode og ??r. 
  mutate(educ = as.numeric(educ, na.rm = TRUE)) %>% # Se til at variabelen er numerisk
  ddply("iso3c",transform,
        avg_educ=mean(educ, na.rm = TRUE))  # Beregne gjennomsnittlig ??r i skole for tidsperioden 2000 - 2019 for hvert land, basert p?? tilgjenglig data (vil v??re 2000.2005,2010)

df_educ <- subset(df_educ, select = c(country, region, income, iso2c, iso3c, avg_educ)) # Her tar jeg vekk variabelen "year". Jeg gj??r dette fordi vi bare har en observasjon p?? utdanning per land. Vi ??nsker ?? bruke denne verdi for alle ??r. 
df_educ <- df_educ[!duplicated(df_educ[c("iso3c")]), ]  %>%  arrange(iso3c) # Ta vekk duplikater for hvert land.
```

```{r warning=FALSE}
# 3. Gjennomsnittlig sparing for perioden 2000-2015 (lagg fordi det kan ta litt tid for sparing ?? bli til investering)
df_nsy0<-WDI(
  country = "all",
  indicator = c( 'nsy'="NY.ADJ.NNAT.GN.ZS"),  
  start = 2000,
  end = 2015,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_nsy <- subset(df_nsy0, select = c(country, region, income, iso2c, iso3c, year, nsy) ) %>%  arrange(iso3c, year) #Behold n??dvendige variabler
df_nsy <- df_nsy[complete.cases(df_nsy$nsy),] %>%  arrange(iso3c, year) # Slett observasjoner med manglende data


df_nsy = df_nsy %>%  
  arrange(iso3c, year) %>%  # Sorter etter Iso-kode og ??r. 
  mutate(nsy = as.numeric(nsy, na.rm = TRUE)) %>% # Se til at variabelen er numerisk
  ddply("iso3c",transform,
        avg_nsy=mean(nsy, na.rm = TRUE))  # Beregne gjennomsnittlig ??r i skole for tidsperioden 2000 - 2019 for hvert land, basert p?? tilgjenglig data (vil v??re 2000.2005,2010)

df_nsy <- subset(df_nsy, select = c(country, region, income, iso2c, iso3c, avg_nsy)) # Her tar jeg vekk variabelen "year". Jeg gj??r dette fordi vi bare har en observasjon p?? utdanning per land. Vi ??nsker ?? bruke denne verdi for alle ??r. 
df_nsy <- df_nsy[!duplicated(df_nsy[c("iso3c")]), ]  %>%  arrange(iso3c) # Ta vekk duplikater for hvert land.
```

```{r warning=FALSE}
# 4. Vekst i arbeidskraften (n)
df_lf0<-WDI(
  country = "all",
  indicator = c('lf'="JI.TLF.TOTL"),  # lf = labor force
  start = 2000,
  end = 2019,
  extra = TRUE, 
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_lf <- subset(df_lf0, select = c(country, region, income, iso2c, year, lf) ) %>%  arrange(iso2c, year) # velg ut relevante variabler
df_lf <-   plyr:: rename(df_lf,c("iso2c" = "iso3c")) # variabelen som identifiserer land med kode er feil i datasetet. Dette korrigerer dette
df_lf <-  df_lf %>% mutate_all(na_if,"") 
df_lf [df_lf == 0]<-NA
df_lf <- df_lf[complete.cases(df_lf$iso3c, df_lf$lf),] # Ta vekk observasjoner som mangler data p?? lf og iso3c. 
df_lf = df_lf  %>%  
  mutate(year = as.numeric(year)) # Se til at year er en numerisk variabel. 

df_lf <- df_lf[!duplicated(df_lf[c("iso3c", "year")]), ]  %>%  arrange(iso3c, year) # Ta vekk duplikater for land og ??r

# Ta fram vekstraten i arbeidskraften (n). Vi har ikke data for hvert ??r i alle land. 
# For ?? beregne gjennomsnittlig ??rlig vekst m?? vi lage en variabel som m??ler antallet tidsperioder mellom hver observasjon.
df_n = df_lf %>%  
  arrange(iso3c, year) %>%  # Sorter p?? ??r og land
  ddply("iso3c",transform,
        t=c(NA,diff(year)),
        lf_growth=c(NA,diff(log(lf)))) #Vekstrate uten hensyn til tidsintervall

df_n <- df_n[complete.cases(df_n$t, df_n$lf_growth),] # Ta vekk observasjoner som mangler data p?? t

#N?? kan vi ta fram ??rlig vekstrate
df_n = df_n %>%  
  mutate(t = as.numeric(t)) %>%   
  mutate(lf_growth = as.numeric(lf_growth))
df_n <- transform(df_n, n =lf_growth/t)

# gjennomsnittlig vekstrate i arbeidskraften for hvert land
df_n <- df_n %>% # 
  ddply("iso3c",transform,
        avg_n=mean(n, na.rm = TRUE)) #Gjennomsnittlig ??rlig vekstrate i arbeidskraften

df_n <- subset(df_n, select = c(iso3c, avg_n) )
df_n <- df_n[!duplicated(df_n["iso3c"]), ]  %>%  arrange(iso3c) # Ta vekk duplikater for land
```

```{r warning=FALSE}
# 5. Lag et datasett som inneholder BNP data, utdanningsdata, sparing, og arbeidskraftsdata

df <- left_join(df_gdp, df_educ, by=c("country", "iso2c", "iso3c", "region", "income"))
df <- left_join(df, df_nsy, by=c("country", "iso2c", "iso3c", "region", "income"))
df <- left_join(df, df_n, by="iso3c")
df <- subset(df, select = c(country, region, income, iso2c, iso3c, year, gdppc, gdppc0, avg_educ, avg_nsy, avg_n)) # Behold n??dvendige variabler

# Mange observasjoner representerer aggregerte regioner. Vi ??nsker ?? ta vekk disse. Det finnes helt sikkert en bedre m??te ?? gj??re dette p??. Dette er den m??ten jeg kom p??.
df <- df  %>%  filter(iso2c!='1A' & iso2c !='1W' & iso2c != '4E' & iso2c != '7E' & iso2c !='8S'
                      & iso2c !='B8' & iso2c !='EU' & iso2c !='F1' & iso2c !='OE' & iso2c !='S1' & iso2c !='S2' & iso2c !="S3" 
                      & iso2c !='S4' & iso2c !='T2' & iso2c !='T3' & iso2c !='T4' & iso2c !='T5' & iso2c !='T6' & iso2c !='T7' 
                      & iso2c !='V1' & iso2c !='V2' & iso2c !='V3' & iso2c !='V4' & iso2c !='XC' & iso2c !='XD' & iso2c !='XE' 
                      & iso2c !='XF' & iso2c !='XG' & iso2c !='XH' & iso2c !='XI' & iso2c !='XJ' & iso2c !='XL' & iso2c !='XM' 
                      & iso2c !='XN' & iso2c !='XO' & iso2c !='XP' & iso2c !='XQ' & iso2c !='XT' & iso2c !='XU' & iso2c !='Z4' 
                      & iso2c !='Z7' & iso2c !='ZF'& iso2c !='ZG'  & iso2c !='ZH' & iso2c !='ZI'  & iso2c !='ZJ'  & iso2c !='ZQ'  
                      & iso2c !='ZT'  & iso2c !='Z7')  %>% arrange(iso3c, year) 
```

```{r warning=FALSE}
# 6. Lag et datasett for resterende variabler.

df_rest0<-WDI(
  country = "all",
  indicator = c('poptot'="SP.POP.TOTL", 'gi'="NE.GDI.FTOT.KD.ZG", 'gx'="NE.EXP.GNFS.KD.ZG", 'nry'="NY.ADJ.DRES.GN.ZS", 'p'="SP.POP.GROW" ),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_rest0<-df_rest0 %>% mutate_all(na_if,"")
df_rest <- df_rest0[complete.cases( df_rest0$iso3c),]  %>%  arrange(iso2c) 


# Ta vekk observasjoner som ikke representerer land.
df_rest <- df_rest  %>%  filter(iso2c!='1A' & iso2c !='1W' & iso2c != '4E' & iso2c != '7E' & iso2c !='8S'
                                & iso2c !='B8' & iso2c !='EU' & iso2c !='F1' & iso2c !='OE' & iso2c !='S1' & iso2c !='S2' & iso2c !="S3" 
                                & iso2c !='S4' & iso2c !='T2' & iso2c !='T3' & iso2c !='T4' & iso2c !='T5' & iso2c !='T6' & iso2c !='T7' 
                                & iso2c !='V1' & iso2c !='V2' & iso2c !='V3' & iso2c !='V4' & iso2c !='XC' & iso2c !='XD' & iso2c !='XE' 
                                & iso2c !='XF' & iso2c !='XG' & iso2c !='XH' & iso2c !='XI' & iso2c !='XJ' & iso2c !='XL' & iso2c !='XM' 
                                & iso2c !='XN' & iso2c !='XO' & iso2c !='XP' & iso2c !='XQ' & iso2c !='XT' & iso2c !='XU' & iso2c !='Z4' 
                                & iso2c !='Z7' & iso2c !='ZF'& iso2c !='ZG'  & iso2c !='ZH' & iso2c !='ZI'  & iso2c !='ZJ'  & iso2c !='ZQ'  
                                & iso2c !='ZT'  & iso2c !='Z7')  %>% arrange(iso3c, year) 

df_rest <- subset(df_rest, select = c("country", "region", "income", "iso3c", "iso2c", "year", "poptot", "p", "nry", "gi", "gx"))
df_all <- left_join(df, df_rest, by=c("country", "region", "income", "iso2c", "iso3c", "year"))

# Lag en rekkef??lge til variablene slik at det er enklere ?? f?? en oversikt over datamaterialet.
col_order <- c("country",  "region", "income", "iso3c", "iso2c", "year", "gdppc", "gdppc0", "poptot", "p", "avg_n", "avg_nsy", "nry", "gi", "gx", "avg_educ")
df_all <- df_all[, col_order]
```

```{r warning=FALSE}
# Ta fram vekstraten og gjennomsnitt for resterende variabler
df_growth0 = df_all %>%  
  arrange(iso3c, year) %>%  # Sorter p?? ??r og land
  ddply("iso3c",transform,
        gdpgrowth=c(NA,diff(log(gdppc)))*100) %>%   # ??rlig vekstrate i gdppc for hvert land
  mutate(gdpgrowth = as.numeric(gdpgrowth, na.rm = TRUE)) %>% # 
  ddply("iso3c",transform,
        avg_gdpgrowth=mean(gdpgrowth, na.rm = TRUE), #Gjennomsnittlig ??rlig vekstrate i BNP per innbygger for hvert land i perioden
        avg_gi=mean(gi, na.rm = TRUE), # Gjennomsnittlig ??rlig vekstrate i investeringer for hvert land  i perioden
        avg_nry=mean(nry, na.rm = TRUE),  # Gjennomsnittlig ??rlig vekstrate (negativ) i naturressurser for hvert land  i perioden
        avg_gx=mean(gx, na.rm = TRUE),  # Gjennomsnittlig ??rlig vekstrate i eksport for hvert land  i perioden
        avg_p=mean(p, na.rm = TRUE))  # Gjennomsnittlig ??rlig vekstrate i befolkningen for hvert land  i perioden

#View(df_growth0)
df_growth0 <-  df_growth0 %>% mutate_all(na_if,"") 
df_growth <- df_growth0[complete.cases( df_growth0$country, df_growth0$income, df_growth0$iso3c, df_growth0$avg_gdpgrowth, df_growth0$gdppc0, df_growth0$avg_n, df_growth0$avg_p, df_growth0$avg_nsy, df_growth0$avg_nry,df_growth0$avg_gi, df_growth0$avg_gx, df_growth0$avg_educ),] # Ta vekk land som mangler data 


df_growth <- subset(df_growth, select = c("country",  "region", "income", "iso3c", "iso2c","year", "poptot", "gdppc", "gdppc0", "avg_gdpgrowth", "avg_n", "avg_p", "avg_nsy", "avg_nry", "avg_gi", "avg_gx", "avg_educ"))

# Lage datasettet du vil bruke til analysen din
df_growth2019  <- df_growth %>%  arrange(iso3c, year) %>% group_by(iso3c) %>% 
  slice(n()) %>% # Behold den SISTE observasjonen for hvert land
  ungroup()
head(df_growth2019)
```

```{r warning=FALSE}
# Lag en variabel som er logaritmen av BNP per innbygger (enklere tolkning og presser sammen fordelingen)
df_growth2019$dppc <-as.numeric(df_growth2019$gdppc)
df_growth2019$ln_gdppc<-log(df_growth2019$gdppc) 
df_growth2019$ln_gdppc0<-log(df_growth2019$gdppc0) 
```

## Kapittel 4.1

```{r warning=FALSE}
suppressPackageStartupMessages(library(vtable))
```
```{r warning=FALSE}
figur 1: df <- subset(df_growth2019, select = c("avg_gdpgrowth", "avg_p", "avg_nsy", "avg_educ", "avg_nry", "avg_gi", "avg_n", "avg_gx", "ln_gdppc0"))
# Gi beskrivende navn til variablene (i samme rekkef??lge som de ligger i datasettet)
labs <- c("Gjennomsnittlig vekstrate i BNP pc 2000-2019 (%)", "Gjennomsnittlig befolkningsvekst (%)", "Nettosparing", "Humankapital", "reduksjonsrate i naturressurser", "Vekstrate i investeringer", "Vekstrate i arbeidskraft", "Vekstrate i eksport", "BNP per innbygger i 2000") 

# Lag tabellen
st(df, labels=labs,
   summ = list(
     c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'), # Beskriv hvilken statistikk du ??nsker ?? vise
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Gjennomsnitt','SD','Min','Maks') # Gi navn til kolumnene
   ))
```

```{r echo=FALSE, warning=FALSE}
suppressPackageStartupMessages(library(scales))





#Viser bare ett plott av alle 8 som kan lages, ettersom det er bare ?? endre X og Y verdien f??r annet resultat samt bytte navn p?? x og y aksen. 

#Verdier: "avg_gdpgrowth", "avg_p", "avg_nsy", "avg_educ", "avg_nry", "avg_gi", "avg_n", "avg_gx", "ln_gdppc0"

# Navn: "Gjennomsnittlig vekstrate i BNP pc 2000-2019 (%)", "Gjennomsnittlig befolkningsvekst (%)", "Nettosparing", "Humankapital", "reduksjonsrate i naturressurser", 
# "Vekstrate i investeringer", "Vekstrate i arbeidskraft", "Vekstrate i eksport", "BNP per innbygger i 2000") 


df_growth2019_n <- df_growth2019[complete.cases(df_growth2019$region,df_growth2019$poptot, df_growth2019$avg_p, df_growth2019$gdppc),] #Her tar jeg vekk land som mangler observasjoner. Dette gj??r grafen penere.  

plot1 <- ggplot(df_growth2019_n, aes(x = avg_p , y = ln_gdppc, na.rm = TRUE)) +
  xlab("Befolkningsvekst") + # Beskrivelse for x-akselen
  ylab("BNP per innbygger 2019") + # Beskrivelse for y-akselen
  theme_minimal(base_size = 14) + # Tekstst??rrelse og font
  ggtitle("Figur 1") +
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + # St??rrelse (farge) p?? bobblene avhenger befolkningsst??rrelse (region)
  scale_x_continuous(labels = dollar)  + # Legg til et dollar tegn til y-akselen
  scale_size_area(guide = "none", max_size = 14) + #Ta vekk legend for befolkningsst??rrelse
  theme(legend.text = element_text(size = 10,color="black"))+  # Bestem font-st??rrelse p?? legend
  scale_colour_manual(values = rainbow(9)) +# Velg farger til bobblene
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))+
  scale_y_continuous(trans = 'log2', labels = dollar, breaks=c(500, 2000, 8000, 32000, 120000)) + # logaritmere BNP pc og velg hvilke "ticks" som skal vises
  scale_x_continuous(breaks=c(-1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5 )) # Velg vilke "ticks" som skal vises p?? x-akselen
plot1
```
```{r echo=FALSE, warning=FALSE}
suppressPackageStartupMessages(library(sjPlot))
suppressPackageStartupMessages(library(sjmisc))
suppressPackageStartupMessages(library(sjlabelled))

model1 <- lm(ln_gdppc  ~ avg_educ, data= df_growth2019)
tab_model(model1)
```

```{r echo=FALSE, warning=FALSE}
model2 <- lm(ln_gdppc  ~ avg_nsy + avg_educ + avg_nry + avg_gi + avg_n + avg_p + avg_gx + gdppc0, data= no_outliers)
tab_model(model2, show.intercept = TRUE, show.p = TRUE, show.r2 = TRUE, show.fstat = TRUE)
summary(model2)
```






























