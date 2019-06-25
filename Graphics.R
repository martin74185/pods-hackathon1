library(pacman)
pacman::p_load(dplyr,tidyverse,poliscidata,magrittr,stringr,lubridate,ggplot2,gganimate,wesanderson,LaCroixColoR)
theme_set(theme_get() + theme(text = element_text(family = 'Fira Sans')))

english.french <- read.csv("~/Desktop/pods-2019/english-french.csv", stringsAsFactors=FALSE)
mother.tongues <- read.csv("~/Desktop/pods-2019/mother-tongues.csv", stringsAsFactors=FALSE)
geogdata <- read.csv("~/Desktop/pods-2019/geogdata.csv", comment.char="#", stringsAsFactors=FALSE)

colnames(mother.tongues) <- c("name.english", 
                   "name.french", 
                   "type.english",
                   "type.french", 
                   "census.division.code", 
                   "economic.region.code", 
                   "province.code", 
                   "canadian.heritage.region.code", 
                   "total", "English.first", "French.first", "other.first")
lang0 <- mother.tongues %>%
  mutate(key = paste(name.english, census.division.code)) %>%
  select(key, total, English.first, French.first, other.first)
lang1 <- english.french %>%
  mutate(key = paste(name.english, census.division.code)) %>%
  left_join(lang0, "key") %>%
  mutate(English = as.numeric(str_replace_all(English,",","")),
         French = as.numeric(str_replace_all(French,",","")),
         both = as.numeric(str_replace_all(both,",","")),
         either = as.numeric(str_replace_all(either,",","")),
         neither = as.numeric(str_replace_all(neither,",","")),
         total = as.numeric(str_replace_all(total,",","")),
         English.first = as.numeric(str_replace_all(English.first,",","")),
         French.first = as.numeric(str_replace_all(French.first,",","")),
         other.first = as.numeric(str_replace_all(other.first,",",""))) %>%
  mutate_each(list(~ replace(., which(is.na(.)), 0))) %>%
  mutate(tot1 = English + French - both + neither,
         English.second = English - English.first,
         French.second = French - French.first,
         perceng = round(English / tot1 * 100, 1),
         percfren = round(French / tot1 * 100, 1),
         perceng2 = round(English.second / tot1 * 100, 1),
         percfren2 = round(French.second / tot1 * 100, 1),
         percother = round(other.first / tot1 * 100, 1),
         percboth = round(both / tot1 * 100,1)) %>%
  mutate(name.english = defrench(name.english),
         name.french = defrench(name.french),
         type.french = defrench(type.french),
         key = paste(name.english, str_sub(census.division.code,start=3)))

defrench <- function(x) {
  x <- str_replace_all(x,fixed("\xf4"),"ô")
  x <- str_replace_all(x,fixed("\xce"),"Î")
  x <- str_replace_all(x,fixed("\xe9"),"é")
  x <- str_replace_all(x,fixed("\xe8"),"è")
  x <- str_replace_all(x,fixed("\xe7"),"ç")
  x <- str_replace_all(x,fixed("\xc9"),"É")
  x <- str_replace_all(x,fixed("\xd9"),"ë")
  x <- str_replace_all(x,fixed("\xeb"),"ë")
  x <- str_replace_all(x,fixed("\xe2"),"â")
  x <- str_replace_all(x,fixed("\xaf"),"î")
  x
}

lang2 <- geogdata %>% 
  mutate(key = paste(CSDname.SDRnom,CDuid.DRidu)) %>%
  group_by(key) %>% 
  mutate(lat = median(DArplat.Adlat),
         long = median(DArplong.ADlong)) %>%
  select(lat,long,PRename.PRanom,CSDname.SDRnom,CDuid.DRidu,ERuid.REidu) %>%
  ungroup() %>%
  unique() %>%
  mutate(key = defrench(key))

#For the graphs: change between percfren, perceng, percboth as desired,
#and the maximum colour between darkblue, darkred, and purple.
#Alpha of 0.1 is recommended for a nationwide picture of English.
lang1 %>% #New Brunswick map
  filter(type.english=="census subdivision") %>%
  left_join(lang2,"key") %>%
  ggplot(aes(x=long,y=lat)) + 
  scale_color_gradient(low = "white", high = "darkblue") +
  xlim(-69,-64) +
  ylim(45,48) +
  geom_point(aes(colour=percboth,size=tot1),alpha=0.5)

lang1 %>%    #Coordinates are for Ontario + Québec, log colour scale
  filter(type.english=="census subdivision") %>%
  filter(percother > 0 & percother < 100) %>%
  left_join(lang2,"key") %>%
  ggplot(aes(x=long,y=lat)) + 
  scale_color_gradient(low = "white", high = "darkblue",trans="log10") +
  xlim(-85,-65) +
  ylim(42,49) +
  geom_point(aes(colour=percfren,size=tot1),alpha=0.5)

lang1 %>% #Scatterplot (coordinates: NB)
  filter(type.english=="census subdivision") %>%
  left_join(lang2,"key") %>%
  #filter(lat > 45 & lat < 48 & long > -69 & long < -64) %>%
  ggplot(aes(x=percfren,y=percboth)) + 
  #scale_x_continuous(trans = "log10") +
  #scale_y_continuous(trans = "log10") +
  #scale_color_gradient(low = "white", high = "purple") +
  geom_point(aes(size=tot1),alpha=0.01,colour="blue")

lang1 %>%    #Coordinates are for Québec (St-Laurent)
  filter(type.english=="census subdivision") %>%
  filter(perceng2 > 0) %>%
  left_join(lang2,"key") %>%
  ggplot(aes(x=long,y=lat)) + 
  scale_color_gradient(low = "white", high = "darkblue") +
  xlim(-80,-65) +
  ylim(45,50) +
  geom_point(aes(colour=percfren,size=tot1),alpha=0.5)
  #labs(title = "50 years of official bilingualism")
