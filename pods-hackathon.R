# Martin
# June 21, 2019
# Hackathon using open data canada bilingualism data

library(readxl)
library(dplyr)
library(tidyverse)
library(rgdal)
library(ggplot2)
library(maptools)
library(sf)

# National analysis
shape <- readOGR(dsn = "ecr_bound", layer = "ler_000a16a_e")
shape_df <- fortify(shape)
shape_df$economic_region_code <- shape$ERUID

english_french <- read_csv("Week 4/Hackathon_GovCan/english-french.csv")
mother_tongue <- read_csv("Week 4/Hackathon_GovCan/mother-tongues.csv")

shape_montreal <- shape[shape$ERUID==2440,]
shape_laval <- shape[shape$ERUID==2445,]
shape_monteregie <- shape[shape$ERUID==2435,]
AOI_shape <-shape[shape$ERUID==2440 | shape$ERUID==2445 | shape$ERUID==2435,]

english_wfrench_clean <- english_french %>%
  mutate(economic_region_code = str_sub(`economic region code`, start=3, end=6))

mother_tongue_clean <- mother_tongue %>%
  mutate(economic_region_code = str_sub(`economic region code`, start=3, end=6)) %>%
  select(economic_region_code, `English mother tongue, adjusted`, `French mother tongue, adjusted`, `Other mother tongue, adjusted`, Total)

merged_bilingual <- inner_join(x=english_french_clean, y=mother_tongue_clean, by="economic_region_code")
merged_bilingual %>%
  select(economic_region_code, `speakers of English`, `speakers of French`, `English-French bilinguals`, `English mother tongue, adjusted`, `French mother tongue, adjusted`, `Other mother tongue, adjusted`, )

plot(shape, fill=merged_bilingual$`speakers of English`)

# Montreal analysis
mtl_district <- read_csv("mtl.districts.csv")
mtl_shape <- readOGR(dsn=path.expand("."), "LIMADMIN.shp")

glimpse(mtl_district)
mtl_district$X1[mtl_district$X1 == "Plateau-Mont-Royal"] <- "Le Plateau-Mont-Royal"
mtl_district$X1[mtl_district$X1 == "Villeray-Saint-Michel-Parc-Extension"] <- "Villeray-Saint-Michel-Parc-Extension"
mtl_district$X1[mtl_district$X1 == "Rosemont-La Petite-Patrie"] <- "Rosemont-La Petite-Patrie"
write_csv(mtl_district, "mtl_district2.csv")

#had to modify cities2 to cities 3 manually to be able to do barplot

cities4 <- cities3%>%
  
  filter(language !="Total")



#plot as percentage barplot

ggplot(cities4, aes(fill=language, y=count, x=name)) + 
  
  geom_bar(stat="identity", position="fill", colour="black")+
  
  labs(title="Languages in Canadian Cities", 
       
       subtitle="Native Speakers of Different Languages as a Percentage of the Total Population",
       
       caption="Hackathon 2: Koji, Martin, and Sara",
       
       x="City",
       
       y="Percentage",
       
       fill="Languages")+
  
  scale_y_continuous(labels =  c("0%", "25%", "50%", "75%", "100%"))
