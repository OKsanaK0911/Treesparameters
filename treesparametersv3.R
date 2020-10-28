install.packages("xlsx, dep = T")
install.packages("readr")
library(tidyverse)
library(sf)
library(ggplot2)




Первая работа

library(readr)
Treesparameters <- read_delim("C:/Users/podcherninami/Desktop/Treesparameters.csv", 
                              ";", escape_double = FALSE, col_types = cols(Tno = col_double()), 
                              locale = locale(decimal_mark = ","), 
                              trim_ws = TRUE)
View(Treesparameters)

plot(Treesparameters$`Ht (m)`, Treesparameters$`Crown Diameter (m)`)

ggplot(Treesparameters, aes(x = `Ht (m)`, y = `Crown Diameter (m)`, color = Species)) + geom_point()


Вторая работа

# Все переменные имеют корректный тип данных
# Повторяющиеся переменные убраны
# Из имен переменных убраны размерности
# Всем переменам заданы их реальные размерности
# Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной
# Категориальные переменные должны быть факторами
# Категории переменной из имени должны быть убраны
# Коды категориальных переменных заменены их категориями
# Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84
# Виды должны быть переименованы на латыне

КОММЕНТАРИЙ: Все переменные имеют корректный тип данных - в import dataset мы изменили decimal mark с точки на запятую



Повторяющиеся переменные убраны

Treesparameters = Treesparameters %>% select(-`dbh (mm)`, -HR)

КОММЕНТАРИЙ: из повторяющихся переменных убрали dbh (mm) и HR-пустую колонку





Из имен переменных убраны размерности

Treesparameters = Treesparameters %>% rename(dbh = `dbh (m)`)
Treesparameters = Treesparameters %>% rename(Ht = `Ht (m)`)
Treesparameters = Treesparameters %>% rename(Clearance_Ht = `Clearance Ht (m)`)
Treesparameters = Treesparameters %>% rename(Crown_Depth = `Crown Depth (m)`)
Treesparameters = Treesparameters %>% rename(Average_Radial_Crown_spread = `Average Radial Crown spread (m)`)
Treesparameters = Treesparameters %>% rename(Total_Mean_Radial_Crown_Spread = `Total Mean Radial Crown Spread (m)`)
Treesparameters = Treesparameters %>% rename(Crown_Diameter = `Crown Diameter (m)`)
Treesparameters = Treesparameters %>% rename(Stem_diameter_Jan_2017 = `Stem diameter Jan 2017 (mm)`)
Treesparameters = Treesparameters %>% rename(Annual_Girth_Increment = `Annual Girth Increment (mm)`)
Treesparameters = Treesparameters %>% rename(Two_yr_dia_gain = `2yr dia gain (mm)`)
Treesparameters = Treesparameters %>% rename(Total_NSEW_Radial_Crown_Spread = `Total N,S,E,W Radial Crown Spread (m)`)





Всем переменам заданы их реальные размерности

library(units)
units(Treesparameters$dbh) = as_units("m")
units(Treesparameters$Ht) = as_units("m")
units(Treesparameters$Clearance_Ht) = as_units("m")
units(Treesparameters$Crown_Depth) = as_units("m")
units(Treesparameters$Average_Radial_Crown_spread) = as_units("m")
units(Treesparameters$Total_NSEW_Radial_Crown_Spread) = as_units("m")
units(Treesparameters$Total_Mean_Radial_Crown_Spread) = as_units("m")
units(Treesparameters$Crown_Diameter) = as_units("m")
units(Treesparameters$Stem_diameter_Jan_2017) = as_units("mm")
units(Treesparameters$Two_yr_dia_gain) = as_units("mm")
units(Treesparameters$Annual_Girth_Increment) = as_units("mm")
units(Treesparameters$`Predicted crown diamet using combined formulla`) = as_units("m")
units(Treesparameters$`Predicted Crown Diameter`) = as_units("m")


Treesparameters %>% as.data.frame()





Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной

Treesparameters = Treesparameters %>% mutate(error = `Predicted crown diamet using combined formulla` - Crown_Diameter)

Treesparameters$error

Treesparameters = Treesparameters %>% rename(Crown_Diameter_Using_Combined_Formulla_Error = Crown_Diameter_Error)

Treesparameters = Treesparameters %>% mutate(Crown_Diameter_Error = `Predicted Crown Diameter` - Crown_Diameter)

Treesparameters = Treesparameters %>% select(-Difference, Diference)





Категориальные переменные должны быть факторами

library(forcats)

names(Treesparameters)

Treesparameters$`Age Index 1=Y 2=SM 3=EM 4=M`
Treesparameters = Treesparameters %>% mutate(`Age Index 1=Y 2=SM 3=EM 4=M` = as.numeric(`Age Index 1=Y 2=SM 3=EM 4=M`)

Treesparameters = Treesparameters %>% mutate(AgeIndex = as_factor(`Age Index 1=Y 2=SM 3=EM 4=M`)) %>% mutate(AgeIndex = fct_recode(AgeIndex,Y = "1", SM = "2",EM = "3", M = "4"))  

Treesparameters$AgeIndex[Treesparameters$AgeIndex == "<NA>"]

Treesparameters$AgeIndex

Treesparameters$`Data Set      1=Norwich                0= Peterborough`

Treesparameters = Treesparameters %>% 
  mutate(DataSet = as_factor(`Data Set      1=Norwich                0= Peterborough`)) %>%
  mutate(DataSet = fct_recode(DataSet, Norwich = "1", Peterborough = "0"))

Treesparameters$DataSet
Treesparameters$`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`
Treesparameters = Treesparameters %>%
  mutate(PruningIndex = as_factor(`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`)) %>%
  mutate(PruningIndex = fct_recode(PruningIndex,`pruned within 5yrs` = "5", `pruned between 5 and 10yrs` = "10"))

Treesparameters$PruningIndex
Treesparameters$`Type of Prunning None= 0 CR= 1 Other = 2 Both = 3`
Treesparameters = Treesparameters %>%
  mutate(TypeOfPruning = as_factor(`Type of Prunning None= 0 CR= 1 Other = 2 Both = 3`)) %>%
  mutate(TypeOfPruning = fct_recode(TypeOfPruning,None = "0", CR = "1", Other = "2", Both = "3"))
Treesparameters$TypeOfPruning

Treesparameters$`Soil Code 1=sand and gravel 2= Clay 3=silt`
Treesparameters = Treesparameters %>%
  mutate(SoilCode = as_factor(`Soil Code 1=sand and gravel 2= Clay 3=silt`)) %>%
  mutate(SoilCode = fct_recode(SoilCode,`Sand and Gravel` = "1", Clay = "2", Slit = "3"))

Treesparameters$SoilCode
Treesparameters$SoilCode %>% as.integer()

Treesparameters = Treesparameters %>% rename(geology = `Superfical Geology From British Geological Survey Geology of Britain Viewer`)
Treesparameters$geology

Treesparameters = Treesparameters %>% 
  mutate(is_river = geology %>% str_detect("River"))
mutate(Soil= case_when(is_river & SoilCode == "Sand and Gravel" ~ "River Sand and Gravel", is_river & SoilCode == "Clay" ~ "River Clay", is_river & SoilCode == "Silt" ~ "River Silt", TRUE ~ as.character(Soil))

Treesparameters$is_river




Виды должны быть переименованы на латыне

Treesparameters$Species
Treesparameters$Species[Treesparameters$Species == "Oak"] = "Quercus robur"
Treesparameters$Species[Treesparameters$Species == "Norway maple"] = "Acer platanoides"
Treesparameters$Species[Treesparameters$Species == "Norway Maple"] = "Acer platanoides"
Treesparameters$Species[Treesparameters$Species == "Silver Birch"] = "Betula pendula"
Treesparameters$Species[Treesparameters$Species == "Sycamore"] = "Platanus occidentalis"




Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84

library(stringr)
Treesparameters$`Grid Reference`
coord = str_replace_all(Treesparameters$`Grid Reference`,' ','')
coord_N = str_trunc(coord, 12, "right", ellipsis = "") %>% str_trunc(5,"left", ellipsis = "")
coord_E = str_trunc(coord, 7, "right", ellipsis = "") %>% str_trunc( 5, "left", ellipsis = "")
quadr = str_trunc(coord, 2, "right", ellipsis = "")
table_c = data.frame(as.integer(coord_E), as.integer(coord_N), quadr)

names(table_c)=c("E", "N", "quadr")
head(table_c)
table_c = na.exclude(table_c)

table_c = table_c %>% mutate("Easting_BC" = case_when(quadr == "TF" ~ E +600000, quadr == "TG" ~ E +700000, quadr == "TL" ~ E +600000,))
table_c = table_c %>% mutate("Northing_BC" = case_when(quadr == "TF" ~ N +300000, quadr == "TG" ~ N +300000, quadr == "TL" ~ N +200000,))

table_c = na.exclude(table_c)


library(sf)
table_WGS = table_c %>% st_as_sf(coords = c("Easting_BC", "Northing_BC"), crs = 27700) %>% st_transform(4326) %>% st_coordinates() %>% as.data.frame()

table_WGS = data.frame(Lat = table_WGS$Y, Lon = table_WGS$X)
table_WGS %>% head

Treesparameters$`Grid Reference`[1]
table_c[1,]
table_WGS[1,]

coord = cbind(table_c,table_WGS)
head(coord)
