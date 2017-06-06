download.file(url = "http://docs.google.com/spreadsheet/pub?key=phAwcNAVuyj0NpF2PTov2Cw&output=xlsx", 
              
              destfile = "Data/indicator gapminder infant_mortality.xlsx")




download.file(url = "http://docs.google.com/spreadsheet/pub?key=phAwcNAVuyj0NpF2PTov2Cw&output=xlsx", 
              
              destfile = "Data/indicator undata total_fertility.xlsx")


library("readxl")

raw_fert<-read_excel(path="Data/indicator undata total_fertility.xlsx", sheet= "Data")
raw_infantMort<-read_excel(path="Data/indicator gapminder infant_mortality.xlsx", sheet= "Data")

#tidyr, data manipulation, long and wide formats
#long definition, only one observation (measure)
#wide definition, multiple variables/obsercations of the same data

#gapminder are not long og wide, but tidy

#raw_fert a very wide format, same data distributed over several columns; fertility rate sampled each day

fert<- raw_fert %>%
  rename(country=`Total fertility rate`) %>% 
  gather(key=year, value=fert, -country) %>% #id, name of new variable, remove country name
  mutate(year=as.integer(year))
  
  
fert

?as.integer
