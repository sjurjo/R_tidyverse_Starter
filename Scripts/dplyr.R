#loading the tidyverse package

library("tidyverse")
library("maps")

gapminder<-read_csv(file = "Data/gapminder-FiveYearData.csv")

gapminder

rep("Pipe", times=3)

"Pipe" %>%
  rep(times=3)

year_country_gdp<-select(gapminder, year, country, gdpPercap)
head(year_country_gdp) #out put top of the data frame

#pipe line + select function
year_country_gdp<-gapminder %>% 
  select(year, country, gdpPercap)

  head(year_country_gdp)
  
#filter command, choosing year 2002
gapminder %>% 
  filter(year==2002) %>%  # == equals to 2 = T
  ggplot(mapping = aes(x=continent, y=pop))+
  geom_boxplot()


year_country_gdp_euro<- gapminder %>% 
  filter(continent== "Europe") %>% 
  select(year, country, gdpPercap)

year_country_gdp_euro

#filter Norway
year_country_gdp_Norway<- gapminder %>% 
  filter(country == "Norway") %>% 
  select(year, lifeExp, gdpPercap)

year_country_gdp_Norway

#grouped by continent, five groups, average for each group (5 continent)
gapminder %>% 
  group_by(continent)

#ggplot, grouped by continent and summarized by gdpPercap
gapminder %>% 
  group_by(continent) %>% 
  summarize(mean_gdpPercap=mean(gdpPercap)) %>% 
  ggplot(mapping=aes(x=continent, y=mean_gdpPercap))+geom_point()


#filter, Asia in continent column, grouped by country and filter into the min and max country lifeExp
gapminder %>% 
  filter(continent=="Asia") %>% 
  group_by(country) %>%
  summarize(mean_lifeExp=mean(lifeExp)) %>% 
  filter(mean_lifeExp==min(mean_lifeExp)|mean_lifeExp==max(mean_lifeExp))


gapminder %>% 
  filter(continent=="Asia") %>% 
  group_by(country) %>% #group by country
  summarize(mean_lifeExp=mean(lifeExp)) %>% 
  ggplot(mapping=aes(x=country, y=mean_lifeExp))+
  geom_point()+
  coord_flip() #change x and y axis

#mutate function to add a variable in the data frame

gapminder %>% 
  mutate(gdp_billion=gdpPercap*pop/10^9) %>% 
  group_by(continent, year) %>% 
  summarize(mean_gdp_billion=mean(gdp_billion))



# merging data set JOIN
gapminder_country_summary<-gapminder %>% 
  group_by(country) %>% 
  summarize(mean_lifeExp=mean(lifeExp))

map_data("world") %>%
  rename(country=region) %>% #renamed the region variable name to country
  left_join(gapminder_country_summary, by= "country") %>% #merging data set
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=mean_lifeExp))+
  scale_fill_gradient(low="blue", high= "red")+
  coord_equal()

##however usa is not plotted due to country name is probably spelled diffrently in the two data set



