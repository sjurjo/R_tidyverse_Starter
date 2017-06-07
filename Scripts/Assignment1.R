library("tidyverse")

download.file(url = "https://raw.githubusercontent.com/dmi3kno/SWC-tidyverse/master/data/gapminder_plus.csv", 
              
              destfile = "Data/gapminder_plus.csv")


gapminder_pluss<-read_csv(file = "Data/gapminder_plus.csv")

gapminder_pluss %>% 
  filter(continent=="Africa", year==2007) %>%
  mutate(babiesDeath= infantMort*pop/1000) %>% 
  filter(babiesDeath>2e6) %>% 
  select(country) %>% 
  left_join(gapminder_pluss) %>%  #joining dataset to selceted variable country
  mutate(babiesDeath= infantMort*pop/1000, gdp_bln=gdpPercap*pop/1e9, pop_mln=pop/1e6) %>% 
  select(-c(continent, babiesDeath, pop)) %>% 
  gather(key= variables, value=, values, -c(country, year)) %>%  #key headings, #value, values #last:drop values you want in the new columns (-)c()
  ggplot() + 
  geom_text(data=. %>% filter(year==2007) %>% group_by(variables) %>% 
              mutate(max_value=max(values)) %>% 
                       filter(values==max_value),
                     aes(x=year, y=values, label=country, color=country), hjust="right", vjust=0.6)+
  geom_line(mapping=aes(x=year, y=values, color=country))+
  facet_wrap(~variables, scales= "free_y")+
  labs(y = NULL,
       x= "Year",
       title="Key parameters for selected African countries",
       subtitle= "with over 2 min baby death in 2007",
       caption= "Fertility")+
    theme_bw()+
    theme(legend.position="none")
  
#inner_join match only datavvariables 
#full_join all data but varibles my be missing NA
#left_join retaining all data that have been modified (x) and new data are Y (most common used)
#right_join other way around
# . is a place holder for all the data above, says where the data to drop



