x<- 5*6
x

is.vector(x)
length(x)

x[2] <-31 #added an other value to our vector

x[5] <-44 #added value 44 in position 5

x[0] #numeric 0 non cosisting character (error)

x<- 1:4 #including series of nr


y<-x^2 #apply to each value in the vector
y


x<-1:5
y<-3:7

x+y

z<-y[-5]

x+z #longer object recycling warning

z<-1:10

x+z

z^x

x

y<-x[-5]
y[5]<-NA
y #beaware of missing values in r

Coercion

str(c("Hello", "workshop", "participants"))

str(c(9:11, 200, x))

str(c("something", pi, 2:4, pi>3))

w<- rnorm(10)
seq_along(w)
w

which(w < 0)

w[which(w < 0)]
w[w < 0]

w[-c(2,5)]



str(list("something", pi, 2:4, pi>3)) #vector can only contain one factor

x<-list(vegetable = "cabbage",
     number = pi,
     series = 2:4,
     telling = pi>3)
str(x)

str(x$vegetable) #return as a vetor

str(x[1]) #retur as a list

str(x[[3]]) #single [] return the list, double [] off the list or $
  
x<-list(vegetable = list("cabbage", "carrot", "spinach"),
          number = list(c(pi, 0, 2.14, NA)),
          series = list(list(2:4, 3:5)),
          telling = pi>3)

str(x)

mod<-lm(lifeExp~gdpPercap, data=gapminder_pluss)
str(mod)

str(mod$df.residual)
str(mod[[8]])

#draw out an object
str(mod$qr$qr[1,1])

#group_by grouping the data set to continent
gapminder_pluss %>% 
  group_by(continent) %>% 
  summarise(mean_le=mean(lifeExp),
            min_le=min(lifeExp),
            max_le=max(lifeExp))

#what happens to the countries with the outliers
gapminder_pluss %>% 
  ggplot() + 
  geom_line(mapping=aes(x=year, y=lifeExp, color=continent, group=country)) +
  geom_smooth(mapping=aes(x=year, y=lifeExp), method="lm", color="black")+
  facet_wrap(~continent)


#fitting the model per country, looking at the residuals
by_country<- gapminder_pluss %>% 
  group_by(continent, country) %>% 
  nest() # getting a list containing another list

#picking out the list (the first here for instance)
by_country$data[[1]]


#map(list, function) #makes me apply a function to a certain list

model_by_country<- by_country %>% 
  mutate(model=purrr::map(data, ~lm(lifeExp~year, data=.x))) %>% 
  mutate(summr=map(model, broom::glance)) %>% #now we are extracting out the model column into a new column summr
  unnest(summr) #%>% #unwrapped so we explicit get the values, visulized data know
model_by_country %>% 
  arrange(r.squared) %>% 
  ggplot()+
  geom_jitter(mapping=aes(x=continent, y=r.squared))


model_by_country$summr[[1]]

by_country %>% 
  mutate(model=purrr::map(data, ~lm(lifeExp~year, data=.x))) %>% 
  mutate(summr=map(model, broom::glance)) %>%
  unnest(summr) %>%
  arrange(r.squared) %>%
  filter(r.squared<0.3) %>% #which country are poor fit to the model
  select(country) %>% 
  left_join(gapminder_pluss) %>% 
  ggplot() + 
  geom_line(mapping=aes(x=year, y=lifeExp, color=country, group=country))

#modify lifeexp dependent over gdpercapita
gapminder_pluss %>% 
  ggplot()+
  geom_line(mapping=aes(x=log(gdpPercap), y=lifeExp, color=continent, group=country))


by_country %>% 
  mutate(model=purrr::map(data, ~lm(lifeExp ~ log(gdpPercap),data=.x))) %>% 
  mutate(summr=map(model, broom::glance)) %>%
  unnest(summr) %>%
  arrange(r.squared) %>%
  filter(r.squared<0.1) %>% #which country are poor fit to the model
  select(country) %>% 
  left_join(gapminder_pluss) %>% 
  ggplot() + 
  geom_point(mapping=aes(x=log(gdpPercap), y=lifeExp, color=country))


#save methods
saveRDS(by_country, "by_country_tibble.rds")

fdsf<-readRDS("by_country_tibble.rds")

write_csv(gapminder_pluss, "gapminderplussforprofessor.csv")






