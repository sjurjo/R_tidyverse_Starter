# loading the tidyverse package

library("tidyverse")

gapminder<-read_csv(file = "Data/gapminder-FiveYearData.csv")

gapminder

ggplot(data=gapminder)+
  geom_point(mapping=aes(x= gdpPercap, y= lifeExp))


ggplot(data=gapminder)+
  geom_jitter(mapping=aes(x= gdpPercap, y= lifeExp, color=continent))

ggplot(data=gapminder)+
  geom_jitter(mapping=aes(x= gdpPercap, y= lifeExp, color=continent, size= pop))
#three variables in a plot



ggplot(data=gapminder)+
  geom_point(mapping=aes(x= log(gdpPercap), y= lifeExp), alpha=0.1, size =2, color="blue")
#want to affect aes function put in to the parenthesis, out side parenthesis affect whole ggplot


ggplot(data=gapminder)+
  geom_line(mapping=aes(x= year, y= lifeExp, group = country, color= continent))


ggplot(data=gapminder)+
  geom_boxplot(mapping=aes(x= continent, y= lifeExp))

#out put 2 different layers stacked
ggplot(data=gapminder)+
  geom_jitter(mapping=aes(x= continent, y= lifeExp, color=continent))+ #specifying each point
  geom_boxplot(mapping=aes(x= continent, y= lifeExp, color=continent)) #quartiles

ggplot(data=gapminder)+
  geom_boxplot(mapping=aes(x= continent, y= lifeExp, color=continent))+
  geom_jitter(mapping=aes(x= continent, y= lifeExp, color=continent))


ggplot(data=gapminder,
       mapping = aes(x= continent, y= lifeExp, color=continent))+
  geom_jitter()+
  geom_boxplot()

ggplot(data=gapminder,
       mapping = aes(x= log(gdpPercap), y= lifeExp, color=continent))+
  geom_jitter(alpha=0.1)+ #transparent 
  geom_smooth(method="lm")

#jitter are spreading the points slighty
#change in a layer put it in the belonging parenthesis

ggplot(data=gapminder,
       mapping = aes(x= log(gdpPercap), y= lifeExp))+
  geom_jitter(mapping=aes(color=continent),alpha=0.1)+ #One specific linear model at put it in a layer but refer to mapping
  geom_smooth(method="lm")


#ggplot experiment       
ggplot(data=gapminder)+
  geom_boxplot(mapping=aes(x=as.factor(year), y=lifeExp))

ggplot(data=gapminder)+
  geom_boxplot(mapping=aes(x=as.factor(year), y=log(gdpPercap)))

ggplot(data=gapminder)+
  geom_density2d(mapping=aes(x=lifeExp, y=log(gdpPercap)))



ggplot(data=gapminder, mapping=aes(x=gdpPercap, y=lifeExp))+
  geom_point()+
  geom_smooth()+
  scale_x_log10()+
  facet_wrap(~ continent) #~function

#to get a linear model in the facet by year or continent
ggplot(data=gapminder, mapping=aes(x=gdpPercap, y=lifeExp))+
  geom_point()+
  geom_smooth(method= "lm")+
  scale_x_log10()+
  facet_wrap(~ continent) #~function, facet split up data in wanted out put



ggplot(data=filter(gapminder, year== 2007))+ #filter, want one special year
  geom_bar(mapping =aes(x= continent), stat= "count") #just count (default), subsitute to Y variable not specified

filter(gapminder, year== 2007, continent=="Oceania")

ggplot(data=filter(gapminder, year== 2007, continent== "Asia"))+ #filter year and continent i dataset
  geom_bar(mapping =aes(x= country, y=pop), stat= "identity") #identity means give raw dataset

ggplot(data=filter(gapminder, year== 2007, continent== "Asia"))+ #filter year and continent i dataset
  geom_col(mapping =aes(x= country, y=pop))+ #geom_col, avoiding stat function
  coord_flip() #flip coordinates


ggplot(data=gapminder, mapping=aes(x=gdpPercap, y=lifeExp, color= continent, size= pop/10^6))+ #size by millions
  geom_point()+
  scale_x_log10()+
  facet_wrap(~ year)+
          labs(title="Life expectancy vs GDP per capita over time",
               subtitle="In the last 50 years, life expectancy has improved in most countries of the world",
               caption="Source:Gapminder foundation, gapminder.com",
               x="GDP per capita, in 000 USD",
               y="Life expectancy in years",
               color= "Continent",
               size= "Populations in millions people")


#remember the last plot that is printet, saved in the working area. Specify scale etc.
ggsave("my_fancy_plot.png")

