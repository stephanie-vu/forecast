#Forecasting
#Stephanie Vu

#install packages (fpp3)

library(fpp3)

#--------Part 1: Time-series line model----------------

#Data preparation
gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population) %>%
  select(Year, Country, GDP, Population, GDP_per_capita)

gdppc

#Data visualization for US GDP_per_capita
gdppc %>%
  filter(Country == "United States") %>%
  autoplot(GDP_per_capita) +
  labs(title = "GDP per capita for United States", y = "$US")

#Start forecasting
fit <- gdppc %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend())) #<--- the model func trains models for data
fit 

#Forecast 5 years for GDP_per_capita
fc <- fit %>% forecast(h = "5 years")

fc %>%
  filter(Country == "United States") %>%
  autoplot(gdppc) +
  labs(title = "5 years forecast GDP per capita for United States", y = "$US")

#print and present forecast number in a table

filter(fc, Country == "United States")

write.csv(filter(fc, Country == "United States"), 
          "C:\\STEPHY\\BACKUP\\Forecasting\\GDP.csv")

#Forecast 5 years for US population 

population <- global_economy %>%
  select(Year, Country, GDP, Population)


population_fit <- population %>%
  model(trend_model = TSLM(Population ~ trend()))
population_fit

population_fc <- population_fit %>%
  forecast (h= "5 years")

population_fc %>%
  filter(Country == "United States") %>%
  autoplot(population) +
  labs(title = "5 years forecast of population for United States", y = "people")

filter(population_fc, Country == "United States")

write.csv(filter(population_fc, Country == "United States"), 
          "C:\\STEPHY\\BACKUP\\Forecasting\\population.csv")

#--------Part 2: Seasonal Naive Forecasting Method----------------

beer_fit <- aus_production %>%
  filter(!is.na(Beer)) %>%
  model(
    Seasonal_naive = SNAIVE(Beer)
  )

beer_fc <- beer_fit %>%
  forecast(h = "5 years")

beer_fc %>%
  autoplot(aus_production)

z <- beer_fc %>%
  hilo(level = 95) %>%
  pull(`95%`)
z$lower

#Plot
beer_fc %>%
  autoplot(aus_production, level = NULL) +
  labs(
    title = "Beer production forecast",
    y = "Megalitres"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

beer_fc

write.csv (beer_fc, "C:\\STEPHY\\BACKUP\\Forecasting\\beer.csv")


