
#package declarations
library(ggplot2)

#Chicago bikeshare dataset assigning
chi = read.csv('chicago.csv')

#a glance through the dataset
head(chi)

#summary statistics
summary(chi)

#plotting using ggplot : dataset, aesthetics, axis labels and binwidth assigning
ggplot(data=chi)+
  geom_histogram(aes(x=Trip.Duration/60,  y=(..count..)*100/sum(..count..)),fill='gray65',col="gray40", binwidth=50)+ 
  labs(title="Chicago Bike Trip Durations",
       x="Trip Duration in Minutes",
       y="Percent of Total"
       )+
  scale_y_continuous(breaks=seq(0,100,20))+
  scale_x_continuous(breaks=seq(0,500,100))

#package declarations
library(ggplot2)
library(dplyr)
library(forcats)

#New York bikeshare dataset assigning
ny = read.csv('new_york_city.csv')

#a glance through the dataset
head(ny)

#summary statistics
summary(ny)

#Data analysis using dplyr : dataset, top n(where n=20) and descending order arrangement
ny%>%
  group_by(Start.Station) %>%
  tally() %>%
  top_n(20) %>%
  arrange(desc(n)) %>%
  mutate(n=n/100,Start.Station=fct_reorder(Start.Station,n,last)) %>%
  
#plotting using ggplot : dataset, aesthetics, axis labels and grouping and arranging by maximum Start Stations

  ggplot()+
  geom_point(aes(y=Start.Station, x=n), fill='gray10', col="gray40")+
  geom_text(aes(y=Start.Station, x=n,label=round(n,1), hjust=-0.3), col="gray25", size=4.5)+
  labs(title="Top 20 most popular starting bike share locations in NYC",
       x="Popularity Measure",
       y="Stations"
)

#package declarations
library(ggplot2)
library(dplyr)
library(forcats)

#assigning the Washington bikeshare dataset
wash = read.csv('washington.csv')

#a glance through the dataset
head(wash)

#summary statistics
summary(wash)

#Data analysis using dplyr : dataset, top n(where n=20) and descending order arrangement
wash%>%
  group_by(End.Station) %>%
  tally() %>%
  top_n(20) %>%
  arrange(desc(n)) %>%
  mutate(n=n/100,End.Station=fct_reorder(End.Station,n,last)) %>%

#plotting using ggplot : dataset, aesthetics, axis labels and grouping and arranging by maximum End Stations

  ggplot()+
  geom_point(aes(y=End.Station, x=n), fill='gray10', col="gray40")+
  geom_text(aes(y=End.Station, x=n,label=round(n,1), hjust=-0.3), col="gray25", size=4.5)+
  labs(title="Top 20 most popular ending bike share locations",
       x="Popularity Measure",
       y="Stations"
)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
