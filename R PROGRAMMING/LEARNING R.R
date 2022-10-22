5+6
?mean
 a<-6
 b<-5
 sum(a,b)
 ages <- c(5,6)
 ages
 sum(ages)
 names<-c("John",'tracy')
 friends <-data.frame(names,ages)
 View(friends)
 str(friends)
 friends$ages
 sum(friends$ages)
 friends$names
 friends[1,1 ]# rows,coloums
 friends[1,]
 friends[ ,1]
 #BUILT IN DATA SETS TO PRACTICE WITH
 data()
 data(package = .packages(all.available = TRUE))
 View(AirPassengers)
 ?diamonds
 View(diamonds)
 ?AirPassengers
 View(AirPassengers)
 (AirPassengers)
 #INSTALLING AND USING PACKAGES
 install.packages("tidyverse")
 (starwars)
 View(starwars)
 library(tidyverse)
 
 starwars %>% 
   filter(height > 150 & mass < 200) %>% 
   mutate(height_in_meters=height/100) %>% 
   select(height_in_meters, mass) %>% 
   arrange(mass) %>% 
   #View()
 plot()
 
 #EXPLORING YOUR DATA SET
 #DATA
 
 
 
 View(msleep)
 #to overview the data 
 glimpse(msleep)
 #the first 6 rows
 head(msleep)
 #gives you the type of variable 
 class(msleep$name)
 #length/no of entries of the data frame
 length(msleep)
 #length of entries for this particular variable
 length(msleep$name)
 #returns the names of the variables
 names(msleep)
 #returns the unique variables under that coloumn
 unique(msleep$vore)
 #creating a variable that will return cases that are NOT complete
 #thus will return every row that has missing data
  missing <- !complete.cases(msleep)
  msleep[missing,]
 #creating a variable that will return cases that are complete
 #will return every row with completed data 
 full<- complete.cases(msleep)
 msleep[full,]
 
 
 ####CLEANING YOUR DATA 
 #SELECT VARIABLES
 
 
 
starwars %>% 
  select(name,height,mass)

#select variables 1 to 3
starwars %>% 
  select(1:3)

#select those variables that end with color
starwars %>% 
  select(ends_with("color")) %>% 
  View()

#changing variable order
#original -name height mass....
starwars %>% 
  select(name,mass,height,everything()) %>% 
  View()
 
#Changing the variable name
starwars %>% 
  rename("characters"="name") %>% 
  #head()
 View()

#Changing variable type
class(starwars$hair_color)

starwars$hair_color <-as.factor(starwars$hair_color)
class(starwars$hair_color)

starwars %>% 
  mutate(hair_color=as.character(hair_color)) %>% 
  glimpse()

#changing factor levels
df<-starwars
df$sex<-as.factor(df$sex)
levels(df$sex)

df<-df %>% 
  mutate(sex=factor(sex,    
                    levels = c("male","female","hermaphroditic","none")))
levels(df$sex)
 
##FILTER ROWS
starwars %>% 
  select(mass,sex) %>% 
  filter(mass<55 &
          sex=="male")

#RECODE DATA
starwars %>% 
  select(sex) %>% 
  mutate(sex=recode(sex,
                    "male"="man",
                    "female"="woman"))

#DEALING WITH MISSING DATA
#if there is missing data the function mean will return NA
#however adding(,not available.remove=true) a mean shall be returned
mean(starwars$height,na.rm=TRUE)

#DEALING WITH DUPLICATES
Names <- c("tracy","timothy","arnold","allan","tracy")
Age<-c(20,13,19,26,20)
family<-data.frame(Names,Age)
family

family %>% 
  distinct()

distinct(family)

#manipulate
#create or change a variable
library(tidyverse)

starwars %>% 
  mutate(height_in_meters=height/100) %>% 
  select(name,height,height_in_meters) %>% 
  mutate(tallness=
           if_else(height_in_meters<1,
                   "short",
                   "tall"))

###RESHAPE DATA WITH PIVOT WIDER
library(gapminder)
View(gapminder)
#first create a data set
data <-select(gapminder,country,year,lifeExp)
View(data)
 
library(tidyverse)
wide_data<- data %>% 
  pivot_wider(names_from=year,values_from=lifeExp) %>% 
  View(wide_data)

 #this code simply shows your data in different format and in a WIDER VIEW

#Reshape data with pivot longer
long_data<-wide_data %>% 
  pivot_longer(2:13,
               names_to="year",
               values_to="lifeExp")
View(long_data)



###describing your data
View(msleep)
#range/spread
min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)#INTERQUARTILE RANGE 

#centrality 
mean(msleep$awake)
median(msleep$awake)


#variance
var(msleep$awake)

summary(msleep$awake)

msleep %>% 
  select(awake,sleep_total) %>% 
  summary()

#SUMMARIZE YOUR DATA
msleep %>% 
  drop_na(vore) %>% #dropping missing
  group_by(vore) %>% 
  summarise(Lower=min(sleep_total),
            Average=mean(sleep_total),
            Upper=max(sleep_total),
            Difference=
              max(sleep_total)-min(sleep_total)) %>% 
  arrange(Average) %>% 
  View()

#CREATE TABLES

table(msleep$vore)

msleep %>% 
  select(vore,order) %>% 
  filter(order %in% c("Rodentia","Primates")) %>% 
  table()
##VISUALIZING DATA

plot(pressure)

#THE GRAMMAR OF GRAPHICS
#data
#mapping
#geometry

#bar plots
library(tidyverse)
ggplot(data=starwars,
       mapping=aes(x=gender))+
  geom_bar()
#histograms
starwars %>% 
  drop_na(height) %>%
  ggplot(aes(height))+
  geom_histogram()
#box plots
starwars %>% 
  drop_na(height) %>%
  ggplot(aes(height))+geom_boxplot(fill="steelblue")+
  theme_bw()+
  labs(title ="BOX PLOT OF HEIGHT",
       x="height of characters")
#Density plots
starwars %>% 
  drop_na(height) %>% 
  filter(sex %in% c("male","female")) %>% 
  ggplot(aes(height,color=sex,fill=sex))+
  geom_density(alpha=0.2)+theme_bw()

  
  #Scatter plots
starwars %>% 
  filter(mass<200) %>% 
  ggplot(aes(height,mass,color=sex))+
  geom_point(size=5,alpha=0.5)+
  theme_bw()
labs(title="height and mass by sex")
 
 #smoothed model
starwars %>%
  filter(mass<200) %>% 
  #filter(sex %in% c("male","female")) %>% 
  ggplot(aes(height,mass,color=sex))+
  geom_point(size=3,alpha=0.8)+
  geom_smooth()+
  facet_wrap(~sex)+
  theme_bw()+
  labs(title="height and mass by sex")


#ANALYZING THE DATA 
#HYPOTHESIS TESTING 
#T-TEST

library(gapminder)
library(tidyverse)
gapminder %>% 
  arrange(-country)
View(gapminder)
t_test_plot