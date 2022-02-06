library(tidyverse)
covid_variant <-read.csv("covid-variants.csv",header = TRUE)
library(ISLR)

#create EDA report####
DataExplorer::create_report(covid_variant)

########how to change header name of column##########
names(covid_variant)[names(covid_variant) =="num_sequences_total"] <- "total number of cases"

names(covid_variant)[names(covid_variant) =="num_sequences"] <- "number of case per kind"
names(covid_variant)[names(covid_variant) =="perc_sequences"] <- "percentage of cases"


###########################################################################
      #Most apperance of cases countries via Treemap analysis#

library(treemapify)
library(breakDown)
library(treemap)
library(ggplot2)
library(dplyr)
library(data.table)
library(datasets)

tree_data_<- covid_variant%>%###2
  count(`total number of cases`)
tree_data9<- covid_variant%>%###3
  count(location)

#simple treemap####4
ggplot(tree_data9,aes(fill= location,area =n)) +
  geom_treemap() +
  labs(title ="Covid19 variant cases")
#create a treemap with title labels####5
ggplot(tree_data9, aes(fill = location,area = n, label = n, vSize =n)) +
  geom_treemap() +
  geom_treemap_text(color ='white',
                    place ='centre') +
  labs(title = "Covid19 variant cases by country") +
  labs(subtitle = "Top20 countriesby total cases") +
  theme(legend.position = "none") 


scale_fill_manual(values = palette)
###################################
Data_b<-tree_data9[1:117, ]####6

treemap(Data_b,####7
        index = "location",
        vSize = "n",
        title ="Countrywise Covid19 variant cases",
        type = "index")
Data_b
############## creating treemap#################

treemap(covid_variant ,
        index = "location",
        vSize = "total number of cases",
        title = "Covid19 variant across countries",
        type = "index")####8

##################################
########Popular variant and most effected countries##### 
#############By stacked group plot################
##########For stacked bar chart####################
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)

##############Sum or mean of numeric column by character,no.of case by location and type of variant###
x<-aggregate( num_sequences ~  location + variant, covid_variant, sum)#1(A)

sort(x$location)#1(B)
we <- x[order(x$location), ]#2

#####################$$$$$$$$$$$$$$$$$$$####################

#####Top values by numsequence(numeric)######
rm(top20)
top20 <-we %>%dplyr::arrange(-num_sequences)%>%head(50) #3


##################################################
####we will use a dataset which come with the package ggplot.
####view the data frame to see the structure.

mp <- ggplot(data = top20, aes(x = location))
mp <- mp + geom_bar(stat = "count")
mp

mp <- ggplot(data = top20, aes(x = location , fill =variant))#4
mp <- mp + geom_bar(stat = "count",position = "fill")#5
mp

mp <- ggplot(data = top20 , aes(x = location, fill ='num_sequences'))
mp <- mp + geom_bar(stat = "count",position = "dodge")
mp <- mp +ggthemes::theme_economist()
mp <- mp +theme(axis.text.x = element_text(angle = 90,hjust = 0))# to turn plot at 90degree angle
mp <- mp +labs(title = "Covid19 variants across countries")
mp
##################$$$$$$$$$$$$$$$$$$$$$$$$$######################



#####################***********######################
mp <- ggplot(top20, aes(x = location, y = num_sequences, fill =variant))#6
mp <- mp + geom_bar(stat = "identity",position = "dodge")#7
mp <- mp + theme_minimal()
mp <- mp +theme(axis.text.x = element_text(angle = 90,hjust = 0))# to turn
mp#9
mp <- mp +labs(title = "Covid19 variant across the countries 2020-21")
mp <- mp +labs(x ="location",y ="Number of cases")
mp <- mp + scale_y_continuous(name ="Number of cases",labels = scales::comma) #10
mp <- mp + coord_flip()#11
#mp <- mp +theme(legend.position = "none")
mp#12
