library(tidyverse)
covid_variant <-read.csv("covid-variants.csv",header = TRUE)
library(ISLR)

#EDA report####
DataExplorer::create_report(covid_variant)

#################
names(covid_variant)[names(covid_variant) =="num_sequences_total"] <- "total number of cases"

names(covid_variant)[names(covid_variant) =="num_sequences"] <- "number of case per kind"
names(covid_variant)[names(covid_variant) =="perc_sequences"] <- "percentage of cases"


###########################################################################
      

library(treemapify)
library(breakDown)
library(treemap)
library(ggplot2)
library(dplyr)
library(data.table)
library(datasets)

tree_data_<- covid_variant%>%
  count(`total number of cases`)
tree_data9<- covid_variant%>%
  count(location)

#simple treemap####4
ggplot(tree_data9,aes(fill= location,area =n)) +
  geom_treemap() +
  labs(title ="Covid19 variant cases")
# treemap with title labels####5
ggplot(tree_data9, aes(fill = location,area = n, label = n, vSize =n)) +
  geom_treemap() +
  geom_treemap_text(color ='white',
                    place ='centre') +
  labs(title = "Covid19 variant cases by country") +
  labs(subtitle = "Top20 countriesby total cases") +
  theme(legend.position = "none") 


scale_fill_manual(values = palette)
###################################
Data_b<-tree_data9[1:117, ]

treemap(Data_b,
        index = "location",
        vSize = "n",
        title ="Countrywise Covid19 variant cases",
        type = "index")
Data_b
###############################

treemap(covid_variant ,
        index = "location",
        vSize = "total number of cases",
        title = "Covid19 variant across countries",
        type = "index")

##################################
#############################
##############################
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)

########################################
x<-aggregate( num_sequences ~  location + variant, covid_variant, sum)

sort(x$location)
we <- x[order(x$location), ]

#####################$$$$$$$$$$$$$$$$$$$####################

#####Top values by numsequence(numeric)######
rm(top20)
top20 <-we %>%dplyr::arrange(-num_sequences)%>%head(50) 


##################################################
#### ggplot###################


mp <- ggplot(data = top20, aes(x = location))
mp <- mp + geom_bar(stat = "count")
mp

mp <- ggplot(data = top20, aes(x = location , fill =variant))
mp <- mp + geom_bar(stat = "count",position = "fill")
mp

mp <- ggplot(data = top20 , aes(x = location, fill ='num_sequences'))
mp <- mp + geom_bar(stat = "count",position = "dodge")
mp <- mp +ggthemes::theme_economist()
mp <- mp +theme(axis.text.x = element_text(angle = 90,hjust = 0))# to turn plot at 90degree angle
mp <- mp +labs(title = "Covid19 variants across countries")
mp
##################$$$$$$$$$$$$$$$$$$$$$$$$$######################



#####################***********######################
mp <- ggplot(top20, aes(x = location, y = num_sequences, fill =variant))
mp <- mp + geom_bar(stat = "identity",position = "dodge")
mp <- mp + theme_minimal()
mp <- mp +theme(axis.text.x = element_text(angle = 90,hjust = 0))# to turn
mp#9
mp <- mp +labs(title = "Covid19 variant across the countries 2020-21")
mp <- mp +labs(x ="location",y ="Number of cases")
mp <- mp + scale_y_continuous(name ="Number of cases",labels = scales::comma) 
mp <- mp + coord_flip()
#mp <- mp +theme(legend.position = "none")
mp
