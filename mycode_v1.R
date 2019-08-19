rm(list=ls())
library(glmnet)
library(coefplot)
library(formattable)

pretty_green = read.csv('C:/Ananya/Masters/Summer/Predictive Modelling/Scott Take Home/Green Buildings/rentColdDays.csv')
colnames(pretty_green)
pretty_green$green_rating_fac <- as.factor(pretty_green$green_rating)

ggplot(data = pretty_green, aes(x = pretty_green$cd_total_07_quantiles, y = pretty_green$Rent_cooling, fill = pretty_green$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+ 
  labs(title="Rent of Green Status Buildings by no. of cooling days", 
       y="Rent per SqFt ($)",
       x = "No. of cooling days",
       fill="Green Status")
# green buildings have a higher rent across all kinds of cooling days (barring 1000-2000 days),
# so we can conclude that no of cooling days does not contribute to the rent of the building

ggplot(data = pretty_green, aes(x = pretty_green$hd_total07_quantiles, 
                                y = pretty_green$Rent_heating, 
                                fill = pretty_green$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title="Rent of Green Status Buildings by no. of heating days", 
       y="Rent per SqFt ($)",
       x = "No. of Heating days",
       fill="Green Status")
# From the graph we see that green_rated buildings charga a higher rent when:
# 1. The no of heating degree days are low, implying that the region is hot, or warm enough to not require heating.
# 2. The no of heating degree days are high, implying that there is a need for heating on most days. This implies that the 
# savings in energy costs are higher than the rent of a green building
# In places with heating degree days between 2000-5000, green buildings are in fact commanding a lower rent
# One reason for this could be that for places which are moderately cold, the savings in energy costs are higher 
# than the rent of a green building. We need to check with the total no of degree days to further understand the correlation
# betweem these phenomenon

ggplot(data = pretty_green, aes(x = pretty_green$total_dd_07_quantiles, 
                                y = pretty_green$Rent_total, 
                                fill = pretty_green$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title="Rent of Green Status Buildings by no. of degree days", 
       y="Rent per SqFt ($)",
       x = "No. of degree",
       fill="Green Status")
# We cann now confidently say that (a) in places that have less than 5000 degree days (moderate temperatures) and (b) places that 
# have more than 8000 degree days (extreme temperatures), green buildings charge a higher rent. For (b), we can hypothesise
# the reason for higher rent is the savings in energy costs. We will not hold the degree days constant and check if there
# is another feature that can attributed to the variation in rent.

# Feature 1: class of the building
# It is possible that because buildings with degree days > 2000 are better built, and hence charge a premium rent

#holding degree days > 8000 constant
l <- pretty_green[which(pretty_green$total_dd_07_quantiles == ">8000"),]
ggplot(data = l, aes(x = l$class, fill = l$green_rating_fac)) +
  geom_bar(aes(col = l$green_rating_fac), position = position_dodge())
# We see that the number of buildings is too low forclass_b and class_c, to understand if the green premium
# significant enough. So from the next graph, we take a look at only the class A buildings
ggplot(data = l, aes(x = l$class,  y = l$Rent_total,
                     fill = l$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title="Rent of Green Status Buildings by class of buildings , for >8000 degree days", 
       y="Rent per SqFt ($)",
       x = "Class",
       fill="Green Status")
# This is telling us for sure that there is a higher rent asscociated with a green building, if it is class_a and in an 
# area with degree days > 8000

#holding degree days between 2000-3000 constant
l <- pretty_green[which(pretty_green$total_dd_07_quantiles == "2000-3000"),]
ggplot(data = l, aes(x = l$class, fill = l$green_rating_fac)) +
  geom_bar(aes(col = l$green_rating_fac), position = position_dodge())
# Once again, we see that the number of buildings is too low class_c, to understand if the green premium
# significant enough. So from the next graph, we take a look at only the class A and B buildings
ggplot(data = l, aes(x = l$class,  y = l$Rent_total,
                                fill = l$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title="Rent of Green Status Buildings by class of buildings for degree days between 2000-3000", 
       y="Rent per SqFt ($)",
       x = "Class",
       fill="Green Status")
# This too assures us that there is a green premium for areas with 2000-3000 degree days

#holding degree days between 3000-4000 constant
l <- pretty_green[which(pretty_green$total_dd_07_quantiles == "3000-4000"),]
# by number of buildings in each class
ggplot(data = l, aes(x = l$class, fill = l$green_rating_fac)) +
  geom_bar(aes(col = l$green_rating_fac), position = position_dodge())
# mean rent vs green_rating
ggplot(data = l, aes(x = l$class,  y = l$Rent_total,
                     fill = l$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title="Rent of Green Status Buildings by class of buildings for degree days between 3000-4000", 
       y="Rent per SqFt ($)",
       x = "Class",
       fill="Green Status")

#holding degree days between 4000-5000 constant
l <- pretty_green[which(pretty_green$total_dd_07_quantiles == "4000-5000"),]
# by number of buildings in each class
ggplot(data = l, aes(x = l$class, fill = l$green_rating_fac)) +
  geom_bar(aes(col = l$green_rating_fac), position = position_dodge())
# mean rent vs green_rating
ggplot(data = l, aes(x = l$class,  y = l$Rent_total,
                     fill = l$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title="Rent of Green Status Buildings by class of buildings for degree days between 4000-5000", 
       y="Rent per SqFt ($)",
       x = "Class",
       fill="Green Status")

# However, we see that the no. of degree days is correlated with the green premium on the rent. In all areas with greater than
# > 2000 degree days, we see that across the class of the buildings (wherever significant/relevant), 
# the rent is higher for green_rated buildings. Hence we should invest in a green building if they are going to be 
# built in areas with a high number of degree days (>2000), ie areas with extremes of temperature.
