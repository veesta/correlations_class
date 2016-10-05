library(tidyverse)

#load data from online source 
my.data <- read_csv("https://jeroenooms.github.io/multilevel/data/chapter2/popular2.csv")

#may want to save data - see handout 
#write_csv(my.data,path="hox_data_chapter2.csv")



#extract relevant 4 columns and create analytic data 

analytic.data <- select(my.data, class, extrav, popular, texp)

# don't create factors leads to not being able to say greater than or less than so we keep them as a numeric value 
#if it is categorical, need to make it numerical 

#correlation between 2 variables - extraversion and popularity 
analytic.data$extrav
analytic.data$popular

#using what we defined above, calculate the correlation between the 2 with confidence intervals - below
cor.test(x=analytic.data$extrav,y=analytic.data$popular)

#i would report r=.32, 95% CI[.28,.35], p<.001

#UNDERSTANDING HOW R WORKS --- 

#We can store correlation information as a variable to use in subsequent analyses 

my.cor.result <- cor.test(x=analytic.data$extrav,y=analytic.data$popular)

#view
my.cor.result

#or just get the basic correlation estimate 
my.cor.result$estimate

#plotting a single correlation

my.scatter <- qplot(x=extrav,y=popular,data=analytic.data)

print(my.scatter)
#note: not an apa graph, but make it that way with other handouts 

#add regression line with geom_smooth, make it linear with 'lm', get a regression line with se=TRUE, don't want one, se=FALSE
#help = ?geom_smooth

my.scatter <- my.scatter + geom_smooth(method = "lm", se = TRUE, color="black")
print(my.scatter)

#CALCULATE SEVERAL CORRELATIONS ----

#convert data set to a data frame 

library(apaTables)
apa.cor.table(as.data.frame(analytic.data))

#View multiple scatter plots for all our data with the pairs.panels command from the psych package
#lm=TRUE makes sure we get a regression line 

psych::pairs.panels(as.data.frame(analytic.data),lm=TRUE)

#SUBGROUP ANALYSIS - correlation for a single subgroup, i.e. females only ------------
#need tidyverse
#can swap out class with another variable

analytic.data.class.is.5 <- filter(analytic.data,class==5)

#View(analytic.data.class.is.5) to see that we have just class 5's info 

#get the correlation for class 5 - extrav & popularity 

cor.test(x=analytic.data.class.is.5$extrav,y=analytic.data.class.is.5$popular)

#calculate correlations for several subgroups - need tidyverse package ---------------

#group it by class
analytic.data.grouped.by.class <- group_by(analytic.data, class)

#correlation for each group 

cor.by.group <- summarise(analytic.data.grouped.by.class, cor.test(x=extrav,y=popular)$estimate)

#print to make sure it got all 100 
print(cor.by.group)

#print all the rows - not with a short form of (with 90 more rows)
print(as.data.frame(cor.by.group))

#PLOTTING CORRELATIONS FOR SEVERAL GROUPS ---------------

#pick the subtest of groups you want, 15 to 30 using filter - need tidyverse
analytic.data.groups.15.to.30 <- filter(analytic.data.grouped.by.class, class>=15 & class<=30)

#create scatter plot for this data 

my.scatter <- qplot(x=extrav,y=popular,data=analytic.data.groups.15.to.30)
my.scatter <- my.scatter + facet_wrap(~class)
print(my.scatter)

#add regression line 

my.scatter <- my.scatter + geom_smooth(method = "lm", se=TRUE, color="black")

print(my.scatter)

#Save graph 

ggsave("groupScatterPlotWithCI.pdf",plot=my.scatter,width=6,height=6)
