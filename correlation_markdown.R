library(tidyverse)
my.data <- read_csv("https://jeroenooms.github.io/multilevel/data/chapter2/popular2.csv")

#extract relevant columns and create analytic data 

analytic.data <- select(my.data, class, extrav, popular, texp)

# don't create factors leads to not being able to say greater than or less than so we keep them as a numeric value 

#extrav column from the data set
analytic.data$extrav
analytic.data$popular

#confidence interval of the correlation 
cor.test(x=analytic.data$extrav,y=analytic.data$popular)

#i would report r=.32, 95% CI[.28,.35], p<.001

#store correlation information as a variable 

my.cor.result <- cor.test(x=analytic.data$extrav,y=analytic.data$popular)

#view
my.cor.result

#or just get the basic correlation estimate 
my.cor.result$estimate

#plotting a single correlation 

my.scatter <- qplot(x=extrav,y=popular,data=analytic.data)

print(my.scatter)

#add regression line 

my.scatter <- qplot(x=extrav,y=popular,data=analytic.data)
my.scatter <- my.scatter + geom_smooth(method = "lm", se = TRUE, color="black")
print(my.scatter)

#make se = TRUE to get a grey confidence interval and = FALSE for no confidence interval 

#calculate several correlations

#convert data set to a data frame 

library(apaTables)
apa.cor.table(as.data.frame(analytic.data))

psych::pairs.panels(as.data.frame(analytic.data),lm=TRUE)

#SUBGROUP ANALYSIS 

analytic.data.class.is.5 <- filter(analytic.data,class==5)

#View(analytic.data.class.is.5) to see that we have just class 5's info 

#get the correlation for class 5

cor.test(x=analytic.data.class.is.5$extrav,y=analytic.data.class.is.5$popular)

#calculate correlations for several subgroups - need tidyverse package 

analytic.data.grouped.by.class <- group_by(analytic.data, class)

#correlation for each group 

cor.by.group <- summarise(analytic.data.grouped.by.class, cor.test(x=extrav,y=popular)$estimate)

print(cor.by.group)

#print all the rows

print(as.data.frame(cor.by.group))

#plotting correlations for several groups 

analytic.data.groups.15.to.30 <- filter(analytic.data.grouped.by.class, class>=15 & class<=30)

#create scatter plot for this data 

my.scatter <- qplot(x=extrav,y=popular,data=analytic.data.groups.15.to.30)
my.scatter <- my.scatter + facet_wrap(~class)

#add regression line 

my.scatter <- my.scatter + geom_smooth(method = "lm", se=TRUE, color="black")

print(my.scatter)

#Save graph 

ggsave("groupScatterPlotWithCI.pdf",plot=my.scatter,width=6,height=6)
