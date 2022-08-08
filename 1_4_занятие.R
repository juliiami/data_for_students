##### Примеры из методички

### lattice

library(lattice)
attach(mtcars)

# histogram 
graph1 <- histogram(mpg, breaks = 10, 
          main="Histogram",
          xlab="Miles per Gallon")

# kernel density plot
graph2 <-densityplot(~mpg,
            main="Density Plot",
            xlab="Miles per Gallon")

plot(graph1, position=c(0.4, 0, 1, 1))
plot(graph2, position=c(0, 0, 0.4, 1), newpage=FALSE)

# bar chart
aggr <- aggregate(mtcars,by = list(cyl), mean)

barplot(mpg~cyl, data=aggr, main="Bar Chart",
        xlab="Miles per Gallon")

# create factors with value labels
gear.f<-factor(gear,levels=c(3,4,5),
               labels=c("3gears","4gears","5gears"))
cyl.f <-factor(cyl,levels=c(4,6,8),
               labels=c("4cyl","6cyl","8cyl"))


# kernel density plots by factor level
densityplot(~mpg|cyl.f,
            main="Density Plot by Number of Cylinders",
            xlab="Miles per Gallon")

# kernel density plots by factor level (alternate layout)
densityplot(~mpg|cyl.f,
            main="Density Plot by Number of Cylinders",
            xlab="Miles per Gallon",
            layout=c(1,3))


# boxplots for each combination of two factors
bwplot(cyl.f~mpg|gear.f,
       ylab="Cylinders", xlab="Miles per Gallon",
       main="Mileage by Cylinders and Gears",
       layout=c(1,3))
       

# scatterplots for each combination of two factors
xyplot(mpg~wt|cyl.f*gear.f,
              main="Scatterplots by Cylinders and Gears",
              ylab="Miles per Gallon", xlab="Car Weight")
       

# 3d scatterplot by factor level
cloud(mpg~wt*qsec|cyl.f,
             main="3D Scatterplot by Cylinders")
       

# dotplot for each combination of two factors
dotplot(cyl.f~mpg|gear.f,
               main="Dotplot Plot by Number of Gears and Cylinders",
               xlab="Miles Per Gallon")
       

# scatterplot matrix
splom(mtcars[c(1,3,4,5,6)],
             main="MTCARS Data")


  
### ggplot2

library(ggplot2)
library(dplyr)

ggplot(data = mtcars,                          # Data Layer
       aes(x = hp, y = mpg, col = disp)) +     # Aesthetic Layer
       geom_point()                            # Geometric layer

# Adding size
ggplot(data = mtcars, 
       aes(x = hp, y = mpg, size = disp)) + geom_point()

# Adding color and shape
ggplot(data = mtcars, 
       aes(x = hp, y = mpg, col = factor(cyl), shape = factor(am))) +
       geom_point()

# Histogram plot
ggplot(data = mtcars, aes(x = hp)) +
       geom_histogram(binwidth = 5)



# Facet Layer
p <- ggplot(data = mtcars, 
            aes(x = hp, y = mpg, 
                shape = factor(cyl))) + geom_point()

# Separate rows according to transmission type
p + facet_grid(am ~ .)

# Separate columns according to cylinders
p + facet_grid(. ~ cyl)


# Statistics layer
ggplot(data = mtcars, aes(x = hp, y = mpg)) + 
  geom_point() + 
  stat_smooth(method = lm, col = "red")


# Add coord_cartesian() to proper zoom in
ggplot(data = mtcars, aes(x = wt, y = hp, col = am)) +
  geom_point() + 
  coord_cartesian(xlim = c(3, 6))



ggplot(data = mtcars, aes(x = hp, y = mpg)) +
  geom_point() + facet_grid(am ~ cyl) + 
  theme_dark()



### plotly

library(plotly)

# scatterplot
plot_ly(data = mtcars, x = ~hp, y = ~mpg, type='scatter', mode = 'markers')


# color and size
plot_ly(data=mtcars, x = ~hp, y = ~mpg, type='scatter', mode = 'markers',
               color = ~mpg, size = ~mpg)

# frame
plot_ly(data = mtcars, x = ~wt, y = ~mpg, type='scatter', mode = 'markers',
        frame = ~cyl)

# histogram

plot_ly(mtcars, x = ~mpg) %>% add_histogram()

# boxplot

plot_ly(mtcars, x = ~factor(am), y = ~mpg) %>% add_boxplot()
 
# violin plot

plot_ly(mtcars, x = ~factor(am), y = ~mpg) %>% add_trace(type = "violin")

# 3d chart

plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>% add_markers(color = ~cyl)




##### Самостоятельная работа

library(lattice)
library(ggplot2)
library(plotly)

prices <- read.table('https://raw.githubusercontent.com/juliiami/data_for_students/main/housing.csv', sep=',', header=TRUE)

attach(prices)

# maps

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)



fig <- plot_geo(prices, lat = ~latitude , lon = ~longitude )
fig <- fig %>% add_markers(
  text = ~paste(paste("Housing median age:", housing_median_age) , paste("Prices:", median_house_value), sep = "<br />"),
  color = ~median_house_value , symbol = I("square"), size = I(8), hoverinfo = "text"
)
fig <- fig %>% colorbar(title = "Prices")
fig <- fig %>% layout(
  title = 'House prices', geo = g
)

fig


head(prices)


