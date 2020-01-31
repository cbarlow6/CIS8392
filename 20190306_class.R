install.packages("rmarkdown")
install.packages("tidyverse")
install.packages("ggthemes") # install the package on your machine first 
library(ggthemes) # load it into R before you can use it
library(tidyverse)


list.files()
f_name <- "Catresa"
l_name <- "Barlow"

length(f_name)
first <- nchar(f_name)
last <- nchar(l_name)
full_name <- paste(f_name, l_name, sep = "_")
mul <-  first * last
div <- first / last
first > last
1:10

vec1 <- 1:4
vec2 <- 3:4

vec1 - vec2

4 %in% vec1

vec1 <- c(1, 2, 3, 4, 5)
logic_vec <- c(T, T, F)
vec1[logic_vec]

list1 <- list("Alice", "Bob", "Claire", "Daniel")
list1[[2]]

x <- list(
  name = c("Alice", "Bob", "Claire", "Daniel"),
  female = c(T, F, T, F),
  age = c(20, 25, 30,35))

x$name[2]
x[[1]][2]
name <- c("Alice", "Bob", "Claire", "Daniel")
female <- c(T, F, T, F)
age <- c(20, 25, 30, 35)
df <- data.frame(name, female, age,
                 stringsAsFactors=F)
rownames(df) <- c("row1", "row2", "row3", "row4")
mean(df$age)
df[df$name == "Daniel", "age" ]
library("jsonlite")
prices <- read.csv("HousePrices.csv")
json_content = toJSON(prices) 
df2=fromJSON(json_content)

result <- stream_in(file("https://api.github.com/repos/tidyverse/ggplot2"))
result <- stream_in(file("https://api.github.com/repos/tidyverse/ggplot2"))
class(result)
str(result)
summary(result)

mpg

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2) # y axis ~ x axis

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))


ggplot(data = prices,
       mapping = aes(x = lotsize, y = price,
       color=aircon, size=bedrooms)) +
  geom_point() + 
  geom_smooth() +
  ggtitle("House Price Data") +
  facet_wrap(~ driveway) +
  theme_gdocs() + 
  scale_color_gdocs()
