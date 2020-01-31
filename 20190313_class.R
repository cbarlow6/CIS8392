library(tidyverse)

hello_world <- function() { # this particular function takes no input 
  print("Hello world!")
  # This function has no return statement; return nothing.
}
hello_world() 

n = 1e8

vec1 = c() 
vec3 = (1:n)^2 # vectorized operation
all.equal(vec1, vec2, vec3)

View(iris)
iris[ , 5]
mean_vec = vector('numeric', 5)
mean_vec
seq_len(5)
for(i in seq_len(5)){
  if(is.numeric(iris[ ,i])){
    mean_vec[i] <- mean(iris[ , i])
} else{
    mean_vec[i] <- NA
}
}  

map(c(3,4,5), sqrt)


for (col in 1:5){
    print (mean(iris[ ,col]))
}
install.packages("repurrrsive")
library(repurrrsive)
repurrrsive::gh_users
str(gh_users, max.level = 1)
?seq_along

n = length(gh_users)
followers <- vector("numeric", n)
for(i in seq_along(gh_users)) {
  followers[i] <- gh_users[[i]]$followers
}
followers


map_dbl(sw_people, function(x) {
  bmi = as.numeric(x$mass) / ((as.numeric(x$height)/100) ^2)
  return (round(bmi, 2))
  })



#bmi_df <- data.frame(sw_people, c("name", "gender", "height", "mass"))
map_df(gh_users,
       magrittr::extract, # run the extract function in the magrittr package
       c("name", "followers", "following", "public_repos") #other inputs for extract()
)

mass = map_dbl(sw_people, ~ as.numeric(.$mass))

followers = map_dbl(gh_users, ~.$followers) 
followers

map_dbl(iris, mean)

class(gh_users[[1]]$created_at)

as.Date(gh_users[[1]]$created_at) -
  as.Date(gh_users[[1]]$updated_at)

map_dbl(gh_users, ~ 
          as.Date(.$updated_at) -
          as.Date(.$created_at) )

?rnorm
vec <- rnorm(100)
hist(vec)

mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)

?map

library(ggplot2) 
plots <- mtcars %>%
  split(.$cyl) %>%
  map(~ggplot(., aes(mpg, wt)) + geom_point()) 
paths <- stringr::str_c(names(plots), ".png")
pwalk(list(paths, plots), ggsave) #save to the working directory

library(foreach) 
library(doParallel) 
parallel::detectCores()


install.packages(c("httr", "devtools")) 
devtools::install_github("r-lib/gh") #https://github.com/r-lib/gh
library(httr) 
library(gh) 

response = GET(url = "https://api.github.com/repos/tidyverse/ggplot2")
class(response)
status_code(response)
response_content = content(response)


str(response_content,
    max.level = 1, nchar.max=17)

str(response_content,
    max.level = 1, nchar.max=17)

response_content

#https://github.com/hadley
"https://api.github.com/users/hadley"
"https://api.github.com/users/hadley/repos"
"https://api.github.com/users/hadley/followers"
 repos = GET(url = "https://api.github.com/users/hadley/repos")
 repos_content = content(repos)
 repos_content
response = GET(url = "https://api.github.com/repos/tidyverse/ggplot2")

hadley <-gh("/users/hadley")

hadley_followers <- gh("/users/hadley/followers", .limit = 100)
length(hadley_followers)

google_members <- gh("/orgs/google/members", .limit = 100)
length(google_members)

df_google_members = map_df(
  google_members, magrittr::extract, names(google_members[[1]]))
df_google_members