# Title     : CS3012_AssignmentFour
# Objective : Github API access request
# Created by: conor
# Created on: 01/11/2018
install.packages("jsonlite")
library(jsonlite)
install.packages("httpuv")
library(httpuv)
install.packages("httr")
library(httr)
library(dplyr)
library("purrr")


# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you
myapp <- oauth_app(appname = "API_request_hollanco",
                   key = "3bac685edb30c57644c5",
                   secret = "792daccb0cf839ecc2e7ebe59e8155a1feb5388c")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
req

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"]

# ---------------------------
# Extract data from hollanco user on GitHub
hollancoData = fromJSON("https://api.github.com/users/hollanco")
hollancoData$login
hollancoData$followers
hollancoData$name
hollancoData$email
hollancoData$following
hollancoData$public_repos

#Store information regarding the hollanco repositories within hollancoRepos
hollancoRepos = fromJSON("https://api.github.com/users/hollanco/repos")
hollancoRepos

# Information about the hollanco repositories 
hollancoRepos$name
hollancoRepos$language

# Allows for viewing of data as JSON, easier to navigate
hollancoJSON = toJSON(hollancoData, pretty = TRUE)
hollancoJSON

# The below code give specific details on the people following me

myFollowers = fromJSON("https://api.github.com/users/hollanco/followers")
myFollowers$login
length = length(myFollowers$login)
length #Number of people following me

# The below code will give more information about my repositories

repos = fromJSON("https://api.github.com/users/hollanco/repos")
repos$name
repos$created_at

# The below allows you to view the data as JSON, the way it is done in browser

myDataJSon = toJSON(myData, pretty = TRUE)
myDataJSon

#---------------------------------------------------------------------------------------------------------
#VISUALIZATIONS
#---------------------------------------------------------------------------------------------------------
#A number of different elements of the software engineering process have been explored and visualized below
#I have found my analysis extremely illuminating and has massively furthered my understanding of the software engineering process
#Relationships between users, their languages and their followers I have found particularly interesting

#Dataframe w/ info on Current Users Followers
getFollowers <- function(username)
{
  i <- 1
  x <- 1
  followersDF <- data_frame()
  while(x!=0)
  {
    followers <- GET( paste0("https://api.github.com/users/", username, "/followers?per_page=100&page=", i),gtoken)
    followersContent <- content(followers)
    currentFollowersDF <- lapply(followersContent, function(x) 
    {
      df <- data_frame(user = x$login, userID = x$id, followersURL = x$followers_url, followingURL = x$following_url)
    }) %>% bind_rows()
    i <- i+1
    x <- length(followersContent)
    followersDF <- rbind(followersDF, currentFollowersDF)
  }
  return (followersDF)
}

#Returns a dataframe with information on the Current Users Repositories
getRepos <- function(username)
{
  i <- 1
  x <- 1
  reposDF <- data_frame()
  while(x!=0)
  {
    repos <- GET( paste0("https://api.github.com/users/", username, "/repos?per_page=100&page=", i),gtoken)
    reposContent <- content(repos)
    currentReposDF <- lapply(reposContent, function(x) 
    {
      df <- data_frame(repo = x$name, id = x$id, commits = x$git_commits_url, language = x$languages) #language = x$language)
    }) %>% bind_rows()
    i <- i+1
    x <- length(reposContent)
    reposDF <- rbind(reposDF, currentReposDF)
  }
  return (reposDF)
}

#Returns a dataframe with the language used in each of the users repos
getLanguages <- function(username)
{
  
  reposDF <- GET( paste0("https://api.github.com/users/", username, "/repos?per_page=100"),gtoken)
  repoContent <- content(reposDF)
  i <- 1
  languageDF <- data_frame()
  numberOfRepos <- length(repoContent)
  for(i in 1:numberOfRepos)
  {
    repoLanguage <- repoContent[[i]]$language
    repoName <- repoContent[[i]]$name
    if(is_null(repoLanguage))
    {
      currentLanguageDF <- data_frame(repo = repoName, language = "No language specified")
    }else
    {
      currentLanguageDF <- data_frame(repo = repoName, language = repoLanguage)
    }
    i <- i+1
    languageDF <- rbind(languageDF, currentLanguageDF)
  }
  return (languageDF)
}

#Returns a pie chart which depicts the languages information for the current user
languagesVisualization <- function(username)
{
  z <- getLanguages(username)
  x <- data.frame(table(z$language))
  
  pie <- plot_ly(data =x, labels = ~Var1, values = ~Freq, type = 'pie') %>%
    layout(title = paste('Languages by Github User', username),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(pie)
  
}

#Returns a dataframe giving the number of followers and number of repos a user has
getFollowersInformation <- function(username)
{
  
  followersDF <- getFollowers(username)
  numberOfFollowers <- length(followersDF$userID)
  followersUsernames <- followersDF$user
  data <- data.frame()
  #Iterating through the current users followers to extract number of followers 
  #and number of repos
  for(i in 1:numberOfFollowers)
  {
    userName <- followersUsernames[i]
    repos <- getRepos(userName)
    followers <- getFollowers(userName) 
    numberOfRepositories <- length(repos$repo)
    numberOfFollowers <- length(followers$user)
    newRow <- data.frame(userName, numberOfRepositories, numberOfFollowers)
    data <- rbind(data, newRow)
    i <- i+1;
  }
  return(data)
}

#Checks for duplicate observations in a dataframe
checkDuplicate <- function(dataframe)
{
  noDuplicates <- distinct(dataframe)
  return(noDuplicates)
}

#Generate data for followers and repos starting at user phadej (commonly used user)
currentUser <- "phadej"
x <- getFollowers(currentUser)
followersUsernames <- x$user
numberOfFollowers <- length(x$userID)
fullData <- getFollowersInformation(currentUser)
i <- 1
while(nrow(fullData)<500)
{
  current <- followersUsernames[i]
  newData <- getFollowersInformation(current)
  fullData <- rbind(newData, fullData)
  i <- i+1
}
fullData <- checkDuplicate(fullData)
fullData
#Use plotly to graph the relationship between a users number of followers and repositories 
library(plotly)
scatter <- plot_ly(data = fullData, x = ~numberOfFollowers, y = ~numberOfRepositories,
                   text = ~paste("User: ", userName, '<br>Followers: ', numberOfFollowers, '<br>Repos:', numberOfRepositories),
                   marker = list(size = 10, color = 'rgba(255, 180, 195, .9)',
                                 line = list(color = 'rgba(150, 0, 0, .8)',width = 2))) %>%
  layout(title = 'Relationship between Followers and Repositories',yaxis = list(zeroline = FALSE),xaxis = list(zeroline = FALSE),
         plot_bgcolor='rgba(60, 190, 165,0.2)')
scatter

#Extracting data for users with over 1000 followers or repositories
mostFollowers <- fullData[which(fullData$numberOfFollowers>=1000),]
mostFollowers$code = 1
mostRepos <- fullData[which(fullData$numberOfRepositories>=1000),]
mostRepos$code = 0

combined <- rbind(mostFollowers,mostRepos)
scatter2 <- plot_ly(data = combined, x = ~numberOfFollowers, y = ~numberOfRepositories, color = ~code, colors = "Set1",
                    text = ~paste("User: ", userName, '<br>Followers: ', numberOfFollowers, '<br>Repos:', numberOfRepositories)) %>%
  layout(title = 'Most Followers and Repositories',yaxis = list(zeroline = FALSE),xaxis = list(zeroline = FALSE),
         plot_bgcolor='rgba(60, 190, 165,0.2)')
scatter2



#language Information for user
pie <- languagesVisualization("mishin") #Note that interestingly varied language use, unlike most users where one language tends to dominate
pie

#Function to visualise bar chart of languages used in repos of Github User
barChartLanguages <- function(user)
{
  z <- getLanguages(user)
  x <- data.frame(table(z$language))
  
  p <- plot_ly(data=x, x = ~Var1, y = ~Freq, type = 'bar', name = 'Languages of Repositories') %>%
    layout(yaxis = list(title = 'Repository Count'), xaxis = list(title='Language'),
           title = paste("Languages Used in Repositories of Github User",user))
  
  return(p)
}
barchartLanguages <- barChartLanguages("mishin")
barchartLanguages

#Function to get the companies of a Github users followers

getCompanies <- function(user)
{
  followerInfo <- content(GET(paste0("https://api.github.com/users/",user,"/followers?per_page=100;"),gtoken))
  companyData <- data.frame()
  numOfUsers <- length(followerInfo)
  
  for(i in 1:numOfUsers)
  {
    userLogin <- followerInfo[[i]]$login
    userInfo <- content(GET(paste0("https://api.github.com/users/",userLogin),gtoken))
    
    if(is.null(userInfo$company))
    {
      next
    }
    else 
    {
      currentCompanyData <- data.frame(login = userLogin, company = userInfo$company )
      companyData <- rbind(companyData, currentCompanyData)
    }
    
  }
  
  return(companyData)
}


c <- getCompanies("mishin")
c

#Function to visualize the companies of the followers of a user
visualizeCompanies <- function(user)
{
  
  z <- getCompanies(user)
  x <- data.frame(table(z$company))
  
  p <- plot_ly(data =x, labels = ~Var1, values = ~Freq, type = 'pie') %>%
    layout(title = paste("Companies of", user,"'s Followers"),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(p)
}

companiesFollowers <- visualizeCompanies("mishin")
companiesFollowers

Sys.setenv("plotly_username"="hollanco")
Sys.setenv("plotly_api_key"="7m9Tl1sWmQtgCy1pc2rn")
api_create(scatter, filename = "Followers and Repository Relationship")
api_create(scatter2, filename="Users with the most Followers and Repositories")
api_create(pie, filename="User Language Visualization")
api_create(barchartLanguages, filename = "Bar Chart of Languages Used")
api_create(companiesFollowers, filename = "Vizualization of companies")

#Further visualizations
