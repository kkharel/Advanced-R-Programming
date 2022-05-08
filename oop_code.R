
# Part 2

# data small study on indoor air pollution on 10 subjects
# each subject was visited 3 times
# pollution level recorded every 5 mins for 1 week

# variables: 
# id = subject identification number, 
# visit = the visit number which can be 0,1,2 
# room = room in which the monitor was placed, 
# value = the level of pollution in micrograms per cubic meter, 
# timepoint = the time point of the monitor value for a given visit/room


# design a class called longitudinaldata that characterizes the structure of this longitudinal dataset
# also need to design classes to represent the concept of "subject", "visit", "room"

# implement following functions

# make_LD: a function that converts a data frame into a "LongitudinalData" object
# subject: a generic function for extracting subject-specific information
# visit: a generic function for extracting visit-specific information
# room: a generic function for extracting room-specific information

getwd()

setwd("C:/Users/kkhar/OneDrive/Documents/r oop, fibo, factoria")

sink(file = "oop_output.txt")

#Defining Functions

subject <- function(longdata, id) UseMethod("subject")

visit <- function(subject, visit_id) UseMethod("visit")

room <- function(visit, room_id) UseMethod("room")

# Defining methods for LongitudionalData objects 
make_LD <- function(dataframe) {
  longdata <- dataframe %>% nest(data = -id)
  structure(longdata, class = c("LongitudinalData"))
}

print.LongitudinalData <- function(x) {
  cat("Longitudinal dataset with", length(x[["id"]]), "subjects")
  invisible(x)
}

subject.LongitudinalData <- function(longdata, id) {
  index <- which(longdata[["id"]] == id)
  if (length(index) == 0)
    return(NULL)
  structure(list(id = id, data = longdata[["data"]][[index]]), class = "Subject")
}

# Defining methods for Subject objects
print.Subject <- function(x) {
  cat("Subject ID:", x[["id"]])
  invisible(x)
}

summary.Subject <- function(x) {
  output <- x[["data"]] %>% 
    group_by(visit, room) %>%
    summarise(value = mean(value)) %>% 
    spread(room, value) %>% 
    as.data.frame
  structure(list(id = x[["id"]],
                 output = output), class = "Summary")
}

visit.Subject <- function(subject, visit_id) {
  data <- subject[["data"]] %>% 
    filter(visit == visit_id) %>% 
    select(-visit)
  structure(list(id = subject[["id"]],
                 visit_id = visit_id,
                 data = data), class = "Visit")
}


# Defining methods for Visit objects
room.Visit <- function(visit, room_id) {
  data <- visit[["data"]] %>% 
    filter(room == room_id) %>% 
    select(-room)
  structure(list(id = visit[["id"]],
                 visit_id = visit[["visit_id"]],
                 room = room_id,
                 data = data), class = "Room")
}

# Defining methods for Room objects 

print.Room <- function(x) {
  cat("ID:", x[["id"]], "\n")
  cat("Visit:", x[["visit_id"]], "\n")
  cat("Room:", x[["room"]])
  invisible(x)
}

# Summary of the pollutant values

summary.Room <- function(x) {
  output <- summary(x[["data"]][["value"]])
  structure(list(id = x[["id"]],
                 output = output), class = "Summary")
}

# Defining methods for Summary objects


print.Summary <- function(x) {
  cat("ID:", x[[1]], "\n")
  print(x[[2]])
  invisible(x)
}

longdata <- read.csv("MIE.csv")
x <- make_LD(longdata)
print(class(x))
print(x)
out <- subject(x, 10)
print(out)
out <- subject(x, 14)
print(out)
out <- subject(x, 54) %>% summary
print(out)
out <- subject(x, 14) %>% summary
print(out)
out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)
out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)
