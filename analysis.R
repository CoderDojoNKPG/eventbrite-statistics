library(httr)
library(jsonlite)
library(ggplot2)
library (plyr)
library(reshape2)

token <- read.csv("token", FALSE)$V1 #Eventbrite token has to be written in a textfile "token" containing the token in quotation marks: e.g. "123ABCDEFK3"
endpointUrl <- "https://www.eventbriteapi.com/v3/"

getAll <- function(request, listName) {
  data = data.frame()
  has_more = TRUE
  page = 1
  
  while (has_more) {
    r <- GET(paste(endpointUrl, request, "/?token=", token, "&page=", page, sep=""))
    txt <- content(r, "text", encoding="UTF-8")
    objects <- fromJSON(txt, flatten=TRUE)[[listName]]
    pagination <- fromJSON(txt, flatten=TRUE)$pagination
    if (length(objects) > 0) {
      data = rbind.fill(data,objects)
    }

    page <- page + 1;
    has_more <- pagination$has_more_items
  }
  data
}


events <- getAll("users/me/owned_events", "events")
events <- events[events$status=="completed",] #analyse only completed events
events$start.local <- as.Date(events$start.local)

g <- ggplot(data=events, aes(x=start.local, y=capacity))
g <- g + geom_line()
print(g)

getGenders <- function (attendees) {
  genders=list()
  if (length(attendees) > 0) {
    for(i in 1:nrow(attendees)) {
      if (attendees$checked_in[[i]]) {
        answers <- attendees$answers[[i]]
        if (length(answers) > 0) {
          genders <- c(genders,answers[answers$question == "Gender",]$answer)
        }
      }
    }
  }
  genders <- ldply (genders)
}

getAttendeeStatistics <- function(eventid) {
  attendees <- getAll(paste("events/", eventid, "/attendees", sep=""), "attendees")
  genders <- getGenders(attendees)
  if (length(genders) > 0) {
    gender_unkown = length(which(is.na(genders$V1)))
    gender_boy = length(which(genders$V1=="Boy"))
    gender_girl = length(which(genders$V1=="Girl"))
    gender_other = length(which(genders$V1=="Other"))
    data.frame(id = eventid, gender_boy = gender_boy, gender_girl = gender_girl, gender_other = gender_other,gender_unkown = gender_unkown)
  }
  else {
    data.frame()
  }
  
}

attendeeStat <- ldply(events$id, getAttendeeStatistics, .progress = progress_text())
events <- merge(events, attendeeStat, by.x="id", all.x=TRUE)
events$girls_share <- events$gender_girl/(events$gender_boy + events$gender_girl)
events$checkedin <- events$gender_boy + events$gender_girl + events$gender_other + events$gender_unkown

g <- ggplot(events, aes(x = start.local)) + 
     geom_line(aes(y = gender_girl), color="green") +
     geom_line(aes(y = gender_boy), color="red")
print(g)

g <- ggplot(events, aes(x = start.local, y=girls_share)) + 
  geom_line(color="green") +
  geom_smooth(method = "loess", na.rm=TRUE)
print(g)


g <- ggplot(events, aes(x = start.local, y=checkedin)) + 
  geom_line(color="black") +
  geom_smooth(method = "loess", na.rm=TRUE)
print(g)
