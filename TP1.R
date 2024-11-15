#1.1
e1 = c(2,5,0,8)
e2 = 1:200
e3 = seq(-200, -210, by=-2)
e4 = 2^(1:7)
v =(-1)^(1:50)
e5 = c(e2, -e3[2:6])
e7 = seq(0,1,length.out=70)
e7 = rep(e1,10)
e2-e3

#1.2
vowels = c('a','e','i','o', "u")
letters %in% vowels
vowels %in% letters
vowels_numbers = which(letters %in% vowels)
letters_numbers = which(!letters %in% vowels)
letters[vowels_numbers + 1]      
myname = "luz"
listname = strsplit(myname,"")
listname[[1]]
mynumbers = which(letters %in% listname[[1]])
othername = "cristian"
listname2 = strsplit(othername,"")
othernumbers = which(letters %in% listname2[[1]])
mean(mynumbers)
mean(othernumbers)

#2.1
df = data.frame(alphabet = letters, numbers = which(letters %in% letters), vowel = letters %in% vowels)
df[mynumbers,]

library(ggplot2)
msleep = msleep     
head(msleep)
str(msleep)
names(msleep)
summary(msleep)
msleep[["sleep_total"]] + msleep[["awake"]]
msleep[which.max(msleep$sleep_total),]
length(which((msleep$bodywt < 100) & (msleep$sleep_total > 12)))
mean(msleep$brainwt/msleep$bodywt, na.rm = TRUE)
max(msleep$brainwt/msleep$bodywt, na.rm = TRUE)

#2.2
msleep2 = data.frame(msleep)
levels = c("lc", "domesticated", "cd", "nt", "vu", "en", "cr")
msleep2$conservation = factor(msleep2$conservation,levels = levels, ordered = TRUE)
msleep2[order(msleep2$conservation),]
mean(msleep2[which(msleep2$conservation > "nt"), "bodywt"])
mean(msleep2$bodywt)
mean(msleep2[which(msleep2$conservation <= "nt"), "bodywt"])
msleep2 = transform(msleep2, threatened = ifelse(conservation > "nt", TRUE, FALSE))

#2.3
letter_numbers <- function(name) {
  listname <- unlist(strsplit(tolower(name),""))
  positions <- numeric(length(listname))
                       
  if(length(listname) > 0){
    for (i in 1:length(listname)) {
      position = which(letters %in% listname[i])
      if(length(position) > 0)
      {
        positions[i] <- position
      }
      else{
        positions[i] <- NA
      }
    } 
  }
  return(positions)
}

letter_numbers("CRIS tian")
