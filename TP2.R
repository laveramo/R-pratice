# 1 Plots and Given Names
# Exploring the Pink city
# • read the table prenoms.csv
# getwd() # To get the current folder address
prenoms_address = "~/visualisation-donnees/R-pratice/prenoms.csv"
prenoms <- read.csv(prenoms_address, header = TRUE, sep = ";", stringsAsFactors = FALSE)

# • inspect it
# • Plot:

install.packages("ggplot2")     # Library needed to make graphics
library(ggplot2)

#   – The number of births by year

prenoms[["Année"]]           # Print all values in Annéé
print(prenoms[, 1])          # Print all values in Annéé

births_table = as.data.frame(table(prenoms$Année))   # table(prenoms$Année), counts the total of repetitions by year.  as.data.frame: converts the data in a table
colnames(births_table) <- c("Year", "Total_Births")  # rename the columns
print(births_table)

# PLOT :
# Method 1 - Using Plot with simple graphics
births_table$Year <- as.numeric(as.character(births_table$Year)).  # Converts the column Year in Number

plot(births_table$Year, births_table$Total_Births, type = "b",
     main = "Total births by year",
     xlab = "Year", ylab = "Total Births",
     col = "blue")

# Method 2 - Using ggplot to create more personalized and complex graphics
ggplot(births_table, aes(x = Year, y = Total_Births, group = 1)) +
  geom_line(color = "red") +
  labs(title = "Total births by year",
       x = "Year",
       y = "Total Births") +
  theme_minimal()

# Show the range of the values
scale_y_continuous(limits = c(min(births_table$Total_Births) - 10, max(births_table$Total_Births) + 10))


# – The number of male/female births by year

# Library needed to convert data between long and wide.
# Long is only one column with all the variable, Wide divide the data into columns by variables
install.packages("reshape2")    
library(reshape2)

births_gender_table = as.data.frame(table(prenoms$Année, prenoms$Sexe))
colnames(births_gender_table) = c("Year", "Gender", "Total_Births")

# Using reshape2, converts a long data into wide data to have an independent column by gender
births_gender_table_wide = dcast(births_gender_table, Year ~ Gender, value.var = "Total_Births")

print(births_gender_table)

ggplot(births_gender_table, aes(x = Year, y = Total_Births, color = Gender ,group = Gender)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Total births by year",
       x = "Year",
       y = "Total Births") +
  theme_minimal()

# – Is your name in the dataset ?
# – Represent the 10 most given names
names_table = as.data.frame(table(prenoms$Prénom))
colnames(names_table) <- c("Name", "Total_Names")
sorted_names_table = names_table[order(-names_table$Total_Names), ]
head(sorted_names_table, 10)

# – Select for each year the top 5 given names by sex and represent their evolution along years.
table(prenoms$Année, prenoms$Prénom)


# – Plot the average number of letters by year
# – Plot the average number of vowels/consonants by year
# – How the number of composed names (like Jean-Baptiste or Lou-Ann
# – Define a “hype” criteria and find the hypest names


# Exploring the Gray city
# • read the table prenomsParis.csv
# • repeat what you’ve done with Toulouse, rewriting as little as possible


# A tale of two cities
# • Combine observations made on the two cities.
# • Normalise by the number of births.
# • What are the most unshared names ?


# A tale of many cities
# • Read the table prenomsRennesStrassNantesToul.csv
# • Inspect it. On the opendata website the description is the following:
#   This file contains given names to childrens born in Rennes, Strasbourg,
#   Nantes and Toulouse urban areas from 2002 to 2012

# Is this really what you observe ?


# Datos de ejemplo con menos puntos
x <- 1:5
y <- c(2, 5, 3, 8, 6)

# Gráfico de puntos
plot(x, y, type = "p", main = "Gráfico de puntos", col = "blue", pch = 16)

# Gráfico de líneas
plot(x, y, type = "l", main = "Gráfico de líneas", col = "red")

# Gráfico de puntos y líneas
plot(x, y, type = "b", main = "Gráfico de puntos y líneas", col = "green", pch = 16)

# Gráfico de líneas discontinuas
plot(x, y, type = "c", main = "Gráfico de líneas discontinuas", col = "purple")

# Gráfico de escalones
plot(x, y, type = "s", main = "Gráfico de líneas escalonadas", col = "orange")

# Gráfico de histograma de densidad
plot(x, y, type = "h", main = "Gráfico de densidad tipo histograma", col = "brown")