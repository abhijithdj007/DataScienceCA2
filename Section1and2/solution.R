library(dplyr)
library(readr)
library(mice)
library(VIM)

# Complete the following tasks using the NIPostcodes dataset described above.

.postcode_file_path <- file.path('NIPostcodes.csv') # CSV file path

NIpostcodes <- read.csv(.postcode_file_path, header=FALSE, stringsAsFactors=FALSE) # Parsing of CSV file 

# a) Show the total number of rows, the structure of the data frame, and first 10 rows of
# the data frame containing all of the NIPostcode data.

nrow(NIpostcodes) # number of rows

str(NIpostcodes) # returns structure of data 

head(NIpostcodes,10) # returns mentioned limit number of top records

# b) Add a suitable title for each attribute of the data.

header_names <- c("Organisation.Name", "Sub-building.Name", "Building.Name", "Number", "Primary.Thorfare", 
                  "Alt.Thorfare", "Secondary.Thorfare", "Locality", "Townland", "Town", "County", "Postcode", 
                  "x-coordinates", "y-coordinates", "Primary.Key") # Column Header names

colnames(NIpostcodes) <- header_names  # inserting header names into dataframe

str(NIpostcodes) # returns structure of data

# c) Replace and recode all missing entries with a suitable identifier. 
# Decide whether it is best to remove none, some or all of the missing data. 
# Discuss the missing data using suitable graphical output to explain your answer and justify your decision in detail.

NIpostcodes[NIpostcodes == ""] <- NA
# 
# head(NIpostcodes)

str(NIpostcodes)

sum(is.na(NIpostcodes)) # total number of missing values

md.pattern(NIpostcodes) #graphically display missing data

missing_values <- aggr(NIpostcodes, prop = FALSE, numbers = TRUE) #using vim packages to display the missing values

summary(missing_values) # missing values summary

# d) Show the total number of missing values for each column in the postcode data frame 
# both before and after your decision to deal with the missing data variables.

sapply(NIpostcodes, function(x) sum(is.na(x))) # display column wise count of missing values


# e) Move the primary key identifier to the start of the dataset.

new_NIpostcodes <- subset(NIpostcodes, select=c(15,1:14)) # select columns which need to move conditionally

str(new_NIpostcodes)

# f) Create a new dataset called Limavady_data. Store within it only information where locality, 
# townland and town contain the name Limavady. Count and display the number of rows. 
# Store this information in a csv file called Limavady.


df_limavady <- subset(new_NIpostcodes, tolower(Locality) == "limavady" | tolower(Townland) == "limavady" | 
                        tolower(Town) == "limavady" ) # selects value on condition of locality

write.csv(df_limavady, file="Limavady.csv", row.names=FALSE) # save dataframe locally

str(df_limavady) # displays dataframe structure

nrow(df_limavady) # displays total rows in dataframe


# g) Save the modified NIPostcode dataset in a csv file called

write.csv(new_NIpostcodes, file="CleanNIPostcodeData.csv", row.names=FALSE) # save dataframe locally



#####################################################################################################################


##########################################  SECTION - 2 #############################################################


#####################################################################################################################

# Complete these tasks using the NICrimeData datasets described above.

# (a) Using R, amalgamate all of the crime data from each csv file into one dataset. 
# Save this dataset into a csv file called AllNICrimeData. 
# Count and show the number of rows in the AllNICrimeData dataset. 
# Do not hard code the dataset path into your R code.

file_paths = list.files(path="./NI Crime Data", pattern="*.csv", 
                        full.names=TRUE, recursive= TRUE) # list of CSV file in directory 

merged_df <- sapply(file_paths, read_csv, simplify=FALSE, col_types = cols(.default = "c")) %>%
  bind_rows() # merges all the data frames togather, and attribute type to character

.file_path <- file.path('AllNICrimeData.csv')

write.csv(merged_df, file=.file_path, row.names=FALSE) # save file locally

AllNICrimeData <- read.csv(.file_path, header=TRUE, stringsAsFactors=FALSE)#, na.strings= c("")) # reads csv file

str(AllNICrimeData) # displays structure

nrow(AllNICrimeData) # total row count


# (b) Modify the structure of the newly created AllNICrimeData csv file and 
# remove the following attributes: CrimeID, Reported by, Falls within, LSOA code, LSOA name, last outcome and context. 
# Save this new structure and show the structure of the modified file.

# str(AllNICrimeData)

drop_col <- c("Crime.ID", "Reported.by", "Falls.within", "LSOA.code", "LSOA.name", "Last.outcome.category", 
              "Context") # columns which need to be removed

new_crime_df = AllNICrimeData[, !(colnames(AllNICrimeData) %in% drop_col)] # select columns other than mentioned array

str(new_crime_df) # display structure

# (c) In the AllNICrimeData csv file, shorten each crime type as follows:

new_crime_df$Crime.type[new_crime_df$Crime.type == "Anti-social behaviour"] <- "ASBO"
new_crime_df$Crime.type[new_crime_df$Crime.type == "Bicycle theft"] <- "BITH"
new_crime_df$Crime.type[new_crime_df$Crime.type == "Burglary"] <- "BURG"
new_crime_df$Crime.type[new_crime_df$Crime.type == "Criminal damage and arson"] <- "CDAR"
new_crime_df$Crime.type[new_crime_df$Crime.type == "Drugs"] <- "DRUG"
new_crime_df$Crime.type[new_crime_df$Crime.type == "Other theft"] <- "OTTH"
new_crime_df$Crime.type[new_crime_df$Crime.type == "Public order"] <- "PUBO"
new_crime_df$Crime.type[new_crime_df$Crime.type == "Robbery"] <- "ROBY"
new_crime_df$Crime.type[new_crime_df$Crime.type == "Shoplifting"] <- "SHOP"
new_crime_df$Crime.type[new_crime_df$Crime.type == "Theft from the person"] <- "THPR"
new_crime_df$Crime.type[new_crime_df$Crime.type == "Vehicle crime"] <- "VECR"
new_crime_df$Crime.type[new_crime_df$Crime.type == "Violence and sexual offences"] <- "VISO"
new_crime_df$Crime.type[new_crime_df$Crime.type == "Other crime"] <- "OTCR"

print(sample_n(new_crime_df, 10))


# (d) Using the plot() function, show a plot of each crime frequency from the crime.type field. 
# Specify relevant options such as title, axes labels, bar colours. 
# Provide a detailed discussion about what you found from this chart

crime_freq <- prop.table(table(new_crime_df$Crime.type)) # dplyr library function to calculate entire table
print(crime_freq)

barplot(crime_freq, ylab = "Frequency", xlab = "Crime Type", 
        main = "Different crime and frequency in Northern Ireland", col = rainbow(14)) # displays bar chart


# (e) Modify the AllNICrimeData dataset so that the Location attribute contains only a street name. 
# For example, the attribute value “On or near Westrock Square” should be modified to only contain “Westrock Square”. 
# Modify the resultant empty location attributes with a suitable identifier. Show a sample of your data.

new_crime_df$Location <- trimws(gsub("On or near", "", new_crime_df$Location)) # gsub to replace, trimws remove whitespace
new_crime_df[new_crime_df =="" | new_crime_df ==" "] <- NA # replace blanks with NA

print(sample_n(new_crime_df, 10)) # print 10 sample record


# (f) Choose 5000 random samples of crime data from the AllNICrimeData dataset where the location attribute contains 
# location information. This means that the location information should NOT contain an NA identifier. 
# Set the seed value to 100. Store this data in a data frame called random_crime_sample. 
# Then create a function called find_a_town that uses the CleanNIPostcodeData data frame to find correct 
# town/city information for each location variable within the random_crime_sample dataset. 
# Save each matched town into the random_crime_sample data frame under the column name Town.

set.seed(100) # set seed to 100 for constant records

random_crime_sample <- sample_n(new_crime_df[complete.cases(new_crime_df$Location), ], 5000) # non NA 5000 records

find_a_town <- function(name) {
  new_NIpostcodes$Town[match(tolower(name), tolower(new_NIpostcodes$Locality))] # returns town
}

print(random_crime_sample$Location) # prints random sample records

random_crime_sample$Town <- find_a_town(random_crime_sample$Location) # stores town values in dataframe

print(sample_n(random_crime_sample[complete.cases(random_crime_sample$Town), ], 5)) # displat non NA town records



# (g) Create a function called add_town_data that examines the information from each crime record 
# in random_crime_sample and matches each record with relevant data in the VillageList.csv file. 
# Add the population attribute to the random_crime_sample data frame.


.village_file_path <- file.path('VillageList.csv') # village CSV path

village_list <- read.csv(.village_file_path, header = TRUE) # read village CSV

colnames(village_list) # village CSV column names

add_town_data <- function(name) {
  village_list$POPULATION[match(tolower(name), tolower(village_list$ï..CITY.TOWN.VILLAGE))] # returns population
}

random_crime_sample$POPULATION <- add_town_data(random_crime_sample$Town) # stores population column

print(sample_n(random_crime_sample[complete.cases(random_crime_sample$POPULATION), ], 3))

# write.csv(random_crime_sample, file="test.csv", row.names=FALSE)


# (h) Update the random sample AllNICrimeData dataset so that it contains only the following items
# • Month
# • Longitude
# • Latitude
# • Location
# • Crime type
# • City-Town-Village
# • Population
# 
# 
# Save this data into a csv file called random_crime_sample.
# (i) Now we want to display crime for both cities in Northern Ireland which are Belfast and Derry. 
# From the random_crime_sample data frame, sort the chart data by crime type for each city and 
# then show both charts side-by-side by setting relevant graphical parameters using the par() command. 
# Show a suitable main title for the bar chart, and suitable x and y-axis labels and scaling. 
# Make sure all labels on the x-axis can be read. Show the bar plot in your CA document.
# Display both charts and provide some detailed discussions about the results.

crime_belfast <- filter(random_crime_sample, Town == 'BELFAST') # filter with BELFAST town

crime_londonderry <- filter(random_crime_sample, Town == 'LONDONDERRY') # filter with LONDONDERRY town


belfast_count <- count(crime_belfast,Crime.type) # get grouped count
sort(belfast_count$n) # sort via count

londonderry_count <- count(crime_londonderry,Crime.type)  # get grouped count
sort(londonderry_count$n) # sort via count

par(mfrow = c(2,2)) # multiple graph in single plot

plot(factor(belfast_count$Crime.type), belfast_count$n, xlab = "Types of Crime", ylab = "Crime count", 
     main = "All types of crimes and their count in Belfast") # BELFAST graph
plot(factor(londonderry_count$Crime.type), londonderry_count$n, xlab = "Types of Crime", ylab = "Crime count", 
     main = "All types of crimes and their count in Londonderry") # LONDONDERRY graph






