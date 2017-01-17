#-------------------------------------------------------------------------------
# Example code demonstrating multi-language multi-country population plots
#

# Save the current 'par' settings
oldPar <- par()

# NOTE:  Install stringr, RJSONIO and reshape2 if you don't already have them
# install.packages("stringr")
# install.packages("RJSONIO")
# install.packages("reshape2")

library(stringr)
library(RJSONIO)
library(reshape2)

# The plotting function is contained in a separate script
source(paste0(getwd(), '//R//Other Files//populationOverTimePlot.R'))


#----- Configurable options ----------------------------------------------------

language = "en"   # ISO 639-2 language code
country = "US"    # ISO 3166-1 alpha-2 country code (or commaa separated list of these)


#----- Create Lists with needed information ------------------------------------

# NOTE:  We have found it useful to organize the information needed by analysis and
# NOTE:  plotting scripts into three categories and to pass this information around
# NOTE:  in ~Lists. This way, when we need to repurpose some of our code or add new
# NOTE:  functionality to an existing script we don't have change the function 
# NOTE:  arguments.  Instead, we just add new elements to one of the lists.  This 
# NOTE:  manner of programming has proven a good compromise of flexible and predictable.

#----- infoList ------------------------

# The infoList contains information coming from a user request
infoList <- list(language=language, 
                 countryCode=strsplit(country, ",")[[1]])

#----- textList ------------------------

# The textList contains all plot annotations

# Language specific text strings are stored in language.json.
jsonData <- fromJSON(paste0(getwd(), '//R//Other Files//language.json'), encoding="UTF8")

# NOTE:  In more complicated systems we would create the textList in a spearate 
# NOTE:  function that accepts infoList as an argument.

# Here comes the complicated code to extract the countryName necessitated by the 
# structure of the json file.
if (str_detect(infoList$countryCode,',')) {
  countryList <- jsonData[[infoList$language]]$region_names
} else {
  countryList <- jsonData[[infoList$language]]$country_names
}
index <- which( sapply(countryList, function(x) x[['code']] == infoList$countryCode))
countryName <- countryList[[index]][['name']]

# Most labels used to annotate plots are stored in the 'rtxt' block
rtxt <- jsonData[[infoList$language]]$rtxt

textList <- list(countryName = countryName,
                 ylab1 = rtxt[["ylab1"]],
                 ylab2 = rtxt[["ylab2"]],
                 subtitle = rtxt[["subtitle"]],
                 growth = rtxt[["growth"]],
                 decline = rtxt[["decline"]],
                 million = rtxt[["million"]],
                 thousand = rtxt[["thousand"]],
                 millions = rtxt[["millions"]],
                 thousands = rtxt[["thousands"]],
                 population = rtxt[["population"]],
                 projection = rtxt[["projection"]])

#----- dataList ------------------------

# Read in the MidYearPop.csv file
midYearPop = read.csv(paste0(getwd(), '//R//Other Files//MidYearPop.csv'))

# Read in the estimateUpdate.csv file
estimateUpdateYears = read.csv(paste0(getwd(), '//R//Other Files//estimateUpdateYears.csv'))

# NOTE:  The estimateUpdateYers.csv file is organized for ease of human editing.
# NOTE:  It should be modified based on the release notes whenever a new version
# NOTE:  of the US Census Bureau IDB dataset is released.
# NOTE:
# NOTE:    http://www.census.gov/population/international/data/idb/rel_notes.php
# NOTE:
# NOTE:  The next three lines drop the 'countryName' column and then reorganize
# NOTE:  the dataframe to have countryId as columns instead of rows. This then 
# NOTE:  matches the organization of the midYearPop dataframe.
estimateUpdateYears <- subset(estimateUpdateYears, select = -c(countryName))
molten = melt(estimateUpdateYears, id.vars = "countryCode")
estimateUpdateYears = dcast(molten, variable ~ countryCode)

# Create dataList
dataList <- list(midYearPop=midYearPop,
                 estimateUpdateYears=estimateUpdateYears)


# NOTE:  At this point we have assembled all of the necessary information.
# NOTE:  Now we call the plotting script whose job is focused on the fine 
# NOTE:  points of creating a compelling data visualization.
# NOTE:  If, at some point, we need more annotations or additioinal data
# NOTE:  to improve our plots we would insert additional text or data into
# NOTE:  the appropriate ~List. These items would then be available in the
# NOTE:  plotting function.


#----- Create plot -------------------------------------------------------------

# Call the plotting function, passing in the ~Lists
populationOverTimePlot(dataList, infoList, textList)

# Restore 'par' settings
par(oldPar)

