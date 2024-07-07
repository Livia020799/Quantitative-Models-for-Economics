#install.packages("bibliometrix", dependencies = TRUE)
library(bibliometrix)
#install.packages("gridExtra")
#installed.packages("grid")
#install.packages("knitr")
#install.packages("condformat")
library(condformat)
#install.packages("tibble")
library(tibble)
library(gridExtra)
library(grid)
library(knitr)
#install.packages("remotes")
remotes::install_github("massimoaria/bibliometrix")
library(remotes)
bibliometrix::biblioshiny()
biblioshiny()
#############################
#ANALYSIS ON WEB OF SCIENCE #
############################

cat("Analysis on Web od Science")
wos_path_file <- "savedrecs.bib"
wos <- convert2df(file = wos_path_file, dbsource = "isi", format = "bibtex")
wos_missing_data <- missingData(wos) #to evaluate missing data
wos_missing_data

# Assuming m$mandatoryTags is your data frame
# Convert the data frame to a tibble for compatibility with condformat
wos_missing_data$mandatoryTags <- as_tibble(wos_missing_data$mandatoryTags)
colnames(wos_missing_data$mandatoryTags) <- c("Metadata", "Description", "Missing count", "Missing %", "Status")

# Define the conditional formatting rules
wos_formatted_data <- wos_missing_data$mandatoryTags %>%
  condformat() %>%
  rule_fill_discrete(
    columns = "Status",
    expression = Status,
    colours = c("Excellent" = "darkgreen", "Good" = "lightgreen", "Acceptable" = "lightyellow", "Poor" = "gray","Critical"="red", "Completely missing" = "darkred")
  )

# Convert to grob
wos_formatted_table <- condformat2grob(wos_formatted_data)

# Draw the formatted table
grid.newpage()
grid.draw(wos_formatted_table)

#TABLE 5 WoS -> summary table
wos_results <- biblioAnalysis(wos, sep = ";")
options(width=100)
wos_table_5 <- summary(object = wos_results, k = 10, pause = FALSE)
wos_table_5$MainInformationDF #from here we will take the information

#TABLE 6 WoS -> lists the most prolific authors in ITOR based on #publications
wos_table_6 <- authorProdOverTime(wos, k = 12, graph = FALSE)
wos_top_authors <- wos_table_6$dfAU[order(wos_table_6$dfAU$TC, decreasing = TRUE), ]
wos_total_count_per_author <- aggregate(freq ~ Author, data = wos_top_authors, sum) #Aggregate by Author and sum the frequency of publications
# Print the total count of published articles for each author
print(wos_total_count_per_author) 

#TABLE 7 WoS -> top 31 cited articles in ITOR
wos_results<- biblioAnalysis(wos)
summary(wos_results, k = 30, pause = FALSE)
wos_most_cited_papers <- wos_results$MostCitedPapers[1:30, ] # top 30
wos_most_cited_papers


####################
#ANALYSIS ON SCOPUS#
####################
cat("Analysis on Scopus")
scopus_path_file <- "scopus.csv"
scopus <- convert2df(scopus_path_file, dbsource = "scopus", format = "csv")
#NOTE: 31 duplicated documents are removed

scopus_missing_data <- missingData(scopus) #to evaluate missing data
scopus_missing_data

# Assuming m$mandatoryTags is your data frame
# Convert the data frame to a tibble for compatibility with condformat
scopus_missing_data$mandatoryTags <- as_tibble(scopus_missing_data$mandatoryTags)
colnames(scopus_missing_data$mandatoryTags) <- c("Metadata", "Description", "Missing count", "Missing %", "Status")

# Define the conditional formatting rules
scopus_formatted_data <- scopus_missing_data$mandatoryTags %>%
  condformat() %>%
  rule_fill_discrete(
    columns = "Status",
    expression = Status,
    colours = c("Excellent" = "darkgreen", "Good" = "lightgreen", "Acceptable" = "lightyellow", "Poor" = "gray","Critical"="red",  "Completely missing" = "darkred")
  )

# Convert to grob
scopus_formatted_table <- condformat2grob(scopus_formatted_data)

# Draw the formatted table
grid.newpage()
grid.draw(scopus_formatted_table)

#TABLE 5 Scopus 
scopus_results <- biblioAnalysis(scopus, sep = ";")
options(width=100)
scopus_table_5 <- summary(object = scopus_results, k = 10, pause = FALSE)
scopus_table_5$MainInformationDF #from here we will take the information

#TABLE 6 Scoups
scopus_table_6 <- authorProdOverTime(scopus, k = 12, graph = FALSE)
scopus_top_authors <- scopus_table_6$dfAU[order(scopus_table_6$dfAU$TC, decreasing = TRUE), ]
#Aggregate by Author and sum the frequency of publications
scopus_total_count_per_author <- aggregate(freq ~ Author, data = scopus_top_authors, sum) 
# Print the total count of published articles for each author
print(scopus_total_count_per_author)

#TABLE 7 Scopus (ordered by citation)
scopus_results <- biblioAnalysis(scopus)
summary(scopus_results, k = 30, pause = FALSE)
scopus_most_cited_papers <- scopus_results$MostCitedPapers[1:30, ] # top 30
scopus_most_cited_papers

#################################
#ANALYSIS ON 30 EDITORIAL CHOICE#
#################################

cat("Analysis on 30 editorial choice")
ITOR_path_file <- "30_editorial_choice.csv"

ITOR <- convert2df(ITOR_path_file, dbsource = "scopus", format = "csv")


#co-citation Scopus
NetMatrix_scopus =biblioNetwork(scopus, analysis = "co-citation", network = "references", sep = "; ")
par(mar=c(1, 1, 1, 1))
net_scopus=networkPlot(NetMatrix_scopus, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T,
                remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
plot(net_scopus$graph)

#co-citation web of Science
NetMatrix_wos = biblioNetwork(wos, analysis = "co-citation", network = "references", sep = "; ")
par(mar=c(1, 1, 1, 1))
net_wos=networkPlot(NetMatrix_wos , n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T,
                remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
plot(net_wos$graph)

#co-citation on both WoS and Scopus
M <- mergeDbSources(wos, scopus, remove.duplicated = TRUE)
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = "; ")

net <- networkPlot(NetMatrix, n = 30, Title = "United Co-Citation Network", type = "fruchterman", size=T,
                   remove.multiple=FALSE, labelsize=0.7, edgesize = 5)
plot(net$graph)









#TABLE 5 ITOR 
ITOR_results <- biblioAnalysis(ITOR, sep = ";")
options(width=100)
ITOR_table_5 <- summary(object = ITOR_results, k = 10, pause = FALSE)
ITOR_table_5$MainInformationDF #from here we will take the information

#TABLE 7 ITOR (ordered by citation)
ITOR_most_cited_papers <- ITOR_results$MostCitedPapers[1:30, ] # top 30
ITOR_most_cited_papers


###################################

# Relevant Keywords Scopus -> IMPORTANT FOR FIGURES 
scopus_table_5$MostRelKeywords

# Relevant Keywords WoS -> IMPORTANT FOR FIGURES
wos_table_5$MostRelKeywords

# Relevant Keywords ITOR -> IMPORTANT FOR FIGURES
ITOR_table_5$MostRelKeywords






