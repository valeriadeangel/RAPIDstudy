library(networkD3)
library(dplyr)
library(readxl)


#load the data from the same excel workbook but 2 different sheets 5 + 6 - you just need to upload the excel worksheets with your data
links <- read_excel("~/R/1. Recruitment data.xlsx", 5)
nodes <- read_excel("~/R/1. Recruitment data.xlsx", 6)

# Now we have 2 data frames: a 'links' data frame with 3 columns (from, to, value), and a 'nodes' data frame that gives the name of each node.
# Links data frame: the source says where the lines go from, and the Target is where the lines end up. the Value is the thickness (in my case number of participants)
# The nodes data frame: the "key". i.e.  0 = Croydon, 4 = Contacted


p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "Source",
                   Target = "Target", Value = "Value", NodeID = "name",
                   units = "ps", fontSize = 15, nodeWidth = 40)
p
