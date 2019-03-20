library(tidyverse)
library(GGally)
setwd("C:/Users/jaspe/OneDrive/Documents/Classes/Mathematics/318 - Mathematical Analysis of Statistical Software/Final Project/src")

load("campaignFunding.rda")

# Standard Summary Statistics
#summary(congressional)

summaryStats <- function(df) {
  # Calc var on numeric variables only
  numericdf <- select_if(df, is.numeric)
  
  dfsummary <- data.frame(Mean = sapply(numericdf, mean))
  dfsummary <- add_column(dfsummary, Median = sapply(numericdf, median))
  dfsummary <- add_column(dfsummary, Variance = sapply(numericdf, var))
  dfsummary <- add_column(dfsummary, Minimum = sapply(numericdf, min))
  dfsummary <- add_column(dfsummary, Maximum = sapply(numericdf, max))
  
  return(dfsummary)
}


house = subset(congressional, Office=="H")
numericCongressional = select_if(congressional, is.numeric)
numericHouse = select_if(house, is.numeric)

# Summary Statistics
summary(house)
summaryStats(house)

# Number of zeros
colSums(house == 0)

# In Following Graphs, use log scale sometimes and only graph non-zero data

# Histogram of Numeric Data
numericHouse.long <- gather(subset(numericHouse, select = -Numeric_Winner), attribute, value)

histograms_plt <- ggplot(data = numericHouse.long[which(numericHouse.long$value!=0),])+
  geom_histogram(mapping = aes(x = value), bins = 15) +
  ggtitle("Distributions of Numeric Fields (log scale)") +
  facet_wrap(~attribute, scales = "free", ncol = 3) +
  scale_x_continuous(trans = "log10")
print(histograms_plt)
ggsave("../pictures/Histograms.png", histograms_plt, height = 10, width = 12)

# Viewing some Scatter Plots
itemized_vs_unitemized_plt <- ggplot(data = house)+
  geom_point(mapping = aes(x = Ind_Itemized_Con, y = Ind_Unitemized_Con, color = Winner, alpha=0.5)) +
  labs(title = "Individual Unitemized Contributions vs Individual Itemized Contributions", x = "Individual Itemized Contributions", y = "Individual Unitemized Contributions") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")
print(itemized_vs_unitemized_plt)
ggsave("../pictures/Itemized_vs_Unitemized.png", itemized_vs_unitemized_plt, height = 10, width = 8)

party_v_committee_plt <- ggplot(data = house)+
  geom_point(mapping = aes(x = Com_Con, y = Party_Con, color = Winner, alpha=0.5)) +
  labs(title = "Party Contributions vs Committee Contributions", x = "Committee Contributions", y = "Party Contributions") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")
print(party_v_committee_plt)
ggsave("../pictures/Party_vs_Committee.png", party_v_committee_plt, height = 10, width = 8)

netcon_v_operating_Win_plt <- ggplot(data = house)+
  geom_point(mapping = aes(x = Operating_Exp, y = Net_Con, color = Winner, alpha=0.5)) +
  labs(title = "Net Contributions vs Operating Expenditures: Election Winner", x = "Operating Expenditures", y = "Net Contributions") +
  scale_x_continuous(limits = c(0, 5000000)) +
  scale_y_continuous(limits = c(0, 5000000))
print(netcon_v_operating_Win_plt)
ggsave("../pictures/NetCon_vs_Operating_Winner.png", netcon_v_operating_Win_plt, height = 10, width = 8)

netcon_v_operating_Status_plt <- ggplot(data = house[which(house$Seat_Status!=''),])+
  geom_point(mapping = aes(x = Operating_Exp, y = Net_Con, color = Seat_Status, alpha=0.5)) +
  labs(title = "Net Contributions vs Operating Expenditures: Seat Status", x = "Operating Expenditures", y = "Net Contributions") +
  scale_x_continuous(limits = c(0, 5000000)) +
  scale_y_continuous(limits = c(0, 5000000))
print(netcon_v_operating_Status_plt)
ggsave("../pictures/NetCon_vs_Operating_Seat_Status.png", netcon_v_operating_Status_plt, height = 10, width = 8)


scatter_plt <- ggpairs(select_if(house,is.numeric)) +
  ggtitle("Pair Correlations of House Race Numerical Attributes")
print(scatter_plt)
ggsave("../pictures/Cross_Correlations_Small.png", scatter_plt, height = 6, width = 6)


# Just Examine Differences by Party
mainParties <- subset(house, Party=="REP"|Party=="DEM")
levels(mainParties$Winner) <- c("Loser", "Winner")

Party_Boxplot <- ggplot(data = mainParties) +
  geom_boxplot(mapping = aes(x = Party, y = Total_Con)) + 
  labs(title = "Total Contributions split by Main Parties", x = "Party", y = "Total Contributions") +
  facet_wrap(~Winner) +
  scale_y_continuous(trans = "log10")
print(Party_Boxplot)
ggsave("../pictures/Total_Contributions_vs_Party.png", Party_Boxplot, height = 10, width = 8)


# Remove two empty columns
Incumbent_Boxplot <- ggplot(data = house[which(house$Seat_Status!=''),]) +
  geom_boxplot(mapping = aes(x = Seat_Status, y = Total_Con)) + 
  labs(title = "Total Contributions split by Seat Status", x = "Seat Status", y = "Total Contributions") +
  #facet_wrap(~Winner) +
  scale_y_continuous(trans = "log10")
print(Incumbent_Boxplot)
ggsave("../pictures/Total_Contributions_vs_Seat_Status.png", Incumbent_Boxplot, height = 10, width = 8)



