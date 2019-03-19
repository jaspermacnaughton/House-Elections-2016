library(tidyverse)
library(ggfortify)

load("campaignFunding.Rda")



campaignTrimVars <- c("Ind_Itemized_Con", "Ind_Unitemized_Con", "Party_Con", "Com_Con", "Self_Con", "Operating_Exp", "Total_Loans")

congressPCA <- select(congressional,one_of(campaignTrimVars))

# compute principle components
pca.campaign <- prcomp(congressPCA,scale=TRUE)
pr.var <- pca.campaign$sdev^2
pve <- pr.var/sum(pr.var)
campaign.pca.plt <- autoplot(pca.campaign,loadings=TRUE,loadings.label=TRUE, scale = 0) +
  ggtitle("PCA of Campaign Data")
print(campaign.pca.plt)
ggsave("congressionalPCA.png", plot = campaign.pca.plt,width = 6,height = 4)


