# COVID-19 Clustering for European Countries
# 3 Clusters + PCA + Country Lists

#Load Libraries
library(readr)
library(dplyr)
library(cluster)
library(factoextra)
library(FactoMineR)

#Load Data
EUcoviddata <- read_csv("C:\\Users\\Yuvan Rahul\\Downloads\\EUcoviddata (1).csv")

#Aggregate by country to create clustering features
#avg_cases = average daily cases
#avg_deaths = average daily deaths
#cases_per_100k = total cases per 100,000 people
#deaths_per_100k = total deaths per 100,000 people

feat <- EUcoviddata %>%
  group_by(countriesAndTerritories, popData2020) %>%
  summarise(
    avg_cases       = mean(cases,  na.rm = TRUE),
    avg_deaths      = mean(deaths, na.rm = TRUE),
    cases_per_100k  = sum(cases,   na.rm = TRUE) / first(popData2020) * 100000,
    deaths_per_100k = sum(deaths,  na.rm = TRUE) / first(popData2020) * 100000,
    .groups = "drop"
  )

#Create numeric matrix for clustering
X <- feat %>%
  select(avg_cases, avg_deaths, cases_per_100k, deaths_per_100k) %>%
  scale()

# Elbow Method to determine optimal number of clusters
set.seed(123)

wss <- vector()

# Compute within-cluster sum of squares for k = 1 to 10
for (k in 1:10) {
  km <- kmeans(X, centers = k, nstart = 50)
  wss[k] <- km$tot.withinss
}

# Plot elbow curve
plot(
  1:10, wss,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters (k)",
  ylab = "Total within-cluster sum of squares",
  main = "Elbow Method for Choosing Optimal k"
)



#K-means with exactly 3 clusters
set.seed(123)
km3 <- kmeans(X, centers = 3, nstart = 100)

feat$cluster <- factor(km3$cluster)

#List of countries in each cluster
cluster_countries <- feat %>%
  arrange(cluster, countriesAndTerritories) %>%
  group_by(cluster) %>%
  summarise(
    countries = paste(countriesAndTerritories, collapse = ", "),
    .groups = "drop"
  )

print(cluster_countries)

#PCA for visualization
pca_res <- PCA(X, graph = FALSE)

pca_df <- data.frame(pca_res$ind$coord, cluster = feat$cluster)

#PCA plot with clusters:
fviz_pca_ind(
  pca_res,
  geom = "point",
  habillage = feat$cluster,
  addEllipses = TRUE,
  title = "COVID-19 Clustering of European Countries (3 Clusters)"
) +
  xlab("Infection intensity (cases variables)") +
  ylab("Mortality severity (death variables)")