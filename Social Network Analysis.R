# Title: Social Network Analysis
# Team 2: Maryna Pavlenko, Daniel Lee, Yuzi Liu, Ziwei Liang

# Q1. Delete products that are not books from "products" and "copurchase" files. 
options(repos="https://cran.rstudio.com" )
getwd()
products <- read.csv("products.csv")
copurchase <- read.csv("copurchase.csv")
products1 <- products[products$group == "Book", ]
Book <- subset(products1, salesrank <= 150000 & salesrank != -1)
Book$downloads <- NULL
copurchase1 <- subset(copurchase, Source %in% Book$id & Target %in% Book$id)

# Q2. Create a variable named in-degree, to show how many "Source" products people who buy "Target" products buy; 
# i.e. how many edges are to the focal product in "co-purchase" network.
library("igraph")
network <- graph.data.frame(copurchase1, directed = T)
indegree <- degree(network, mode = 'in')

# Q3. Create a variable named out-degree, to show how many "Target" products people who buy "Source" product also buy;
# i.e., how many edges are from the focal product in "co-purchase" network.
outdegree <- degree(network, mode = 'out')

# Q4. Pick up one of the products (in case there are multiple) with highest degree (in-degree + out-degree), 
# and find its subcomponent, i.e., all the products that are connected to this focal product.
alldegree <- degree(network, mode = 'total')
max(alldegree)
which(alldegree==53)
sub_all <- subcomponent(network, "33",'all')


# Q5. Visualize the subcomponent using iGraph, trying out different colors, node and edge sizes and layouts, so that 
# the result is most appealing. Find the diameter, and color the nodes along the diameter. Provide your insights from the visualizations.
newgraph<-subgraph(network,sub_all)
diameter(newgraph, directed=F, weights=NA)
diam <- get_diameter(newgraph, directed=T)
as.vector(diam)
V(newgraph)$color<-"skyblue"
V(newgraph)$size<-2
V(newgraph)[diam]$color<-"darkblue"
V(newgraph)[diam]$size<-5
par(mar=c(.1,.1,.1,.1))
plot.igraph(newgraph,
            vertex.label=NA,
            edge.arrow.size=0.00001,
            layout=layout.kamada.kawai)

# The graph demonstrates 904 vertices, 904 book ids which are connected with each other.  Some of them have stronger connections like clusters in the middle with short edges, some of them have weaker ties which are nodes on the edges of the plot. Diameter shows the longest geodesic distance between two vertices. We have defined node attributes for the diameter that is why it is plotted with larger nodes of dark blue color. In our visualization, the distance is 41 and there are 10 vertices in the diameter (37895 27936 21584 10889 11080 14111 4429  2501  3588  6676) which have the longest distance between them.
# 
# Q6. Compute various statistics about this network (i.e., subcomponent), including degree distribution, density, and 
# centrality (degree centrality, closeness centrality and between centrality), hub/authority scores, etc. Interpret your results.degree distribution (in-degree, out-degree and both).
deg1 <- degree(newgraph, mode = "all")
deg.dist.all <- degree_distribution(newgraph, cumulative=T, mode="all")
centr_degree(newgraph,mode="all",normalized=T)
plot( x=0:max(deg1), y=1-deg.dist.all, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

deg2 <- degree(newgraph, mode = "in")
deg.dist.in <- degree_distribution(newgraph, cumulative=T, mode="in")
centr_degree(newgraph,mode="in",normalized=T)
plot( x=0:max(deg2), y=1-deg.dist.in, pch=19, cex=1.2, col="blue",      
      xlab="Degree", ylab="Cumulative Frequency")

deg3 <- degree(newgraph, mode = "out")
deg.dist.out <- degree_distribution(newgraph, cumulative=T, mode="out")
centr_degree(newgraph,mode="out",normalized=T)
plot( x=0:max(deg3), y=1-deg.dist.out, pch=19, cex=1.2, col="red", 
      xlab="Degree", ylab="Cumulative Frequency")

# The degree of a node refers to the number of ties associated with a node. Deg1 measures all the ties going in and out. In our case, deg1 gives an output of books ids and their corresponding total number of links going to and from the focal product in the network.Degree distribution deg.dist.all  shows the cumulative frequency of nodes with degree Deg1. The centralization function centr_degree returned res - vertex centrality, centralization of 0.02794058, and theoretical_max  1630818 - maximum centralization score for the newgraph. 
# Deg2 measures all the ties going in. In our case, deg2 gives an output of books ids and their corresponding total number of links going to the focal product in the network.Degree distribution deg.dist.in shows the cumulative frequency of nodes with degree Deg2. The centralization function centr_degree returned res - vertex centrality, centralization of 0.05725629, and theoretical_max  816312- maximum centralization score for the newgraph.Deg3 measures all the ties going out. In our case, deg3 gives an output of books ids and their corresponding total number of links going from the focal product.Degree distribution deg.dist.out  shows the cumulative frequency of nodes with degree Deg3. The centralization function centr_degree returned res - vertex centrality, centralization of 0.002992728,and theoretical_max  816312- maximum centralization score for the newgraph.We can observe that maximum centralization score for indegree and outdegree ties is the same 816312.
edge_density(newgraph, loops=F)

# Density is the proportion of present edges from all possible ties. For our network, density is 0.001436951.
closeness<-closeness(newgraph, mode="all", weights=NA)
centr_clo(newgraph,mode="all",normalized=T)

# Closeness refers to how connected a node is to its neighbors. It is inverse of the node's average geodesic distance to others. The higher values are the less centrality is in the network. Besides that, centralization function centr_clo returned centralization score of 0.1074443 and theoretical max of 451.2499 for the graph.
betweenness<-betweenness(newgraph, directed=T, weights=NA)
edge_betweenness(newgraph,directed = T, weights=NA)
centr_betw(newgraph,directed = T,normalized=T)

# Betweenness is the number of shortest paths between two nodes that go through each node of interest. Betweenness
# calculates vertex betweenness, edge_betweenness calculates edge betweenness. Vertix 2501 has a high betweenness 298 which indicates that it has a considerable influence within a network due to its control over information passing between others. Its removal from the network may disrupt communications between other vertices because it lies on the largest number of paths. Its centralization function also returns centralization score of 0.0003616307 and theoretical max of 735498918.
hub<-hub.score(newgraph)$vector
auth<-authority.score(newgraph)$vector
plot(newgraph,
     vertex.size=hub*5,
     main = 'Hubs',
     vertex.color.hub = "skyblue",
     vertex.label=NA,
     edge.arrow.size=0.00001,
     layout = layout.kamada.kawai)
plot(newgraph,
     vertex.size=auth*10,
     main = 'Authorities',
     vertex.color.auth = "skyblue",
     vertex.label=NA,
     edge.arrow.size=0.00001,
     layout = layout.kamada.kawai)

# Hubs refer to max outgoing links whereas Authorities refer to max incoming links. 
# Hubs are pages that are not necessarily itself relevant to the query string, they are pages good at pointing at things that maybe relevant. Given a query to a search engine: Root is set of highly relevant web pages(e.g. pages that contain the query string), i.e., the potential authorities. Potential hubs, on the other hand , is all the pages that link to a page in root.


# Q7. Create a group of variables containing the information of neighbors that "point to" focal products. The variables include:
# a.	Neighbors' mean rating (nghb_mn_rating), 
# b.	Neighbors' mean salesrank (nghb_mn_salesrank), 
# c.	Neighbors' mean number of reviews (nghb_mn_review_cnt)
sub_all1 <-as_ids(sub_all)
Book$id <- as.character(Book$id)
filtered <- Book[Book$id %in% sub_all1,]
copurchase$Target <- as.character(copurchase$Target)
library(dplyr)
mean_values <- inner_join(copurchase, filtered, by = c("Target"="id")) %>%
  group_by(Target) %>%
  summarise(nghb_mn_rating = mean(rating),
            nghb_mn_salesrank = mean(salesrank),
            nghb_mn_review_cnt = mean(review_cnt))

# Q8. Include the variables (taking logs where necessary) created in Parts 2-6 above into the "products" information and fit a Poisson regression to predict salesrank of all the books in this subcomponent using products' own information and their neighbor's information. Provide an interpretation of your results. 
in_degree1 <- as.data.frame(deg2)
in_degree1 <- cbind(id = rownames(in_degree1), in_degree1)
out_degree1 <- as.data.frame(deg3)
out_degree1 <- cbind(id = rownames(out_degree1), out_degree1)

closeness1 <- as.data.frame(closeness)
closeness1 <- cbind(id = rownames(closeness1), closeness1)
betweenness1 <- as.data.frame(betweenness)
betweenness1 <- cbind(id = rownames(betweenness1), betweenness1)

hub_score2 <- as.data.frame(hub)
hub_score2 <- cbind(id = rownames(hub_score2), hub_score2)
authority_score2 <- as.data.frame(auth)
authority_score2 <- cbind(id = rownames(authority_score2), authority_score2)
library("sqldf")
newdf1 <- sqldf("SELECT hub_score2.id,hub, betweenness, auth, closeness, deg2, deg3 
                      FROM hub_score2, betweenness1, authority_score2, closeness1, in_degree1, out_degree1
                      WHERE hub_score2.id = betweenness1.id 
                      and hub_score2.id = authority_score2.id
                      and hub_score2.id = closeness1.id
                      and hub_score2.id = in_degree1.id
                      and hub_score2.id = out_degree1.id")

newdf2 <- sqldf("SELECT Book.id, Book.review_cnt, Book.rating, hub, betweenness, auth, closeness, 
                          deg2, deg3, Book.salesrank
                FROM Book, newdf1,mean_values 
                WHERE newdf1.id = Book.id
                and Book.id=mean_values.Target")

summary(salesrank_prediction<- glm(salesrank ~ review_cnt + rating + hub + betweenness + 
                                       auth + closeness + deg2 + deg3, family="poisson", data=newdf2))


# From the regression result, we can see that all coefficients are highly significant with small p-value < 0.01.
# The coefficient for review_cnt is -3.640e-03. This means that the expected log count of salesrank for a one-unit increase in review_cnt is -3.640e-03. Since lower salesrank means higher sales, increase in reviews decrease salesrank, and increase sales.
# The coefficient for rating is -2.219e-02. This means that the expected log count for a one-unit increase in rating is -2.219e-02. Since lower salesrank means higher sales, increase in rating decrease salesrank, and increase sales.
# The coefficient for hub is 1.351e-01. This means that the expected log count for a one-unit increase in hub is 1.351e-01. Since lower salesrank means higher sales, increase in hub score increase salesrank, and decrease sales.
# The coefficient for betweenness is -5.204e-04. This means that the expected log count for a one-unit increase in betweenness is -5.204e-04. Since lower salesrank means higher sales, increase in betweenness decrease salesrank, and increase sales.
# The coefficient for auth is 3.986e-01. This means that the expected log count for a one-unit increase in auth is 3.986e-01. Since lower salesrank means higher sales, increase in authority score increase salesrank, and decrease sales.
# The coefficient for closeness is -6.295e+01. This means that the expected log count for a one-unit increase in closeness is -6.295e+01. Since lower salesrank means higher sales, increase in closeness decrease salesrank, and increase sales.
# The coefficient for deg2 is -1.644e-03. This means that the expected log count for a one-unit increase in deg2 is -1.644e-03. Since lower salesrank means higher sales, increase in in-degree decrease salesrank, and increase sales.
# The coefficient for deg3 is 4.976e-02. This means that the expected log count for a one-unit increase in deg3 is 4.976e-02. Since lower salesrank means higher sales, increase in out-degree increase salesrank, and decrease sales.
