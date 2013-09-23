My Facebook network
===================
Build off of the first assignment from the Coursera Social Network Analysis course. Below are part of the instructions from the first assignment from that course. After downloading my Facebook network data, I examine centrality and communities in the network using R version 3.0.1 (2013-05-16) with the `igraph` package. I used the RStudio IDE with Knitr to generate this HTML file. This analysis was run on 2013-09-22 06:31:38.


Getting Facebook network data
-----------------------------
The following are instructions for downloading my own GML file is below. These instructions were taken from assignment 1 from the fall 2012 Coursera Social Network Analysis course.

> In order to get your own network, complete the following steps:
>
> * Go to [http://snacourse.com/getnet](http://snacourse.com/getnet)
> * Choose which user data (e.g. "wall posts count") you'd like to include, for this assignment no additional data is necessary, but whatever you do download, you can visualize/analyze (profile age rank: oldest profile = highest value, declarative intensity (length of text in fields like activities, books, etc.)
> * Save the .gml file and load it into Gephi using "File -> Open...".

Read the GML file.

```r
require(igraph, quietly = TRUE)
G <- read.graph(file = "ChanFacebook.gml", format = "gml")
```

My GML file was last modified on 2013-09-21 04:25:59. As of that date, there are 336 nodes.

Create first name and initials vectors from names. I'll want to use these to label nodes when plotting the network.

```r
listName <- strsplit(V(G)$label, " ")
nameF <- sapply(listName, head, 1)
nameL <- sapply(listName, tail, 1)
initF <- substr(nameF, 1, 1)
initL <- substr(nameL, 1, 1)
initials <- paste0(initF, initL)
label <- nameF
head(label)
```

```
## [1] "Christopher" "Kati"        "Carla"       "Kevin"       "Ben"        
## [6] "Colleen"
```



Analysis questions
------------------
I'm interested in two questions:
* What communities exist in my network?
* Who are the people who are "bridges" across communities?


Centrality
----------
Here's a link to [Wikipedia](http://en.wikipedia.org/wiki/Centrality) for some background information on centrality.

Calculate **degree** centrality. This ends up not being too useful. Degree is really just a measure of how connected someone is. In Facebook terms, it's who has the most friends within my network.

```r
deg <- degree(G)
summary(deg)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0     4.0    10.0    12.9    19.0    62.0
```

The median degree, or number of friends, was 10. The highest number of friends a person in my network has was 62.

Even though I don't want to focus on degree centrality, let's see who are the most connected people in my network.

```r
require(xtable, quietly = TRUE)
lim <- sort(deg, decreasing = TRUE)[round(vcount(G) * 0.05)]
top <- data.frame(Name = V(G)$label, Degree = deg)
top <- subset(top, deg >= lim)
top <- top[order(top$Degree, decreasing = TRUE), ]
print(xtable(top, digits = 0), type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 3.0.1 by xtable 1.7-1 package -->
<!-- Sun Sep 22 06:31:38 2013 -->
<TABLE border=1>
<TR> <TH> Name </TH> <TH> Degree </TH>  </TR>
  <TR> <TD> Glenn Tachiyama </TD> <TD align="right"> 62 </TD> </TR>
  <TR> <TD> Yassine Diboun </TD> <TD align="right"> 61 </TD> </TR>
  <TR> <TD> Rainshadow Running </TD> <TD align="right"> 60 </TD> </TR>
  <TR> <TD> Charles Replogle </TD> <TD align="right"> 51 </TD> </TR>
  <TR> <TD> Terry Sentinella </TD> <TD align="right"> 44 </TD> </TR>
  <TR> <TD> Sarah Duncan </TD> <TD align="right"> 43 </TD> </TR>
  <TR> <TD> Todd Janssen </TD> <TD align="right"> 43 </TD> </TR>
  <TR> <TD> Cheri Redwine </TD> <TD align="right"> 42 </TD> </TR>
  <TR> <TD> Steve Walters </TD> <TD align="right"> 41 </TD> </TR>
  <TR> <TD> Capitol Peak Ultras </TD> <TD align="right"> 41 </TD> </TR>
  <TR> <TD> Jeannie Horton </TD> <TD align="right"> 38 </TD> </TR>
  <TR> <TD> Jason Leman </TD> <TD align="right"> 37 </TD> </TR>
  <TR> <TD> Matt Hagen </TD> <TD align="right"> 37 </TD> </TR>
  <TR> <TD> Eric Barnes </TD> <TD align="right"> 37 </TD> </TR>
  <TR> <TD> Paul Nelson </TD> <TD align="right"> 36 </TD> </TR>
  <TR> <TD> Samantha de la Vega </TD> <TD align="right"> 36 </TD> </TR>
  <TR> <TD> Anne Crispino-Taylor </TD> <TD align="right"> 36 </TD> </TR>
   </TABLE>


Calculate **closeness** centrality. Closeness is a measure of how many steps are required to access every other node. It's a measure of how close a node is to all the action. A person with high closeness, however, doesn't necessarily have to have very many friends or be in between relationship.

```r
close <- closeness(G)
summary(close)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 8.88e-06 1.03e-04 1.06e-04 9.83e-05 1.07e-04 1.10e-04
```

Again, I don't want to focus on closeness centrality since it's not really what I'm after in this analysis, so I won't say anything more about it.

Calculate **betweenness** centrality. I'm going to focus on betweenness since it's going to get after one of my analysis questions, who are the "bridges" between people and communities? Betweenness is a measure of how often a node is in the pathway between two other nodes. I.e., a person with high betweenness can be a key player in introducing a large group of friends to another large group of friends. Such a person doesn't necessarily have to have a large number of friends themselves. But they could be in a unique position of influence in the network.

Plot a histogram of betweenness scores. 

```r
require(ggplot2, quietly = TRUE)
btwn <- betweenness(G)
summary(btwn)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0       1      29     531     396   16600
```

```r
qplot(btwn, binwidth = 1000)
```

![plot of chunk Betweenness](figure/Betweenness.png) 

The absolute value of the scores don't mean much. But their relative values tell the story. There are a number of people with extremely high betweenness scores. Who are these people? List the 25 of people with the highest betweeness scores.

```r
rank <- length(btwn) - rank(btwn) + 1
top <- data.frame(Rank = rank, Name = V(G)$label, Betweenness = btwn)
lim <- sort(btwn, decreasing = TRUE)[round(vcount(G) * 0.05)]
# top <- subset(top, btwn >= lim)
top <- subset(top, rank <= 25)
top <- top[order(top$Betweenness, decreasing = TRUE), ]
print(xtable(top, digits = 0), type = "html")
```

<!-- html table generated in R 3.0.1 by xtable 1.7-1 package -->
<!-- Sun Sep 22 06:31:39 2013 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Rank </TH> <TH> Name </TH> <TH> Betweenness </TH>  </TR>
  <TR> <TD align="right"> 60 </TD> <TD align="right"> 1 </TD> <TD> Cat Buckley </TD> <TD align="right"> 16562 </TD> </TR>
  <TR> <TD align="right"> 291 </TD> <TD align="right"> 2 </TD> <TD> Erin 'Davis' Thekkedom </TD> <TD align="right"> 8359 </TD> </TR>
  <TR> <TD align="right"> 76 </TD> <TD align="right"> 3 </TD> <TD> Marc Baumgartner </TD> <TD align="right"> 7098 </TD> </TR>
  <TR> <TD align="right"> 138 </TD> <TD align="right"> 4 </TD> <TD> Heather Fowler </TD> <TD align="right"> 6951 </TD> </TR>
  <TR> <TD align="right"> 101 </TD> <TD align="right"> 5 </TD> <TD> Julie Honse </TD> <TD align="right"> 6069 </TD> </TR>
  <TR> <TD align="right"> 277 </TD> <TD align="right"> 6 </TD> <TD> Evan Freeman </TD> <TD align="right"> 5757 </TD> </TR>
  <TR> <TD align="right"> 49 </TD> <TD align="right"> 7 </TD> <TD> Carrie Kosky Yerton </TD> <TD align="right"> 5043 </TD> </TR>
  <TR> <TD align="right"> 59 </TD> <TD align="right"> 8 </TD> <TD> Yassine Diboun </TD> <TD align="right"> 4672 </TD> </TR>
  <TR> <TD align="right"> 19 </TD> <TD align="right"> 9 </TD> <TD> Bekah Wolf </TD> <TD align="right"> 4609 </TD> </TR>
  <TR> <TD align="right"> 180 </TD> <TD align="right"> 10 </TD> <TD> Micah Wiese </TD> <TD align="right"> 4289 </TD> </TR>
  <TR> <TD align="right"> 81 </TD> <TD align="right"> 11 </TD> <TD> Lucia Longoria </TD> <TD align="right"> 3857 </TD> </TR>
  <TR> <TD align="right"> 183 </TD> <TD align="right"> 12 </TD> <TD> Christopher J. Barker </TD> <TD align="right"> 3737 </TD> </TR>
  <TR> <TD align="right"> 77 </TD> <TD align="right"> 13 </TD> <TD> Kellyn Christison </TD> <TD align="right"> 3533 </TD> </TR>
  <TR> <TD align="right"> 232 </TD> <TD align="right"> 14 </TD> <TD> Heidi Baney </TD> <TD align="right"> 3445 </TD> </TR>
  <TR> <TD align="right"> 174 </TD> <TD align="right"> 15 </TD> <TD> Allison Cox </TD> <TD align="right"> 3332 </TD> </TR>
  <TR> <TD align="right"> 32 </TD> <TD align="right"> 16 </TD> <TD> Giovanni Bencomo </TD> <TD align="right"> 3268 </TD> </TR>
  <TR> <TD align="right"> 155 </TD> <TD align="right"> 17 </TD> <TD> Annie Bencomo </TD> <TD align="right"> 3249 </TD> </TR>
  <TR> <TD align="right"> 226 </TD> <TD align="right"> 18 </TD> <TD> Ann Wallace </TD> <TD align="right"> 2953 </TD> </TR>
  <TR> <TD align="right"> 116 </TD> <TD align="right"> 19 </TD> <TD> Kimberly Livesay </TD> <TD align="right"> 2815 </TD> </TR>
  <TR> <TD align="right"> 119 </TD> <TD align="right"> 20 </TD> <TD> Rebecca Ettlinger Waltz </TD> <TD align="right"> 2715 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD align="right"> 21 </TD> <TD> Colleen Schoonover </TD> <TD align="right"> 2420 </TD> </TR>
  <TR> <TD align="right"> 236 </TD> <TD align="right"> 22 </TD> <TD> Jeannie Horton </TD> <TD align="right"> 2282 </TD> </TR>
  <TR> <TD align="right"> 37 </TD> <TD align="right"> 23 </TD> <TD> David M. Cain </TD> <TD align="right"> 2138 </TD> </TR>
  <TR> <TD align="right"> 126 </TD> <TD align="right"> 24 </TD> <TD> Nedra Howsden Rezinas </TD> <TD align="right"> 2077 </TD> </TR>
  <TR> <TD align="right"> 239 </TD> <TD align="right"> 25 </TD> <TD> Roger Chou </TD> <TD align="right"> 1946 </TD> </TR>
   </TABLE>


Plot the association between degree centrality and betweenness centrality. See if there are any highly influential people (betweenness) who also have a high number of friends (degree).

```r
rsq <- format(cor(deg, btwn)^2, digits = 3)
cntrl <- data.frame(deg, btwn, close)
ggplot(cntrl, aes(x = deg, y = btwn)) + geom_jitter(alpha = 1/2) + scale_y_log10() + 
    labs(x = "Degree", y = "Betweenness") + annotate("text", label = paste("R-sq =", 
    rsq), x = +Inf, y = 1, hjust = 1)
```

![plot of chunk AssociationCentrality](figure/AssociationCentrality.png) 



Communities
-----------
Find communities using the edge betweenness algorithm.

```r
C <- edge.betweenness.community(G)
sizes(C)
```

```
## Community sizes
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 
## 17 95 75 63 15 13 28  1  2  3  2  3  2  1  4  2  1  3  1  1  1  1  1  1
```

```r
commThreshold <- 6
nComm <- length(sizes(C)[sizes(C) > commThreshold])
```

There are 7 clearly large communities in my network. *Large* is defined as having more than 6 people.


Visualize the network
---------------------

Scale the size of a node's plotting symbol according to its betweenness centrality score. Scaling is by percentile (50%, 75%, 90%, 95%, 99%).

```r
q <- quantile(btwn, probs = c(0, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
size <- cut(btwn, breaks = q, include.lowest = TRUE, dig.lab = 5)
summary(size)
```

```
##      [0,29.207] (29.207,395.77] (395.77,1406.3] (1406.3,3027.3] 
##             168              84              50              17 
## (3027.3,6642.2]  (6642.2,16562] 
##              13               4
```

```r
V(G)$size <- unclass(size)
```


Label the nodes with the top betweenness centrality within each community. This will help make sense of who are the important links between each community.

```r
labNode <- nameF
labNode[labNode == "Marc"] <- paste(labNode[labNode == "Marc"], initL[labNode == 
    "Marc"])  # Since there's 2 Marcs, paste their last initial to the label
rankWithin <- ave(btwn, C$membership, FUN = function(x) length(x) - rank(x) + 
    1)
pctWithin <- 7
topWithin <- round(pctWithin/100 * sizes(C))
message(sprintf("The top %3.1f%% within each community leads to labelling %2.0f nodes.", 
    pctWithin, sum(topWithin)))
```

```
## The top 7.0% within each community leads to labelling 21 nodes.
```

```r
# Need to find a more elegant way to do this
isLabelled1 <- C$membership == 1 & rankWithin <= topWithin[1]
isLabelled2 <- C$membership == 2 & rankWithin <= topWithin[2]
isLabelled3 <- C$membership == 3 & rankWithin <= topWithin[3]
isLabelled4 <- C$membership == 4 & rankWithin <= topWithin[4]
isLabelled5 <- C$membership == 5 & rankWithin <= topWithin[5]
isLabelled6 <- C$membership == 6 & rankWithin <= topWithin[6]
isLabelled7 <- C$membership == 7 & rankWithin <= topWithin[7]
isLabelled <- isLabelled1 | isLabelled2 | isLabelled3 | isLabelled4 | isLabelled5 | 
    isLabelled6 | isLabelled7
labNode[!isLabelled] <- NA
# labNode[C$membership != 5 & nameF != 'Cat'] <- NA
```


Create a function for `plot.igraph`.

```r
P <- function(vertex.label.cex=1) {
  set.seed(seed)
  plot(C, G,
       vertex.label=labNode, 
       vertex.label.color="black",
       vertex.label.dist=1/4,
       vertex.label.family="sans",
       vertex.label.cex=vertex.label.cex, 
       vertex.frame.color=NA,
       colbar=node.color,
       mark.groups=NA, 
       mark.col=NA,
       mark.border=palette,
       edge.color=c("gray", "red")[crossing(C, G)+1],
       edge.width=1/2
       )
}
```


Show low resolution plot for HTML file. Set the random number seed so when a high resolution version is created, it will have the same layout as this version.

```r
id <- communities(C)[1:nComm]
require(RColorBrewer, quietly = TRUE)
palette <- brewer.pal(nComm + 1, "Set1")
node.color <- c(palette[1:nComm], rep(palette[nComm + 1], max(C$membership) - 
    nComm))
seed <- Sys.time()
P(vertex.label.cex = 1)
```

![plot of chunk NetworkVisualizationLowRes](figure/NetworkVisualizationLowRes.png) 

Create high resolution PNG file for importing into presentations.

```r
P(vertex.label.cex = 1/2)
```


Just eyeballing these communities, here's what I'm seeing:
* The green community is a core group of friends
  * This is a diverse group, reflecting the many ways I'm connected to them
  * One of the subgroups that's connected to the rest of the blue community through Cat isn't really a group of friends; they're folks on Cat's side of the family
* The purple community is [Union High School](https://www.facebook.com/pages/Union-High-School/127917213038)
  * Within the Union community, the smaller teacher subgroup is apparent and distinct from the larger student subgroup
  * There are a few [Mountain View](https://www.facebook.com/GoThunder.org) students in this community who are connected to Union people
  * The only connections between the Union community and the rest of my network is through Erin and Heather; without either, my Union community would be totally separated from the rest of my network
* The blue community is running friends, including [Marathon Maniacs](http://www.marathonmaniacs.com/), Trail Factor, Animal Athletics, and BananaSluggers
* The brown community is the [Concordia MATE](http://www.cu-portland.edu/coe/graduate/mat/) cohort
* The red community is family
* The orange community is a secondary group of Portland friends which have ties to the OHSU BMU
* The yellow community is the OHSU BICC group
  * 2 subgroups are the EPC and DMICE
* There are a few people who don't fit in with a larger community within my network (pink nodes)

It's obvious from the plot and the betweenness scores that Cat and Erin are crucial bridges tying a few large communities together.

Create interactive 3-D high resolution.

```r
l <- layout.fruchterman.reingold(G, dim=3)
rglplot(G,
        layout=l,
        vertex.label=labNode,
        vertex.label.color="black",
        vertex.label.dist=1/4,
        vertex.label.family="sans",
        vertex.label.cex=1/2, 
        vertex.frame.color=NA,
        edge.color=c("gray", "red")[crossing(C, G)+1],
        edge.width=1/8
        )
```

```
## Loading required package: rgl
```

