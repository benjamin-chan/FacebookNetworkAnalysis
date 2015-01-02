# My Facebook network
Benjamin Chan (https://www.facebook.com/benjamin.ks.chan)  
  
Build off of the first assignment from the Coursera Social Network Analysis course. Below are part of the instructions from the first assignment from that course. After downloading my Facebook network data, I examine centrality and communities in the network using R version 3.1.1 (2014-07-10) with the `igraph` package. I used the RStudio IDE with Knitr to generate this HTML file. This analysis was run on 2015-01-02 05:51:21.


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
setwd("~/GitHub repositories/FacebookNetworkAnalysis")
require(igraph, quietly=TRUE)
G <- read.graph(file="ChanFacebook.gml", format="gml")
```

My GML file was last modified on 2015-01-02 04:46:21. As of that date, there are 374 nodes.

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
## [1] "Christopher" "David"       "Kati"        "Carla"       "Kevin"      
## [6] "Ben"
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
##       0       5      11      16      22      93
```

The median degree, or number of friends, was 11. The highest number of friends a person in my network has was 93.

Even though I don't want to focus on degree centrality, let's see who are the most connected people in my network.


```r
require(xtable, quietly=TRUE)
lim <- sort(deg, decreasing=TRUE)[round(vcount(G) * 0.05)]
top <- data.frame("Name"=V(G)$label, "Degree"=deg)
top <- subset(top, deg >= lim)
top <- top[order(top$Degree, decreasing=TRUE),]
print(xtable(top, digits=0), type="html", include.rownames=FALSE)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Fri Jan 02 05:51:21 2015 -->
<table border=1>
<tr> <th> Name </th> <th> Degree </th>  </tr>
  <tr> <td> Yassine Diboun </td> <td align="right"> 93 </td> </tr>
  <tr> <td> Todd Janssen </td> <td align="right"> 78 </td> </tr>
  <tr> <td> Glenn Tachiyama </td> <td align="right"> 75 </td> </tr>
  <tr> <td> Jason Leman </td> <td align="right"> 72 </td> </tr>
  <tr> <td> Charles Replogle </td> <td align="right"> 67 </td> </tr>
  <tr> <td> Paul Nelson </td> <td align="right"> 66 </td> </tr>
  <tr> <td> Willie McBride </td> <td align="right"> 65 </td> </tr>
  <tr> <td> Trevor Hostetler </td> <td align="right"> 63 </td> </tr>
  <tr> <td> Sarah Duncan </td> <td align="right"> 62 </td> </tr>
  <tr> <td> Jennifer Love </td> <td align="right"> 62 </td> </tr>
  <tr> <td> Samantha de la Vega </td> <td align="right"> 60 </td> </tr>
  <tr> <td> Kevin Karr </td> <td align="right"> 59 </td> </tr>
  <tr> <td> Anne Crispino-Taylor </td> <td align="right"> 59 </td> </tr>
  <tr> <td> Renee Seker </td> <td align="right"> 56 </td> </tr>
  <tr> <td> Larry Stephens </td> <td align="right"> 56 </td> </tr>
  <tr> <td> Cheri Redwine </td> <td align="right"> 55 </td> </tr>
  <tr> <td> Sean Meissner </td> <td align="right"> 55 </td> </tr>
  <tr> <td> T.J. Ford </td> <td align="right"> 55 </td> </tr>
  <tr> <td> Moe Codino </td> <td align="right"> 54 </td> </tr>
   </table>

Calculate **closeness** centrality. Closeness is a measure of how many steps are required to access every other node. It's a measure of how close a node is to all the action. A person with high closeness, however, doesn't necessarily have to have very many friends or be in between relationship.


```r
close <- closeness(G)
summary(close)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 7.17e-06 8.25e-05 8.42e-05 7.81e-05 8.50e-05 8.72e-05
```

Again, I don't want to focus on closeness centrality since it's not really what I'm after in this analysis, so I won't say anything more about it.

Calculate **betweenness** centrality. I'm going to focus on betweenness since it's going to get after one of my analysis questions, who are the "bridges" between people and communities? Betweenness is a measure of how often a node is in the pathway between two other nodes. I.e., a person with high betweenness can be a key player in introducing a large group of friends to another large group of friends. Such a person doesn't necessarily have to have a large number of friends themselves. But they could be in a unique position of influence in the network.

Plot a histogram of betweenness scores. 


```r
require(ggplot2, quietly=TRUE)
btwn <- betweenness(G)
summary(btwn)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0       1      42     513     415   14900
```

```r
qplot(btwn, binwidth=1000)
```

![plot of chunk Betweenness](./FacebookNetwork_files/figure-html/Betweenness.png) 

The absolute value of the scores don't mean much. But their relative values tell the story. There are a number of people with extremely high betweenness scores. Who are these people? List the 25 of people with the highest betweeness scores.


```r
rank <- length(btwn) - rank(btwn) + 1
top <- data.frame("Rank"=rank, "Name"=V(G)$label, "Betweenness"=btwn)
lim <- sort(btwn, decreasing=TRUE)[round(vcount(G) * 0.05)]
# top <- subset(top, btwn >= lim)
top <- subset(top, rank <= 25)
top <- top[order(top$Betweenness, decreasing=TRUE),]
print(xtable(top, digits=0), type="html")
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Fri Jan 02 05:51:23 2015 -->
<table border=1>
<tr> <th>  </th> <th> Rank </th> <th> Name </th> <th> Betweenness </th>  </tr>
  <tr> <td align="right"> 62 </td> <td align="right"> 1 </td> <td> Cat Buckley </td> <td align="right"> 14905 </td> </tr>
  <tr> <td align="right"> 150 </td> <td align="right"> 2 </td> <td> Heather Fowler </td> <td align="right"> 13894 </td> </tr>
  <tr> <td align="right"> 299 </td> <td align="right"> 3 </td> <td> Evan Freeman </td> <td align="right"> 10927 </td> </tr>
  <tr> <td align="right"> 168 </td> <td align="right"> 4 </td> <td> Annie Bencomo </td> <td align="right"> 8748 </td> </tr>
  <tr> <td align="right"> 82 </td> <td align="right"> 5 </td> <td> Marc Baumgartner </td> <td align="right"> 8019 </td> </tr>
  <tr> <td align="right"> 86 </td> <td align="right"> 6 </td> <td> Lucia Longoria </td> <td align="right"> 4373 </td> </tr>
  <tr> <td align="right"> 61 </td> <td align="right"> 7 </td> <td> Yassine Diboun </td> <td align="right"> 4226 </td> </tr>
  <tr> <td align="right"> 191 </td> <td align="right"> 8 </td> <td> Allison Cox </td> <td align="right"> 3897 </td> </tr>
  <tr> <td align="right"> 107 </td> <td align="right"> 9 </td> <td> Julie Honse </td> <td align="right"> 3714 </td> </tr>
  <tr> <td align="right"> 200 </td> <td align="right"> 10 </td> <td> Christopher J. Barker </td> <td align="right"> 3413 </td> </tr>
  <tr> <td align="right"> 7 </td> <td align="right"> 11 </td> <td> Jeff Waskowiak </td> <td align="right"> 3214 </td> </tr>
  <tr> <td align="right"> 336 </td> <td align="right"> 12 </td> <td> Henry Barrera </td> <td align="right"> 3096 </td> </tr>
  <tr> <td align="right"> 185 </td> <td align="right"> 13 </td> <td> Gregory Gourdet </td> <td align="right"> 3004 </td> </tr>
  <tr> <td align="right"> 179 </td> <td align="right"> 14 </td> <td> Johnny Buell </td> <td align="right"> 2964 </td> </tr>
  <tr> <td align="right"> 224 </td> <td align="right"> 15 </td> <td> Sandy Bacharach </td> <td align="right"> 2744 </td> </tr>
  <tr> <td align="right"> 217 </td> <td align="right"> 16 </td> <td> Fernando Viciconte </td> <td align="right"> 2454 </td> </tr>
  <tr> <td align="right"> 194 </td> <td align="right"> 17 </td> <td> Nadia Khater </td> <td align="right"> 2401 </td> </tr>
  <tr> <td align="right"> 173 </td> <td align="right"> 18 </td> <td> Glenn Tachiyama </td> <td align="right"> 2297 </td> </tr>
  <tr> <td align="right"> 20 </td> <td align="right"> 19 </td> <td> Alex Asselin </td> <td align="right"> 2242 </td> </tr>
  <tr> <td align="right"> 316 </td> <td align="right"> 20 </td> <td> Erin 'Davis' Thekkedom </td> <td align="right"> 2134 </td> </tr>
  <tr> <td align="right"> 127 </td> <td align="right"> 21 </td> <td> Rebecca Ettlinger Waltz </td> <td align="right"> 2129 </td> </tr>
  <tr> <td align="right"> 243 </td> <td align="right"> 22 </td> <td> Darin Swanson </td> <td align="right"> 2091 </td> </tr>
  <tr> <td align="right"> 19 </td> <td align="right"> 23 </td> <td> James Wallace </td> <td align="right"> 2048 </td> </tr>
  <tr> <td align="right"> 22 </td> <td align="right"> 24 </td> <td> Bekah Wolf </td> <td align="right"> 1995 </td> </tr>
  <tr> <td align="right"> 245 </td> <td align="right"> 25 </td> <td> Nancy Lee </td> <td align="right"> 1964 </td> </tr>
   </table>

Plot the association between degree centrality and betweenness centrality. See if there are any highly influential people (betweenness) who also have a high number of friends (degree).


```r
rsq <- format(cor(deg, btwn) ^2, digits=3)
cntrl <- data.frame(deg, btwn, close)
ggplot(cntrl, aes(x=deg, y=btwn)) +
  geom_jitter(alpha=1/2) +
  scale_y_log10() +
  labs(x="Degree", y="Betweenness") +
  annotate("text", label=paste("R-sq =", rsq), x=+Inf, y=1, hjust=1)
```

![plot of chunk AssociationCentrality](./FacebookNetwork_files/figure-html/AssociationCentrality.png) 


Communities
-----------
Find communities using the edge betweenness algorithm.


```r
C <- edge.betweenness.community(G)
sizesOrdered <- sizes(C)[order(sizes(C), decreasing=TRUE)]
sizesOrdered
```

```
## Community sizes
##  6  7 15  4 11  1  5 10 17 13  8  9 18 33  2 19 14 23 25 27 34 41 42  3 12 
## 88 50 38 29 27 18 14 14 12 11  8  5  5  4  3  3  2  2  2  2  2  2  2  1  1 
## 16 20 21 22 24 26 28 29 30 31 32 35 36 37 38 39 40 43 44 45 46 47 48 49 50 
##  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
## 51 52 53 54 
##  1  1  1  1
```

```r
maxComm <- 11
commThreshold <- sizesOrdered[maxComm]
```

There are 11 clearly large communities in my network. *Large* is defined as having more than 8 people.

Set the 12 community as a *junk* community.


```r
C$membership[!(C$membership %in% as.numeric(names(sizesOrdered[1:maxComm])))] <- 999
C$membership <- unclass(factor(C$membership))
table(C$membership)
```

```
## 
##  1  2  3  4  5  6  7  8  9 10 11 12 
## 18 29 14 88 50  8 14 27 11 38 12 65
```


Visualize the network
---------------------

Scale the size of a node's plotting symbol according to its betweenness centrality score. Scaling is by percentile (75%, 90%, 95%, 97.5%, 99%).


```r
q <- quantile(btwn, probs=c(0, 0.75, 0.9, 0.95, 0.975, 0.99, 1))
size <- cut(btwn, breaks=q, include.lowest=TRUE, dig.lab=5)
summary(size)
```

```
##         [0,415]      (415,1279]   (1279,2171.9] (2171.9,3348.1] 
##             280              56              19               9 
## (3348.1,8215.8]  (8215.8,14905] 
##               6               4
```

```r
V(G)$size <- unclass(size)
```

Label the nodes with the top betweenness centrality within each community. This will help make sense of who are the important links between each community.


```r
labNode <- nameF
labNode[labNode == "Marc"] <- paste(labNode[labNode == "Marc"], initL[labNode == "Marc"])  # Since there's 2 Marcs, paste their last initial to the label
rankWithin <- ave(btwn, C$membership, FUN=function(x) length(x) - rank(x) + 1)
pctWithin <- 7
topWithin <- round(pctWithin / 100 * sizes(C))
message(sprintf("The top %3.1f%% within each community leads to labelling %2.0f nodes.", pctWithin, sum(topWithin)))
```

```
## The top 7.0% within each community leads to labelling 28 nodes.
```

```r
# Need to find a more elegant way to do this
isLabelled1  <- C$membership ==  1 & rankWithin <= topWithin[ 1]
isLabelled2  <- C$membership ==  2 & rankWithin <= topWithin[ 2]
isLabelled3  <- C$membership ==  3 & rankWithin <= topWithin[ 3]
isLabelled4  <- C$membership ==  4 & rankWithin <= topWithin[ 4]
isLabelled5  <- C$membership ==  5 & rankWithin <= topWithin[ 5]
isLabelled6  <- C$membership ==  6 & rankWithin <= topWithin[ 6]
isLabelled7  <- C$membership ==  7 & rankWithin <= topWithin[ 7]
isLabelled8  <- C$membership ==  8 & rankWithin <= topWithin[ 8]
isLabelled9  <- C$membership ==  9 & rankWithin <= topWithin[ 9]
isLabelled10 <- C$membership == 10 & rankWithin <= topWithin[10]
isLabelled11 <- C$membership == 11 & rankWithin <= topWithin[11]
isLabelled12 <- C$membership == 12 & rankWithin <= topWithin[12]
isLabelled <- isLabelled1 | isLabelled2 | isLabelled3 | isLabelled4 | isLabelled5 | isLabelled6 | isLabelled7 | isLabelled8 | isLabelled9 | isLabelled10 | isLabelled11 | isLabelled12
# isLabelledOther <- grepl("Cat$|Jackie|Moi", nameF)
# isLabelled <- isLabelled | isLabelledOther
labNode[!isLabelled] <- NA
# labNode[C$membership != 5 & nameF != "Cat"] <- NA
```

Create a function for `plot.igraph`.


```r
P <- function(vertex.label.cex=1, vertex.label.color="#0000007F") {
  set.seed(seed)
  plot(C, G,
       vertex.label=labNode, 
       vertex.label.color=vertex.label.color,
       vertex.label.dist=1/4,
       vertex.label.family="sans",
       vertex.label.cex=vertex.label.cex, 
       vertex.frame.color=NA,
       colbar=node.color,
       mark.groups=NA, 
       mark.col=NA,
       mark.border=palette,
       edge.color=c("#7F7F7F3F", "#FF00003F")[crossing(C, G)+1],
       edge.width=1/2
       )
}
```

Show low resolution plot for HTML file. Set the random number seed so when a high resolution version is created, it will have the same layout as this version.


```r
id <- communities(C)[1:maxComm]
require(RColorBrewer, quietly=TRUE)
palette <- c(brewer.pal(maxComm, "Spectral"), "gray")
node.color <- c(palette[1:maxComm], rep(palette[maxComm + 1], max(C$membership) - maxComm))
seed <- file.info("ChanFacebook.gml")$mtime
P(vertex.label.cex=1)
```

![plot of chunk NetworkVisualizationLowRes](./FacebookNetwork_files/figure-html/NetworkVisualizationLowRes.png) 

Create high resolution PNG file for importing into presentations.


```r
png(filename="NetworkVisualizationHiRes.png", width=12, height=12, units="in", res=600)
P(vertex.label.cex=1/2, vertex.label.color="#000000")
dev.off()
```

```
## pdf 
##   2
```

Create a data frame of the network.


```r
D <- data.frame(name=paste(nameF, nameL), comm=C$membership, isLabelled, btwn, close, deg)
D[order(D$comm, !D$isLabelled), ]
```

```
##                      name comm isLabelled      btwn     close deg
## 30            Judy Kroone    1       TRUE 1.271e+03 8.443e-05  17
## 1        Christopher Chan    1      FALSE 6.521e+00 8.214e-05  13
## 3                 Kati Wu    1      FALSE 7.949e-01 8.214e-05  13
## 16              Eric Chan    1      FALSE 2.652e+00 8.216e-05  16
## 17             Derek Chan    1      FALSE 2.652e+00 8.216e-05  16
## 23    é³ä¸æº é³ä¸æº    1      FALSE 1.590e+00 8.214e-05  14
## 45               Jason Wu    1      FALSE 1.868e+00 8.215e-05  15
## 93             Jeannie Wu    1      FALSE 3.929e-01 8.212e-05  11
## 97            Kiyomi Ueno    1      FALSE 1.111e+00 8.214e-05  13
## 103          Jocelyn Chan    1      FALSE 9.417e+02 8.442e-05  15
## 111              Vania Ng    1      FALSE 3.020e-01 8.212e-05  11
## 116            Chris Sing    1      FALSE 3.020e-01 8.212e-05  11
## 143             Carter Ng    1      FALSE 2.652e+00 8.216e-05  16
## 151             Anna Liao    1      FALSE 1.115e+03 8.443e-05  17
## 178            Brian Chan    1      FALSE 2.000e-01 8.212e-05  10
## 271               Amy Lim    1      FALSE 6.560e+01 8.433e-05   3
## 279          Mable Cheung    1      FALSE 8.867e+02 8.441e-05  14
## 322              Karen Ng    1      FALSE 4.068e-01 8.212e-05  10
## 106          Marc Frommer    2       TRUE 1.211e+03 8.562e-05  33
## 264            Roger Chou    2       TRUE 9.850e+02 8.554e-05  24
## 5           Kevin Douglas    2      FALSE 3.862e+01 8.488e-05  25
## 25          Craig Dickson    2      FALSE 9.431e+01 8.495e-05  31
## 46      Michael Samarelli    2      FALSE 3.468e+02 8.400e-05  11
## 112           Kathy Klass    2      FALSE 6.370e-01 8.365e-05  12
## 133         Juli Danielle    2      FALSE 2.210e+01 8.479e-05  16
## 146         Steve Walters    2      FALSE 4.846e+02 8.511e-05  46
## 165    Christopher Warren    2      FALSE 8.893e+02 8.544e-05  28
## 166            Steven Yee    2      FALSE 9.309e+01 8.473e-05  28
## 183             May Cheng    2      FALSE 5.176e-01 8.361e-05  10
## 184            Matt Hagen    2      FALSE 1.982e+02 8.503e-05  38
## 190          Donald Mukai    2      FALSE 2.607e+00 8.454e-05  11
## 210            Andy Fritz    2      FALSE 1.374e+01 8.461e-05  19
## 215            Bob Martin    2      FALSE 1.027e+01 8.376e-05  20
## 216          Roger Michel    2      FALSE 2.879e+01 8.475e-05  22
## 219             Dave Mari    2      FALSE 2.498e+01 8.460e-05  17
## 227       Patti Krebsbach    2      FALSE 4.814e+01 8.468e-05  25
## 231           Eric Barnes    2      FALSE 1.626e+02 8.496e-05  37
## 239         David Spooner    2      FALSE 2.343e+00 8.394e-05  14
## 246      Terry Sentinella    2      FALSE 3.831e+02 8.503e-05  44
## 258         Ginger Gruber    2      FALSE 1.071e+01 8.400e-05  21
## 275           Alvin Crain    2      FALSE 1.109e+02 8.493e-05  32
## 280              Van Phan    2      FALSE 4.584e+01 8.488e-05  28
## 296      Shannon Willford    2      FALSE 1.053e+01 8.406e-05  15
## 298            Russ Smith    2      FALSE 2.251e+00 8.394e-05  14
## 311          Bill Barmore    2      FALSE 4.954e+00 8.403e-05  16
## 358             Lisa Wood    2      FALSE 2.419e+01 8.470e-05  19
## 371          Stevie Lopez    2      FALSE 8.062e+01 8.491e-05  26
## 62            Cat Buckley    3       TRUE 1.491e+04 8.674e-05  35
## 6              Ben Aldred    3      FALSE 0.000e+00 8.423e-05   2
## 20           Alex Asselin    3      FALSE 2.242e+03 8.595e-05   4
## 38         Kelly McCarthy    3      FALSE 0.000e+00 8.424e-05   3
## 87         Mark Hernandez    3      FALSE 3.257e+02 8.492e-05   3
## 88          Aaron Buckley    3      FALSE 5.914e+01 8.485e-05   8
## 148         Heather Sutch    3      FALSE 0.000e+00 8.422e-05   1
## 169     Elizabeth Buckley    3      FALSE 4.258e+01 8.485e-05   7
## 221          Conn Buckley    3      FALSE 4.916e+01 8.485e-05   7
## 232        Jackie Kersten    3      FALSE 1.600e+03 8.578e-05   5
## 273             Lea Davis    3      FALSE 5.000e-01 8.426e-05   6
## 335            Kelly Anne    3      FALSE 0.000e+00 8.422e-05   1
## 365       Setsuko Buckley    3      FALSE 0.000e+00 8.425e-05   5
## 366         Yumin Buckley    3      FALSE 0.000e+00 8.425e-05   5
## 7          Jeff Waskowiak    4       TRUE 3.214e+03 8.561e-05  30
## 61         Yassine Diboun    4       TRUE 4.226e+03 8.686e-05  93
## 82       Marc Baumgartner    4       TRUE 8.019e+03 8.619e-05  32
## 173       Glenn Tachiyama    4       TRUE 2.297e+03 8.592e-05  75
## 185       Gregory Gourdet    4       TRUE 3.004e+03 8.620e-05  37
## 243         Darin Swanson    4       TRUE 2.091e+03 8.529e-05  22
## 9             Jason Leman    4      FALSE 9.600e+02 8.604e-05  72
## 14           David Herron    4      FALSE 9.910e+00 8.526e-05  14
## 21           Ben Blessing    4      FALSE 4.908e+01 8.496e-05  23
## 32              Dana Katz    4      FALSE 2.138e+01 8.483e-05  40
## 37         Willie McBride    4      FALSE 1.170e+03 8.642e-05  65
## 52           Gary Robbins    4      FALSE 4.635e+02 8.511e-05  25
## 54          Desiree Marek    4      FALSE 3.189e+02 8.581e-05  51
## 57       Alessandra Novak    4      FALSE 6.700e+02 8.499e-05  21
## 64         Jocelyn Nelson    4      FALSE 1.061e+02 8.566e-05  31
## 67          Rhiannon Wood    4      FALSE 3.936e+00 8.470e-05  24
## 68            Jeremy Hurl    4      FALSE 4.440e+00 8.481e-05  25
## 72           Marta Fisher    4      FALSE 1.378e+01 8.484e-05  34
## 78             Neil Baker    4      FALSE 2.911e+01 8.511e-05  22
## 100             Mac Smith    4      FALSE 1.100e+01 8.473e-05  15
## 105          Christine An    4      FALSE 9.291e+01 8.508e-05  13
## 115         Sarah Bradham    4      FALSE 2.151e+02 8.592e-05  43
## 118         Jason Fedchak    4      FALSE 0.000e+00 8.448e-05   8
## 121            Sarah Shea    4      FALSE 1.397e+01 8.418e-05   7
## 123        Jennifer Allen    4      FALSE 9.087e+01 8.551e-05  27
## 124           Mary Ramsay    4      FALSE 1.896e+01 8.496e-05  25
## 126         Cheri Redwine    4      FALSE 4.177e+02 8.524e-05  55
## 129         Sean Meissner    4      FALSE 6.596e+02 8.545e-05  55
## 130            Kevin Karr    4      FALSE 6.684e+02 8.612e-05  59
## 134              Sara Lee    4      FALSE 2.623e+01 8.501e-05  29
## 145 Andrea Jarzombek-Holt    4      FALSE 2.517e+02 8.606e-05  52
## 154             T.J. Ford    4      FALSE 6.740e+02 8.593e-05  55
## 156             Olga King    4      FALSE 8.093e-01 8.435e-05   8
## 158         Leslie Gerein    4      FALSE 9.801e-01 8.387e-05   9
## 161       Heather McGrath    4      FALSE 4.180e+02 8.587e-05  37
## 172      Charles Replogle    4      FALSE 1.165e+03 8.624e-05  67
## 174        Diana Bartolus    4      FALSE 1.856e+02 8.530e-05  27
## 176           Renee Seker    4      FALSE 5.488e+02 8.635e-05  56
## 181          Joshua Marks    4      FALSE 2.481e+01 8.480e-05  10
## 186          Joel Dippold    4      FALSE 1.370e+03 8.583e-05  13
## 187         Leif Rustvold    4      FALSE 1.422e+00 8.472e-05   9
## 196          Sarah Duncan    4      FALSE 8.437e+02 8.610e-05  62
## 206           Paul Nelson    4      FALSE 6.199e+02 8.618e-05  66
## 208    Katie Christianson    4      FALSE 1.665e+02 8.563e-05  53
## 213            Hugh Davis    4      FALSE 1.191e+02 8.554e-05  41
## 214          Gregg Webber    4      FALSE 5.044e+01 8.501e-05  13
## 226         Robert Orcutt    4      FALSE 4.006e+01 8.473e-05  15
## 228           Nancy Innis    4      FALSE 0.000e+00 8.422e-05   5
## 229            Mike Davis    4      FALSE 3.197e+02 8.595e-05  51
## 230          Julie Thomas    4      FALSE 4.393e+02 8.565e-05  49
## 234         Moises Lucero    4      FALSE 1.664e+03 8.581e-05  21
## 237             Josh Owen    4      FALSE 5.224e+01 8.490e-05  22
## 238             Jesse Cox    4      FALSE 6.946e+00 8.496e-05  21
## 241            Matt Helms    4      FALSE 9.668e+01 8.565e-05  44
## 252          Angela Drake    4      FALSE 1.318e-01 8.447e-05   9
## 253         Nathan Herzog    4      FALSE 6.164e+01 8.478e-05  15
## 261        Jeannie Horton    4      FALSE 1.336e+03 8.593e-05  46
## 265     Ronda Sundermeier    4      FALSE 4.031e+02 8.575e-05  40
## 268          Jeff Boggess    4      FALSE 2.662e+02 8.598e-05  51
## 272         Samantha Vega    4      FALSE 6.353e+02 8.620e-05  60
## 276             Pam Smith    4      FALSE 1.941e+02 8.549e-05  37
## 291           Jim Kennedy    4      FALSE 1.188e+01 8.495e-05  35
## 292              Syd Long    4      FALSE 9.132e+01 8.549e-05  31
## 293          Todd Janssen    4      FALSE 1.887e+03 8.666e-05  78
## 297         Brian Janecek    4      FALSE 1.606e+02 8.570e-05  43
## 305       John Liebeskind    4      FALSE 1.603e+00 8.460e-05   9
## 314  Anne Crispino-Taylor    4      FALSE 6.855e+02 8.598e-05  59
## 315           Pete Savage    4      FALSE 2.398e+01 8.483e-05   9
## 318      Trevor Hostetler    4      FALSE 4.354e+02 8.585e-05  63
## 323         Brandon Drake    4      FALSE 3.565e+02 8.601e-05  46
## 324         Jeff McAlpine    4      FALSE 2.573e+02 8.526e-05  32
## 326          Silvia Reyes    4      FALSE 2.983e+01 8.531e-05  29
## 329        Justin Huggins    4      FALSE 1.136e+01 8.492e-05  18
## 331           Eric Lubell    4      FALSE 1.577e+02 8.558e-05  39
## 334   Rikilynn Mclenithan    4      FALSE 4.320e+01 8.522e-05  29
## 337         Rick Kneedler    4      FALSE 9.523e+01 8.541e-05  34
## 342            Mike Burke    4      FALSE 4.471e+01 8.488e-05  23
## 345         David Chilson    4      FALSE 0.000e+00 8.426e-05   6
## 346        Ricky Bartolus    4      FALSE 1.947e-01 8.461e-05  12
## 348        Larry Stephens    4      FALSE 4.323e+02 8.610e-05  56
## 356           Megan Bruce    4      FALSE 4.202e+01 8.512e-05  38
## 360           Scott Dumdi    4      FALSE 2.429e+00 8.477e-05  21
## 361            Moe Codino    4      FALSE 1.448e+02 8.553e-05  54
## 362        Juliano Wilson    4      FALSE 2.101e-01 8.462e-05   8
## 368        Dennis Gamroth    4      FALSE 1.033e+01 8.456e-05  14
## 369         Jennifer Love    4      FALSE 4.757e+02 8.615e-05  62
## 370        Keith Shishido    4      FALSE 1.604e+01 8.487e-05  18
## 373         Avery McCombs    4      FALSE 3.145e+01 8.471e-05  16
## 8      Colleen Schoonover    5       TRUE 1.067e+03 8.338e-05  19
## 299          Evan Freeman    5       TRUE 1.093e+04 8.488e-05  21
## 316        Erin Thekkedom    5       TRUE 2.134e+03 8.304e-05   8
## 332        Marcus Fischer    5       TRUE 1.771e+03 8.337e-05  19
## 31             Justin Kim    5      FALSE 2.439e+01 8.157e-05  13
## 44          Ashlyn Jaswal    5      FALSE 1.588e+00 8.078e-05   4
## 51      Ankit Upadhyayula    5      FALSE 6.213e+02 8.331e-05  29
## 59              Kim Huynh    5      FALSE 5.995e+02 8.335e-05  28
## 73              Victor So    5      FALSE 3.120e+01 8.132e-05  24
## 74        Dakota McMillan    5      FALSE 2.082e+02 8.157e-05  27
## 77           Jon Anderson    5      FALSE 3.643e+02 8.324e-05  23
## 79           Donovan Tran    5      FALSE 4.709e+02 8.304e-05  24
## 83      Kellyn Christison    5      FALSE 5.377e+02 8.207e-05  10
## 94           Megan Baxter    5      FALSE 6.110e+02 8.333e-05  14
## 153             Grace Xia    5      FALSE 2.333e+02 8.299e-05  22
## 155         Christina Hsu    5      FALSE 5.084e+02 8.326e-05  24
## 175            Bre Ongley    5      FALSE 6.259e+01 8.145e-05  15
## 177       Kyle Yasumiishi    5      FALSE 3.703e+01 8.163e-05  21
## 189            Cathy Dinh    5      FALSE 3.155e+02 8.122e-05  11
## 193           Danny Cohen    5      FALSE 1.051e+03 8.337e-05  31
## 198         Ydali Olivera    5      FALSE 1.121e+01 8.151e-05   9
## 204         Sean Morrison    5      FALSE 2.852e+01 8.127e-05   3
## 211          Nicki Parker    5      FALSE 4.702e+02 8.290e-05  12
## 254         Jordan Brazda    5      FALSE 2.130e+02 8.315e-05  10
## 256              Eric Kim    5      FALSE 1.509e+02 8.185e-05  24
## 263        Paige Singhose    5      FALSE 3.871e+02 8.321e-05  17
## 289           Briana Chui    5      FALSE 1.019e+02 8.109e-05   4
## 294          Nick Cropley    5      FALSE 6.510e+02 8.308e-05  26
## 295    Megan Schermerhorn    5      FALSE 2.515e+02 8.302e-05  22
## 300          Nick Hartley    5      FALSE 1.468e+00 8.111e-05   5
## 303        Allison Rogers    5      FALSE 6.650e+01 8.177e-05  17
## 306       DjBlast Sanchez    5      FALSE 2.456e+02 8.297e-05  12
## 308      Mindy Kirschbaum    5      FALSE 6.148e+02 8.147e-05   7
## 309          Jordan LeBle    5      FALSE 4.082e+01 8.289e-05  11
## 310             Will Kent    5      FALSE 1.563e+01 8.129e-05  19
## 313        Brian Grimsted    5      FALSE 5.882e-02 8.127e-05   4
## 317          Garrett Mann    5      FALSE 5.091e+01 8.153e-05  11
## 321           Brady Ogden    5      FALSE 1.743e+02 8.294e-05  11
## 327          Carly DeLapp    5      FALSE 3.594e+02 8.315e-05  17
## 330          Kyle Shorter    5      FALSE 5.147e+00 8.149e-05   7
## 338           Marcus Kwon    5      FALSE 4.657e+00 8.118e-05   8
## 339             Eryn Sych    5      FALSE 4.290e+01 8.163e-05  20
## 341         Vlad Shapoval    5      FALSE 3.628e+02 8.304e-05  24
## 343         Kaitie DoupÃ©    5      FALSE 2.831e+02 8.317e-05  15
## 347        McKenna Spieth    5      FALSE 1.324e+02 8.155e-05  25
## 349       Lauren Marshall    5      FALSE 4.156e+01 8.120e-05   9
## 351         Sierra Monaco    5      FALSE 8.768e+01 8.266e-05   5
## 352        Connor Brennan    5      FALSE 1.093e+00 8.117e-05   8
## 353         Shane Brennan    5      FALSE 8.831e+00 8.131e-05  21
## 372          Kyle Shorter    5      FALSE 4.690e+00 8.114e-05   7
## 194          Nadia Khater    6       TRUE 2.401e+03 8.465e-05   9
## 10    Krystle Flerchinger    6      FALSE 1.736e+01 8.177e-05   6
## 11          Justin Pierce    6      FALSE 3.529e+01 8.296e-05   7
## 15           Amanda Smith    6      FALSE 1.198e+02 8.303e-05   9
## 24         Melissa Holmes    6      FALSE 3.285e+01 8.289e-05   8
## 95             Ryan Alice    6      FALSE 4.558e+01 8.291e-05   9
## 225         Thomas Gehrke    6      FALSE 3.463e+01 8.281e-05   8
## 240         Kellie Kutkey    6      FALSE 5.903e+01 8.277e-05   6
## 19          James Wallace    7       TRUE 2.048e+03 8.155e-05   7
## 13         Theresa Nguyen    7      FALSE 2.977e+02 8.041e-05   2
## 28        Julie Bergstrom    7      FALSE 3.333e-01 7.937e-05   4
## 66             Annette Vu    7      FALSE 6.533e+00 7.834e-05   2
## 80        Natalie Jacuzzi    7      FALSE 0.000e+00 8.040e-05   2
## 128        Carter Wallace    7      FALSE 0.000e+00 7.936e-05   3
## 139          Diane Doctor    7      FALSE 0.000e+00 8.040e-05   2
## 149            Todd Bates    7      FALSE 0.000e+00 7.729e-05   2
## 202            Katy Smith    7      FALSE 2.231e+02 7.939e-05   7
## 245             Nancy Lee    7      FALSE 1.964e+03 8.263e-05   6
## 269           Patty Lewis    7      FALSE 7.567e+00 7.939e-05   4
## 282           Marie Bates    7      FALSE 1.702e+02 7.937e-05   5
## 287          Kim Peterson    7      FALSE 0.000e+00 8.040e-05   2
## 301        Miranda Pappas    7      FALSE 7.482e+01 8.047e-05   6
## 22             Bekah Wolf    8       TRUE 1.995e+03 8.572e-05  22
## 336         Henry Barrera    8       TRUE 3.096e+03 8.527e-05   9
## 18          Donnie Drobny    8      FALSE 1.525e+02 8.480e-05  15
## 26          Nikki O'Brien    8      FALSE 5.380e-01 8.347e-05  11
## 41          Heather Wiese    8      FALSE 0.000e+00 8.457e-05   3
## 50          Carrie Yerton    8      FALSE 1.283e+03 8.442e-05  18
## 58          Chris Frazier    8      FALSE 4.471e-01 8.345e-05   8
## 69       Nicholas Burnett    8      FALSE 0.000e+00 8.293e-05   4
## 75       Jeffrey Prescott    8      FALSE 1.757e+01 8.390e-05  13
## 104           Nathan Enns    8      FALSE 6.506e+02 8.488e-05  21
## 109             Greg Pugh    8      FALSE 2.110e+02 8.477e-05  17
## 122      Kimberly Livesay    8      FALSE 6.121e+02 8.470e-05  19
## 140      Samantha Johnson    8      FALSE 1.298e+02 8.402e-05  10
## 144           Amy McBride    8      FALSE 1.477e+01 8.396e-05  14
## 163         Mike Klausman    8      FALSE 6.512e+02 8.489e-05  23
## 197           Micah Wiese    8      FALSE 1.558e+03 8.507e-05  21
## 205       Kieffer Tarbell    8      FALSE 3.440e+02 8.488e-05  22
## 222        Julie Hurliman    8      FALSE 3.103e+01 8.393e-05  17
## 251         Tonia Gebhart    8      FALSE 2.261e+01 8.393e-05  17
## 255        Debbie Tavares    8      FALSE 9.021e+01 8.400e-05  20
## 257           Heidi Baney    8      FALSE 7.847e+02 8.422e-05  11
## 278         Nathan Conant    8      FALSE 1.672e+02 8.483e-05  15
## 290        Micah Stickler    8      FALSE 7.692e-02 8.336e-05   9
## 319            Bryan Agee    8      FALSE 0.000e+00 8.247e-05   1
## 350           Heidi Baney    8      FALSE 6.534e+01 8.425e-05   7
## 355       Jennifer Waters    8      FALSE 3.895e+01 8.392e-05  15
## 374           Heidi Baney    8      FALSE 1.873e+02 8.369e-05   7
## 191           Allison Cox    9       TRUE 3.897e+03 8.511e-05  10
## 29          Carolina Main    9      FALSE 1.701e+03 8.278e-05   5
## 33            Linda Jones    9      FALSE 0.000e+00 8.128e-05   1
## 65       Elissa Kevrekian    9      FALSE 1.938e+03 8.457e-05   4
## 76           Reniera Eddy    9      FALSE 4.068e+02 8.362e-05   5
## 96             Dan Lankow    9      FALSE 5.000e-01 8.271e-05   3
## 159             Eric Main    9      FALSE 0.000e+00 8.274e-05   3
## 182       Skye Macalester    9      FALSE 0.000e+00 8.273e-05   2
## 192           Michael Cox    9      FALSE 5.102e+02 8.366e-05   6
## 207     James Kalashnikov    9      FALSE 0.000e+00 8.270e-05   2
## 333             Cory Eddy    9      FALSE 0.000e+00 8.271e-05   2
## 107           Julie Honse   10       TRUE 3.714e+03 8.648e-05  13
## 150        Heather Fowler   10       TRUE 1.389e+04 8.657e-05   5
## 168         Annie Bencomo   10       TRUE 8.748e+03 8.715e-05  20
## 35       Giovanni Bencomo   10      FALSE 8.581e+02 8.594e-05  11
## 36           Rick Rezinas   10      FALSE 5.461e+02 8.571e-05  10
## 47       Gary Blessington   10      FALSE 1.534e+02 8.581e-05   7
## 53       Stephanie Bolson   10      FALSE 2.727e+01 8.563e-05   8
## 55         Keely Phillips   10      FALSE 0.000e+00 8.499e-05   4
## 60           Eric Virshbo   10      FALSE 1.076e+00 8.353e-05   5
## 84          Melissa Hovis   10      FALSE 2.927e+01 8.443e-05  10
## 85             Amy Harris   10      FALSE 8.163e+02 8.584e-05  25
## 89          Kate Horspool   10      FALSE 3.550e+02 8.599e-05   6
## 91          Dustin Harris   10      FALSE 6.232e+02 8.580e-05  21
## 92         Alicia Fuentes   10      FALSE 3.324e+02 8.456e-05  15
## 99           Susan Kucera   10      FALSE 4.954e+00 8.434e-05   7
## 102         Dave Horspool   10      FALSE 0.000e+00 8.488e-05   4
## 108        Thomas O'Leary   10      FALSE 2.917e-01 8.384e-05   6
## 120       Debbie Ginzburg   10      FALSE 9.723e+00 8.406e-05   6
## 127         Rebecca Waltz   10      FALSE 2.129e+03 8.634e-05  26
## 131        Rachel Prewitt   10      FALSE 7.648e+02 8.568e-05  11
## 137         Nedra Rezinas   10      FALSE 1.249e+03 8.579e-05  14
## 138          Brent Harris   10      FALSE 3.704e-01 8.417e-05   6
## 164             Amara Zee   10      FALSE 2.419e+01 8.467e-05  10
## 167        Michael Mouton   10      FALSE 4.788e+00 8.430e-05   8
## 179          Johnny Buell   10      FALSE 2.964e+03 8.587e-05  21
## 199     Holly Fraser-Witt   10      FALSE 7.138e+01 8.560e-05   5
## 200    Christopher Barker   10      FALSE 3.413e+03 8.616e-05  32
## 203            Jason Post   10      FALSE 1.660e+00 8.460e-05   5
## 242           Tracy Meese   10      FALSE 0.000e+00 8.488e-05   4
## 259            Mike Smith   10      FALSE 0.000e+00 8.375e-05   2
## 260             Matt Love   10      FALSE 9.098e+02 8.481e-05  20
## 262           Laura Davis   10      FALSE 9.012e-01 8.426e-05   7
## 266            Sher Sinda   10      FALSE 2.000e-01 8.422e-05   6
## 274          John Stevens   10      FALSE 2.714e-01 8.431e-05   5
## 320           Sean Politz   10      FALSE 1.580e+02 8.566e-05  11
## 325          David Dvorak   10      FALSE 8.480e+02 8.545e-05  15
## 344           Sarah Dyste   10      FALSE 1.250e-01 8.431e-05   5
## 363         Chanda Gandhi   10      FALSE 4.303e+00 8.430e-05   7
## 86         Lucia Longoria   11       TRUE 4.373e+03 8.490e-05  16
## 42      Richmond Fontaine   11      FALSE 0.000e+00 8.258e-05   6
## 90             Angie Burr   11      FALSE 0.000e+00 8.250e-05   2
## 110            Cheryl Ann   11      FALSE 1.167e+01 8.313e-05   5
## 125            Matt Moore   11      FALSE 7.592e+01 8.322e-05  12
## 141       Marne Manoukian   11      FALSE 6.500e+02 8.544e-05   5
## 170        Kristen Broyer   11      FALSE 8.580e+01 8.371e-05   9
## 217    Fernando Viciconte   11      FALSE 2.454e+03 8.374e-05  10
## 247    Pixelface Creative   11      FALSE 2.583e+00 8.257e-05   4
## 277        Jolene Kawecki   11      FALSE 9.778e+00 8.317e-05   7
## 284           Megan Brown   11      FALSE 1.598e+01 8.318e-05   8
## 286        Stacy Benjamin   11      FALSE 1.667e-01 8.259e-05   7
## 162   Dawn Jones-Redstone   12       TRUE 1.396e+03 8.455e-05   2
## 201          Mandy Wilson   12       TRUE 1.090e+03 8.637e-05   5
## 224       Sandy Bacharach   12       TRUE 2.744e+03 8.615e-05   7
## 250           Ann Wallace   12       TRUE 1.723e+03 8.581e-05   8
## 281        Frances Favela   12       TRUE 1.364e+03 8.054e-05   3
## 2           David Allamon   12      FALSE 0.000e+00 8.493e-05   3
## 4              Carla Owen   12      FALSE 0.000e+00 8.336e-05   5
## 12          Naomi Fishman   12      FALSE 6.860e+02 7.840e-05   3
## 27          Erica Allison   12      FALSE 0.000e+00 7.168e-06   0
## 34              Tina Slee   12      FALSE 0.000e+00 8.114e-05   1
## 39       Makiko Yamashita   12      FALSE 3.440e+02 8.347e-05   2
## 40             Amy Begley   12      FALSE 5.135e+00 8.472e-05   7
## 43               Nic Lamb   12      FALSE 0.000e+00 8.368e-05   1
## 48           Orna Izakson   12      FALSE 0.000e+00 8.259e-05   3
## 49         Elizabeth Cole   12      FALSE 0.000e+00 8.368e-05   1
## 56          Shawn Mullaly   12      FALSE 0.000e+00 8.269e-05   1
## 63         Caroline Kobin   12      FALSE 4.250e+02 8.598e-05   4
## 70      Vincent Granville   12      FALSE 0.000e+00 7.188e-06   1
## 71             Jack Cheng   12      FALSE 0.000e+00 8.164e-05   1
## 81             Mark Novak   12      FALSE 8.184e+01 8.386e-05   5
## 98              Ann Visan   12      FALSE 0.000e+00 7.188e-06   1
## 101          Susan Kelley   12      FALSE 8.287e+02 8.570e-05   7
## 113           Keith Parks   12      FALSE 0.000e+00 8.337e-05   4
## 114          Nicole Busto   12      FALSE 3.440e+02 7.635e-05   2
## 117          Julie Fukuda   12      FALSE 0.000e+00 7.188e-06   1
## 119         Ramona DeNies   12      FALSE 0.000e+00 7.168e-06   0
## 132     Kristen Backeberg   12      FALSE 1.790e+02 8.576e-05   6
## 135           Aviva Brown   12      FALSE 0.000e+00 8.259e-05   3
## 136     Urko Larrakoetxea   12      FALSE 0.000e+00 7.188e-06   1
## 142 Karinna Jones-Ianello   12      FALSE 1.664e+01 8.479e-05   4
## 147          Jenny Nicole   12      FALSE 0.000e+00 8.358e-05   2
## 152        Melissa Powell   12      FALSE 0.000e+00 7.839e-05   2
## 157         Nicole Curcio   12      FALSE 0.000e+00 7.168e-06   0
## 160            Ali Jessie   12      FALSE 1.214e+02 8.455e-05   4
## 171            Esther Lai   12      FALSE 0.000e+00 7.226e-06   2
## 180          April Brewer   12      FALSE 0.000e+00 7.188e-06   1
## 188   Elizabeth Rollerson   12      FALSE 3.131e+02 8.531e-05   7
## 195          Lance Adkins   12      FALSE 0.000e+00 7.168e-06   0
## 209             Lisa Kith   12      FALSE 0.000e+00 7.226e-06   2
## 212          Lucien Kress   12      FALSE 0.000e+00 7.188e-06   1
## 218              Kyle Pak   12      FALSE 2.000e+00 7.226e-06   3
## 220       Sean Kilpatrick   12      FALSE 0.000e+00 7.168e-06   0
## 223             David Yan   12      FALSE 0.000e+00 7.226e-06   1
## 233            Kush Patel   12      FALSE 0.000e+00 7.188e-06   1
## 235            Rob Coomer   12      FALSE 0.000e+00 7.188e-06   1
## 236        David Dranchak   12      FALSE 0.000e+00 7.168e-06   0
## 244          Joe Ferguson   12      FALSE 0.000e+00 8.441e-05   5
## 248         Leah Williams   12      FALSE 0.000e+00 7.440e-05   1
## 249            Todd Evans   12      FALSE 1.439e-01 8.423e-05   3
## 267           Jenna Jones   12      FALSE 3.455e+02 7.937e-05   4
## 270      Anthony Petrarca   12      FALSE 0.000e+00 7.188e-06   1
## 283        Patrick Romano   12      FALSE 0.000e+00 7.168e-06   0
## 285    Justine Vanderpool   12      FALSE 0.000e+00 7.726e-05   1
## 288           Eric Barten   12      FALSE 2.936e-01 8.455e-05   4
## 302          Ryan Parrett   12      FALSE 0.000e+00 7.168e-06   0
## 304         Katie Lessner   12      FALSE 0.000e+00 7.188e-06   1
## 307         Mario Delgado   12      FALSE 0.000e+00 7.168e-06   0
## 312        Lindsey Barber   12      FALSE 0.000e+00 8.078e-05   1
## 328         Chris Fanning   12      FALSE 0.000e+00 7.168e-06   0
## 340         Lauren Weigel   12      FALSE 0.000e+00 7.925e-05   1
## 354          Lance Carion   12      FALSE 0.000e+00 7.168e-06   0
## 357           Jennifer Ha   12      FALSE 0.000e+00 7.168e-06   0
## 359              Irina St   12      FALSE 0.000e+00 7.168e-06   0
## 364         Oscar Sanchez   12      FALSE 0.000e+00 7.168e-06   0
## 367          Rebekah Chou   12      FALSE 2.851e+01 8.485e-05   5
```

* Community 1 is family and relatives.
* Community 2 is [Marathon Maniacs](http://www.marathonmaniacs.com/).
* Community 3 is trail runners, including Trail Factor, Animal Athletics, and BananaSluggers.
* Community 4 is [Union High School](https://www.facebook.com/pages/Union-High-School/127917213038), mostly students with non-math teachers and staff. Some [Mountain View](https://www.facebook.com/GoThunder.org) students who are connected to Union people.
* Community 5 is the [Union High School](https://www.facebook.com/pages/Union-High-School/127917213038) math teachers.
* Community 6 is OHSU BICC.
* Community 7 is my [Concordia MATE](http://www.cu-portland.edu/coe/graduate/mat/) cohort.
* Community 8 is Cat's side of the family and her friends.
* Community 9 is a secondary group of Portland friends.
* Community 10 is a core group of Portland friends.
* Community 11 is a secondary group of Portland friends that has connections to Community 9.
* Community 12 is the *misfits* community of people that either
  + Don't fit in with a larger community within my network, or
  + Truly bridge the other 11 well-defined communities to an extent that they don't belong in a single community
* There are a number of key people that connect 2 or more communities.

Create interactive 3-D high resolution.
Open the `network.html` file in a browser.


```r
require(networkD3)
```

```
## Loading required package: networkD3
```

```
## Warning: package 'networkD3' was built under R version 3.1.2
```

```r
edges <- data.frame(get.edgelist(G))
names(edges) <- c("source", "target")
edges$source <- edges$source - 1
edges$target <- edges$target - 1
edges$value <- 1
vertices <- D[, c("name", "comm")]
names(vertices) <- c("name", "group")
vertices$name <- as.character(vertices$name)
N <- forceNetwork(Links=edges, Nodes=vertices,
                  Source="source", Target="target", Value = "value",
                  NodeID = "name", Group="group",
                  opacity=3/4)
saveNetwork(N, "network.html")
```

**Don't use `rglplot`.**


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
