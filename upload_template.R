library("knitr")
library("rmarkdown")
library("RWordPress")
library("XMLRPC")
options(
    WordpressLogin = c( = ''),
    WordpressURL = 'http://www.dtanneberg.de/xmlrpc.php')
)
# render(
#     input = "~/github/blog/ggplot2_orderFacets.Rmd",
#     output_file = "~/github/blog/ggplot2_orderFacets.html"
# )

knit2wp(
    input = "~/github/blog/ggplot2_overlayJointDistribution.Rmd",
    title = "Street Fighting R: Combine Small Multiples with Highlighting",
    action = "editPost",
    postid = 285,
    categories = c('R', 'Stats', 'DataViz', 'ggplot2', 'Street Fighting R'),
    publish = FALSE
)