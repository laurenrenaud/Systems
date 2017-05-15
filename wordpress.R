install.packages("devtools")
install.packages("RCurl")
install.packages("XML")
devtools:::install_github("duncantl/XMLRPC")
devtools:::install_github("duncantl/RWordPress")

library(RWordPress)



if (!require('RWordPress')) {
  devtools::install_github(c("duncantl/XMLRPC", "duncantl/RWordPress"))
}
library(RWordPress)
options(WordpressLogin = c(wacannabisstudyadmin = 'Ku$hdabShatter2017'),
        WordpressURL = 'https://wacannabisstudy.heinz.cmu.edu/xmlrcp.php')
library(knitr)
knit2wp('college_regressions_v3.rmd', title = 'Classifying Extracts for Inhalation', shortcode = TRUE,
        publish = FALSE)


# below from
# http://3.14a.ch/archives/2015/03/08/how-to-publish-with-r-markdown-in-wordpress/#toc_1

# Install RWordpress
if (!require('RWordPress')){
  install.packages('RWordPress', repos = 'http://www.omegahat.org/R', type = 'source')}
library(RWordPress)
# Set login parameters (replace admin,password and blog_url!)
options(WordPressLogin = c(wacannabisstudyadmin = 'Ku$hdabShatter2017'),
        WordPressURL = 'http://wacannabisstudy.heinz.cmu.edu/xmlrpc.php')

# Include toc (comment out if not needed)
library(markdown)
options(markdown.HTML.options =  c(markdownHTMLOptions(default = T),"toc"))

# Upload plots: set knitr options
opts_knit$set(upload.fun = function(file){library(RWordPress);uploadFile(file)$url;})

# Upload featured image / post thumbnail: option: wp_post_thumbnail=postThumbnail$id
postThumbnail <- RWordPress::uploadFile("../graphics/map.png",overwrite = TRUE)

# Post new entry to the wordpress blog and store the post id
library(knitr)
postid <- knit2wp('college_regressions_v3.rmd', title = 'Classifying Extracts for Inhalation',
                  categories=c('R'),mt_keywords = c('knitr', 'wordpress'),
                  wp_post_thumbnail=postThumbnail$id,publish=FALSE)
postid
