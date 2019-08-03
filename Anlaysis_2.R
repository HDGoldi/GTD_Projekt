data <- read.csv("gtd_lite.csv")
data_lite <- read.csv("gtd_lite2.csv")

head(data, 10)
dim(data)
library(dplyr)
glimpse(data)
summary(data)
library(DataExplorer)
DataExplorer::create_report(data)
library(FactoMineR)
library("factoextra")
res.pca <- PCA(data,  graph = FALSE)
scatterplotMatrix(data)
library(purrr)
library(tidyr)
library(ggplot2)
data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()
library(ggpubr)
ggbarplot(data, x = "country", y = "nkill",
          fill = "cyl",               # change fill color by cyl
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "desc",          # Sort the value in dscending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90           # Rotate vertically x axis texts
          )
plot_str(data)
plot_missing(data)
plot_histogram(data)
plot_correlation(data, type = 'continuous','iyear')


#correlation
cor_data <- data_lite[, c("country", "region","success","attacktype","targtype","natlty","weaptype")]
res <- cor(cor_data)
round(res, 2)


library(tm) #Textmining for wordclouds
library(wordcloud)

df %>% filter(!is.na(summary)) -> dfn0
dfn0 %>% filter(summary != "") -> dfn
text <- sample(dfn$summary, nrow(dfn)/100)
myCorpus <- Corpus(VectorSource(text))
#myCorpus = tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus = tm_map(myCorpus, removePunctuation)

docs <- Corpus(VectorSource(summary))
inspect(docs)
#Text transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

#Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


df2 <- inner_join(df, popworld, by= c("year" = "Time"))




data_lite %>% filter(nkill > 0) -> dfk

treemap(dfk, 
        index=c("country"), 
        vSize = "nkill",  
        palette = "Reds",  
        title="Killings in Global Terrorism", 
        fontsize.title = 14 
)

treemap(
  dfk,
  #Your data frame object
  index = c("country", "iyear"),
  type = "value",
  vSize = "nkill",
  vColor = "nwound",
  palette = "RdBu",
  title = "Killings in Global terrorism  (Countries/Years) - size is proportional with the number of killings",
  title.legend = "Number of wounded",
  fontsize.title = 10
)


treemap(dfk, 
        index=c("iyear"), 
        vSize = "nkill",  
        palette = "Reds",  
        title="Killings in Global Terrorism", 
        fontsize.title = 14 
)
