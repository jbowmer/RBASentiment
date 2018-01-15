#Sentiment Analysis of RBA Minutes

setwd("/Users/Jake/Projects/RBASentiment/Data")

library(tidytext)
library(dplyr)
library(tokenizers)

text = as_tibble(read.delim("1102.txt", header = FALSE, stringsAsFactors = FALSE))
text_df = mutate(text, line_number = row_number())
names(text_df) = c("text", "line_number")
text_df = as_tibble(text_df)

tidy_speech = text_df %>% unnest_tokens(word,text)
#head(tidy_speech)
#Remove stopwords
data(stop_words)
tidy_speech = tidy_speech %>% anti_join(stop_words)

#Most common words:
tidy_speech %>% count(word,sort = TRUE)
#Chart of most common words:
tidy_speech %>%
  count(word, sort= TRUE) %>%
  filter(n>5)%>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#For sentiment, we can use either AFINN (a lexicon which assigns scores of minus 5 to plus 5) or
#use bing, which classifies as either positive or negative
#example
#head(get_sentiments("afinn"))

#For financial sentiment specifically, we use the Loughran and McDonald dictionary, which is based on financial reports.
head(get_sentiments("loughran"))

#Sentiment Analysis is an inner join operation.
sentiments = get_sentiments("loughran")
tidy_speech = inner_join(tidy_speech, sentiments)

#Now we can plot using counts
ggplot(tidy_speech, aes(sentiment)) +
  geom_bar()

#What we really want to do is count the various categories, 
#and assign the date of the speech and the count of each category so we can track it over time.

positive_num = nrow(filter(tidy_speech, sentiment == "positive")) #perfect
negative_num = nrow(filter(tidy_speech, sentiment == "negative"))
uncertainty_num = nrow(filter(tidy_speech, sentiment == "uncertainty"))
litigious_num = nrow(filter(tidy_speech, sentiment == "litigious"))
constraining_num = nrow(filter(tidy_speech, sentiment == "constraining"))

#Linking it all together.
#First, we read all the file names from the appropriate path

files = list.files(getwd())

#create a dataframe to store results
sentiment_df = data.frame(date = character(), positive_num = integer(), negative_num = integer(),
                uncertainty_num = integer(), litigious_num = integer(), constraining_num = integer())
#loughran sentiment
sentiments = get_sentiments("loughran")

for (i in files)
{
  #date is the first four letters of file name.
  date = paste("01",substr(i,3,4),substr(i,1,2), sep ="")
  #read in text and prepare for analysis
  text = as_tibble(read.delim(i, header = FALSE, stringsAsFactors = FALSE))
  text_df = mutate(text, line_number = row_number())
  names(text_df) = c("text", "line_number")
  text_df = as_tibble(text_df)
  tidy_speech = text_df %>% unnest_tokens(word,text)
  
  #remove stopwords
  tidy_speech = tidy_speech %>% anti_join(stop_words)
  
  #use loughran to gauge sentiment:
  tidy_speech = inner_join(tidy_speech, sentiments)
  
  #count positive, negative, uncertainty, litigious, constraining words
  positive_num = nrow(filter(tidy_speech, sentiment == "positive")) #perfect
  negative_num = nrow(filter(tidy_speech, sentiment == "negative"))
  uncertainty_num = nrow(filter(tidy_speech, sentiment == "uncertainty"))
  litigious_num = nrow(filter(tidy_speech, sentiment == "litigious"))
  constraining_num = nrow(filter(tidy_speech, sentiment == "constraining"))
  
  #Add to exisiting dataframe:
  
  sentiment_df = rbind(sentiment_df, data.frame(date, as.integer(positive_num), as.integer(negative_num),
                                                as.integer(uncertainty_num), as.integer(litigious_num),
                                                as.integer(constraining_num)))
  
}
names(sentiment_df) = c("date", "positive", "negative", "uncertain", "litigious", "constraining")
sentiment_df$date = as.Date(sentiment_df$date, "%d%m%y")
sentiment_df = mutate(sentiment_df, difference = positive-negative)

#Now that we have our data, lets plot over time.

positive_chart = ggplot(sentiment_df, aes(date,positive)) + geom_line()
negative_chart = ggplot(sentiment_df, aes(date,negative)) + geom_line()
difference_chart = ggplot(sentiment_df, aes(date,difference)) + geom_line()

library(gridExtra)
grid.arrange(positive_chart, negative_chart, difference_chart, ncol = 1)
