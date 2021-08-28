
setwd("C:/Users/Cayoyo/Desktop/R")

library(tidyverse)

Project.Sindicate<- read.csv("Project_syndicate2020.csv", header=T, na.strings=c("","NA"))


Project.Sindicate2<- read.csv("project_syndicateNuevas.csv", header=T, na.strings=c("","NA"))


library(lubridate)

Project.Sindicate$date2<-mdy(Project.Sindicate$date)

y<-Project.Sindicate %>% 
  group_by(month=floor_date(date2, "month")) %>% 
  summarise(capitalism = sum(str_count(body, "capitalism")))

ggplot(y, aes(x=month, y=capitalism)) +
  geom_line()

Project.Sindicate %>% 
  group_by(year=floor_date(date2, "week")) %>% 
  summarise(democracy = sum(str_count(body, "democracy"))) %>% 
  ggplot( aes(x=year, y=democracy)) +
  geom_line() + scale_x_date(limit=c(as.Date("2020-01-01"),as.Date("2020-12-31")))


########TIDYTEXT

library(tidytext)


#Para poder hacer analisis mas avanzados es necesario crear bases en donde se "quebre" el texto
#en elementos individuales. Este proceso se llama tokenización. Para hacer esto es necesario utilizar
#la función unnest_tokens() del paquete tidytext


tidy<-Project.Sindicate %>% filter(str_detect(date1,"2020")) %>% 
  unnest_tokens(word, body) %>% count(word,sort=TRUE)

head(tidy,20)


tidy1 <- tidy %>%
  anti_join(stop_words)

tidy1  %>%  slice_head(n=10) %>% 
  ggplot(aes(n,fct_reorder(word,n)))+geom_col()





###Limpiando la base


Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                          "We hope you're enjoying Project Syndicate", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "To continue reading,", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "subscribe now", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "Register for FREE to access two premium articles per month", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "Already have an account?", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "0  Previous Next", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "Subscribe to Project Syndicate", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
              "Enjoy unlimited access to the ideas and opinions of the world\'s leading thinkers,", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
      "including weekly long reads, book reviews, and interviews; The Year Ahead annual print magazine; the complete PS archive; and more - All for less than", ""))


Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "\\$9 a month", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "Subscribe Now", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "Register", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "Subscribe", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                     "At least since the 2008 financial crisis", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
         "the United States has had a crisis of enforcement when it comes to", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
       "punishing and thus deterring corporate crime and the individuals responsible for it", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
       "The best corrective is to design penalties and enforcement that", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
    "modify executive behavior by changing the incentives", "")) 

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                           "via Getty Images", "")) 

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "Add to Bookmarks", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                  "Reimagining the Platform Economy", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "PS OnPoint", ""))


Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "5  Previous", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "3  Previous", ""))

####################################### TF_IDF ########################################

##La función bind_tf_idf nos genera el tf_idf, siempre que tenga las palabras y sus respectivos n's

library(tidytext)

library(lubridate)


Project.Sindicate$date2<-mdy(Project.Sindicate$date)

date<-Project.Sindicate %>% 
  separate(date, into = c("month", "year"), sep = ",") %>%mutate(year=as.numeric(year)) %>% 
  filter(year==2020) %>% separate(month, into = c("month", "day"), sep = " ") %>% 
  mutate (trimester = case_when(month == "Jan"|month =="Feb"|month =="Mar" ~ "I",
                       month == "May"|month =="Apr"|month =="Jun" ~ "II",
                       month == "Jul"|month =="Aug"|month =="Sep" ~ "III",
                       month == "Oct"|month =="Nov"|month =="Dec" ~ "IV"))

x<-date %>% filter(str_detect(body,"5  Previous"))

glimpse(x)

x[1,53]

palabras <- date %>%
  unnest_tokens(word, body) %>%
  count(trimester, word, sort = TRUE) %>% as_tibble()

total_words <- palabras %>% 
  group_by (trimester)%>% 
  summarize(total = sum(n)) %>% as_tibble()

month_words <- left_join(palabras, total_words)


MONTH_tf_idf <- month_words %>%
  bind_tf_idf(word, trimester, n)

MONTH_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#visualizando

MONTH_tf_idf %>%
  group_by(trimester) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = trimester)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~trimester, ncol = 2, scales = "free_y") +
  labs(x = "tf-idf", y = NULL)+
  theme_light()+ggtitle("TF-IDF By Year Trimester")


###################################################TD_IDF Con Bigramas

Project.Sindicate$date2<-mdy(Project.Sindicate$date)


#Pasando a formato tidy
bigramas2 <- date%>%
  unnest_tokens(bigram, body, token = "ngrams", n = 2)


bigramas2 %>%  select(bigram) %>% as_tibble()

#sacando stopwords

bigrams_separated2 <- bigramas2 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#Ahora sacamos de la muestra todos los bigramas que ocupan stopwords. Además voy a sacar algunas
#palabras que puedan haber quedado del codigo html

custom_stop_words <- bind_rows(tibble(word = c("amp","lt","gt","selection marker"),  
                                      lexicon = c("custom")), stop_words)


bigrams_filtered2 <- bigrams_separated2 %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)

#Puede que en algun analisis queramos unir estas variables, para lo cual ocupamos el comando
#unite de tidyr

bigrams_united2 <- bigrams_filtered2 %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united2 %>% select(bigram) %>% as_tibble()

#Ahora calculo el tf_ifd

bigram_tf_idf <- bigrams_united2 %>% group_by (trimester) %>% 
  count(trimester, bigram) %>%
  bind_tf_idf(bigram, trimester, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

bigram_tf_idf %>% group_by(trimester) %>% mutate(ranking=rank(desc(tf_idf))) %>% 
  filter(trimester=="I")

filter(rank(desc(tf_idf)) < 10)


bigram_tf_idf %>%
  group_by(trimester) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  mutate(trimester = as.factor(trimester),
         bigram = reorder_within(bigram, tf_idf, trimester)) %>% 
  ggplot(aes(tf_idf, bigram, fill = trimester)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~trimester, ncol = 2, scales = "free_y") +
  scale_y_discrete(labels = function(x) gsub("__.+$", "", x))+
  theme_light()+ggtitle("TF-IDF By Year Trimester")



#######################################################REDES ##############################################


bigram_counts <- bigrams_filtered2 %>% 
  count(word1, word2, sort = TRUE)

library(igraph)

bigram_counts %>% as_tibble()

#filtramos solo combinaciones mas comunes
bigram_graph <- bigram_counts %>%
  filter(n > 50) %>%
  graph_from_data_frame()

#para hacer un grafico de redes necesito ocupar el comando "graph_from_data_frame()" el cual va
#una lista con los 3 objetos que necesitamos: from, to y edges(valor numerco en este caso n)

#Para graficas vamos a ocupar el paquete ggraph que tiene una sintaxis similar a ggplot

library(ggraph)

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#El grafico puede quedar un poquito mejor si le agregamos mas cosas

#edge_alpha, para establecer que tan transparentes son las lineas que unen los nodos
#en base a cuan comun o raro sea el biagrama

#Le añadimos direccionalidad con una flecha que contruimos con :arrow()
#Le cambios el color y tamaño al nodo dentro de  "geom_node_point"
#Le añadimos una plantilla al grafico para que se vea mas bonito, theme_void()

a <- arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


################## CORRELACION ENTRE PALABRAS ###############################


palabras<-date %>% unnest_tokens(word, body) %>%
  filter(!word %in% stop_words$word)

palabras %>%select(title,word) %>% as_tibble()

library(widyr)

# contar palabras que ocurren juntas dentro de cada articulo

word_pairs <- palabras %>%
  pairwise_count(word, title, sort = TRUE)

word_pairs <-word_pairs %>% as_tibble()

word_pairs %>%
  filter(item1 == "capitalism")


#CORRELACIONES PAIRWISE

word_cors <- palabras %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, title, sort = TRUE)


#podemos graficar variables palabras particulares que nos interesen

word_cors %>%
  filter(item1 %in% c("capitalism", "education", "democracy", "covid")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


#podemos ocupar el paquete ggraph como hicimos antes pero ahora en vez de frecuencias
#ocupamos los coeficentes

set.seed(2016)

word_cors %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation), edge_colour = "darkred")+
  geom_node_point(size = 3, color = "black") +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

###############################################################


#Analisis de topicos

library(topicmodels)


#tokenizando la base
topics<-date %>% 
  unnest_tokens(word, body) %>% count(title,word,sort=TRUE)

#sacando los stops words
topics <- topics %>%
  anti_join(stop_words) %>% as_tibble()

# Vamos a pasar la base al formato DocumentTermMatrix para poder hacer nuestro analisis

#Este termino es una matriz en donde organizamos los datos. En los DTM los terminos son las columnas y
#los articulos las filas. Con el comando cast_dtm, podemos hacer este cambio de manera rápida

dtm <- topics %>%
  cast_dtm(title, word, n)

ap_lda <- LDA(dtm, k = 2, control = list(seed = 123))
ap_lda

#############################  PROBABILIDAD DE TOPICOS POR PALABRAS  #################################

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

#lo primero que vamos a hacer es visualizar cuales son los terminos
#mas comunes por cada topico

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#-beta es un pequeño truco para que me ordena esa variable en orden descendiente

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#Ahora vamos a mirar que palabras tienen mas posibilidades de estar en cada topico

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#el log ratio o logaritmo binario de la razon de frecuencias relativas
#Representa que tan grande es la diferencia de la probabilidad entre dos "cuerpos" de texto, topicos
#en nuestro caso, con respecto a alguna palabra. 

beta_spread

beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(log_ratio, term)) +
  geom_col() +
  labs(x = "Log2 ratio of beta in topic 2 / topic 1", y = NULL)

###################### PROBABILIDAD DE PERTENECER A UN TOPICO POR ARTICULO

ap_documents <- tidy(ap_lda, matrix = "gamma")

ap_documents

ap_documents%>%
  arrange(desc(document))

#probabilidades dentro de cada topico
ggplot(ap_documents, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))




#######################AÑADIENDO MAS TOPICOS##############################



ap_lda <- LDA(dtm, k = 10, control = list(seed = 123))
ap_lda


ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

#lo primero que vamos a hacer es visualizar cuales son los terminos
#mas comunes por cada topico

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#-beta es un pequeño truco para que me ordena esa variable en orden descendiente

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#############################################################



beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic6 > .001 | topic8 > .001) %>%
  mutate(log_ratio = log2(topic6 / topic8))

#el log ratio o logaritmo binario de la razon de frecuencias relativas
#Representa que tan grande es la diferencia de la probabilidad entre dos "cuerpos" de texto, topicos
#en nuestro caso, con respecto a alguna palabra. 

beta_spread

beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(log_ratio, term)) +
  geom_col() +
  labs(x = "Log2 ratio of beta in topic 2 / topic 1", y = NULL)



########################
##########Mirar textos con mayor probabilidad de estar en el topico


ap_documents <- tidy(ap_lda, matrix = "gamma")

ap_documents

ap_documents%>%
  arrange(desc(document))

ap_documents%>%
  arrange(desc(document)) %>% filter(topic==3&gamma>0.5)
#there's no many texts with this topic

#it's about the tension that the pandemic puts on developmening economics

#probabilidades dentro de cada topico
ggplot(ap_documents, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))


###############################################

dtm <- topics %>%
  cast_dtm(title, word, n)

ap_lda <- LDA(dtm, k = 10, control = list(seed = 123))

dtm = convert(dfm, to = "topicmodels") 
set.seed(1)
m = LDA(dtm, method = "Gibbs", k = 10,  control = list(alpha = 0.1))
m

logLik

ap_lda

install.packages("LDAvis")
library(LDAvis)   


dtm1 = dtm[slam::row_sums(dtm) > 0, ]
phi = as.matrix(posterior(ap_lda)$terms)
theta <- as.matrix(posterior(ap_lda)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json)

ap_lda$plot
lda_model$plot(out.dir = "topic_modeling_files/ldavis", open.browser = FALSE)


servr::daemon_stop(2)

library(htmlwidgets)

p<-serVis(json)

serVis(json, out.dir = "C:/Users/Cayoyo/Desktop/R", open.browser = FALSE)



library(servr)


serVis(json)(out.dir = "C:/Users/Cayoyo/Desktop/R", open.browser = FALSE)


###########################HEAT MAP con 

ap_documents%>%
  arrange(desc(document))



#pegar con fecha del otro

x<-Project.Sindicate %>% select(title,date) %>% rename(document=title)

month_words <- left_join(ap_documents, x)

month_words %>% mutate(gamma1=ifelse(gamma>0.5,"1","0"))


month_words$date2<-mdy(month_words$date)

month_words %>% mutate(gamma1=ifelse(gamma>0.5,1,0))%>%
  group_by(
    month = week(date2),
    topic) %>%
  summarise(gamma1 = mean(gamma1)) %>%
  ggplot(aes(month, topic, fill = gamma1)) +
  geom_tile(alpha = 0.8) +
  scale_fill_viridis_c(labels = scales::percent) +
  labs(fill = "% artitles", x = "month of the year", y = "topic")+
  theme_light()


month_words %>% mutate(gamma1=ifelse(gamma>0.5,1,0))%>%
  group_by(
    month = month(date2),
    topic) %>%
  summarise(gamma1 = mean(gamma1)) %>%
  ggplot(aes(month, topic, fill = gamma1)) +
  geom_tile(alpha = 0.8) +
  scale_fill_viridis_c(labels = scales::percent) +
  labs(fill = "% artitles", x = "month of the year", y = "topic")+
  theme_light()



ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()



month_words %>% filter(topic=="6" & str_detect(date,"Oct")&gamma>0.75) %>% arrange(desc(gamma))


##########################LO HACEMOS POR AUTORES



autores<-Project.Sindicate %>% filter(str_detect(date,"2020")) %>% 
  unite(autor,author1, author2,author3,author4,author5,author6,author7,author8,
        author9,author10, sep = "&",na.rm = TRUE) %>% 
  separate_rows(autor,sep= "&") %>% count(autor,sort=TRUE) %>% head(10) %>% select(autor)


base<-Project.Sindicate %>% filter(str_detect(date,"2020")) %>% 
  unite(autor,author1, author2,author3,author4,author5,author6,author7,author8,
          author9,author10, sep = "&",na.rm = TRUE) %>% 
  separate_rows(autor,sep= "&") %>% filter(autor %in% (autores$autor)) 
    


x<-base %>% select(title,autor) %>% rename(document=title)

x_words <- inner_join(ap_documents, x)

x_words %>% mutate(gamma1=ifelse(gamma>0.5,"1","0")) %>% filter(gamma1==1) %>% 
  count(autor, topic) %>% group_by(autor) %>%  mutate(prop = n / sum(n)) %>% 
  plot_ly(x =~prop, y = ~autor, color = ~as.factor(topic)) %>% 
  add_bars() %>% 
  layout(barmode = "stack",title = "Topics by Top 10 authors",
         yaxis=list(title =""))
  
x_words %>% mutate(gamma1=ifelse(gamma>0.5,"1","0")) %>% filter(gamma1==1&autor=="Andrew Sheng ") 
 

library(plotly)




################Ciudades 




ciudades<-Project.Sindicate %>% filter(str_detect(date,"2020")) %>% 
  unite(city,city1, city2,city3,city4, sep = "&",na.rm = TRUE) %>% 
  separate_rows(city,sep= "&") 

ciudades<-ciudades %>% mutate(city=str_replace(ciudades$city,
                                               "\\n                            |\\n", ""))

ciudades<-ciudades %>% mutate(city=str_replace(ciudades$city,
                                               " *$", ""))

ciudades<-ciudades%>% count(city,sort=TRUE) %>% head(10)


base<-Project.Sindicate %>% filter(str_detect(date,"2020")) %>% 
  unite(city,city1, city2,city3,city4, sep = "&",na.rm = TRUE) %>% 
  separate_rows(city,sep= "&")

base<-base %>% mutate(city=str_replace(base$city,
                                               "\\n                            |\\n", ""))

base<-base %>% mutate(city=str_replace(base$city,
                                               " *$", ""))


base<-base%>% filter(city %in% (ciudades$city)) 

glimpse(base)

x<-base %>% select(title,city) %>% rename(document=title)

x_words <- inner_join(ap_documents, x)

x_words %>% mutate(gamma1=ifelse(gamma>0.5,"1","0")) %>% filter(gamma1==1) %>% 
  count(city, topic) %>% group_by(city) %>%  mutate(prop = n / sum(n)) %>% 
  plot_ly(x =~prop, y = ~city, color = ~as.factor(topic)) %>% 
  add_bars() %>% 
  layout(barmode = "stack",title = "Topics by Top 10 City",
         yaxis=list(title =""))

x_words %>% mutate(gamma1=ifelse(gamma>0.5,"1","0")) %>% filter(city=="PARIS") 




################   ANALISIS DE SENTIMIENTOS ######################

library(sentimentr)
x<-Project.Sindicate %>% filter(str_detect(date,"2020"))

a<-x[1:40,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

b<-x[41:80,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

c<-x[81:120,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

d<-x[121:160,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

e<-x[161:200,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

f<-x[201:250,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

g<-x[251:300,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

h<-x[301:350,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

i<-x[351:400,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

j<-x[401:450,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

k<-x[451:500,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

l<-x[501:550,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

m<-x[551:600,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

n<-x[601:651,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

ñ<-x[652:700,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

o<-x[701:750,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

p<-x[751:800,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

q<-x[801:850,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

r<-x[851:900,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

s<-x[901:950,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

t<-x[951:1000,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

u<-x[1001:1050,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

v<-x[1051:1100,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()

z<-x[1100:1174,] %>% sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% select(body,sentiment,title,date) %>% as_tibble()


polaridad<-rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,ñ,o,p,q,r,s,t,u,v,z)
  
#eliminar puntos y cosas raras

polaridad<-polaridad %>% filter(body!=",  ."&body!="."&body!="or"&body!="Log in"&body!="?"&
                                  body!="Make your inbox smarter.")



polaridad$date2<-mdy(polaridad$date)


polaridad %>% group_by(title,date2) %>% 
  summarize(avg_sentiment=mean(sentiment)) %>% group_by(month=floor_date(date2, "month")) %>% 
  summarize(avg_sentiment=mean(avg_sentiment))%>% 
  ggplot(aes(x=month, y=avg_sentiment)) +
  geom_line() 


polaridad %>% group_by(title,date2) %>% 
  summarize(avg_sentiment=mean(sentiment)) %>% group_by(week=floor_date(date2, "week")) %>% 
  summarize(avg_sentiment=mean(avg_sentiment))%>% 
  ggplot(aes(x=week, y=avg_sentiment)) +
  geom_line() 

#mirar articulos en octubre

polaridad %>% group_by(title,date2) %>% 
  summarize(avg_sentiment=mean(sentiment))%>%
  filter(str_detect(date2,"2020-10")) %>% 
  arrange((avg_sentiment))%>%print(n=15)


#NOVIEMBRE EL VETO de hungria y polonia al recovery fund

#Miremos lo mas positivo

polaridad %>% group_by(title,date2) %>% 
  summarize(avg_sentiment=mean(sentiment))%>%
  filter(str_detect(date2,"2020-01")) %>% 
  arrange(desc(avg_sentiment))%>%print(n=15)

####miremos por semana

polaridad %>% group_by(title,date2) %>% 
  summarize(avg_sentiment=mean(sentiment)) %>% mutate(week=floor_date(date2, "week")) %>% 
  filter(week=="2020-01-19") %>% arrange(desc(avg_sentiment))

#en enero esa semana es tan positiva por un par de articulos

#The Case for Consumption Equality 0.324
#A Data Revolution for All 0.218
#Combating Child Labor in Global Supply Chains 0.206
#The War on Talent
#

polaridad %>% group_by(title,date2) %>% 
  summarize(avg_sentiment=mean(sentiment)) %>% group_by(week=floor_date(date2, "week")) %>% 
  summarize(avg_sentiment=mean(avg_sentiment)) %>% arrange(avg_sentiment) %>% print(n=15)

polaridad %>% group_by(title,date2) %>% 
  summarize(avg_sentiment=mean(sentiment)) %>% mutate(week=floor_date(date2, "week")) %>% 
  filter(week=="2020-07-19") %>% arrange(avg_sentiment) %>% print(n=15)

#COVID-19 Is Clarifying the Climate Challenge /el efecto del covd sobre los mas pobres


#Controversial ruling by Germany's Federal Constitutional Court against the European Central Bank
#The EU's Nullification Crisis

#By challenging the right of an EU court to rule on the European Central Bank's monetary-policy 
#decisions, Germany's Federal Constitutional Court most likely has picked a fight it cannot win.


#A recent ruling by Germany's Federal Constitutional Court (GCC) has opened a deep rift 
#in the eurozone. In three months, the Bundesbank will be prohibited from participating 
#in the European Central Bank's Public Sector Purchase Program (PSPP) unless the GCC
#receives a satisfactory explanation that the ECB's bond buying constitutes a 
#"proportionate" measure for maintaining price stability.

##Ahora arreglar descendentemente

#Octubre

polaridad %>% group_by(title,date2) %>% 
  summarize(avg_sentiment=mean(sentiment)) %>% mutate(week=floor_date(date2, "week")) %>% 
  filter(week=="2020-11-22") %>% arrange(avg_sentiment) %>% print(n=15)

#articulos de como Biden iba a restablecer relaciones diplomaticas con China y el mundo.

#############MAS EMOCIONES


#Podemos encontrar las emociones con el comando emotion

emotions<-emotion_by(x[1:5,]$body)

x[1100,]$body


Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                               ",  .  .", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "or\\n", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "Log in\\n", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "\\?\\n", ""))

Project.Sindicate<-Project.Sindicate %>% mutate(body=str_replace(Project.Sindicate$body,
                                                                 "Make your inbox smarter.", ""))

x<-Project.Sindicate %>% filter(str_detect(date,"2020"))

x[1100,]$body



####Obviar ciertas palabras como



nrc_key <- lexicon::hash_nrc_emotions %>% 
  dplyr::filter(
  emotion %in% c('trust',"anger","anticipation","fear","sadness","surprise","disgust","joy")) %>% 
  dplyr::filter(token %in% c("policy","trade","bank","economy","president","school","pay","money",
                             "cash","official","star","committee","cabinet","law","diplomatic",
                             "framework","system","journalism","doctor","white","labor","lesson",
                        "governor","academic","ambassador","chancellor","budget","food",
                        "organization","elect","personal","trump","officer","calls","professional",
                        "reporter","crew","income","center","chairman","director","educational",
                        "associate","deputy","association","attorney","carol","general",
                        "council","professor","laureate","ministry","scientific","politics",
                        "politic"))


nrc_key<-lexicon::hash_nrc_emotions %>%
  dplyr::filter(
    emotion %in% c('trust',"anger","anticipation","fear","sadness","surprise","disgust","joy")) %>% 
      anti_join(nrc_key) %>% 
  filter(token!="police"|emotion!="trust") %>%
  filter(token!="policeman"|emotion!="trust") %>%
  filter(token!="medical"|emotion!="trust") %>% 
  filter(token!="medical"|emotion!="anticipation")  

#####Reemplazar terminos por palabras



x<-x %>% mutate(body=str_replace(x$body,"United Kingdom", "U.K"))

x<-x %>% mutate(body=str_replace(x$body,"Central Bank", "C.B"))

x<-x %>% mutate(body=str_replace(x$body,"White House", "W.H"))

x<-x %>% mutate(body=str_replace(x$body,"United States", "U.S.A"))

x<-x %>% mutate(body=str_replace(x$body,"UN Security Council", "U.S.C"))

x<-x %>% mutate(body=str_replace(x$body,"United Nations", "UN"))

x<-x %>% mutate(body=str_replace(x$body,"General Assembly", "G.A"))

x<-x %>% mutate(body=str_replace(x$body,"United Arab Emirates", "UAE"))

x<-x %>% mutate(body=str_replace(x$body,"United Nations", "UN"))

x<-x %>% mutate(body=str_replace(x$body,"World Bank", "UN"))



xxx<-extract_emotion_terms(x[1:5,]$body,emotion_dt =nrc_key)




aa1<-emotion_by(x[1:50,]$body,emotion_dt =nrc_key)

bb<-emotion_by(x[51:100,]$body,emotion_dt =nrc_key)

cc<-emotion_by(x[101:150,]$body,emotion_dt =nrc_key)

dd<-emotion_by(x[151:200,]$body,emotion_dt =nrc_key)

ee<-emotion_by(x[201:250,]$body,emotion_dt =nrc_key)

ff<-emotion_by(x[251:300,]$body,emotion_dt =nrc_key)

gg<-emotion_by(x[301:400,]$body,emotion_dt =nrc_key)

hh<-emotion_by(x[401:500,]$body,emotion_dt =nrc_key)

ii<-emotion_by(x[501:600,]$body,emotion_dt =nrc_key)

jj<-emotion_by(x[601:700,]$body,emotion_dt =nrc_key)

kk<-emotion_by(x[701:800,]$body,emotion_dt =nrc_key)

mm<-emotion_by(x[801:900,]$body,emotion_dt =nrc_key)

nn<-emotion_by(x[901:1000,]$body,emotion_dt =nrc_key)

ññ<-emotion_by(x[1001:1100,]$body,emotion_dt =nrc_key)

oo<-emotion_by(x[1101:1174,]$body,emotion_dt =nrc_key)

emociones<-rbind(aa1,bb,cc,dd,ee,ff,gg,hh,ii,jj,kk,mm,nn,ññ,oo)

xxx<-x %>% filter(title=="What the G20 Should Do Now")
xxx<-extract_emotion_terms(xxx$body,emotion_dt =nrc_key)



vec <- c(1:1174)

emociones$id<-rep(vec, each = 16)


x$id<-1:1174


x1<-x %>% select(date,title,id)

emociones<-inner_join(emociones,x1)




emociones$date2<-mdy(emociones$date)


emociones %>% 
  group_by(month=floor_date(date2, "month"),emotion_type) %>% 
  summarize(ave_emotion=mean(ave_emotion))%>% 
  filter(str_detect(emotion_type,"negated")==FALSE) %>% 
  ggplot(aes(x=month, y=ave_emotion,colour=emotion_type )) +
  geom_line() 



emociones %>% 
  group_by(week=floor_date(date2, "week"),emotion_type) %>% 
  summarize(ave_emotion=median(ave_emotion))%>% 
  filter(str_detect(emotion_type,"negated")==FALSE) %>% 
  ggplot(aes(x=week, y=ave_emotion,colour=emotion_type )) +
  geom_line() 

emociones %>% filter(emotion_type=="trust") %>% 
  arrange(desc(ave_emotion)) %>% select(title,ave_emotion,date) %>% as_tibble() %>% 
  print(n=20)


emociones %>% filter(emotion_type=="trust") %>% filter(str_detect(date,"Jun")) %>% 
  arrange(desc(ave_emotion)) %>% select(title,ave_emotion,date) %>% as_tibble() %>% 
  print(n=20)
#negación

emociones %>% pivot_wider(names_from = emotion_type, 
                               values_from = c(ave_emotion,sd,word_count,emotion_count)) %>% 
  mutate(anger=ave_emotion_anger-ave_emotion_anger_negated,
         anticipation=ave_emotion_anticipation-ave_emotion_anticipation_negated,
         disgust=ave_emotion_disgust-ave_emotion_disgust_negated,
         fear=ave_emotion_fear-ave_emotion_fear_negated,
         joy=ave_emotion_joy-ave_emotion_joy_negated,
         sadness=ave_emotion_sadness-ave_emotion_sadness_negated,
         surprise=ave_emotion_surprise-ave_emotion_surprise_negated,
         trust=ave_emotion_trust-ave_emotion_trust_negated) %>% select(date2,70:77) %>% 
  pivot_longer(c(2:9), names_to = "emotion_type", values_to = "ave_emotion")%>% 
  group_by(month=floor_date(date2, "month"),emotion_type) %>% 
  summarize(ave_emotion=median(ave_emotion))%>% 
  filter(str_detect(emotion_type,"negated")==FALSE) %>% 
  ggplot(aes(x=month, y=ave_emotion,colour=emotion_type )) +
  geom_line() 

#####################################word count
  

emociones %>% 
  group_by(month=floor_date(date2, "month"),emotion_type) %>% 
  summarize(ave_emotion=sum(emotion_count))%>% 
  filter(str_detect(emotion_type,"negated")==FALSE) %>% 
  group_by(month) %>% mutate(total=sum(ave_emotion),perc=(ave_emotion*100)/total) %>% 
  ggplot(aes(x=month, y=perc,colour=emotion_type )) +
  geom_line() 



emociones %>% pivot_wider(names_from = emotion_type, 
                          values_from = c(ave_emotion,sd,word_count,emotion_count)) %>% 
  mutate(anger=ave_emotion_anger-ave_emotion_anger_negated,
         anticipation=ave_emotion_anticipation-ave_emotion_anticipation_negated,
         disgust=ave_emotion_disgust-ave_emotion_disgust_negated,
         fear=ave_emotion_fear-ave_emotion_fear_negated,
         joy=ave_emotion_joy-ave_emotion_joy_negated,
         sadness=ave_emotion_sadness-ave_emotion_sadness_negated,
         surprise=ave_emotion_surprise-ave_emotion_surprise_negated,
         trust=ave_emotion_trust-ave_emotion_trust_negated) %>% select(date2,70:77) %>% 
  pivot_longer(c(2:9), names_to = "emotion_type", values_to = "ave_emotion")




trust<-get_sentiments("nrc") %>% filter(sentiment=="trust")







x_words %>% mutate(gamma1=ifelse(gamma>0.5,"1","0")) %>% filter(gamma1==1) %>% 
  count(city, topic) %>% group_by(city) %>%  mutate(prop = n / sum(n)) %>% 
  mutate(topic= case_when(topic == 1 ~ 2,
                          topic == 2 ~ 6,
                          topic == 3 ~ 10,
                          topic == 4 ~ 4,
                          topic == 5 ~ 8,
                          topic == 6 ~ 9,
                          topic == 7 ~ 7,
                          topic == 8 ~ 5,
                          topic == 9 ~ 1,
                          topic == 10 ~ 3)) %>% 
  plot_ly(x =~prop, y = ~city, color = ~as.factor(topic),colors = "Blues") %>% 
  add_bars() %>% 
  layout(barmode = "stack",title = "Topics By Top 10 City",
         yaxis=list(title =""))




polaridad %>% group_by(title,date2) %>% 
  summarize(avg_sentiment=mean(sentiment)) %>% group_by(week=floor_date(date2, "week")) %>% 
  summarize(avg_sentiment=mean(avg_sentiment)) %>% 
  plot_ly( x = ~week) %>% add_trace(y = ~avg_sentiment, name = 'trace 0',mode = 'lines')%>% 
  layout(title = "Sentiment Analysis (Polarity) 2020")





```{r,echo=FALSE}
MONTH_tf_idf %>%
  group_by(trimester) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = trimester)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~trimester, ncol = 2, scales = "free_y") +
  labs(x = "tf-idf", y = NULL)+
  theme_light()+ggtitle("TF-IDF By Year Trimester (2020)")
```

Let's use bigrams to have a better idea.


```{r,echo=FALSE}
bigram_tf_idf %>%
  group_by(trimester) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  mutate(trimester = as.factor(trimester),
         bigram = reorder_within(bigram, tf_idf, trimester)) %>% 
  ggplot(aes(tf_idf, bigram, fill = trimester)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~trimester, ncol = 2, scales = "free_y") +
  scale_y_discrete(labels = function(x) gsub("__.+$", "", x))+
  theme_light()+ggtitle("TF-IDF By Year Trimester (2020)")

```





