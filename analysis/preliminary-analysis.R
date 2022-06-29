library(tidyverse)
library(tidygraph)
library(ggraph)
library(readxl)
library(scales)
library(ggthemes)
library(igraph)
library(RColorBrewer)
library(intergraph)
library(ergm.count)

### LOAD DATA AND REMOVE DUPLICATES
# authors = academics with profiles on google scholar
authors <- read_excel('../data/consolidated-data.xlsx',4) %>%
  group_by(scholar_id) %>%
  mutate(row = row_number()) %>%
  filter(row == 1) %>%
  select(-row) %>%
  ungroup() %>%
  mutate(email_tld = str_extract(email_domain,'\\.[^\\.]+$'))

# author_tags = the self-selected 'interests' identified on google scholar profiles
# @todo: some consolidation of similar tags is done below, but this could be more comprehensive
author_tags <- read_excel('../data/consolidated-data.xlsx',5) %>%
  distinct() %>%
  mutate(tag2 = str_to_title(tag)) %>%
  left_join(read_csv('tag-recoding.csv',col_names = c('tag','recoding'))) %>%
  mutate(codedTag = ifelse(is.na(recoding),tag2,recoding))

# author_cites_per_year = number of citations across all of an author's publications
author_cites_per_year <- read_excel('../data/consolidated-data.xlsx',6) %>%
  group_by(scholar_id,year) %>%
  arrange(desc(citations)) %>%
  mutate(row = row_number()) %>%
  filter(row == 1) %>%
  select(-row) %>%
  ungroup()

# articles = publications harvested from google scholar profiles
articles <- read_excel('../data/consolidated-data.xlsx',1) %>%
  filter(is.na(cite_id)) %>% 
  group_by(pub_year,title) %>%
  arrange(desc(num_citations)) %>%
  mutate(row = row_number()) %>%
  filter(row == 1) %>%
  select(-row) %>%
  ungroup() %>%
  bind_rows(
    articles <- read_excel('../data/consolidated-data.xlsx',1) %>%
      filter(!is.na(cite_id)) %>% 
      group_by(cite_id) %>%
      arrange(desc(num_citations)) %>%
      mutate(row = row_number()) %>%
      filter(row == 1) %>%
      select(-row) %>%
      ungroup()
  )

# article_name = the names associated as authors of each articles (which may correspond to authors)
article_author <- read_excel('../data/consolidated-data.xlsx',2) %>%
  distinct() %>%
  arrange(cite_id,author,order) %>%
  group_by(cite_id,author) %>%
  mutate(row = row_number()) %>%
  filter(row == 1) %>%
  select(-row) %>%
  ungroup()

# article_cites_per_year = the total number of times an article has been cited each year
article_cites_per_year <- read_excel('../data/consolidated-data.xlsx',3) %>%
  group_by(cite_id,year) %>%
  arrange(desc(citations)) %>%
  mutate(row = row_number()) %>%
  filter(row == 1) %>%
  select(-row) %>%
  ungroup()

# name_scholar_lookup = links between author names and author scholar profile identifiers
name_scholar_lookup <- read_excel('../data/consolidated-data.xlsx',7) %>%
  group_by(name) %>%
  mutate(row = row_number()) %>%
  filter(row == 1) %>%
  select(-row) %>%
  ungroup()


# join the article_author/scholar data
article_author <- article_author %>%
  left_join(name_scholar_lookup, by = c('author' = 'name'))


# use google scholar photos as the first source of gender information  
photo_gender <- read_excel('../data/consolidated-data.xlsx',8) %>%
  group_by(scholar_id) %>%
  mutate(row = row_number()) %>%
  filter(row == 1) %>%
  select(-row,-filename) %>%
  ungroup()

# use a classification of first names as a secondary source
name_gender <- read_excel('../data/consolidated-data.xlsx',9) %>%
  filter(!scholar_id %in% photo_gender$scholar_id) %>%
  group_by(scholar_id) %>%
  mutate(row = row_number()) %>%
  filter(row == 1) %>%
  select(-row) %>%
  ungroup()

# add gender data to authors tibble
gender <- photo_gender %>% bind_rows(name_gender)
authors <- authors %>% left_join(gender)

# add country and continent information to authors based on the tld of their profile email address
tld <- read_csv('emailtld.csv')
authors <- authors %>% left_join(tld, by = c('email_tld' = 'tld'))

# have the list of manually identified game_studies scholars as a separate tibble
manual_authors <- read_csv('manual-name-scholar-ids.csv')



###############
###############

# colours for gender plots
reds <- brewer.pal(3,'Reds')
blues <- brewer.pal(3,'Blues')
purples <- brewer.pal(3,'Purples')

# Get a subset of Game Studies authors based on having that tag in their profile
gameStudiesAuthors <- authors %>%
  filter(scholar_id %in% manual_authors$scholar_id | scholar_id %in% (author_tags %>% filter(str_to_lower(tag) == 'game studies'))$scholar_id)


# Get a subset of Game Studies articles based on keywords in publication titles
# @todo: validate this approach
gameArticles <- articles %>%
  filter(str_detect(title,".*gam[ei]*") |
           str_detect(title,".*play*") |
           str_detect(title,".*spie?l*") |
           str_detect(title,".*juego*") |
           str_detect(title,"twitch") |
           str_detect(title,"console")
  )


### OVERALL GENDER SPLIT OF GAME STUDIES SCHOLARS ###
gameArticleAuth <- article_author %>%
  filter(cite_id %in% gameArticles$cite_id) %>%
  inner_join(authors) %>%
  inner_join(articles) %>%
  select(cite_id,order,scholar_id,gender,citedby,citedby5y,country,continent,pub_year,num_citations) %>%
  rename(pub_citations = num_citations)

authorCount <- gameArticleAuth %>% group_by(cite_id) %>% summarise(authorCount = n())


geoData <- gameArticleAuth %>%
  filter(order == 1) %>%
  group_by(country) %>%
  filter(is.na(country) == FALSE) %>%
  summarise(citations = sum(pub_citations, na.rm = TRUE))

top <- geoData %>% slice_max(citations,n = 10)
geoData <- geoData %>%
  mutate(country = ifelse(country %in% top$country,country,'Other')) %>%
  group_by(country) %>%
  summarise(citations = sum(citations,na.rm = TRUE))

ggplot(geoData,aes(fct_reorder(str_wrap(country,12),citations,.desc = TRUE),citations)) +
  geom_col(fill = blues[3]) +
  scale_y_continuous(labels = comma_format()) +
  theme_hc() +
  labs(
    title = 'Number of Game Studies Citations, by Country of First-Author',
    y = 'Citations',
    x = NULL
  )
  

genderData <- gameStudiesAuthors %>%
  count(gender) %>%
  filter(!is.na(gender)) %>%
  mutate(pct = n/sum(n)) %>%
  mutate(type = 'Number of\nSelf-identifying Authors') %>%
  bind_rows(
    gameArticleAuth %>%
      count(gender) %>%
      filter(!is.na(gender)) %>%
      mutate(pct = n/sum(n)) %>%
      mutate(type = 'Number of\nAuthored Publications')
  ) %>%
  bind_rows(
    gameArticleAuth %>%
      filter(order == 1) %>%
      count(gender) %>%
      filter(!is.na(gender)) %>%
      mutate(pct = n/sum(n)) %>%
      mutate(type = 'Number of First\nAuthor Publications')
  ) %>%
  bind_rows(
    gameArticleAuth %>%
      group_by(gender) %>%
      summarise(n = sum(pub_citations,na.rm = TRUE)) %>%
      filter(!is.na(gender)) %>%
      mutate(pct = n/sum(n)) %>%
      mutate(type = 'Number of\nCitations')
  ) %>%
  bind_rows(
    gameArticleAuth %>%
      filter(order == 1) %>%
      group_by(gender) %>%
      summarise(n = sum(pub_citations,na.rm = TRUE)) %>%
      filter(!is.na(gender)) %>%
      mutate(pct = n/sum(n)) %>%
      mutate(type = 'Number of First\nAuthor Citations')
  ) %>%
  mutate(type = factor(type,levels = c('Number of\nSelf-identifying Authors',
                                       'Number of\nAuthored Publications',
                                       'Number of First\nAuthor Publications',
                                       'Number of\nCitations',
                                       'Number of First\nAuthor Citations')))


ggplot(genderData,aes(type,pct,fill=gender,label = scales::percent(pct, accuracy = 1))) +
  geom_col() +
  geom_text(position = position_stack(vjust = 0.5), colour = 'white') +
  scale_fill_manual(values = c(reds[3],blues[3])) +
  scale_y_continuous(labels = percent_format()) +
  theme_hc() +
  labs(
    title = 'Game Studies Scholarship by Gender',
    fill = NULL,
    x = NULL,
    y = NULL
  )


### HIGH CITATION
citationData <- gameArticleAuth %>%
  group_by(scholar_id,gender) %>%
  summarise(citations = sum(pub_citations)) %>%
  filter(citations > 500) %>%
  mutate(gender = ifelse(is.na(gender),'Unspecified',gender)) %>%
  top_n(100,desc(citations))

ggplot(citationData,aes(fct_reorder(scholar_id,citations,.desc = TRUE),citations,fill=gender)) +
  geom_col() +
  theme_hc() +
  scale_fill_manual(values = c(reds[3],blues[3],purples[3])) +
  scale_y_continuous(labels = comma_format()) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    title = 'Top Cited Game Studies Scholars',
    subtitle = 'Total Number of Citations, by Gender',
    fill = NULL,
    y = 'Total Citations'
  )


# HOW IS CITATION OF FEMALE LEAD-AUTHOR ARTICLES CHANGING OVER TIME
genderData2 <- article_cites_per_year %>%
  filter(cite_id %in% gameArticles$cite_id) %>%
  inner_join(article_author %>% filter(order == 1)) %>%
  inner_join(authors) %>%
  select(year,citations,gender,country,continent) %>%
  group_by(year,gender) %>%
  summarise(n = sum(citations,na.rm = TRUE)) %>%
  filter(!is.na(gender)) %>%
  mutate(pct = n/sum(n)) %>%
  mutate(type = 'Cited Articles') %>%
  bind_rows(
    gameArticles %>%
      inner_join(article_author) %>%
      filter(order == 1) %>%
      inner_join(authors) %>%
      select(pub_year,gender,country,continent) %>%
      group_by(pub_year,gender) %>%
      summarise(n = n()) %>%
      filter(!is.na(gender)) %>%
      mutate(pct = n/sum(n)) %>%
      rename(year = pub_year) %>%
      mutate(type = 'Number of Publications')
  )

genderData2 %>% filter(gender == 'Female' & year >= 2001 & year <= 2021) %>%
  ggplot(aes(year,pct,colour = type,label = scales::percent(pct,accuracy = 1))) +
  geom_line(size=1) +
  geom_text(data = . %>% filter(year == 2021),vjust = c(-1,-1), size = 4, show.legend = FALSE) +
  geom_point(data = . %>% filter(year == 2021),size = 4, show.legend = FALSE) +
  theme_hc() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = 'Female Game Studies Scholarship, 2001-2021',
    subtitle = 'Proportion of First-Author Articles by Females: Publications per year // Citations per year',
    x = 'Year',
    y = NULL,
    colour = NULL
  )


# GAME STUDIES SCHOLARS
gameStudyAuthorTags <- author_tags %>%
  filter(scholar_id %in% (author_tags %>% filter(codedTag == 'Game Studies'))$scholar_id) %>%
  left_join(authors) %>%
  select(scholar_id,tag,gender,codedTag)


data <- gameStudyAuthorTags %>%
  filter(codedTag != 'Game Studies')
overallCounts <- data %>%
  count(codedTag) %>%
  filter(n >= 11) %>%
  mutate(tag2 = str_wrap(codedTag,8)) %>%
  mutate(pctOverall = n/nrow(gameStudiesAuthors)) %>%
  arrange(desc(n))
data <- data %>%
  filter(codedTag %in% overallCounts$codedTag) %>%
  group_by(codedTag,gender) %>%
  filter(!is.na(gender)) %>%
  summarise(n2 = n()) %>%
  mutate(pct = n2 / sum(n2)) %>%
  left_join(overallCounts, by = 'codedTag') %>%
  mutate(tag2 = factor(tag2,levels = overallCounts$tag2))

ggplot(data,aes(tag2,n2,fill=gender)) + 
  geom_col() +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1),y=n),
    data = data %>% filter(gender == 'Female'),
    vjust = -.5,
    colour = reds[3]) +
  theme_hc() +
  labs(
    title = 'Additional Google Scholar Tags used by Game Studies Academics',
    subtitle = 'By Gender',
    x = NULL,
    y = 'Number of all Game Studies Scholars',
    fill = NULL
  )



## TOP 30 most cited game studies scholars
top30ScholarIds <- gameArticleAuth %>%
  group_by(scholar_id) %>%
  summarise(citations = sum(pub_citations)) %>%
  slice_max(citations,n=30)

top30CiteIds <- gameArticleAuth %>% filter(scholar_id %in% top30ScholarIds$scholar_id) %>% select(cite_id) %>% distinct()

gameArticleAuthByTop30 <- gameArticleAuth %>%
  filter(cite_id %in% top30CiteIds$cite_id)

top30Data <- top30ScholarIds %>%
  inner_join(authors) %>%
  select(scholar_id,citations,name,gender,continent)

top30Data %>% ggplot(aes(fct_reorder(name,citations,.desc = TRUE),citations,fill=gender)) +
  geom_col() +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  theme_hc() +
  labs(
    title = 'Top 30 Most Cited Game Studies Authors',
    fill = NULL,
    y = 'Number of Citations in Game Studies Scholarship',
    x = NULL
  )

# create a bipartite network of scholars and tags
vAuthors <- gameArticleAuth %>%
  filter(order == 1) %>%
  select(scholar_id,gender,citedby,citedby5y,country,continent) %>%
  distinct() %>%
  mutate(id = paste0('a',as.character(row_number()))) %>%
  mutate(type = FALSE)

vTags <- author_tags %>%
  filter(scholar_id %in% vAuthors$scholar_id) %>%
  select(codedTag) %>%
  distinct() %>%
  mutate(id = paste0('t',as.character(row_number()))) %>%
  rename(name = codedTag) %>%
  select(id,name) %>%
  mutate(type = TRUE)

vertices = vAuthors %>%
  bind_rows(vTags)

edges <- gameArticles %>%
  inner_join(article_author) %>%
  filter(order == 1) %>%
  select(scholar_id) %>%
  inner_join(author_tags) %>%
  inner_join(vTags, by = c('codedTag' = 'name')) %>%
  inner_join(vAuthors, by = 'scholar_id') %>%
  rename(from = id.x, to = id.y) %>%
  select(from,to)

g <- tbl_graph(nodes = vertices %>% select(id,gender,type), edges = edges, directed = FALSE) %>%
  activate(nodes) %>%
  mutate(name = vertices$name)
graphs <- bipartite.projection(g)
tagGraph <- as_tbl_graph(graphs$proj2) %>%
  filter(name != 'Game Studies') %>%
  activate(edges) %>%
  filter(weight > 1) %>%
  activate(nodes) %>%
  mutate(allDegree = centrality_degree(mode='all')) %>%
  filter(allDegree > 15)
weights <- E(tagGraph)$weight
tagGraph <- tagGraph %>%
  mutate(cluster = group_louvain(weights=weights))

tagGraph %>%
  ggraph() + 
  geom_edge_link(
    edge_colour='gray',
    aes(edge_width = weight),
    alpha = 0.8,
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_point(
    aes(size = allDegree,colour=as.factor(cluster)),
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_colour_brewer(palette = 'Dark2') +
  geom_node_text(
    aes(label = str_wrap(name,10),color=as.factor(cluster)),
    show.legend = FALSE,
    check_overlap = TRUE
  ) +
  theme_graph()
  


# co-authorship network
vAuthors <- authors %>%
  select(scholar_id,name,gender,citedby,citedby5y,country,continent) %>%
  filter(scholar_id %in% gameArticleAuthByTop30$scholar_id) %>%
  distinct() %>%
  mutate(id = paste0('a',as.character(row_number()))) %>%
  mutate(gender = ifelse(is.na(gender),'Unspecified',gender)) %>%
  mutate(continent = ifelse(is.na(continent),'Unspecified',continent)) %>%
  mutate(type = FALSE)

vArticles <- gameArticles %>%
  filter(cite_id %in% gameArticleAuthByTop30$cite_id) %>%
  select(cite_id) %>%
  mutate(name = cite_id) %>%
  distinct() %>%
  mutate(id = paste0('p',as.character(row_number()))) %>%
  mutate(type = TRUE)

vertices = vAuthors %>%
  bind_rows(vArticles)

edges <- gameArticleAuthByTop30 %>%
  inner_join(vArticles, by = 'cite_id') %>%
  inner_join(vAuthors, by = 'scholar_id') %>%
  rename(from = id.x, to = id.y) %>%
  select(from,to)


g <- tbl_graph(nodes = vertices %>% select(id,gender,type,continent,citedby,citedby5y), edges = edges, directed = FALSE) %>%
  activate(nodes) %>%
  mutate(name = vertices$name)
graphs <- bipartite.projection(g)
authorGraph <- as_tbl_graph(graphs$proj1) %>%
  activate(edges) %>%
  filter(weight > 1) %>%
  activate(nodes) %>%
  mutate(allDegree = centrality_degree(mode='all')) %>%
  filter(allDegree > 1) %>%
  mutate(component = group_components())
weights <- E(authorGraph)$weight
authorGraph <- authorGraph %>%
  mutate(cluster = group_louvain(weights=weights))

authorGraph %>%
  ggraph(layout = 'dh') + 
  geom_edge_link(
    edge_colour='gray',
    aes(edge_width = weight),
    alpha = 0.8,
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_point(
    aes(size = allDegree,colour=as.factor(cluster)),
    alpha = 0.5,
    show.legend = FALSE
  ) +
  geom_node_text(aes(filter=allDegree > 12,label = str_wrap(name,12)), size = 4) +
  scale_colour_brewer(palette = 'Dark2') +
  theme_graph()


coAuthorNetwork <- asNetwork(authorGraph)
coAuthor.models.baseline <- ergm(coAuthorNetwork ~ edges)
summary(coAuthor.models.baseline)
coAuthor.models.1 <- ergm(coAuthorNetwork ~ edges +
                          gwesp(log(2),fixed=TRUE) +
                          nodefactor('gender') +
                          nodematch('gender') +
                          nodecov('citedby5y') +
                          nodematch('continent') +
                          absdiff('citedby5y'))
summary(coAuthor.models.1)
