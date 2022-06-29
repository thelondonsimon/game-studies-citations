# game-studies-citations

SICSS Sydney project examining citation trends in Game Studies scholarship.

This repository contains Python and R code used for data retrieval and analysis of academic publications in the field of Game Studies.

## Installation

The python scripts depend on `scholarly` and `pandas` which can be installed by running:
```
pip install requirements.txt
```

## Data

Game studies data was harvested using the scripts detailed below to query all Google Scholar profiles identified with **Game Studies**, supplemented by a [manually assembled list of additional authors](data/manual-names-to-add.xlsx). All publications by each of these seed authors were added to the dataset.

The resulting data is available in the worksheets of a [consolidated Excel spreadsheet](data/consolidated-data.xlsx):

- **publications**: Individual academic publications; `cite_id` uniquely identifies the publication in Google Scholar, however not all publications are assigend this value by Google Scholar
- **publications-author**: Links publications (`cite_id`) to author names (`author`), including the position (`order`) of the author in the list of all authors
- **publications-cites-per-year**: The total number of citations a publication (`cite_id`) has received in a given year
- **authors**: Individual academic profiles; `scholar_id` uniquely identifies the author; `input_name` is the term which was used to search for the profile
- **author-tags**: The disciplinary tags which individual authors have associated with their Google Scholar profile
- **author-cites-per-year**: The total number of citations an author (`scholar_id`) has recieved in a given year
- **name-to-scholar-id**: Provides links between the names in **publications-author** and `scholar_id` of academic profiles
- **photo-gender**: Gender classification of academics (`scholar_id`) based on algorithmic analysis of their profile picture
- **name-gender**: Gender classification of academics (`scholar_id`) based on API analysis of their first name

### Tag Recoding
[tag-recoding.csv](analysis/tag-recoding.csv) re-codes the free-form tags which academics can associate with their Google Scholar profile by coalescing variants which use slightly different terminology or languages.

### Top Level Domains
[emailtld.csv](analysis/emailtld.csv) maps the top-level domains found in the email addresses listed in Google Scholar profiles to their respective country and continent to permit geographic analysis.

## Data Retrieval scripts

### search-scholarship.py
This script uses the `scholarly` library to harvest author and publication data.

Due to bot-limitations, it is currently limited to searching based on querying Google Scholar profiles rather than publications.

To run the script, alter the following variables defined at the top of the script:
- `firstRun`: should typically be set to `True`, unless you want to re-run a script to append data to existing output files
- `filePrefix`: a prefix which is used to uniquely name the various CSV outputs relating to the current search, e.g. `game-studies`
- `queryType`: determines the kind of query - either **keyword** (e.g. a tag or name) or **scholar_id** to retrieve data for a known list of Google Schoalr profile identifiers
- `searchQueries`: a Python list of strings to form the basis of author queries, e.g. `['game_studies','gamification']` or `['rOcL0NgAAAAJ','ysa30PIAAAAJ']`

Running the script will then produce the following CSV outputs:
- **authors**: Authors matching the query
- **authors-tags**: Disciplinary tags associated with each author
- **authors-cites-per-year**: The total number of citations for an author for a particular year
- **authors-coatuhors**: Nominated coatuhors listed on an author's profile
- **publications**: Publications identified by iterating through each of the publications listed for each author
- **publications-cites-per-year**: The total number of citations for a publication for a particular year
- **publciations-authors**: The raw names of each publication's authors as derived from the citation field

### coauthor-scholar-lookup.py
This script is designed to identify the Google Scholar profile for coauthors identified through the `search-scholarship.py` script above.

Because `scholarly` only returns the authorship of publciations as names - without any identifiers - this step partially addresses this by enabling a link to be formed between a name and a Google Scholar record. This also helps address the multiple name tokens which can be used to reference the same author.

The script's input is a list of name tokens and a single `cite_id` (the citation identifier used by Google Scholar to identify a publication) which the previous script has identified as being a publication on which that name was a co-author.

`scholarly` is used to find matching results for each name token, and then looks through their publications to see if it contains the corresponding cite_id. For performance reasons only the top 5 results are examined. Matches are output using the same **-author-** files as above, while also including the original name token to permit linking of data.

## Analysis
The preliminary analysis conducted in R is documented in [preliminary-analysis.R](analysis/preliminary-analysis.R).