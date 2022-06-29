from scholarly import scholarly
import csv

#########################################################################################
### This script captures author and publication data from Google Scholar based on either
### a list of search terms (e.g. tags or a list of names)
###
### The scholarly package is used to store the following outputs:
### - 1. ./data/{prefix}-authors.csv                    Author profile information
### - 2. ./data/{prefix}-authors-tags.csv               Author tags
### - 3. ./data/{prefix}-authors-cites-by-year.csv      Author citations per year
### - 4. ./data/{prefix}-authors-coauthors.csv          Author nominated coauthors
### - 5. ./data/{prefix}-publications.csv               Publication information
### - 6. ./data/{prefix}-publications-cites-by-year.csv Publication citations per year
### - 7. ./data/{prefix}-publications-authors.csv       Publication authors
###
#########################################################################################


# configuration
firstRun = False                    # [True | False] denotes if this is the first time this script is being run to perform lookups
filePrefix = 'gs_manual'            # A prefix to use in file outputs
queryType = 'scholarid'             # [scholarid | keyword]
searchQueries = []                  # A list of queries to search


# setup the CSV output files based on whether this is the first time the script is being run
outputWriteMode = 'w' if firstRun == True else 'a'

fAuthors = open('./data/' + filePrefix + '-authors.csv',outputWriteMode, encoding='UTF8', newline = '')
wAuthors = csv.writer(fAuthors)
if firstRun:
    wAuthors.writerow(['scholar_id','name','affiliation','url_picture','email_domain','organization','homepage','citedby','citedby5y','hindex','hindex5y','i10index','i10index5y'])


fAuthTags = open('./data/' + filePrefix + '-authors-tags.csv',outputWriteMode, encoding='UTF8', newline = '')
wAuthTags = csv.writer(fAuthTags)
if firstRun:
    wAuthTags.writerow(['scholar_id','tag'])


fAuthCitesByYear = open('./data/' + filePrefix + '-authors-cites-per-year.csv',outputWriteMode, encoding='UTF8', newline = '')
wAuthCitesByYear = csv.writer(fAuthCitesByYear)
if firstRun:
    wAuthCitesByYear.writerow(['scholar_id','year','citations'])


fAuthCoauthors = open('./data/' + filePrefix + '-authors-coauthors.csv',outputWriteMode, encoding='UTF8', newline = '')
wAuthCoauthors = csv.writer(fAuthCoauthors)
if firstRun:
    wAuthCoauthors.writerow(['scholar_id','coauthor_scholar_id','coauthor_name'])


fPubs = open('./data/' + filePrefix + '-publications.csv',outputWriteMode, encoding='UTF8', newline = '')
wPubs = csv.writer(fPubs)
if firstRun:
    wPubs.writerow(['cites_id','pub_year','title','citation','conference','authors','pages','publisher','abstract','num_citations'])


fPubsCitesByYear = open('./data/' + filePrefix + '-publciations-cites-per-year.csv',outputWriteMode, encoding='UTF8', newline = '')
wPubsCitesByYear = csv.writer(fPubsCitesByYear)
if firstRun:
    wPubsCitesByYear.writerow(['cite_id','year','citations'])


fPubsAuthors = open('./data/' + filePrefix + '-publlications-authors.csv',outputWriteMode, encoding='UTF8', newline = '')
wPubsAuthors = csv.writer(fPubsAuthors)
if firstRun:
    wPubsAuthors.writerow(['cite_id','author','order'])


# keep a list of ids already retrieved to avoid duplication
processedScholarIds = []
processedCiteIds = []


# process each query term
for queryTerm in searchQueries:
    print('Processing "' + queryTerm + '"')
    if queryType == 'scholarid':
        query = [scholarly.search_author_id(queryTerm)]
    else:
        scholarly.search_keyword(queryTerm)

    for author in query:
        if author['scholar_id'] in processedScholarIds:
            print('Skipping Author "' + author['name'] + '" (already processed)')
            continue

        print('Processing Author "' + author['name'] + '"')

        # put the fill function in a try/except loop as it can fail if there is unexpected data
        try:
            author = scholarly.fill(author)
        except Exception:
            print('Unable to fill author')
            continue
        
        scholar_id = author['scholar_id'] if 'scholar_id' in author else None
        name = author['name'] if 'name' in author else None
        affiliation = author['affiliation'] if 'affiliation' in author else None
        url_picture = author['url_picture'] if 'url_picture' in author else None
        email_domain = author['email_domain'] if 'email_domain' in author else None
        organization = author['organization'] if 'organization' in author else None
        homepage = author['homepage'] if 'homepage' in author else None
        citedby = author['citedby'] if 'citedby' in author else None
        citedby5y = author['citedby5y'] if 'citedby5y' in author else None
        hindex = author['hindex'] if 'hindex' in author else None
        hindex5y = author['hindex5y'] if 'hindex5y' in author else None
        i10index = author['i10index'] if 'i10index' in author else None
        i10index5y = author['i10index5y'] if 'i10index5y' in author else None

        wAuthors.writerow([scholar_id,name,affiliation,url_picture,email_domain,organization,homepage,citedby,citedby5y,hindex,hindex5y,i10index,i10index5y])
        processedScholarIds.append(scholar_id)

        if 'interests' in author:
            for tag in author['interests']:
                wAuthTags.writerow([scholar_id,tag])
        
        if 'cites_per_year' in author:
            for year,cites in author['cites_per_year'].items():
                wAuthCitesByYear.writerow([scholar_id,year,cites])

        if 'coauthors' in author:
            for coauthor in author['coauthors']:
                wAuthCoauthors.writerow([scholar_id,coauthor['scholar_id'],coauthor['name']])
        
        if 'publications' in author:
            print("--" + len(author['publications']) + " publications")
            for pub in author['publications']:
                if 'cites_id' in pub and len(pub['cites_id']) > 0:
                    citeId = pub['cites_id'][0]
                    if citeId in processedCiteIds:
                        continue
                    else:
                        processedCiteIds.append(citeId)
                else:
                    citeId = None

                # put the fill function in a try/except loop as it can fail if there is unexpected data
                try:
                    pub = scholarly.fill(pub)
                except Exception:
                    print('Unable to fill publication')
                    continue

                title = pub['bib']['title'] if 'title' in pub['bib'] else None
                pub_year = pub['bib']['pub_year'] if 'pub_year' in pub['bib'] else None
                citation = pub['bib']['citation'] if 'citation' in pub['bib'] else None
                conference = pub['bib']['conference'] if 'conference' in pub['bib'] else None
                authors = pub['bib']['author'] if 'author' in pub['bib'] else None
                pages = pub['bib']['pages'] if 'pages' in pub['bib'] else None
                publisher = pub['bib']['publisher'] if 'publisher' in pub['bib'] else None
                abstract = pub['bib']['abstract'] if 'abstract' in pub['bib'] else None
                num_citations = pub['num_citations'] if 'num_citations' in pub else None

                wPubs.writerow([citeId,pub_year,title,citation,conference,authors,pages,publisher,abstract,num_citations])
                
                if citeId and 'cites_per_year' in pub:
                    for year,cites in pub['cites_per_year'].items():
                        wPubsCitesByYear.writerow([citeId,year,cites])
                
                if citeId and 'author' in pub['bib']:
                    for index,auth in enumerate(pub['bib']['author'].split(' and ')):
                        wPubsAuthors.writerow([citeId,auth,(index+1)])
                        



fAuthors.close()
fAuthTags.close()
fAuthCitesByYear.close()
fAuthCoauthors.close()
fPubs.close()
fPubsCitesByYear.close()
fPubsAuthors.close()
