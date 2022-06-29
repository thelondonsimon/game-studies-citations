from scholarly import scholarly
import csv
import pandas

#########################################################################################
### This script attempts to identify a Google Scholar profile for names mentioned in the
### citation field of Publications. The data incldues the Google Scholar 'cites_id' value
### for one of the known publications for the name being looked up.
###
### The script uses the scholarly `search_author` function to find matching profiles and
### then, for each author search result, loops through each of their publications to find
### the known cites_id value associated with the name. If that value is found, their
### profile data is appended to CSV outputs
###
### The input data is a csv file with two columns:
### - 1. The name to lookup
### - 2. A Google Scholar cite_id value to verify any results against
#########################################################################################

# configuration
firstRun = True    # [True | False] denotes if this is the first time this script is being run to perform lookups
filePrefix = 'game_studies3'
lookupFile = './data/game-lookups.csv'
maxAttempts = 5     # Limits how many author search results will be examined to find a match to the cites_id value


# setup the CSV output files based on whether this is the first time the script is being run
outputWriteMode = 'w' if firstRun == True else 'a'

fAuthors = open('./data/' + filePrefix + '-coauthor-lookup-authors.csv',outputWriteMode, encoding='UTF8', newline = '')
wAuthors = csv.writer(fAuthors)
if firstRun:
    wAuthors.writerow(['inputname','scholar_id','name','affiliation','url_picture','email_domain','organization','homepage','citedby','citedby5y','hindex','hindex5y','i10index','i10index5y'])


fAuthTags = open('./data/' + filePrefix + '-coauthor-lookup-author-tags.csv',outputWriteMode, encoding='UTF8', newline = '')
wAuthTags = csv.writer(fAuthTags)
if firstRun:
    wAuthTags.writerow(['scholar_id','tag'])


fAuthCitesByYear = open('./data/' + filePrefix + '-coauthor-lookup-authors-cites-per-year.csv',outputWriteMode, encoding='UTF8', newline = '')
wAuthCitesByYear = csv.writer(fAuthCitesByYear)
if firstRun:
    wAuthCitesByYear.writerow(['scholar_id','year','citations'])


fAuthCoauthors = open('./data/' + filePrefix + '-coauthor-lookup-author-coauthors.csv',outputWriteMode, encoding='UTF8', newline = '')
wAuthCoauthors = csv.writer(fAuthCoauthors)
if firstRun:
    wAuthCoauthors.writerow(['scholar_id','coauthor_scholar_id','coauthor_name'])


# obtain the list of author names to attempt to match to a google scholar profile
authors = pandas.read_csv(lookupFile, names = ['name','citeid'], dtype = {'name': str, 'citeid': str})


# keep track of processed scholar ids as we only want to keep the full author details for a single scholar
processedScholarIds = [] 


# loop through each name to lookup
for index,row in authors.iterrows():
    lookupName = row[0]     # the name
    pubCiteId = row[1]      # the cite_id used to verify the name
    
    print("Searching for '" + lookupName + "' to match to citeId #" + pubCiteId)

    query = scholarly.search_author(lookupName)
    # for performance reasons, only check the first 5 names returned
    i = 0
    for author in query:
        if i >= maxAttempts:
            break
        else:
            i = i + 1
        
        print('-- Checking for match to ' + author['name'])
        author = scholarly.fill(author)
        for pub in author['publications']:
            if 'cites_id' in pub and len(pub['cites_id']) > 0:
                citeId = pub['cites_id'][0]

                if citeId == pubCiteId:
                    print("-- Found match")

                    scholar_id = author['scholar_id'] if 'scholar_id' in author else None
                    if scholar_id in processedScholarIds:
                        print("-- Match already found")
                        wAuthors.writerow([lookupName,scholar_id])
                        break

                    author = scholarly.fill(author)

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

                    wAuthors.writerow([lookupName,scholar_id,name,affiliation,url_picture,email_domain,organization,homepage,citedby,citedby5y,hindex,hindex5y,i10index,i10index5y])
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
                    
                    break
           
        else:
            continue
            
        break

fAuthors.close()
fAuthTags.close()
fAuthCitesByYear.close()
fAuthCoauthors.close()

