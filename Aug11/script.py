#import modules

import os
import re
from pathlib import Path
import pandas as pd
#import numpy as np
#import matplotlib.pyplot as mpl
#import plotly as ptly

#file locations
main = Path(os.getcwd())

# Download files from the week, which you can then read in locally
#pydytuesday.get_date('2025-08-12')---------------------------------------------

data = pd.read_csv(main / 'Aug11' / 'data' / 'attribution_studies.csv')

#add column for seasons
data['season'] = ''
data['event_name'] = data['event_name'].astype('object')

#find where season is fall
temp = data['event_name'].str.contains('Fall|autumn|Autumn|September|October|November')
data.loc[temp, 'season'] = 'Fall'

#do same for other seasons
temp = data['event_name'].str.contains('winter|Winter|December|January|Febuary')
data.loc[temp, 'season'] = 'Winter'

temp = data['event_name'].str.contains('spring|Spring|March|April|May')
data.loc[temp, 'season'] = 'Spring'

temp = data['event_name'].str.contains('summer|Summer|June|July|August')
data.loc[temp, 'season'] = 'Summer'

#cut tables into important information and supplementary
supp = data[['link', 'citation']]
data = data.drop(['link', 'citation'], axis=1)

#extract counts for certain events---------------------------------------------
#make counts table
counts = pd.DataFrame()
tempSeries = pd.Series(data["event_period"])
#add year column to counts table
counts['year'] = 1800

#make loop that adds counts to each year when '-' is present
for i in len(tempSeries):
   #if multiple years, add 1 to each year in counts frame
    if '-' in tempSeries[[i]]:
        temp = counts[[i]].split('-')
        a = temp[0]
        b = temp[1]
        for year in temp[0]:temp[1]:
            counts[''] =
        # for i in 30 add 1 to column[year]

    #if 'since' is involved use event_period year and publication year
    if 'since' in temp[[i]]:
    #if not multiple years just tally single year


tempSeries = s
counts['pubs_per_year'] = tempSeries.value_counts()
