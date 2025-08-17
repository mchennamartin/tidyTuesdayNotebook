#import modules
from xml.dom.minidom import NamedNodeMap

import os
import re
from pathlib import Path
import pandas as pd
#import numpy as np
#import matplotlib.pyplot as mpl
#import plotly as ptly

#file locations
main = Path()

# Download files from the week, which you can then read in locally
#pydytuesday.get_date('2025-08-12')

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

#clean up
data['classification'] = data['classification'].str.replace('Insufficient data/inconclusive', 'Inconclusive')
