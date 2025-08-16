#import modules

import pydytuesday
import os
from pathlib import Path
import pandas as pd
import numpy as np
import matplotlib.pyplot as mpl
import plotly as ptly

#file locations
main = os.getcwd()

# Download files from the week, which you can then read in locally
#pydytuesday.get_date('2025-08-12')

data = pd.read_csv('attribution_studies.csv')
#data cleaning