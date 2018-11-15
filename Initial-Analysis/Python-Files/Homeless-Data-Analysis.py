"""
Homeless Data Analysis using Python
[Reproducing in Python, an original analysis that was done using R]

@author: sudha
"""

from pandas import read_csv
import pandas as pd
from pandas import set_option
from matplotlib import pyplot as plt

# Read data from local file - that holds CoC names and the lat/lon
COCNames = ['CoCNumber', 'lon', 'lat']
filename = 'COCNumWithGeoCodes.csv'
dataframe = read_csv(filename, names=COCNames)
array = dataframe.values

# Perform some data cleaning 
# Remove rows with NAs in the lat / lon columns
dfCoC = read_csv(filename, names=COCNames)
dfCoC.dropna(subset=['lat'], inplace=True)
dfCoC.dropna(subset=['lon'], inplace=True)

HomelessNames = ['CoCNumber', 'Indv', 'ShelteredIndv', 'UnshelteredIndv', 'PeopleFamilies', 
                 'ShelteredPeopleFamilies', 'UnshelteredPeopleFamilies', 'ShelteredVeterans', 
                 'UnshelteredVeterans', 'ShelteredYouth', 'UnshelteredYouth']
HomelessFilename = 'HomelessData2016.csv'
dfHomeless = read_csv(HomelessFilename, names=HomelessNames)

# Combine values in specific columns to aggregate counts in each category and form new columns
dfHomeless['Sheltered'] = dfHomeless['ShelteredIndv'] + dfHomeless['ShelteredPeopleFamilies']
dfHomeless['Unsheltered'] = dfHomeless['UnshelteredIndv'] + dfHomeless['UnshelteredPeopleFamilies']

# Aggregate counts to get totals in each category and form new columns
dfHomeless['Total'] = dfHomeless['Sheltered'] + dfHomeless['Unsheltered']
dfHomeless['Veterans'] = dfHomeless['ShelteredVeterans'] + dfHomeless['UnshelteredVeterans']
dfHomeless['Youth'] = dfHomeless['ShelteredYouth'] + dfHomeless['UnshelteredYouth']

set_option('display.width', 100)
set_option('precision', 3)
description=dfHomeless.describe()
print(description)

# Select aggregate columns into a separate dataset
data = dfHomeless[['Total', 'Veterans', 'Youth']]
data.Total.plot(kind='box', subplots=True, layout=(3,3), sharex=False, sharey=False)
plt.show()

subdata = data[data['Total'] < 20000]
subdata.Total.plot(kind='box', subplots=True, layout=(3,3), sharex=False, sharey=False)
plt.show()

subdata.Total.hist()
plt.show()

# Merge the datasets - similar to inner join in Database, based on a key column, CoCNumber in this case
# Purpose is to associate the lat and lon values with the homeless dataset
newdf = pd.merge(dfHomeless, dfCoC, on='CoCNumber')

# Extract state name from the CoCNumber as a separate column
newdf['State'] = newdf['CoCNumber'].str[:2]

# Read States Dataset
StatesNames = ['StateName', 'State']
StateFilename = 'StateNames.csv'
dfStates = read_csv(StateFilename, names=StatesNames)

# Now get totals in each state and sort in descending order
#grouped = newdf.groupby('State').size().to_frame(name='Total').reset_index()
group1 = newdf.groupby(by=['State'])['Total'].sum().sort_values(ascending=False)
group2 = newdf.groupby(by=['State'])['Veterans'].sum().sort_values(ascending=False)
group3 = newdf.groupby(by=['State'])['Youth'].sum().sort_values(ascending=False)

df1 = group1.rename(None).to_frame()
df2 = group2.rename(None).to_frame()
df3 = group3.rename(None).to_frame()

# form new columns with names for merging into one
df1['State'] = df1.index
df1['Total'] = df1[[0]]
df2['State'] = df2.index
df2['Veterans'] = df2[[0]]
df3['State'] = df3.index
df3['Youth'] = df3[[0]]


# Merge State names into the newly formed dataset
grouped = pd.merge(df1, df2, on='State')
grouped = pd.merge(grouped, df3, on='State')
grouped.drop(grouped.index[0], inplace=True)

# Reset index and group only columns of interest that captues total in each category
grouped = grouped.reset_index(drop=True)
grouped = grouped[['State', 'Total', 'Veterans', 'Youth']]

# Display to console top 10 states with high homeless counts
print("Below are top 10 states with high homeless counts:")
print(group1.head(10))


# Display to console top 10 states with high homeless counts among Veterans
print("Below are top 10 states with high homeless Veteran counts:")
print(group2.head(10))

# Display to console top 10 states with high homeless counts (Total)
print("Below are top 10 states with high homeless counts:")
print(grouped.head(10))

grouped.Total.hist()
plt.title("Total Homeless Counts across States")
plt.xlabel("Homeless Counts")
plt.ylabel("Frequency")
plt.show()

grouped.Veterans.hist()
plt.title("Total Veteran Homeless Counts across States")
plt.xlabel("Veterans Homeless Counts")
plt.ylabel("Frequency")
plt.show()

grouped.Youth.hist()
plt.title("Total Youth Homeless Counts across States")
plt.xlabel("Youth Homeless Counts")
plt.ylabel("Frequency")
plt.show()
