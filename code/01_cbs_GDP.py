import pandas as pd
import cbsodata
import matplotlib.pyplot as plt
import numpy as np
import functools as ft
import datetime

pd.set_option('display.max_columns', 40)

###################################
# https://cbsodata.readthedocs.io/en/latest/readme_link.html

# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.cpb.nl/sites/default/files/publicaties/download/cpb-technical-background-document-bvar-models-used-cpb.pdf
###################################

#### GDP
# https://opendata.cbs.nl/statline#/CBS/nl/dataset/84105NED/table?ts=1706684088769
# https://opendata.cbs.nl/statline#/CBS/nl/dataset/84087NED/table?ts=1696490590802

def macro_data_cbs(identifier, verbose = False):
    start_date = '01/01/1995'

    if verbose:
        info = cbsodata.get_info(identifier)
        print(info)
        tables = pd.DataFrame(cbsodata.get_table_list())
        tables.to_csv("tmp_CBStable.csv")

    # get data
    data = pd.DataFrame(cbsodata.get_data(identifier))

    if verbose:
        data.to_csv("data/tmp_ramdata.csv")
        print(data.Perioden)

    data = data[data["SoortGegevens"] == 'Prijsniveau 2015']
    data = data[data['Perioden'].str.contains('kwartaal')]
    data.index = pd.date_range(start = start_date, periods = data.shape[0], freq = "Q").to_period('Q')

    gdp_total = data[['BrutoBinnenlandsProduct_2', 'Totaal_3', 'Huishoudens_9', 'Overheid_10', 'Totaal_11', 'BedrijvenEnHuishoudens_12', 'Overheid_13', 'VeranderingInVoorraden_14', 'Totaal_15']]

    gdp_total.columns = ['gdp_total_season', 'imports_goods_services_season', 'household_cons_season', 'gov_consumption_season', 'investments_season', 'gpd_invest_business_households_season', 'gov_invest_season', 'change_supply_season', 'exports_goods_services_season']

    ######################
    # Set data index
    ######################
    gdp_total.index = pd.date_range(start=start_date, periods = gdp_total.shape[0], freq="Q").to_period('Q')
    # this adds one day, so that we can go to the first of a month
    gdp_total.index = pd.PeriodIndex(gdp_total.index, freq='Q').to_timestamp() #+ datetime.timedelta(days=1) #pd.offsets.QuarterEnd()

    return gdp_total

NLD_basic_macro_data = macro_data_cbs(identifier = '84105NED', verbose = True)
NLD_basic_macro_data.to_csv("data/cbs_basic_macro_NOT_SEASONCORRECTED_qt.csv")
print("2015, seasonably adjusted")
print(NLD_basic_macro_data)

NLD_basic_macro_data.plot()
plt.title('GDP (Bruto binnenlands product , bestedingen) and components until 1st Quarter 2024')
plt.savefig("output/figures/NLD_basic_macro_data.png")
plt.show()

#################
#################
df = NLD_basic_macro_data.copy()
print(df.shape[1])

fig, axes = plt.subplots(nrows=df.shape[1], ncols=1)
df.plot(subplots=True, ax=axes, sharex=True)
plt.show()