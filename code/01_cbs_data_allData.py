import pandas as pd
import statsmodels as sm
import cbsodata
import matplotlib.pyplot as plt
import numpy as np
import functools as ft
import datetime
from functools import reduce

# https://opendata.cbs.nl/#/CBS/nl/dataset/83837NED/table
# https://opendata.cbs.nl/#/CBS/nl/dataset/85828NED/table?ts=1723788068156  ## Start Here ##

def macro_data_cbs_01(identifier, sectors, verbose = False):
    start_date = '01/01/2000'
   
    if verbose:
        info = cbsodata.get_info(identifier)
        print(info)
        tables = pd.DataFrame(cbsodata.get_table_list())

    # get data
    data = pd.DataFrame(cbsodata.get_data(identifier))

    if verbose:
        data.to_csv("output/ramdata.csv")
        print(data.Perioden)
        print(data.BedrijfstakkenBranchesSBI2008)
        print(data[data['BedrijfstakkenBranchesSBI2008'] == '58 Uitgeverijen'])

    # prints the exact names of data
    print(data['BedrijfstakkenBranchesSBI2008'].unique())

    # select the data needed, note that the periods are quarters and the oncorrected values are returned
    data1 = data[data['BedrijfstakkenBranchesSBI2008'].isin(sectors)]
    data2 = data1[data1['Perioden'].str.contains("kwartaal")]
    data3 = data2[['Perioden', 'BedrijfstakkenBranchesSBI2008', 'Ongecorrigeerd_1']]

    # reshape data
    data4 = data3.pivot(index = "Perioden",
          columns = "BedrijfstakkenBranchesSBI2008",
          values = 'Ongecorrigeerd_1'
          )

    # add a date index 
    data4.index = pd.date_range(start = start_date, periods=data4.shape[0], freq = "Q").to_period('Q')

    return data4

sectors = ['58 Uitgeverijen', 
           '59 Film- en tv-productie; geluidsopname',
           '60 Radio- en televisieomroepen',
           '61 Telecommunicatie',
           '62 IT-dienstverlening', 
           '63 Diensten op het gebied van informatie',
           '691 Rechtskundige dienstverlening',
           '692 Accountancy, administratie e.d.',
           '702 Managementadviesbureaus',
           '71 Architecten-, ingenieursbureaus e.d.',
           '712 Keurings- en controlediensten',
           '73 Reclamewezen en marktonderzoek',
           '74 Design, fotografie, vertaalbureaus',
           '78 Uitzendbureaus en arbeidsbemiddeling',
           '79 Reisbureaus, reisorganisatie en -info',
           '80 Beveiligings- en opsporingsdiensten',
           '81 Schoonmaakbedrijven, hoveniers e.d.',
           '82 Overige zakelijke dienstverlening',
           '462 Groothandel in landbouwproducten',
           '463 Groothandel in voedingsmiddelen',
           '464 Groothandel in non-food',
           '465 Groothandel in ICT-apparatuur',
           '466 Groothandel in industriemachines',
           '467 Overige gespecialiseerde groothandel',
           '551 Hotels',
           '561 Restaurants, andere eetgelegenheden',
           '562 Kantines en catering',
           '563 Cafés',
           '451 Autohandel en -reparatie',
           '452 Gespecialiseerde autoreparatie',
           '453 Handel in auto-onderdelen',
           '454 Handel en reparatie van motorfietsen',
           '49 Vervoer over land',
           '50 Vervoer over water',
           '51 Vervoer door de lucht',
           '52 Opslag, dienstverlening voor vervoer',
           '53 Post en koeriers', 
           '4711 Supermarkten',
           '472 Winkels in voedingsmiddelen',
           '474 Winkels in consumentenelektronica',
           '475 Winkels in overige huishoudwaren',
           '4752 Winkels in doe-het-zelfartikelen',
           '476 Winkels in recreatieartikelen',
           '4771 Winkels in kleding',
           '4772 Winkels in schoenen en lederwaren',
           '4791 Postorderbedrijven, webwinkels',
           'Winkels in meubels, woninginrichting alg'
           ]

NLD_basic_macro_data_01 = macro_data_cbs_01(identifier = '85828NED', sectors=sectors, verbose = False)
NLD_basic_macro_data_01.to_csv("tmp111.csv", sep=";")

# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/85828NED/table?dl=AB888

##############################################################
##############################################################
##############################################################

# https://opendata.cbs.nl/#/CBS/nl/dataset/85806NED/table?ts=1723801117092


def macro_data_cbs_02(identifier, sectors, verbose = False):
    start_date = '01/01/2005'
    end_date = '07/01/2024'

    if verbose:
        info = cbsodata.get_info(identifier)
        print(info)
        tables = pd.DataFrame(cbsodata.get_table_list())

    # get data
    data = pd.DataFrame(cbsodata.get_data(identifier))

    if verbose:
        data.to_csv("output/ramdata.csv")
        print(data.Perioden)
        print(data.BedrijfstakkenBranchesSBI2008)
        print(data[data['BedrijfstakkenBranchesSBI2008'] == '58 Uitgeverijen'])


    # prints the exact names of data
    print(data['BedrijfstakkenBranchesSBI2008'].unique())

    # select the data needed, note that the periods are quarters and the oncorrected values are returned
    data1 = data[data['BedrijfstakkenBranchesSBI2008'].isin(sectors)]
    data2 = data1[data1['Perioden'].str.contains("kwartaal")]
    data3 = data2[['Perioden', 'BedrijfstakkenBranchesSBI2008', 'TotaleOmzet_4']]

    # reshape data
    data4 = data3.pivot(index = "Perioden",
          columns = "BedrijfstakkenBranchesSBI2008",
          values = 'TotaleOmzet_4'
          )

    # add a date index 
    data4.index = pd.date_range(start = start_date, end=end_date, freq = "Q").to_period('Q')

    return data4

sectors = [ '10 Voedingsmiddelenindustrie', 
            '11 Drankenindustrie',
            '12 Tabaksindustrie',
            '13 Textielindustrie',
            '14 Kledingindustrie',
            '15 Leer- en schoenenindustrie', 
            '16 Houtindustrie',
            '17 Papierindustrie',
            '18 Grafische industrie',
            '19 Aardolie-industrie',
            '20 Chemische industrie', 
            '21 Farmaceutische industrie', 
            '22 Rubber- en kunststofproductindustrie',
            '23 Bouwmaterialenindustrie',
            '24 Basismetaalindustrie',
            '25 Metaalproductenindustrie', 
            '26 Elektrotechnische industrie',
            '27 Elektrische apparatenindustrie', 
            '28 Machine-industrie',
            '29 Auto- en aanhangwagenindustrie', 
            '30 Overige transportmiddelenindustrie', 
            '31 Meubelindustrie',
            '32 Overige industrie', 
            '33 Reparatie en installatie van machines'
            ]

NLD_basic_macro_data_02 = macro_data_cbs_02(identifier = '85806NED', sectors=sectors, verbose = True)
NLD_basic_macro_data_02.to_csv("tmp112.csv", sep=";")

##############################################################
##############################################################
##############################################################

# https://opendata.cbs.nl/#/CBS/nl/dataset/85809NED/table?ts=1723805425535

def macro_data_cbs_03(identifier, sectors, verbose = False):
    start_date = '01/01/2005'
    end_date = '07/01/2024'

    if verbose:
        info = cbsodata.get_info(identifier)
        print(info)
        tables = pd.DataFrame(cbsodata.get_table_list())

    # get data
    data = pd.DataFrame(cbsodata.get_data(identifier))

    if verbose:
        data.to_csv("output/ramdata.csv")
        print(data.Perioden)
        print(data.BedrijfstakkenBranchesSBI2008)
        print(data[data['BedrijfstakkenBranchesSBI2008'] == '58 Uitgeverijen'])


    # prints the exact names of data
    print(data['BedrijfstakkenBranchesSBI2008'].unique())

    # select the data needed, note that the periods are quarters and the oncorrected values are returned
    data1 = data[data['BedrijfstakkenBranchesSBI2008'].isin(sectors)]
    data2 = data1[data1['Perioden'].str.contains("kwartaal")]
    data3 = data2[['Perioden', 'Bedrijfsgrootte', 'BedrijfstakkenBranchesSBI2008', 'IndexcijfersOmzet_1']]
    data3 = data3[data3['Bedrijfsgrootte'] == 'Totaal 1 of meer werkzame personen']
    data3.drop(columns=['Bedrijfsgrootte'], inplace=True)

    data3.to_csv("tmp2.csv")
    print(data3.columns)

    # reshape data
    data4 = data3.pivot(index = "Perioden",
          columns = "BedrijfstakkenBranchesSBI2008",
          values = 'IndexcijfersOmzet_1'
          )

    # add a date index 
    data4.index = pd.date_range(start = start_date, end=end_date, freq = "Q").to_period('Q')

    return data4

sectors = [ '41 Algemene bouw en projectontwikkeling', 
            '42 Grond-, water-  en wegenbouw',
            '43 Gespecialiseerde bouw'
            ]

NLD_basic_macro_data_03 = macro_data_cbs_03(identifier = '85809NED', sectors=sectors, verbose = True)
NLD_basic_macro_data_03.to_csv("tmp113.csv", sep=";")

##############################################################
##############################################################
##############################################################

# https://opendata.cbs.nl/#/CBS/nl/dataset/85809NED/table?ts=1723805425535


def macro_data_cbs_04(identifier, sectors, verbose = False):
    start_date = '01/01/2005'
    end_date = '01/01/2024'

    if verbose:
        info = cbsodata.get_info(identifier)
        print(info)
        tables = pd.DataFrame(cbsodata.get_table_list())

    # get data
    data = pd.DataFrame(cbsodata.get_data(identifier))

    if verbose:
        data.to_csv("output/ramdata.csv")
        print(data.Perioden)
        print(data.BedrijfstakkenBranchesSBI2008)
        print(data[data['BedrijfstakkenBranchesSBI2008'] == '58 Uitgeverijen'])

    # prints the exact names of data
    print(data['BedrijfstakkenBranchesSBI2008'].unique())

    # select the data needed, note that the periods are quarters and the oncorrected values are returned
    data1 = data[data['BedrijfstakkenBranchesSBI2008'].isin(sectors)]
    data2 = data1[data1['Perioden'].str.contains("kwartaal")]
    data3 = data2[['Perioden', 'BedrijfstakkenBranchesSBI2008', 'IndexcijfersOmzet_1']]

    data3.to_csv("tmp2.csv")
    print(data3.columns)

    # reshape data
    data4 = data3.pivot(index = "Perioden",
          columns = "BedrijfstakkenBranchesSBI2008",
          values = 'IndexcijfersOmzet_1'
          )

    # add a date index 
    data4.index = pd.date_range(start = start_date, end=end_date, freq = "Q").to_period('Q')

    return data4

sectors = [ '75 Veterinaire dienstverlening' 
            ]

NLD_basic_macro_data_04 = macro_data_cbs_04(identifier = '83854NED', sectors=sectors, verbose = True)
NLD_basic_macro_data_04.to_csv("tmp114.csv", sep=";")

################################################
################################################
################################################

data_frames = [NLD_basic_macro_data_01, NLD_basic_macro_data_02, NLD_basic_macro_data_03, NLD_basic_macro_data_04]
dx1 = pd.concat(data_frames, axis='columns')

dx1.columns = [x.replace(" ", "_") for x in dx1.columns]
dx1.to_csv("data/allDataOmzet.csv", sep=";")



# allDataOmzet = pd.read_csv("data/allDataOmzet.csv",  index_col=[0], sep=";")
# allDataOmzet.columns = [x.replace(" ", "_") for x in allDataOmzet.columns]
# print(allDataOmzet)


#'4673 Groothandel in Bouwmaterialen #####',
