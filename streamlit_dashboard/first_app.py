import os
import streamlit as st
import pandas as pd
import numpy as np
import geopandas as gpd
import folium
from streamlit_folium import folium_static
import altair as alt

st.set_page_config(
    page_title="Rijksmonumentdichtheid van Nederland",
     layout="wide",
     initial_sidebar_state="expanded")

monuments_df = gpd.read_file(os.path.join(os.getcwd(), "monuments_dashboard_data", "municipal_monument_count.geojson"))

column_mapping_df = pd.read_csv(os.path.join(os.getcwd(), "monuments_dashboard_data", "monument_category_column_mapping.csv"))

st.title("Rijksmonumenten per gemeente")

main_categories = np.insert(np.sort(column_mapping_df['hoofdcategorie'].unique()), 0, 'Alles')

st.sidebar.write("### Selecteer een monumentcategorie")
categorie = st.sidebar.radio(
    "",
    (main_categories))

if categorie == 'Alles':
    sub_categories = []
else:
    sub_categories = np.insert(np.sort(column_mapping_df[column_mapping_df['hoofdcategorie'] == categorie]['subcategorie'].values), 0, 'Alles')

st.sidebar.write("### Selecteer een subcategorie")
subcategorie = st.sidebar.radio(
    "",
    (sub_categories))


st.sidebar.write("### Selecteer berekening")
function = st.sidebar.radio(
    "",
    ('totaal aantal', 'afgerond aantal per 100.000 inwoners'))

st.sidebar.write("### Selecteer classificatiecategorie")
label_classification = st.sidebar.radio(
    "",
    ('machten van 10', 'gelijke intervals'))

if categorie == 'Alles':
    selected_columns = column_mapping_df['column_mapping']
elif subcategorie == 'Alles':
    selected_columns = column_mapping_df[column_mapping_df['hoofdcategorie'] == categorie]['column_mapping']
else:
    selected_columns = column_mapping_df[(column_mapping_df['hoofdcategorie'] == categorie) & (column_mapping_df['subcategorie'] == subcategorie)]['column_mapping']



monuments_df['aantal_monumenten_binnen_categorie'] = monuments_df[selected_columns].sum(axis = 1)

if function == 'afgerond aantal per 100.000 inwoners':
    monuments_df['aantal_monumenten_binnen_categorie'] = np.round(monuments_df['aantal_monumenten_binnen_categorie'] / monuments_df['TotaleBevolking_1'] * 100000, 0)

if label_classification == 'gelijke intervals':
    scale = np.linspace(0, monuments_df['aantal_monumenten_binnen_categorie'].max(), 5)
else:
    n_digits = len(str(int(monuments_df['aantal_monumenten_binnen_categorie'].max())))
    if n_digits < 3:
        n_digits = 3
    scale = [10**i for i in range(n_digits + 1)]
    scale.insert(0, 0)

x_center_coord = np.median(monuments_df.centroid.to_crs('epsg:4326').x)
y_center_coord = np.mean(monuments_df.centroid.to_crs('epsg:4326').y)

f = folium.Figure(width=2000, height=2000)

m = folium.Map(zoom_start=7,
	       location = [y_center_coord, x_center_coord], 
               tiles='https://api.mapbox.com/styles/v1/ivo11235/ckjx01y2e1brw17nqdictt5wk/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiaXZvMTEyMzUiLCJhIjoieV82bFVfNCJ9.G8mrfJOA07edDDj6Bep2bQ',
               attr='© <a href="https://www.mapbox.com/about/maps/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> <a href="https://www.mapbox.com/map-feedback/">(Improve this map)</a>'
).add_to(f)

choropleth = folium.Choropleth(geo_data = monuments_df, 
             data=monuments_df,
             columns=['gemeentenaam', 'aantal_monumenten_binnen_categorie'],
             key_on='feature.properties.gemeentenaam',
             threshold_scale=list(scale),
             fill_color='YlGnBu',
             fill_opacity=0.5,
             line_opacity=0.2,
             # line_color="black",
             legend_name='Aantal monumenten in de gekozen categorie',
             smooth_factor=1).add_to(m)

choropleth.geojson.add_child(
    folium.features.GeoJsonTooltip(['gemeentenaam', 'aantal_monumenten_binnen_categorie'])
)

folium_static(m)

st.write(alt.Chart(monuments_df[['gemeentenaam', 'aantal_monumenten_binnen_categorie']].sort_values('aantal_monumenten_binnen_categorie', ascending = False)[:10], width=500, height=400).mark_bar().encode(
    x=alt.X('gemeentenaam', sort="-y"),
    y='aantal_monumenten_binnen_categorie').configure_axisX(labelAngle=-45))
