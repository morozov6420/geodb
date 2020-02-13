from shapely.geometry import Point
from shapely.geometry import Polygon
from shapely.geometry import MultiPolygon
from shapely.geometry import mapping
import psycopg2
# import requests 
from shapely.geometry import Point,Polygon,MultiPolygon,mapping
import datetime
import pandas as pd
from shapely.wkb import loads
from shapely.wkt import dumps, loads
import datetime
import json
# from ipyleaflet import (
#     Map, Marker,
#     TileLayer, ImageOverlay,
#     Polyline, Polygon, Rectangle, Circle, CircleMarker,
#     GeoJSON
# )
# import random
# import datetime
from datetime import datetime

new_data = pd.read_csv('D:\Программирование\Python\geodb\\ny_crimes.csv')

connection = psycopg2.connect(dbname = 'geo',
                              user = 'postgres',
                              password = 'a')

cursor = connection.cursor()

cursor.execute("CREATE TABLE ny_crimes (id_crime SERIAL PRIMARY KEY, region VARCHAR(255), age_group VARCHAR(255), vic_sex VARCHAR(255), cr_type VARCHAR(255), geom GEOMETRY, date DATE, time TIME)")
connection.commit()

for i in range(0,len(new_data)):
    
    region = new_data['region'][i]
    time = pd.to_datetime(new_data['time'][i],format='%H:%M:%S').time()
    date = datetime.strptime(new_data['date'][i],'%m/%d/%Y').date()
    age = new_data['age_group'][i]
    sex = new_data['vic_sex'][i]
    type_cr = new_data['cr_type'][i]
    # For Leaflet map change the places latLon
    point = Point(float(new_data['longitude'][i]),float(new_data['latitude'][i]))
    
    cursor.execute( "insert into ny_crimes (region, age_group, vic_sex, cr_type, geom, date, time) values ('{}','{}','{}','{}','{}','{}','{}')".format( str(region), str(age), str(sex),
                                                                                                                                                  str(type_cr), point.wkt, str(date), str(time) ) )
connection.commit() 
cursor.close()