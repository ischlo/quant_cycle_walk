
import numpy as np
import pandas as pd
import osmnx as ox
import geopandas as gpd

ox.config(timeout=9000)

def ready_to_export(data):
    
    
    keep = ['element_type','osmid','geometry','highway','bicycle','foot','cycleway'
            #,'pedestrian'
           ]
    
    data = data.reset_index()
    data = data[keep]
    data= data.astype({'highway':str
                    , 'bicycle':str
                    , 'foot':str
                    , 'cycleway':str
                    #, 'pedestrian': str
                      })
    
    return data
  

# we can calculate basic street network metrics and display average circuity
# stats = ox.basic_stats(haringey_ox)
# stats

# getting the data by tag for highways that can accomodate cycling, 
# from osm tags, those are: 

# highway=*

# tags_cycle = {
#   "highway" : ["cycleway","path","primary","secondary","tertiary","unclassified"
#                 ,"residential","primary_link","secondary_link","tertiary_link"
#                 ,"footway"]
#   ,"cycleway": True
#   ,"bicycle" : ["yes", "designated","permissive","destination","dismount"]
#   ,"surface":True
# }
# 
# # tags for the general road network containing all segments
# tags_highway = {
#   "highway" : True
# }
# 
# # tags for walkable segments
# # add tags from map features OSM
# tags_walk = {
#   "highway" : ["pedestrian"
#                ,"footway"
#                ,"path"
#                ,"cycleway"
#                ,"steps"
#                ,"unclassified"
#                ,"residential"
#                ,"living_street"
#                ,"track"
#                ,"footway"
#                ,"bridleway"
#                ,"steps"
#                ,"corridor"
#                ,"path"
#               ]
#   #,"foot" : ["yes", "designated","permissive"]
# }


area_custom_graph = ox.graph_from_bbox(58.620
                                    ,55.292
                                    ,-1.736
                                    ,-6.372
                                    ,network_type="all"
                                    ,simplify = True
                                    ,truncate_by_edge = True
                                    ,clean_periphery = True
                                    )

ox.save_graph_geopackage(area_custom_graph, "custom_graph.gpkg")
