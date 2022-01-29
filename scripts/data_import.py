
import numpy as np
import pandas as pd
import osmnx as ox
import geopandas as gpd
import gc

ox.config(timeout=3600)

haringey_ox = ox.graph_from_place("Haringey, London, UK"
                                  ,network_type="all"
                                  ,buffer_dist = 500
                                  ,simplify = True
                                  ,truncate_by_edge = True
                                  )

ox.save_graph_geopackage(haringey_ox, "data/haringey_all_network.gpkg")

fig, ax = ox.plot_graph(haringey_ox)


# we can calculate basic street network metrics and display average circuity
stats = ox.basic_stats(haringey_ox)
stats

london_cycle = ox.graph_from_place("London, UK"
                                  ,network_type="bike"
                                  ,buffer_dist = 500
                                  ,simplify = True
                                  ,truncate_by_edge = True
                                  )
  
ox.save_graph_geopackage(london_cycle, "data/london_cycle.gpkg")

london_cycle = None

# brighton and countryside cycling network
  
brighton_bbox = [-0.7443,50.7829,0.1071,51.1035]
                                  
brighton_cycle = ox.graph_from_bbox(51.1035
                                    ,50.7829
                                    ,0.1071
                                    ,-0.7443
                                    ,network_type="bike"
                                    ,simplify = True
                                    ,truncate_by_edge = True
                                    ,clean_periphery = True
                                    )

ox.save_graph_geopackage(brighton_cycle, "data/brighton_cycle.gpkg")

brighton_cycle = None
# cleaning env

gc.collect()
