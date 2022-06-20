---
title: "Routing scripts"
dev: "png"
fig_caption: true
output: 
  html_document:
    mathjax: "default"
    keep_md: true

---



The scripts allowing to do routing between various origin destination points are presented here. We use the package `cppRouting`. 

## Distance between two points on a sphere

While on a small scale, the difference between the usually used euclidean distance and the geodesic distance might not really differ, it is safer, especially when working with greater areas, to use the ofrmer one. While it's a bit more complex than just calculating the hypothenuse of a triangle, when done in spherical coordinates, it looks the following:

$$d(P_1,P_2)=\rho\arccos(\cos\Delta\phi\cos(\theta_{P_1})\cos(\theta_{P_2})+\sin(\theta_{P_1})\sin(\theta_{P_2})$$, where $\rho$ is fixed and equal to the raduis of the sphere, the earth. The standard longitude and latitude here correspond to the $\phi$ and $\theta$ angles in the spherical coordinate system and by assuming a fixed earth radius, the 3rd variable becomes simply a constant $\rho = R_{earth} = 6371010$ meters. 

## Selecting origin destination points

The question of routing involves defining precise coordinates of a point from and to which routing is done. Various options present themselves and the ones that seem relevant are selected.

1) First, the geometric centroid as the most obvious option is taken. One limitation of this is that geometric centroids sometime fall in the middle of a greenspace or more generally in a space from which it seems irrelevant to be routing from. However, this method is straightforward and assuming relatively small area units (MSOA), the difference will not be significant. This is refered to as the standard centroid.

2) Additionaly, it is possible to use population weighted centroids. In this case it is reasonable to rout from an verage location in which people live. To make this routing even more realistic, the weighted centroid of OSM amenities distribution is taken as the destination point. This is refered to as the commute centroid. 

3) Finally, a different routing point is taken to be the centroid of a sample of nodes from the network that are in either residential or in business areas. The greenspaces are excluded. This is refered to as the network centroid.


