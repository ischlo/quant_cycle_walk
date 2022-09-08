The scripts allowing to do routing between various origin destination
points are presented here. We use the package `cppRouting`.

Distance between two points on a sphere
---------------------------------------

While on a small scale, the difference between the usually used
euclidean distance and the geodesic distance might not really differ, it
is safer, especially when working with greater areas, to use the ofrmer
one. While it’s a bit more complex than just calculating the hypothenuse
of a triangle, when done in spherical coordinates, it looks the
following:

*d*(*P*<sub>1</sub>, *P*<sub>2</sub>) = *ρ*arccos (cos *Δ**ϕ*cos (*θ*<sub>*P*<sub>1</sub></sub>)cos (*θ*<sub>*P*<sub>2</sub></sub>) + sin (*θ*<sub>*P*<sub>1</sub></sub>)sin (*θ*<sub>*P*<sub>2</sub></sub>)
, where *ρ* is fixed and equal to the raduis of the sphere, the earth.
The standard longitude and latitude here correspond to the *ϕ* and *θ*
angles in the spherical coordinate system and by assuming a fixed earth
radius, the 3rd variable becomes simply a constant
*ρ* = *R*<sub>*e**a**r**t**h*</sub> = 6371010 meters.

Selecting origin destination points
-----------------------------------

The question of routing involves defining precise coordinates of a point
from and to which routing is done. Various options present themselves
and the ones that seem relevant are selected.

1.  First, the geometric centroid as the most obvious option is taken.
    One limitation of this is that geometric centroids sometime fall in
    the middle of a greenspace or more generally in a space from which
    it seems irrelevant to be routing from. However, this method is
    straightforward and assuming relatively small area units (MSOA), the
    difference will not be significant. This is refered to as the
    standard centroid.

2.  Additionaly, it is possible to use population weighted centroids. In
    this case it is reasonable to rout from an verage location in which
    people live. To make this routing even more realistic, the weighted
    centroid of OSM amenities distribution is taken as the destination
    point. This is refered to as the commute centroid.

3.  Finally, a different routing point is taken to be the centroid of a
    sample of nodes from the network that are in either residential or
    in business areas. The greenspaces are excluded. This is refered to
    as the network centroid.
