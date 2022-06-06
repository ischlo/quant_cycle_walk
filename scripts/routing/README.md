Routing scripts
================

The scripts allowing to do routing between various origin destination
points are presented here. We use the package `cppRouting`.

## Distance between two points on a sphere

While on a small scale, the difference between the usually used
euclidean distance and the geodesic distance might not really differ, it
is safer, especially when working with greater areas, to use the ofrmer
one. While it’s a bit more complex than just calculating the hypothenuse
of a triangle, when done in spherical coordinates, it looks the
following:

$$d(P_1,P_2)=\rho\arccos(\cos\Delta\phi\cos(\theta_{P_1})\cos(\theta_{P_2})+\sin(\theta_{P_1})\sin(\theta_{P_2})$$,
where $\rho$ is fixed and equal to the raduis of the sphere, the
earth. The standard longitude and latitude here correspond to the
$\phi$ and $\theta$ angles in the spherical coordinate system and by
assuming a fixed earth radius, the 3rd variable becomes simply a
constant $\rho = R_{earth} = 6371010$ meters.
