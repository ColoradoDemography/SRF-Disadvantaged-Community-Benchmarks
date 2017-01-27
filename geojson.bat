cd C:\Program Files\QGIS Pisa\bin
ogr2ogr -f "GeoJSON" C:\Users\RKemp\Desktop\test.json PG:"host=gis.dola.colorado.gov dbname=acs1115 user=codemog password=demography port=5433" -sql "SELECT geoname, geonum, b06012001,geom AS geojson from carto.county  natural join data.b06012 where ( state=8 )  limit 1000;"
