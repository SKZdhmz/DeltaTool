# DeltaTool_Preprocessing

##	Uvod
Predproces za DELTA TOOL je pisan u R-u, glavni program je „delta_tool.R“. Program pretvara Vesnine excel tablice i izlaz iz modela u format za delta tool te radi dodatne „resource“ file-ove za delta tool. Nakon izvršavanja nastaju 3 direktorija: conversion, data i resource koje je potrebno kopirati u DELTA TOOL input put (C:\Users\Public\Documents\JRC_DELTA\delta).
## Input
Kao input koriste se Vesnine tablice "svi podaci ime_postaje 2006-2016.xlsx" i izlaz iz modela LOTOS-EUROS koji je spojen u jedan file s opcijom:
```bash
cdo cat infiles outfile
```
##	DELTA TOOL
U delta toolu potrebno je  pretvoriti dobivene csv tablice modela u netCDF, Help-> Interactive format conversion tool (Model). Klik na ReadInfo (klik vise puta te se podesi odgovarajuci) i GO. Nakon toga potrebno je restartati tool.
Svi file-ovi su vec unaprijed pripremljeni za 2016 godinu.
