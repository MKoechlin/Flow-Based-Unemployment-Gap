# Flow-based-Unemployment-Gap
This repository calculates the Kharroubi &amp; Koechlin unemployment gap based on labour market flows data and updates it automatically.

The R file **u_gap_update.R** contains the script to calculate the gap, and produces a .csv file and some graphs which are saved in the output folder.

The file ***Kharroubi_Koechlin_u_gap_data.csv*** in the output folder contains the unemployment gap as described in our paper. There are two variables, the *KK_u_gap* and the *KK_u_gap_ma6*, which are the Kharroubi-Koechlin unemployment gap and the 6-month backward looking moving average of that gap (which is used in the paper).
