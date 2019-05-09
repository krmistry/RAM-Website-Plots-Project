# RAM-Website-Plots-Project

The purpose of Region_or_taxGroup_Plots_Code-KM.r is to produce the following plots, pdfs and tables used in the RAM Legacy Stock Assessmement Database website:
-----------
	•	 Biomass Coverage plots -- horizontal bar chart that plots the year range of data covered inthe most recent assessment for each stock, beginning in year_min (1950 as of this revision). Each bar represents an individual stock, sorted vertically from largest to smallest by average biomass over the time series. There are 2 versions of this plot; one that scales with biomass and has all stocks in 1 plot, and the other that doesn't scale with biomass so all stocks are equally visible (stocks are individually labeled in this version)
	•	Surplus Production plots -- vertical bar chart that plots annual surplus production (net increase of biomass plus catch) over time beginning in year_min for the four stocks with the highest average biomass in the region or taxGroup. The plot also includes a subtitle that has the percentage of absolute surplus production that the stock represents in the region or taxGroup.
	•	Surplus Production vs. Biomass plots -- line and point plot that shows path of annual surplus production compared to annual biomass over time (beginning in year_min) for the four stocks that contribute the most to absolute surplus production in the region or taxGroup. Years are represented by a continuous color scale, with the first and last years labeled on the plot. 0 is emphasized with a thick black line.
	•	Summary PDF -- A pdf of the above plots, each on their own page, for each region or taxGroup. 
	•	Summary Table csv file -- csv file of a 2 column dataframe with the information for the tables at the top of each region and taxGroup page. Info includes: 
	⁃	number of stocks, 
	⁃	number of taxGroups for region-separated data or number of regions for taxGroup-separated data, 
	⁃	first year and last year found in that region or taxGroup data set, and 
	⁃	labels for all of the above.



RAM_Plot_Set_up.R is designed to be run as part of Region_or_taxGroup_Plots_Code-KM.R and it produces the transformed and filtered datasets based on RAM data used to make the region and taxGroup plots described in Region_or_taxGroup_Plots_Code-KM.R

Notes on naming convention:
	•	 all variables and dataframes used in the region plots begin with "region”
	⁃	For example, "region_legend_order" is the order that the taxonomy groups are put in for the color legend in the region plots
	•	 the taxGroup categories are different in the timeseries_values_views dataframe compared to the All_TBbest.df dataframe; for all plots that use All_TBbest.df as the data source, use variables and dataframes starting with "TB_taxGroup"; the ones starting with "taxGroup" can be used for plots and tables using data from the timeseries_values_views dataframe.
