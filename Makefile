.PHONY: all clean dirs

all: figures/gap_estimated_k_vs_side_length.png \
figures/shell_clusters_3d_plot.html \
figures/spectral_clustering_gap_statistic_d_threshold_0.8.png \
figures/spectral_clustering_gap_statistic_d_threshold_1.png \
figures/spectral_clustering_gap_statistic_d_threshold_1.2.png report.html 

clean:
	rm -rf figures
	rm -f report.html
	
dirs:
	mkdir -p figures

figures/gap_estimated_k_vs_side_length.png: task1.r | dirs
	Rscript task1.r

figures/shell_clusters_3d_plot.html \
figures/spectral_clustering_gap_statistic_d_threshold_0.8.png \
figures/spectral_clustering_gap_statistic_d_threshold_1.png \
figures/spectral_clustering_gap_statistic_d_threshold_1.2.png: task2.r | dirs
	Rscript task2.r

report.html: report.rmd \
figures/shell_clusters_3d_plot.html \
figures/spectral_clustering_gap_statistic_d_threshold_0.8.png \
figures/spectral_clustering_gap_statistic_d_threshold_1.png \
figures/spectral_clustering_gap_statistic_d_threshold_1.2.png | dirs
	Rscript -e "rmarkdown::render('report.rmd', output_format='html_document')"
