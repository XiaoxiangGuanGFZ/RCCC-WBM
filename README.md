# RCCC-WBM
## Introduction
The RCCC-WBM is a simplified hydrological model, developed by the Research Center for Climate Change ([RCCC](http://rccc.nhri.cn/)), Ministry of Water Resources of China. The model estimates monthly stream flow from precipitation, temperature, and potential evapotranspiration. Its advantages include a simpler structure, fewer parameters and more flexibility. The model has been successfully applied to over 200 typical catchments and showed good performance.
## Software
this web-based application is written by R project, together with [shiny](https://shiny.posit.co/) package, which provides the functions of interactive operation and web deployment. The required packages also include: ggplot2, shinythemes, formattables, and DT.
### Major functionalities 
1. Lumped monthly discharge modelling ([Wang et al., 2013](https://doi.org/10.1175/JHM-D-12-081.1))
2. WBM model parameter sensitivity analysis ([Wang et al., 2014](https://doi.org/10.1016/j.quaint.2013.08.051))
3. Runoff variation attribution (effect partition from climate change and anthropogenic activities) ([Wang et al., 2016](https://doi.org/10.3390/w8060267))
4. Sensitivity analysis of runoff to the changes in precipitation and air temperature ([Wang et al., 2017](https://doi.org/10.1007/s00477-016-1218-6))

### How to use
run the `./scr/app.R` and launch the application after successful installation of the required packages. The UI of this application is straightforward and intuitive. 
### Data preparation
Delimited file format is suggested for data preparation like .txt or .csv. An exemplary data file is provided as `./data/Taohe River-Dataset-monthly-1955-2014-24973.txt`. The data set should contain 6 columns, accommodating year, month, precipitation [mm/month], potential evapotranspiration [mm/month], average air temperature [degree Celsius], monthly average discharge [m3/s] respectively. In addition, the area of the catchment is also required as a key argument in modelling. 

### Brief manual
There are 6 functional panels in this interactive application, and the functionalities are introdued as following in a atabular format. 
| Panel  | Contents | Functionalities |
| ------ | -------- | --------------- |
| Basin info | speficify the basin info | inform the application of the area of catchment |
| Data input | import the data file  | specify the data importing behaviors and get the data in |
| Simulation | hydrological modelling with WBM | set the keys controls in WBM simulation including modelling period, model parameters and performance evaluations |
| Quantitative attribution | quantify the effects from climate change and human activities on runoff variations | specify the change period, and partition the contribution from two sources to the runoff changes |
| Sensitivity analysis | sensitivity of runoff to precipitation and tempeature | develop different scenarios by adjusting precipitation and tempeature (evapotranspiration) and then quantify the runoff response |
| Parameter analysis | WBM model parameter local sensitivity analysis | modulate the 4 WBM parameters and estimate the response of WBM |


## Model algorithm
See the paper: G. Q. Wang, J. Y. Zhang, Y. Q. Xuan, J. F. Liu, J. L. Jin, Z. X. Bao, R. M. He, C. S. Liu, Y. L. Liu, and X. L. Yan. 2013. Simulating the Impact of Climate Change on Runoff in a Typical River Catchment of the Loess Plateau, China. Journal of Hydrometeorology, 14(5):1553-1561 DOI: https://doi.org/10.1175/JHM-D-12-081.1

## Author
[Prof. Dr. Guoqing Wang](http://rccc.nhri.cn/art/2019/6/4/art_699_38963.html) (Email: gqwang@nhri.cn)

[Xiaoxiang Guan](https://www.gfz-potsdam.de/staff/guan.xiaoxiang/sec44) (Email: xxguan@hhu.edu.cn)
