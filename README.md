# Forecasting with Optimal Starting Point

This repository contains the code accompanying the paper ["Forecasting with Optimal Starting Point"]([https://www.sciencedirect.com/science/article/pii/S0957417425004208?via%3Dihub](https://doi.org/10.1016/j.eswa.2025.126798)). The paper introduces a novel approach to time series forecasting by optimizing the selection of the starting point for improved prediction accuracy.

## Overview

Recent advances on time series forecasting mainly focus on improving the forecasting models themselves. However, managing the length of the input data can also significantly enhance prediction performance. In this paper, we introduce a novel approach called Optimal Starting Point Time Series Forecast (OSP-TSP) to capture the intrinsic characteristics of time series data. By adjusting the sequence length via leveraging the XGBoost and LightGBM models, the proposed approach can determine optimal starting point (OSP) of the time series and thus enhance the prediction performances. The performances of the OSP-TSP approach are then evaluated across various frequencies on the M4 dataset and other real-world datasets. Empirical results indicate that predictions based on the OSP-TSP approach consistently outperform those using the complete dataset. Moreover, recognizing the necessity of sufficient data to effectively train models for OSP identification, we further propose targeted solutions to address the issue of data insufficiency.

The code implements the methodology proposed in the paper, including data preprocessing, model training, and evaluation of the optimal starting point selection for forecasting.

## Features
- Implementation of the optimal starting point selection method
- Forecasting using various time series models
- Performance evaluation with different datasets
- Visualization tools for analysis

If you find this work useful, please cite our paper:

```
@article{ZhongY2025OptimalStarting,
	title = {Optimal starting point for time series forecasting},
	issn = {0957-4174},
	doi = {10.1016/j.eswa.2025.126798},
	urldate = {2025-02-14},
	journal = {Expert Systems with Applications},
	author = {Zhong, Yiming and Ren, Yinuo and Cao, Guangyao and Li, Feng and Qi, Haobo},
	month = feb,
	year = {2025},
	keywords = {Time series forecasting, Time series features, Optimal starting point},
	pages = {126798},
}
```

## License

This project is licensed under the MIT License. 

## Contact

For any inquiries, please contact [feng.li@gsm.pku.edu.cn] or open an issue in the repository.
