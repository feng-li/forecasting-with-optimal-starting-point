# forecasting-with-optimal-starting-point

## Introduction

Recent advances on time series forecasting mainly focus on improving the forecasting models themselves. However, managing the length of the input data can also significantly enhance prediction performance. In this paper, we introduce a novel approach called Optimal Starting Point Time Series Forecast (OSP-TSP) to capture the intrinsic characteristics of time series data. By adjusting the sequence length via leveraging the XGBoost and LightGBM models, the proposed approach can determine optimal starting point (OSP) of the time series and thus enhance the prediction performances. The performances of the OSP-TSP approach are then evaluated across various frequencies on the M4 dataset and other real-world datasets. Empirical results indicate that predictions based on the OSP-TSP approach consistently outperform those using the complete dataset. Moreover, recognizing the necessity of sufficient data to effectively train models for OSP identification, we further propose targeted solutions to address the issue of data insufficiency.

## Code Structre

