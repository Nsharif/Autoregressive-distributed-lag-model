# autoregressive-distributed-lag-model
This repository is about forecasting market share using the autoregressive distributed lag approach. Particularly using an ADL(1,1). 

I have also applied a Random Effects to the data to take care of serial correlation / heteroskedasticity. 

Since the dependent varianble needs to be bounded between 0 and 1, I used the berry inversion method to construct that constriant. 

Attached is a pdf paper that you can use to follow allong when forecasting using the autoregressive distributed lag model approach. Additionaly, the R code is available. Note, the data is proprietary and only available internally within Kaiser Permanente. Therefore, external users will not have access to the data.

