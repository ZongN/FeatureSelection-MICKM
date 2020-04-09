# *Maximal information coefficient k-medoids (MICKM)*
## Author : ZONG-EN CAI | [I-Shou University T.D.S](http://etd.lib.isu.edu.tw/main/index)
### Abstract :
In this study, an improved feature selection approach is proposed by combining the maximum information coefficient (MIC) and k-medoids clustering, so as to select discriminative features from numerous features and further construct precise classification models. To begin with, the similarity between each pair of features and the similarity between each feature and classes are calculated with the MIC. Then, all features are grouped into K clusters through the k-medoids clustering and a representative feature which possesses the maximum MIC value with the class variable is selected from each cluster. Finally, K representative features are obtained. To verify the superiority of our approach, four datasets with different properties and four classifiers are considered. Also, comparisons between our approach and other three feature selection approaches are made. Compared with other approaches, experimental results show that our approach presents the better classification performance in a majority of combined conditions of different datasets and different classifiers. 

:exclamation: Paper will be published in 2022 :exclamation:

--- 

This is a feature selection method that I proposed. The more detail information can be found in my graduation thesis.

You can call the feature selection method in my [*FeatureSelection*](https://github.com/ZongN/FeatureSelection) repositories.

* First of all, you need to pull the *CallFAMICKM.R* into the main *Evaluate* folder. And check the paths are correct.

* Second, put the MICKM folder under the *FeatureSelection/* .

* Third, you need to add the call function source code in the [*Lib.R*](https://github.com/ZongN/FeatureSelection/blob/master/Evaluate/Lib.R). Put "source("CallFAMICKM.R",local = T)" into the .R file.

The input and output are the same as other methods in my repositories.

---

The original code is *MICKM.R*, and I followed up with other methods to improve. So the final version is *FAMICKM2.R*. It's improved with reference to OMICFS.

In *FAMICKM2.R*, it's more fast than original. It will exclude most features through the maximal information coefficient with the label.
