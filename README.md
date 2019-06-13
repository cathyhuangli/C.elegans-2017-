# Project C.elegans : 2016-2017
This is the code summary for our work in 2016-2017, when we studied the updated papers and applied different algorithm of time series, run-length-encoding, clustering, classification on our data. Based on our a work, a paper on run-length-encoding study of C.elegans is published. 

## Methodology
![run length encoding on C.elegans](https://github.com/cathyhuangli/Project-C.elegans_2016-2017/blob/master/Run-length-encoding%20methodology%20for%20C.elegans.png)

## Result
![Clustering result](https://github.com/cathyhuangli/Project-C.elegans_2016-2017/blob/master/clustering%20result.png)
The results show that, using the RLE descriptors, we are able to separate 75% of the N2 on food data (cluster 5) from all the other C. elegans data. In addition, N2 off food data are relatively well clustered together with tph-1 off food data in cluster 1 and cluster 3.Most importantly, 100% of the tph-1 on food data is grouped with 64% of N2 off food data (clusters 3 and 4). Our observation of tph-1 mutants, which fail to produce serotonin, suggests that tph-1 mutants continue food search behavior even on food. 100% of the tph-1 off food data were also grouped with 91% of the N2 off food data across three clusters (clusters 1, 2, and 3). These observations show that tph-1mutants either on food or off food behave like wildtype off food animals. Interestingly, through our approach we were also able to distinguish between the two differently collectedN2-nnf data, one in which the wormwas starved for a longer period of time.

## Conclusion
In this study we aimed to quantify the movement behaviors of wild-type N2 and tph-1 mutant animals, so that we could identify if tph-1 mutants show defects in foraging and food search behaviors. By encoding worms’ path, extracting features for each worm, and performing clustering analysis, we conclude that the locomotory behavior of tph-1 on food resembles the wild-type N2 animal off food. In addition, we found that five of the eleven RLE descriptors can be used to efficiently represent the path characteristics for C. elegans data. Our unbiased, unsupervised analysis provides evidence that shallow and sharp turns are the most critical factor that distinguishes C. elegans movement behaviors in the presence and absence of food. This finding is surprising in that we expected that based on previous studies speed is one of the important factors that determine on food and off food behaviors.



