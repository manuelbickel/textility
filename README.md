<a href="https://zenodo.org/badge/latestdoi/118602132"><img src="https://zenodo.org/badge/118602132.svg" alt="DOI"></a>
# textility
Collection of miscellaneous text utility functions for text processing or text mining. Mainly wrapper functions that have emerged from my personal use to speed up or customize specific tasks. These functions leverage the potential of various existing packages, especially `text2vec` as well as others such as `stringi`, etc. 

Dependencies, etc. are not declared very well at the moment (depending on the use case the packages `text2vec`, `Matrix` as well as `slam`, `topicmodels`, `stm`, or `koRpus` should be installed) . Will be updated.

The use of the package is demonstrated in the vignette that was created to publish the following scientific article published 2019 in Energy, Sustainability and Society, DOI:  (the article was the main motivation for creating this package):
<a href="https://link.springer.com/article/10.1186%2Fs13705-019-0226-z" title="Reflecting trends in the academic landscape of sustainable energy using probabilistic topic modeling"</a>

Note: The part the work referring to calculation of coherence metrics was merged into the `text2vec` package: https://github.com/dselivanov/text2vec. Some remarks on calculating coherence metrics are just kept in the `textility` package for reasons of documentation.
