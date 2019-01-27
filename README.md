<a href="https://zenodo.org/badge/latestdoi/118602132"><img src="https://zenodo.org/badge/118602132.svg" alt="DOI"></a>
# textility
Collection of miscellaneous text utility functions for text processing or text mining. Mainly wrapper functions based on existing packages (`stringi`, `text2vec`, etc.) that have emerged from my personal use to speed up or customize specific tasks.

Dependencies, etc. are not declared very well at the moment (depending on the use case the packages `text2vec`, `Matrix` as well as `slam`, `topicmodels`, `stm`, or `koRpus` should be installed) . Will be updated.

Note: The part the work referring to calculation of coherence metrics was merged into the `text2vec` package: https://github.com/dselivanov/text2vec. Some remarks on calculating coherence metrics are just kept in the `textility` package for reasons of documentation.
