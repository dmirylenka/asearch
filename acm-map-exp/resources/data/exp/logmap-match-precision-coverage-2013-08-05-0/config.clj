{:exp-dir "./exp/",
 :file-name-fn :type-suffix,
 :sample-size 100,
 :date-time #inst "2013-08-05T01:25:08.045-00:00",
 :data-dir "./resources/data/",
 :exp-name "logmap-match-precision-coverage-2013-08-05-0",
 :suffix nil,
 :file-ext "clj",
 :date-format "YY-MM-dd'T'HH:mm:ss",
 :mapping-file
 "/Users/dmirylenka/data/dbpedia/logmap-dbpedia-acm-match/logmap2_mappings.txt",
 :description
 "Examine the matching between ACM CCS and Wikipedia-based category hierarchy produced by logmap. Wikipedia-based hierarchy is the result of the sparql query to DBPedia that selects the categories within 7 steps down from Computer_science. Every cycle in the obtained hierarchy is detected and merged into a single node. The experiment reports sample successfull mappings as well as non-mappings.",
 :rand-seed 751881,
 :acm-file "./resources/data/ACMCCS.xml"}
