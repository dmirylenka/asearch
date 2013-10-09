{:exp-dir "./exp/",
 :file-name-fn :type-suffix,
 :sample-size 100,
 :date-time #inst "2013-08-07T00:04:58.243-00:00",
 :data-dir "./resources/data/",
 :exp-name "logmap-match-precision-coverage-2013-08-07-0",
 :suffix nil,
 :file-ext "clj",
 :date-format "YY-MM-dd'T'HH:mm:ss",
 :mapping-file
 "/Users/dmirylenka/data/dbpedia/logmap-dbpedia-acm-match-art/logmap2_mappings.txt",
 :description
 "Examine the matching between ACM CCS and Wikipedia-based category-article hierarchy produced by logmap. Wikipedia-based hierarchy is obtained by querying DBPedia for the categories within 7 steps down from Computer_science, and articles that belong to them. Every cycle in the obtained hierarchy is detected and merged into a single node. The experiment reports sample successfull mappings as well as non-mappings.",
 :rand-seed 751881,
 :acm-file "/Users/dmirylenka/data/dbpedia/ACMCCS.rdf.xml"}
