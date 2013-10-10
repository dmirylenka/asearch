ScienScanJs = function() {

        var topicVisTopics;

        var topicVisRel;

        var resultData;

        var topicIndex;

        var getChildren = function(topic) {
                var result = [];
                for (var i = 0; i < topicVisRel.length; i++) {
                        if (topicVisRel[i][1] == topic) {
                                result.push(topicVisRel[i][0]);
                        }
                }
                return result;
        }

        var contains = function(arr, obj) {
                for (var i = 0; i < arr.length; i++) {
                        if (arr[i] === obj) {
                                return true;
                        }
                }
                return false;
        }
        
        var arrayDiff = function(arr1, arr2) {
           return arr1.filter(function(i) {return !(arr2.indexOf(i) > -1);});
        }

        var getDescendants = function(topic, parents) {
                if (!parents) {
                   parents = [];
                }
                var result = [topic];
                var children = arrayDiff(getChildren(topic), parents);
                parents.push(topic);
                for (var i = 0; i < children.length; i++) {
                        var newDescendants = getDescendants(children[i], parents);
                        result = result.concat(newDescendants);
                }
                return result;
        }

        var getDescendantNodes = function(node, parents) {
               var topic = $(node).find("title").text();
               var descTopics = getDescendants(topic);
               var allNodes = $("#topic-graph g[class=node]");
               var result = [];
               for (i = 0; i < allNodes.length; i++) {
                   var candidate = $(allNodes[i]).find("title").text();
                   if (contains(descTopics, candidate)) {
                       result.push(allNodes[i]);
                   }
               }
               return result;
        }

        var getPapersForTopic = function(topic) {
                return topicVisTopics[topic] || [];
        }

        var distinct = function(arr){
           var u = {}, a = [];
           for(var i = 0, l = arr.length; i < l; ++i){
              if(u.hasOwnProperty(arr[i])) {
                 continue;
              }
              a.push(arr[i]);
              u[arr[i]] = 1;
           }
           return a;
        }

        var getPapersForDescendants = function(topic) {
                var topics = getDescendants(topic);
                var papers = topics.map(getPapersForTopic);
                return distinct([].concat.apply([], papers));
        }

        var getAllPapers = function(topic) {
            return Object.keys(resultData);
        }

        var highlight = function(node) {
           $(node).find("text").attr('fill',"#ffaa00");
           $(node).find("text").attr('stroke',"none");
        }

        var highlightStrongly = function(node) {
           $(node).find("text").attr('fill',"#ffaa00");
           $(node).find("text").attr('stroke',"#ffaa00");
        }
        
        var highlightAll = function(nodes) {
           nodes = nodes || "#topic-graph g[class=node]";
           $(nodes).each(function(idx, node) {highlight(node);});
        }

        var highlightSubtree = function(node) {
           highlightAll(getDescendantNodes(node));
           highlightStrongly(node);
        }

        var unhighlight = function(node) {
           $(node).find("text").attr('fill',"#6688AA");
           $(node).find("text").attr('stroke',"");
        }

        var unhighlightAll = function() {
                var topicNodes = $("#topic-graph g[class=node]");
                for (var i = 0; i < topicNodes.length; i++) {
                    unhighlight(topicNodes[i]);
                }
        }

        var masPaperUrl = function(id) {
                return 'http://academic.research.microsoft.com/Publication/' + id;
        }

        var masCitUrl = function(id) {
                return 'http://academic.research.microsoft.com/Detail?entitytype=1&searchtype=5&id=' + id;
        }

        var citationHtml = function(paper) {
           if (paper['ncit'] != 0) {
              return $('<span>').addClass('citation')
                    .append('&nbsp;(')
                    .append(
                        //$('<a>')
                        $('<span>')
                       .attr('href', masCitUrl(paper.id))
                       .append('Citations: ')
                       .append(paper['ncit']))
                    .append(')');
           }
           return '';
        }

        var titleHtml = function(paper) {
           return $('<div>').addClass('title-download')
              .append($('<div>').addClass('title')
                    .append($('<h3>')
                       .append($(
                           //'<a>',{href:masPaperUrl(paper.id)}
                           '<a>',{}
                       )
                          .append(paper.title))
                       .append(citationHtml(paper))));
        }

        var authorUrl = function(id) {
           return 'http://academic.research.microsoft.com/Author/' + id;
        }

        var authorHtml = function(author) {
           return $('<a>')
              .addClass('author-name-tooltip')
              .append(author['full-name']);
              // .append(author['first-name'] + " ")
              // .append(author['middle-name'] + " ")
              // .append(author['last-name']);
        }

        var contentHtml = function(paper) {
           var result = $('<div>').addClass('content');
           for (k = 0; k < paper.author.length; k++) {
              if (k != 0) {
                 result.append(', ') .append($('<span>').addClass('span-break'));
              }
              result.append(authorHtml(paper.author[k]));
           }
           return result;
        }

        var abstrHtml = function(paper) {
           if (paper.abstract) {
              return $('<div>').addClass('abstract')
                 .append($('<span>')
                       .append(paper.abstract)
                       .append('...'));
           }
           return ''; 
        }

        var venueUrl = function(venueType, id) {
           return 'http://academic.research.microsoft.com/' + venueType + '/' + id;
        }

        var venueHtml = function(paper) {
           var result = '';
           var venue = paper.conference || paper.journal;
           var venueType = paper.conference ? "Conference" : "Journal";
           if (venue) {
              result = $('<div>').addClass('conference')
                 .append($('<span>')
                       .append(venueType)
                       .append(': '))
                 .append($('<a>')
                       .attr('href',venueUrl(venueType, venue.id))
                       .addClass('conference-name')
                       .append(venue['full-name']));
              if (paper.year) {
                 result.append($('<span>').addClass('year')
                       .append(', ')
                       .append(paper.year));
              }
           }
           if (result == '' && paper.year) {
              result = $('<div>').addClass('conference')
                 .append('Published in ').append(paper.year);
           }
           return result;
        }

        var paperHtml = function(paper) {
                return $('<li>').addClass('paper-item')
                                .append(titleHtml(paper))
                                .append(contentHtml(paper))
                                .append($('<div>').addClass('clear'))
                                .append(abstrHtml(paper))
                                .append(venueHtml(paper));
        }

        var papersHtml = function(papers) {
             var result = $("<ul>");
             for (j = 0; j < papers.length; j++) {
                result.append(paperHtml(resultData[papers[j]]));
             }
             return result;
        }

        var resultTitle = function(query, topic) {
           var result = $('<p>').append("Results for ")
                        .append($("<span>").addClass("result-title-query").append(query));
           if (topic) {
              var topicTitle = topicIndex[topic].title;
              result.append(" in ")
                 .append($("<span>").addClass("result-title-topic").append(
//topic.replace(new RegExp('_',"g"), ' ')
                     topicTitle
));
           } else {
              result.append(" in all topics")
           }
           return result;
        }

        var graphTitle = function(query) {
           return $('<p>').append("Topic map for ")
              .append($("<span>").addClass("result-title-query").append(query));
        }

        var chosenTopic = null;

        var findTopicNode = function(topic) {
           //TODO:rewrite in proper jquery
           return $("#topic-graph g[class=node]")
              .filter(function(){return $(this).find('title').text() == topic})[0];
        }


	var paperSortFn = function(x,y) {
		return resultData[y]["ncit"] - resultData[x]["ncit"];
	}	

        var showTopic = function(topic){
                chosenTopic = topic;
                var papers = getPapersForDescendants(topic).sort(paperSortFn);
                var query = $('#search-box').val();
                unhighlightAll();
                var topicNode = findTopicNode(topic);
                highlightSubtree(topicNode);
                $('#result-list').html(papersHtml(papers));
                $('#graph-title').html(graphTitle(query))
                   // .append(showHidePagesButton());
                $('#result-title').html(resultTitle(query, topic))
                   .append(showAllButton(true));
        }

        var showAll = function() {
           query = $('#search-box').val();
           var papers = getAllPapers().sort(paperSortFn);
           highlightAll();
           $('#result-list').html(papersHtml(papers));
           $('#graph-title').html(graphTitle(query))
              // .append(showHidePagesButton());
           $('#result-title').html(resultTitle(query))
              .append(showAllButton(false));
        }

        var showResults = function() {
            if (chosenTopic && findTopicNode(chosenTopic)) {
               showTopic(chosenTopic);
            } else {
               showAll();
            }
        }

        var showAllButton = function(enabled) {
           var disabled = enabled ? false : 'disabled';
           var result = $('<button>').attr('disabled', disabled)
              .addClass('show-all')
              .click(function () {
                 chosenTopic = null;
                 showAll()
              });
           if (enabled) {
              result.append('Show for all topics');
           } else {
              result.append('Showing all topics');
              $(result).css('border','0px');
           }
           return result;
        }

        var hidePages = function() {
           $('#keep-pages').val('false');
           changeMinFreqValue();
        }

        var showPages = function() {
           $('#keep-pages').val('true');
           changeMinFreqValue();
        }

        var showHidePagesButton = function() {
           var hide = ($('#keep-pages').val() == "true");
           var result = $('<button>')
              .addClass('show-hide-pages');
           if (hide) {
              result.append('Hide micro-topics')
                 .click(hidePages);
           } else {
              result.append('Show micro-topics')
                 .click(showPages);
           }
           return result;
        }

        var init = function() {
                topicVisTopics = topicVisData["topic-titles"];
                topicVisRel = topicVisData["rel"];
                resultData = topicVisData["title-results"];
                onlyPages = topicVisData["only-pages"];
                topicIndex = topicVisData["topic-index"];
                var topicNodes = $("#topic-graph g[class=node]");
                topicNodes.bind('click',function() {
                   showTopic($(this).find('title').text());
                });
                showResults();
                $('body').css({'cursor':'default'});
                $('#minFreqSlider a').css({'cursor':'default'});
        }

        var timeLastChange = 0; 

        var changeMinFreqValue = function(value) {
           $('body').css({'cursor':'wait'});
            $('#minFreqSlider a').css({'cursor':'wait'});
           if (typeof(value) === 'undefined') {
               value = $('#minFreqSlider').slider('value'); 
           }
           xmlhttp=new XMLHttpRequest();
           xmlhttp.onreadystatechange=function() {
              if (xmlhttp.readyState==4 && xmlhttp.status==200) {
                 data = JSON.parse(xmlhttp.responseText);
                 document.getElementById('topic-graph').innerHTML=data.svg;
                 topicVisData = data.json;
                 init();
              }
           }
           var query = $('#search-box').val();
           var keepPages = $('#keep-pages').val();
           xmlhttp.open('POST','/refine?query=' + query + '&n-topics=' + value + '&keep-pages=' + keepPages,true);
           xmlhttp.send();
        }

        return {
                init : init,
                refine : changeMinFreqValue
        }
}();

$(document).ready(function () {

    $('body').css({'cursor':'wait'})

    // $('body').ajaxStart(function() {
    //     $(this).css({'cursor':'wait'})
    // }).ajaxStop(function() {
    //     $(this).css({'cursor':'default'})
    // });

   ScienScanJs.init();
   //TODO:rewrite in proper jquery
   $('#show-more').click(function() {
      var sl = $('#minFreqSlider');
      var value = sl.slider('value');
      sl.slider('value', value + 1);
      ScienScanJs.refine(sl.slider('value'));
   });
   $('#show-less').click(function() {
      var sl = $('#minFreqSlider');
      var value = sl.slider('value');
      sl.slider('value', value - 1);
      ScienScanJs.refine(sl.slider('value'));
   });
   $('#search-button').click(function() {
      var sl = $('#minFreqSlider');
      var value = sl.slider('value');
      ScienScanJs.refine(sl.slider('value'))
   });
   $('#search-box').focus().select();
});
