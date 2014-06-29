var addLabelsToContainer = function(str, container, hiddenString) {
	str.split(',').filter(function(s) {return /^[\w \-]+$/.test(s);}).forEach(function(s) {
		var title = s.trim();
		hiddenString.val(hiddenString.val() + ',' + title);
		var idRandomPart = Math.round(Math.random() * 100000);
		$('<a href="#" id="' + title + idRandomPart + '" class="label label-info" style="margin-left:4px;">' + title + ' ×</a>').appendTo(container).click(function() {
			var titles = hiddenString.val().split(',');
			titles = titles.filter(function(e) {return e !== title;});
			hiddenString.val(titles.join());
			$('#' + title + idRandomPart).remove();
		});
	});
};

var initAutocomplete = function(serviceUrl, textInput, container, hiddenString) {
	textInput.keypress(function(event) {
		if (event.keyCode == 13 && textInput.val()) {
			event.preventDefault();
			addLabelsToContainer(textInput.val(), container, hiddenString);
			textInput.val('');
		}
	});
	textInput.autocomplete({
		serviceUrl: serviceUrl,
		deferRequestBy: 250,
		delimiter: ',',
		triggerSelectOnValidInput: false,
		onSelect: function(suggestion) {
			addLabelsToContainer(suggestion.value, container, hiddenString);
			textInput.val('');
		}
	});
};

var isLoading = false;
var insertResponse = function(serviceUrl, parametersObject, urlInput, uploadButton, contentTextArea, callback) {
	if (isLoading)
		return;
	isLoading = true;
	var buttonText = uploadButton.text();
	uploadButton.text('loading');
	parametersObject.url = urlInput.val();
	$.get(serviceUrl, parametersObject, function(response) {
		if (response) {
			if (callback)
				response = callback(response);
			contentTextArea[0].value = insertInString(contentTextArea[0].value, response, contentTextArea[0].selectionEnd);
			urlInput.val('');
		}
		uploadButton.text(buttonText);
		isLoading = false;
	});
};

$('.chat-panel').perfectScrollbar({wheelSpeed: 20});
$('#pageScroll').perfectScrollbar({wheelSpeed: 20});
hljs.initHighlightingOnLoad();

var favoriteTagList = $('#favoriteTagList');
var tagInput = $('#favoriteTags');
var addFavoriteTags = function(str) {
	str.
		split(',').
		filter(function(s) {return /^[\w \-]+$/.test(s);}).
		forEach(function(s) {
			var tagName = s.trim();
			$.post('/favorite/add/' + tagName, null, function(response) {
				if ('true' === response) {
					$('<a href="/tag/' + tagName + '" id="favorite' + tagName + '" class="list-group-item">' + tagName +
						'<button type="button" class="btn btn-default btn-xs pull-right" id="removeFavorite' + tagName + '">remove</button></a>').
						appendTo(favoriteTagList);
					$('#removeFavorite' + tagName).click(function(event) {
						event.preventDefault();
						$.post('/favorite/remove/' + tagName, null, function(response) {
							if ('true' === response)
								$('#favorite' + tagName).remove();
						});
					});
				}
			});
		}
	);
};
tagInput.keypress(function(event) {
	if (event.keyCode == 13 && tagInput.val()) {
		event.preventDefault();
		addFavoriteTags(tagInput.val());
		tagInput.val("");
	}
});
tagInput.autocomplete({
	serviceUrl: '/tags',
	deferRequestBy: 250,
	delimiter: ',',
	triggerSelectOnValidInput: false,
	onSelect: function(suggestion) {
		addFavoriteTags(suggestion.value);
		tagInput.val("");
	}
});

var html2bb = function(html) {
	var text = html;
	var replacements = [
		[/<br\/>/g, '\r\n'],
		[/<strong>([\s\S]+?)<\/strong>/g, '[b]$1[/b]'],
		[/<i>([\s\S]+?)<\/i>/g, '[i]$1[/i]'],
		[/<u>([\s\S]+?)<\/u>/g, '[u]$1[/u]'],
		[/<s>([\s\S]+?)<\/s>/g, '[s]$1[/s]'],
		[/<font size=(\d+?)>([\s\S]+?)<\/font>/g, '[size=$1]$2[/size]'],
		[/<a href='(\S+?)'>([\s\S]+?)<\/a>/g, '[url=$1]$2[/url]'],
		[/<img src='(\S+?)'\/>/g, '[img]$1[/img]'],
		[/<img width='(\d*?)' height='(\d*?)' src='(\S+?)'\/>/g, '[img=$1,$2]$3[/img]'],
		[/<blockquote>([\s\S]+?)<\/blockquote>/g, '[quote]$1[/quote]'],
		[/<ol>([\s\S]+?)<\/ol>/g, '[ol]$1[/ol]'],
		[/<li>([\s\S]+?)<\/li>/g, '[li]$1[/li]'],
		[/<div align='center'>([\s\S]+?)<\/div>/g, '[center]$1[/center]'],
		[/<pre><code class='(\S*?)'>([\s\S]+?)<\/code><\/pre>/g, '[code=$1]$2[/code]'],
		[/<p>([\s\S]+?)<\/p>/g, '[p]$1[/p]']
	];
	var temp = '';
	while (temp != text) {
		temp = text;
		replacements.forEach(function(pair) {text = text.replace(pair[0], pair[1])});
	}
	return text;
};