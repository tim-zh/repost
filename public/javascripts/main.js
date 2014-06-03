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