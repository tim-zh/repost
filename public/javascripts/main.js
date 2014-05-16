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
			textInput.val("");
		}
	});
	textInput.autocomplete({
		serviceUrl: serviceUrl,
		deferRequestBy: 250,
		delimiter: ',',
		triggerSelectOnValidInput: false,
		onSelect: function(suggestion) {
			addLabelsToContainer(suggestion.value, container, hiddenString);
			textInput.val("");
		}
	});
}