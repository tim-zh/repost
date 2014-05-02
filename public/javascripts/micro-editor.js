/**
@param {String} source
@param {String} insertion
@param {Number} position
@returns {string}
*/
insertInString = function(source, insertion, position) {
	return source.substr(0, position) + insertion + source.substr(position);
};
/**
@param {HTMLInputElement} element
@param {HTMLElement} buttonContainer
@param {Object} [options]
*/
microEditor = function(element, buttonContainer, options) {
	var defaultOptions = {
		buttonElement: 'button',
		buttonClassName: 'microEditor-button',
		buttons: 'bold,italic,underline,strike,size,link,image,quote,list,code,center,paragraph,preview',
		previewReplaces: 'newLine,bold,italic,underline,strike,size,link,link2,image,image2,quote,list,code,center,paragraph'
	};
	var defaultButtons = {
		bold: ['<b>b</b>', '[b]', '[/b]'],
		italic: ['<i>i</i>', '[i]', '[/i]'],
		underline: ['<u>u</u>', '[u]', '[/u]'],
		strike: ['<s>s</s>', '[s]', '[/s]'],
		size: ['size', '', '', function(text, selectionStart, selectionEnd) {
			var openTag = '[size=' + prompt('size (1..7)') + ']';
			var result = insertInString(text, openTag, selectionStart);
			result = insertInString(result, '[/size]', selectionEnd + openTag.length);
			return result;
		}],
		link: ['url', '', '', function(text, selectionStart, selectionEnd) {
			var url = prompt('url');
			var openTag = url ? '[url=' + url + ']' : '[url]';
			var result = insertInString(text, openTag, selectionStart);
			result = insertInString(result, '[/url]', selectionEnd + openTag.length);
			return result;
		}],
		image: ['image', '', '', function(text, selectionStart, selectionEnd) {
			var width = prompt('width');
			var height = prompt('height');
			var openTag = width || height ? '[img=' + width + ',' + height + ']' : '[img]';
			var result = insertInString(text, openTag, selectionStart);
			result = insertInString(result, '[/img]', selectionEnd + openTag.length);
			return result;
		}],
		quote: ['quote', '[quote]', '[/quote]'],
		list: ['list', '[ol][li]', '[/li][/ol]'],
		code: ['code', '[code]', '[/code]'],
		center: ['center', '[center]', '[/center]'],
		paragraph: ['¶', '[p]', '[/p]']
	};
	var defaultPreviewReplaces = {
		newLine: [/(\r\n|\r|\n|\n\r)/g, '<br/>'],
		bold: [/\[b\]([\s\S]+?)\[\/b\]/g, '<strong>$1</strong>'],
		italic: [/\[i\]([\s\S]+?)\[\/i\]/g, '<i>$1</i>'],
		underline: [/\[u\]([\s\S]+?)\[\/u\]/g, '<u>$1</u>'],
		strike: [/\[s\]([\s\S]+?)\[\/s\]/g, '<s>$1</s>'],
		size: [/\[size=(\d+?)\]([\s\S]+?)\[\/size\]/g, '<font size=$1>$2</font>'],
		link: [/\[url\](\S+?)\[\/url\]/g, '<a href="$1">$1</a>'],
		link2: [/\[url=(\S+?)\]([\s\S]+?)\[\/url\]/g, '<a href="$1">$2</a>'],
		image: [/\[img\](\S+?)\[\/img\]/g, '<img src="$1"/>'],
		image2: [/\[img=(\d*?),(\d*?)\](\S+?)\[\/img\]/g, '<img width="$1" height="$2" src="$3"/>'],
		quote: [/\[quote\]([\s\S]+?)\[\/quote\]/g, '<blockquote>$1</blockquote>'],
		list: [/\[ol\]([\s\S]+?)\[\/ol\]/g, '<ol>$1</ol>'],
		listItem: [/\[li\]([\s\S]+?)\[\/li\]/g, '<li>$1</li>'],
		code: [/\[code\]([\s\S]+?)\[\/code\]/g, '<pre><code>$1</code></pre>'],
		center: [/\[center\]([\s\S]+?)\[\/center\]/g, '<div align="center">$1</div>'],
		paragraph: [/\[p\]([\s\S]+?)\[\/p\]/g, '<p>$1</p>']
	};

	if (options)
		for (var f in defaultOptions)
			if (defaultOptions.hasOwnProperty(f))
				defaultOptions[f] = options[f] || defaultOptions[f];

	var addButton = function(id, title, startString, endString, classNames, handler) {
		var btn = document.createElement(defaultOptions.buttonElement);
		btn.setAttribute('class', defaultOptions.buttonClassName + classNames);
		btn.innerHTML = title;
		btn.addEventListener('click', function(event) {
			event.preventDefault();
			var caretStart = element.selectionStart;
			var caretEnd = element.selectionEnd;
			if (handler) {
				element.value = handler(element.value, caretStart, caretEnd);
				caretEnd = caretStart;
			} else {
				element.value = insertInString(element.value, startString, caretStart);
				element.value = insertInString(element.value, endString, caretEnd + startString.length);
			}
			element.focus();
			element.setSelectionRange(caretStart + (startString ? startString.length : 0),
				caretEnd + (startString ? startString.length : 0));
		});
		buttonContainer.appendChild(btn);
	};

	defaultOptions.buttons.split(',').forEach(function (btn) {
		if (defaultButtons[btn])
			addButton('microEditor' + btn, defaultButtons[btn][0], defaultButtons[btn][1], defaultButtons[btn][2], '',
				defaultButtons[btn][3]);
		else if (options.customButtons[btn]) {
			var b = options.customButtons[btn];
			addButton('microEditor' + btn, b[0], b[1], b[2], b[3], b[4]);
		}
	});

	if (defaultOptions.buttons.indexOf('preview') >= 0) {
		var btn = document.createElement(defaultOptions.buttonElement);
		btn.setAttribute('class', defaultOptions.buttonClassName);
		btn.innerHTML = 'preview';

		var previewContainer = document.createElement('div');
		previewContainer.style.display = 'none';
		previewContainer.style.width = element.offsetWidth + 'px';
		previewContainer.style.height = element.offsetHeight + 'px';
		element.parentNode.appendChild(previewContainer);
		var isPreview = false;
		var elementDisplay = element.style.display;
		btn.addEventListener('click', function(event) {
			event.preventDefault();
			isPreview = !isPreview;
			if (isPreview) {
				var text = element.value;
				defaultOptions.previewReplaces.split(',').filter(function(e) {return defaultPreviewReplaces[e]}).
					forEach(function(rule) {text = text.replace(defaultPreviewReplaces[rule][0], defaultPreviewReplaces[rule][1])});
				if (options.customPreviewReplaces)
					options.customPreviewReplaces.forEach(function(rule) {
						text = text.replace(rule[0], rule[1])});
				previewContainer.innerHTML = text;
				if (options.onPreview)
					options.onPreview(previewContainer);
			}
			element.style.display = isPreview ? 'none' : elementDisplay;
			previewContainer.style.display = isPreview ? 'block' : 'none';
			btn.innerHTML = isPreview ? 'source' : 'preview';
		});
		buttonContainer.appendChild(btn);
	}
};