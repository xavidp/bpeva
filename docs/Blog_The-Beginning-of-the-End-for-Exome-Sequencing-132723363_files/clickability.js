var c8mImageRotator = null;
var c8mPhotoBox = null;

function loadClickability()
{
	c8mImageRotator = new c8mImageRotatorObject("c8mImageRotator", 4000,  $$("div.c8m_tab_current", "div.c8m_tab"),  $$("ul.c8m_tabs li"));
	c8mPhotoBox = new c8mPhotoBoxObject("c8mPhotoBox", $$("div.c8m_photo_box"));
	
	prepareContact();
	
	fixBreadcrumb();
	fixHeaders();
	fixColumnHeights();
	fixCenterColumnHeight();
}

var c8mImageRotatorObject = Class.create(
{
	initialize: function (name, delay, layers, buttons)
	{
		this.Name = name;
		this.Delay = delay;
		this.Layers = layers;
		this.Buttons = buttons;
		
		this.Current = 0;
		this.Timeout = null;
		this.Animation = null;
		
		this.Setup();
	},
	
	Setup: function ()
	{
		for (var i = 0; i < this.Layers.length && i < this.Buttons.length; i++)
		{
			if (i == 0)
			{
				this.Buttons[i].addClassName("current");
			}
			else
			{
				this.Layers[i].setOpacity(0);
			}
			
			this.Layers[i].style.display = "block";
			
			this.Buttons[i].down("a").href = "javascript:void(" + this.Name + ".ShowTab(" + i + "));";
		}
		
		this.Timeout = setTimeout(this.Name + ".NextTab();", this.Delay);
	},
	
	NextTab: function ()
	{
		var index = this.Current + 1;
		
		if (index >= this.Layers.length || index >= this.Buttons.Length)
		{
			index = 0;
		}
		
		this.ShowTab(index);
	},
	
	ShowTab: function (index)
	{
		if (index != this.Current)
		{
			if (this.Timeout != null)
			{
				clearTimeout(this.Timeout);
			}
			
			if (this.Animation != null)
			{
				this.Animation.cancel();
			}
			
			this.Layers[this.Current].style.zIndex = 100;
			this.Layers[index].style.zIndex = 101;
			
			var name = this.Name;
			
			this.Animation = new  Effect.Opacity(this.Layers[index],
			{
				"from": 0,
				"to": 1,
				"duration": 0.5,
				
				"afterFinish": function ()
				{
					eval(name + ".Finish(" + index + ");");
				}
			});
		}
	},
	
	Finish: function (index)
	{
		this.Buttons.each(function (button)
		{
			button.removeClassName("current");
		});
		
		for (var i = 0; i < this.Layers.length; i++)
		{
			var opacity = (i == index) ? 1 : 0;
			
			this.Layers[i].setOpacity(opacity);
		}
		
		this.Current = index;
		
		this.Buttons[this.Current].addClassName("current");
		
		this.Timeout = setTimeout(this.Name + ".NextTab();", this.Delay);
	}
});

var c8mPhotoBoxObject = Class.create(
{
	initialize: function (name, layers)
	{
		this.Name = name;
		this.Layers = layers;
		
		this.Boxes = [];
		this.Animation = null;
		this.Active = false;
		
		if (layers.length > 0)
		{
			this.Setup();
		}
	},
	
	Setup: function ()
	{
		for (var i = 0; i < this.Layers.length; i++)
		{
			var previousButtons = this.Layers[i].select("#c8m_arrow_left");
			var nextButtons = this.Layers[i].select("#c8m_arrow_right");
			var wrappers = this.Layers[i].select("#c8m_theImages");
			var photos = this.Layers[i].select("div.c8m_photo_box_colummn");
			
			if (previousButtons.length > 0 && nextButtons.length > 0 && wrappers.length > 0 && photos.length > 0)
			{
				var name = this.Name;
				var index = i;
				
				this.Boxes[i] = [];
				this.Boxes[i]["previous"] = previousButtons[0];
				this.Boxes[i]["next"] = nextButtons[0];
				this.Boxes[i]["wrapper"] = wrappers[0];
				this.Boxes[i]["photos"] = photos;
				this.Boxes[i]["active"] = 0;
				
				this.Boxes[i]["previous"].observe("click", function ()
				{
					eval(name + ".Previous(" + index + ");");
				});
				
				this.Boxes[i]["next"].observe("click", function ()
				{
					eval(name + ".Next(" + index + ");");
				});
				
				this.Boxes[i]["wrapper"].setStyle(
				{
					"left": "auto",
					"height": this.Layers[i].getHeight() + "px"
				});
				
				for (var j = 0; j < this.Boxes[i]["photos"].length; j++)
				{
					this.Boxes[i]["photos"][j].setStyle(
					{
						"left": this.Boxes[i]["wrapper"].getWidth() + "px",
						"display": "block",
						"width": this.Boxes[i]["wrapper"].getWidth() + "px"
					});
					
					if (j == 0)
					{
						this.Boxes[i]["photos"][j].style.left = "0";
					}
					
					var images = this.Boxes[i]["photos"][j].select("div.image img");
					
					if (images.length > 0)
					{
						var extraWidth = (this.Boxes[i]["wrapper"].getWidth() - images[0].getWidth()) / 2;
						var extraHeight = (this.Boxes[i]["wrapper"].getHeight() - this.Boxes[i]["photos"][j].getHeight()) / 2;
						
						images[0].setStyle(
						{
							"paddingTop": Math.floor(extraHeight) + "px",
							"paddingRight": Math.ceil(extraWidth) + "px",
							"paddingBottom": Math.ceil(extraHeight) + "px",
							"paddingLeft": Math.floor(extraWidth) + "px"
						});
						
						images[0].up().setAttribute("rel", "lightbox[c8m_photo_box" + i + "]");
					}
				}
			}
		}
	},
	
	Previous: function (boxIndex)
	{
		if (!this.Active)
		{
			this.Active = true;
			
			var active = this.Boxes[boxIndex]["active"];
			var showPhoto = (active == 0) ? this.Boxes[boxIndex]["photos"].length - 1 : active - 1;
			
			this.Slide(boxIndex, showPhoto, true);
		}
	},
	
	Next: function (boxIndex)
	{
		if (!this.Active)
		{
			this.Active = true;
			
			var active = this.Boxes[boxIndex]["active"];
			var showPhoto = (active == this.Boxes[boxIndex]["photos"].length - 1) ? 0 : active + 1;
			
			this.Slide(boxIndex, showPhoto, false);
		}
	},
	
	Slide: function (boxIndex, photoIndex, previous)
	{
		var name = this.Name;
		var width = this.Boxes[boxIndex]["wrapper"].getWidth();
		var multiplier = (previous) ? -1 : 1;
		var active = this.Boxes[boxIndex]["active"];
		
		this.Boxes[boxIndex]["photos"][photoIndex].style.left = (width * multiplier) + "px";
		this.Boxes[boxIndex]["active"] = photoIndex;
		
		this.Animation = new Effect.Parallel(
		[
			new Effect.Move(this.Boxes[boxIndex]["photos"][active],
			{
				"sync": true,
				"x": width * (multiplier * -1)
			}),
			
			new Effect.Move(this.Boxes[boxIndex]["photos"][photoIndex],
			{
				"sync": true,
				"x": width * (multiplier * -1)
			})
		],
		{
			"duration": 0.25,
			
			"afterFinish": function ()
			{
				eval(name + ".Active = false;");
			}
		});
	}
});

function prepareContact()
{
	$$("ul.contact").each(function (ul)
	{
		var li = ul.select("li");
		
		for (var i = 0; i < li.length; i++)
		{
			li[i].id = "contact-" + i;
		}
	});
}

function fixBreadcrumb()
{
	$$("div.breadCrumb").each(function (div)
	{
		if (div.up().previous() != null)
		{
			div.up().previous().style.height = div.up().getHeight() + "px";
		}
	});
}

function fixHeaders()
{
	$$("div.top h2").each(function (h2)
	{
		h2.up().up().addClassName("boxHeader");
	});
}

function fixColumnHeights()
{
	var row = 1;
	
	while ($("row" + row) != null)
	{
		var columns = [];
		var boxes = [];
		var tallest = 0;
		
		$("row" + row).select("div.center-content").each(function (div)
		{
			var column = div.up().up();
			
			if (columns.length == 0 || columns[columns.length - 1] != column)
			{
				columns[columns.length] = column;
				boxes[boxes.length] = div;
				
				var height = column.getHeight();
				
				if (height > tallest)
				{
					tallest = height;
				}
			}
			else
			{
				boxes[boxes.length - 1] = div;
			}
		});
		
		for (var i = 0; i < columns.length && i < boxes.length; i++)
		{
			var height = columns[i].getHeight();
			
			if (height < tallest)
			{
				var padding = boxes[i].style.paddingBottom;
				
				if (padding.length > 0)
				{
					padding = parseInt(padding);
				}
				else
				{
					padding = 0;
				}
				
				boxes[i].style.paddingBottom = (tallest - height + padding) + "px";
			}
		}
		
		row++;
	}
}

function fixCenterColumnHeight()
{
	var center = $("columnCenter");
	var right = $("columnRight");
	
	if (center != null && right != null && center.getHeight() < right.getHeight())
	{
		var boxes = center.select("div.boxDefault");
		
		if (boxes.length > 0)
		{
			var last = boxes[boxes.length - 1].select("div.center-content")[0];
			var extra = last.style.paddingBottom;
			
			if (extra.length > 0)
			{
				extra = parseInt(extra);
			}
			else
			{
				extra = 0;
			}
			
			last.style.paddingBottom = (right.getHeight() - center.getHeight() + extra) + "px";
		}
	}
}

// Previously in click-express.js
function createCookie(name, value, days)
{
	if (days) {
		var date = new Date();
		date.setTime(date.getTime()+(days*24*60*60*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";
	document.cookie = name+"="+value+expires+"; path=/";
}

function readCookie(name)
{
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i=0;i < ca.length;i++) {
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1,c.length);
		if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
	}
	return null;
}

function eraseCookie(name, path, domain)
{
	 document.cookie = name + "=" +
   ( (path) ? ";path=" + path : "") +
   ( (domain) ? ";domain=" + domain : "") +
   ";expires=Thu, 01-Jan-1970 00:00:01 GMT;"
}

function clickabilityPageQuery(q)
{
	if (q.length > 1) this.q = q.substring(1, q.length);
	else this.q = null;
	this.keyValuePairs = new Array();
	if (q) {
		for (var i = 0; i < this.q.split("&").length; i++) {
			this.keyValuePairs[i] = this.q.split("&")[i];
		}
	}
	this.getKeyValuePairs = function() {
		return this.keyValuePairs;
	}
	this.getValue = function(s) {
		for (var j = 0; j < this.keyValuePairs.length; j++) {
			if (this.keyValuePairs[j].split("=")[0] == s)
			return this.keyValuePairs[j].split("=")[1];
		}
		return false;
	}
	this.getParameters = function() {
		var a = new Array(this.getLength());
		for (var j = 0; j < this.keyValuePairs.length; j++) {
			a[j] = this.keyValuePairs[j].split("=")[0];
		}
		return a;
	}
	this.getLength = function() {
		return this.keyValuePairs.length;
	}
}

function clickabilityQueryString(key)
{
	var page = new clickabilityPageQuery(window.location.search);
	return unescape(page.getValue(key));
}

// Previously Inline
function validateForm(form)
{
	var formok = true;
	
	if (form.name.value == "")
	{
		document.getElementById("errorMsgName").className = "errorMsgshow";
		
		formok = false;
	}
	else
	{
		document.getElementById("errorMsgName").className = "errorMsghide";
	}
	
	if (form.email.value == "")
	{
		document.getElementById("errorMsgEmail").className = "errorMsgshow";
		
		formok = false;
	}
	else
	{
		document.getElementById("errorMsgEmail").className = "errorMsghide";
	}
	
	if (form.field.value == "")
	{
		document.getElementById("errorMsgComment").className = "errorMsgshow";
		
		formok = false;
	}
	else
	{
		document.getElementById("errorMsgComment").className = "errorMsghide";
	}
	
	if (formok)
	{
	   return postForm(form, false);
	}
	
	return formok;
}

function newPopupWindow(url) {
	popupWindow = window.open(
		url,'popUpWindow','height=700,width=800,left=10,top=10,resizable=yes,scrollbars=yes,toolbar=yes,menubar=no,location=no,directories=no,status=yes')
}

