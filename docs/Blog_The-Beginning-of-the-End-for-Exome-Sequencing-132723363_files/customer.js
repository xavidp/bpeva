Event.observe(window, "load", function ()
{

	preparePrimaryNav();
	prepareImageRotator();
	prepareItemSeparators();
	prepareShareButtons();
	
	fixBreadcrumb();
	fixMinHeight();
	
	loadClickability();
});

function preparePrimaryNav()
{
	$$("div.boxPrimaryNav").each(function (div)
	{
		var buttons = div.down("ul").immediateDescendants();
		
		for (var i = 0; i < buttons.length; i++)
		{
			if (i == 0)
			{
				buttons[i].down("a").addClassName("first");
			}
			else if (i == buttons.length - 1)
			{
				buttons[i].down("a").addClassName("last");
			}
		}
	});
}

function prepareImageRotator()
{
	var buttons = $$("ul.c8m_tabs li");
	
	for (i = 0; i < buttons.length; i++)
	{
		if (i == 0)
		{
			buttons[i].down("a").addClassName("first");
		}
		else if (i == buttons.length - 1)
		{
			buttons[i].down("a").addClassName("last");
		}
	}
	
	var layers = $$("div.c8m_tab");
	
	for (i = 0; i < layers.length; i++)
	{
		var layer = layers[i];
		
		layer.observe("click", function ()
		{
			window.location = layer.down("a").href;
		});
	}
}

function prepareItemSeparators()
{
	$$("div.boxFullListIndex div.center-content", "div.boxMainFullListIndex div.center-content").each(function (div)
	{
		var layers = div.select("div.contentListItem", "#top", "div.articleListItem");
		
		for (var i = 1; i < layers.length; i++)
		{
			layers[i].addClassName("contentListItemSeparator");
		}
	});
}

function prepareShareButtons()
{
	$$("div.boxMainContent div.center-content").each(function (div)
	{
		if (div.down().hasClassName("shareButtons"))
		{
			div.up().addClassName("boxShareTop");
		}
	});
}

function fixBreadcrumb()
{
	var layers = $$("div.breadCrumb");
	
	if (layers.length > 0)
	{
		var parent = layers[0].up();
		var previous = parent.previous();
		
		if (previous != null)
		{
			previous.style.display = "none";
			
			parent.removeClassName("alpha");
			parent.addClassName("grid_12");
			parent.addClassName("alpha");
			parent.addClassName("omega");
		}
	}
}

function fixMinHeight()
{
	var center = $("columnCenter");
	
	if (center != null && center.getHeight() < 550)
	{
		var boxes = center.select("div.boxDefault");
		
		if (boxes.length > 0 && $$("div.homepage").length == 0)
		{
			var last = null;
			var current = boxes.length - 1;
			
			while (last == null && current >= 0)
			{
				var children = boxes[current].select("div.center-content");
				
				if (children.length > 0)
				{
					last = children[0];
				}
				
				current--;
			}
			
			if (last != null)
			{
				var extra = last.style.paddingBottom;
				
				if (extra.length > 0)
				{
					extra = parseInt(extra);
				}
				else
				{
					extra = 0;
				}
				
				var tabs = last.select("#tabs");
				
				if (tabs.length > 0)
				{
				
				}
				else
				{
					last.style.paddingBottom = (550 - center.getHeight() + extra) + "px";
				}
			}
		}
	}
}

jQuery(document).ready(function () {
    if(jQuery('#CaptivateContent').length > 0) {
        jQuery('#CaptivateContent').flash(
            {
                swf: 'http://media.completegenomics.com/video/Accessing_Solutions.swf',
                width: 802,
                height: 632,
                quality: 'high',
                name: 'Captivate',
                id: 'Captivate',
                wmode: 'transparent',
                bgcolor: '#f5f4f1',
                menu: 'false'
            }
        );
    }

    if(jQuery('#tweets').length > 0) {    
        jQuery("#tweets").tweet({
            username: "CompleteGenomic",          
            count: 3,
            join_text: "auto",
            auto_join_text_default: "",
            auto_join_text_ed: "",
            auto_join_text_ing: "",
            auto_join_text_reply: "",
            auto_join_text_url: "",            
            loading_text: "loading tweets..."
        });
    }
    
    jQuery('.watch-vid').fancybox({
        'width' : '760',
        'height' : '600'
    });  
});
