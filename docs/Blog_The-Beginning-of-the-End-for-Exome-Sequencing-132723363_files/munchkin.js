/*
 * Copyright (c) 2010, Marketo, Inc. All rights reserved.
 * Marketo marketing automation web activity tracking script
 *
 * $Date: 2010-03-01 11:00:00 -0800 (Mon, 01 Mar 2010) $
 * $Rev: 14670 $
 */
var $marketo=jQuery.noConflict();function mktoMunchkin(id,options){if(typeof gMunchkin=='undefined'){var munch=new mktMunchkin();munch.capable=false;munch.inited=false;munch.id=id;munch.cookie=null;munch.customName=null;munch.override=null;munch.oldToken=null;munch.wsInfo=null;munch.debug=false;munch.debugSeq=1;munch.version='111';var loc=document.location;gMunchkin=munch;if(!mktCookie.enabled()){return;}
if(munch.id==null||munch.id.length==0){return;}
munch.notifyPrefix=((loc.protocol=='https:')?'https':'http')+'://'+munch.id+'.mktoresp.com/';if(mktMunchkin.isDefined(options)){if(mktMunchkin.isDefined(options.customName)){munch.customName=options.customName;}
if(mktMunchkin.isDefined(options.notifyPrefix)){munch.notifyPrefix=options.notifyPrefix;}
if(mktMunchkin.isDefined(options.wsInfo)){munch.wsInfo=options.wsInfo;}}
var lpview='';var qp=window.location.search;if(qp!=null&&qp.length>0){var params=qp.substr(1).split('&');for(var i=0;i<params.length;i++){var nv=params[i].split('=');switch(nv[0]){case'mkt_tok':munch.override=decodeURIComponent(nv[1]);break;case'lpview':lpview=nv[1];break;case'mkt_dbg':munch.debug=nv[1]==1;break;default:break;}}}
if(lpview=='preview'&&/\/lpeditor\/editor$/.test(loc.pathname)){return;}
var domain=mktMunchkin.getDomain(loc.hostname);munch.cookie=new mktCookie('_mkto_trk');if(munch.cookie.id!=null&&munch.cookie.id!=munch.id){munch.cookie.token=null;}
munch.cookie.id=munch.id;if(munch.cookie.token==null){munch.cookie.token=mktMunchkin.generateToken(domain);}
else if(!/^_mch-/.test(munch.cookie.token)){if(munch.debug){alert('Found old style tracking token: '+munch.cookie.token);}
munch.oldToken=munch.cookie.token;munch.cookie.token=mktMunchkin.generateToken(domain);}
munch.cookie.store(730,'/',domain,false);munch.capable=true;}}
function mktoMunchkinDebug(message,scriptId){var params={};params._mchId=gMunchkin.cookie.id;params._mchTk=gMunchkin.cookie.token;if(gMunchkin.override!=null){params.mkt_tok=gMunchkin.override;}
params._mchMsg=gMunchkin.debugSeq++;if(message!=null){params._mchMsg+=(' - '+message);}
params._mchUa=navigator.userAgent;if(scriptId!=null){var scriptSrc=$marketo('#'+scriptId);if(scriptSrc.length>0){params._mchScr=scriptSrc.html();}}
params._mchVr=gMunchkin.version;var ajaxUrl=gMunchkin.notifyPrefix+'webevents/debug';ajaxUrl+='?_mchNc='+new Date().getTime()+'&'+$marketo.param(params);var done=false;var img=new Image(1,1);img.onload=function(){done=true;}
img.src=ajaxUrl;}
function mktoMunchkinFunction(fn,attrs,key){var params={};if(key!=null){params._mchKy=key;}
switch(fn){case'associateLead':for(var attr in attrs){params['_mchAt'+attr]=attrs[attr];}
gMunchkin.post('webevents/associateLead',params);break;case'clickLink':if(attrs.href!=null){params._mchHr=params._mchLr=decodeURIComponent(attrs.href);}
gMunchkin.post('webevents/clickLink',params,{_mchCn:(gMunchkin.customName!=null?gMunchkin.customName:'')},0);break;case'visitWebPage':if(attrs.url!=null){params._mchRu=decodeURIComponent(attrs.url);}
if(attrs.params!=null){params._mchQp=decodeURIComponent(attrs.params.replace(/&/g,'__-__'));}
if(attrs.name!=null){params._mchCn=attrs.name;}
gMunchkin.post('webevents/visitWebPage',params,{_mchRe:decodeURIComponent(document.location.href)},0);break;default:break;}}
$marketo(function(){gMunchkin.init();});function mktMunchkin(){}
mktMunchkin.prototype.init=function(){if(!this.capable){this.updateMktoForm('');return;}
else if(this.inited){return;}
this.inited=true;this.updateMktoForm('id:'+this.cookie.id+'&token:'+this.cookie.token);this.instrumentLinks();var loc=document.location;this.post('webevents/visitWebPage',{_mchCn:(this.customName!=null?this.customName:'')},{_mchHa:decodeURIComponent(loc.hash),_mchRe:decodeURIComponent(document.referrer),_mchQp:decodeURIComponent(loc.search.substr(1).replace(/&/g,'__-__'))},0);}
mktMunchkin.prototype.post=function(url,params,extraParams,timeout){var loc=document.location;params._mchId=this.cookie.id;params._mchTk=this.cookie.token;if(this.override!=null){params.mkt_tok=this.override;}
else if(this.oldToken!=null){params.mkt_tok=this.oldToken;}
if(this.wsInfo!=null){params._mchWs=this.wsInfo;}
params._mchHo=loc.hostname;params._mchPo=loc.port;if(params._mchHo=='www.jigsaw.com'){return;}
if(params._mchRu==null){params._mchRu=decodeURIComponent(loc.pathname);}
params._mchPc=loc.protocol;if(extraParams!=null){for(var extra in extraParams){params[extra]=extraParams[extra];}}
params._mchVr=this.version;if(params._mchHo==null||params._mchHo.length==0||params._mchPc=='file:'){if(this.debug){alert('Ignoring munchkin post: '+url);}
return;}
var ajaxUrl=this.notifyPrefix+url;ajaxUrl+='?_mchNc='+new Date().getTime()+'&'+$marketo.param(params);if(this.debug){alert('Munchkin post: '+ajaxUrl);}
var done=false;var img=new Image(1,1);img.onload=function(){done=true;}
img.src=ajaxUrl;if(timeout>0){var start=new Date().getTime();while(!done){var now=new Date().getTime();if(now-start>timeout){break;}}}}
mktMunchkin.prototype.instrumentLinks=function(){$marketo("a[href]:not([href^=#]),area[href]:not([href^=#])").live('mouseup',function(evt){if(this.href!=null&&this.href.length>0&&!/^mailto:/.test(this.href)){gMunchkin.post('webevents/clickLink',{_mchCn:(gMunchkin.customName!=null?gMunchkin.customName:''),_mchHr:decodeURIComponent(this.href)},null,100);}
else{if(gMunchkin.debug){alert('Ignoring click link for link: '+this.id);}}
return true;});}
mktMunchkin.prototype.updateMktoForm=function(value){$marketo("input[type=hidden][name=_mkt_trk]").attr('value',value);}
mktMunchkin.getDomain=function(hostname){var normTldRE=/([^.]+\.[^.]{3,})$/i;var found=normTldRE.exec(hostname);if(found!=null){return found[1];}
else{var countryTldRE=/([^.]+\.[^.]+\.[^.]{2})$/i;found=countryTldRE.exec(hostname);if(found!=null){return found[1];}
else{return hostname;}}}
mktMunchkin.generateToken=function(domain){return'_mch-'+domain+'-'+new Date().getTime()+'-'+mktMunchkin.rand(10000,99999);}
mktMunchkin.isUndefined=function(a){return typeof a=='undefined';}
mktMunchkin.isDefined=function(a){return!mktMunchkin.isUndefined(a);}
mktMunchkin.rand=function(min,max){if(max){return Math.floor(Math.random()*(max-min+1))+min;}else{return Math.floor(Math.random()*(min+1));}}
function mktCookie(cname){this.$name=cname;var allcookies=document.cookie;if(allcookies=="")return;var cookies=allcookies.split(';');var cookie=null;for(var i=0;i<cookies.length;i++){var curCookie=cookies[i].replace(/^\s+/,'');if(curCookie.substring(0,cname.length+1)==(cname+"=")){cookie=curCookie;break;}}
if(cookie==null)return;var cookieval=cookie.substring(cname.length+1);var a=cookieval.split('&');for(var i=0;i<a.length;i++)
a[i]=a[i].split(':');for(var i=0;i<a.length;i++){this[a[i][0]]=decodeURIComponent(a[i][1]);}}
mktCookie.prototype.store=function(daysToLive,path,domain,secure){var cookieval="";for(var prop in this){if((prop.charAt(0)=='$')||((typeof this[prop])=='function'))
continue;if(cookieval!="")cookieval+='&';var propval=encodeURIComponent(this[prop]);cookieval+=prop+':'+propval;}
var cookie=this.$name+'='+cookieval;if(daysToLive>0){var dt=new Date();dt.setTime(dt.getTime()+daysToLive*24*60*60*1000);cookie+="; expires="+dt.toGMTString();}
if(path)cookie+="; path="+path;if(domain&&domain.indexOf('.')!=-1)cookie+="; domain="+domain;if(secure)cookie+="; secure";document.cookie=cookie;}
mktCookie.enabled=function(){if(navigator.cookieEnabled!=undefined)return navigator.cookieEnabled;if(Cookie.enabled.cache!=undefined)return Cookie.enabled.cache;document.cookie="testcookie=test; max-age=10000";var cookies=document.cookie;if(cookies.indexOf("testcookie=test")==-1){return Cookie.enabled.cache=false;}
else{document.cookie="testcookie=test; max-age=0";return Cookie.enabled.cache=true;}}
