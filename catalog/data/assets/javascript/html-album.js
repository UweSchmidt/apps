/* Navigation in picture pages */

function trc (t, Text) {
  if ( t > 0 ) {
    alert(Text);
    /* nice try
       if (window.statusbar && window.statusbar.visible == true) {
       window.status = Text;
       window.defaultStatus = Text;
       } else {
       alert(Text);
    }
    */
  }
}

/* global state: will be threaded through URL search string */

var isPicture = true;
var slideShow = 0;
var speed     = 1000;

function initState() {
  var s = window.location.search;
  s = s.slice(1);
  var params = s.split("+");

  /* set default param values */
  slideShow = 0;

  for (var i = 0; i < params.length; ++i) {
    var kv = params[i].split("=");
    var k = kv[0];
    var v = kv[1];
    if ( k == "slideShow" ) { slideShow = parseInt(v); }
    if ( k == "speed" )     { speed = parseInt(v); }
  }
  trc(0, "init: " + "slideShow=" + slideShow);
}

function stateToString () {
  var res = "?";
  res += "slideShow=" + slideShow;
  res += "+speed=" + speed;
  return res;
}

var theNextPage = '';
var theNextPic  = new Image();

function changeToNextPage() {
  changePage(theNextPage);
}

function changePicPage(url,imgurl) {
  if (imgurl == '') {
    changePage(url);
  } else {
    /* load picture in advance and change to next page after picture is loaded */
    theNextPage = url;
    theNextPic.onload = changeToNextPage;
    theNextPic.src    = imgurl;
  }
}

function changePage(url) {
  trc(0, "changePage: " + "url=" + url);
  if (url != "") {
    window.location.href = url + stateToString();
  }
}

var slideShowTimer;

function advanceSlideShow() {
  trc(0, "advance SlideShow");
  nextPage();
}

function stopSlideShow () {
  if (typeof slideShowTimer != "undefined") {
    window.clearTimeout(slideShowTimer);
    trc(0, "timer cleared");
  }
  slideShow = 0;
  trc(0, "slideShow stopped");
}

function startSlideShow () {
  var d = 5000;
  if ( typeof duration == "number" ) {
    d = duration;
  }
  d = (d * speed) / 1000;
  slideShowTimer = window.setTimeout("advanceSlideShow();", d);
  slideShow = 1;
  trc(0, "SlideShow started with msec: " + d);
}

function startStopSlideShow() {
  duration = 1000;
  toggleSlideShow();
}

function toggleSlideShow() {
  if (slideShow > 0) {
    stopSlideShow();
  } else {
    startSlideShow();
  }
}

function initSpeedSlideShow() {
  speed = 1000;
}

function slowDownSlideShow() {
  speed = Math.round(speed * 1.2);
}

function speedUpSlideShow() {
  speed = Math.round(speed / 1.2);
  if ( speed < 100 ) { speed = 100; }
}

function initSlideShow () {
  if (slideShow > 0) {
    slideShow = 0;
  } else {
    slideShow = 1;
  }
  if ( isPicture ) {
    toggleSlideShow();
  }
}

var opacity         = "0.6";
var backgroundColor = "#666666";

var titleVisible = false;
var infoVisible  = false;

function toggleTitle() {
  if (titleVisible) {
    hideTitleElement();
  } else {
    showTitleElement();
  }
}

function showTitle() {
  /* showTitleElement(); /* disable mouse over effects */
}

function hideTitle() {
  /* hideTitleElement(); /* disable mouse over effects */
}

function showTitleElement() {
  if (! titleVisible) {
    stopSlideShow();
    hideInfoElement();
    document.getElementById("title-area-line").style.visibility      = "visible";
    document.getElementById("title-area-line").style.opacity         = opacity;
    document.getElementById("title-area-line").style.backgroundColor = backgroundColor;
    titleVisible = true;
  }
}

function hideTitleElement() {
  if (titleVisible) {
    document.getElementById("title-area-line").style.visibility      = "";
    document.getElementById("title-area-line").style.opacity         = "";
    document.getElementById("title-area-line").style.backgroundColor = "";
    titleVisible = false;
  }
}

function toggleInfo() {
  if (infoVisible) {
    hideInfoElement();
  } else {
    showInfoElement();
  }
}

function showInfo() {
  /* showInfoElement(); /* disable mouse over effects */
}

function hideInfo() {
  /* hideInfoElement(); /* disable mouse over effects */
}

function showInfoElement() {
  if (! infoVisible) {
    stopSlideShow();
    hideTitleElement();
    document.getElementById("info-area-content").style.visibility      = "visible";
    document.getElementById("info-area-content").style.opacity         = opacity;
    document.getElementById("info-area-content").style.backgroundColor = backgroundColor;
    infoVisible = true;
  }
}

function hideInfoElement() {
  if (infoVisible) {
    document.getElementById("info-area-content").style.visibility      = "";
    document.getElementById("info-area-content").style.opacity         = "";
    document.getElementById("info-area-content").style.backgroundColor = "";
    infoVisible = false;
  }
}

function keyPressed (KeyEvent) {
  if (! KeyEvent)
    KeyEvent = window.event;

  trc(0, "KeyCode=" + KeyEvent.keyCode + " which=" + KeyEvent.which);

  if ( KeyEvent.keyCode == 39 ||    /* right arrow */
       ( ( KeyEvent.keyCode == 0 || KeyEvent.keyCode == KeyEvent.which )
	 &&
         ( KeyEvent.which == 32 ||   /* space bar */
           KeyEvent.which == 62 ||   /* ">" */
	   KeyEvent.which == 110     /* n */
	 )
       )
     ) {
    trc(0, "weiter");
    stopSlideShow();
    nextPage();
    return false;
  }

  if ( KeyEvent.keyCode   == 37 ||    /* left arrow */
       KeyEvent.keyCode   == 8  ||    /* backspace */
       ( ( KeyEvent.keyCode == 0 || KeyEvent.keyCode == KeyEvent.which )
	 &&
         ( KeyEvent.which == 60 ||    /* or "<" */
	   KeyEvent.which == 112      /* or "p" */
	 )
       )
     ) {
    trc(0, "zurück");
    stopSlideShow();
    prevPage();
    return false;
  }

  if ( KeyEvent.keyCode   == 38 ||    /* up arrow */
       ( ( KeyEvent.keyCode == 0 || KeyEvent.keyCode == KeyEvent.which )
	 &&
         ( KeyEvent.which  == 94 ||     /* or "^" */
	   KeyEvent.which  == 117       /* or "u" */
	 )
       )
     ) {
    trc(0, "nach oben");
    stopSlideShow();
    parentPage();
    return false;
  }

  if ( KeyEvent.keyCode   == 40 ||    /* down arrow */
       ( ( KeyEvent.keyCode == 0 || KeyEvent.keyCode == KeyEvent.which )
	 &&
         ( KeyEvent.which == 118 ||  /* or "v" */
	   KeyEvent.which == 100     /* or "d" */
	 )
       )
     ) {
    trc(0, "nach unten");
    child1Page();
    return false;
  }

  if ( ( KeyEvent.keyCode == 0 || KeyEvent.keyCode == KeyEvent.which ) &&    /* "s" */
       KeyEvent.which   == 115
     ) {
    startStopSlideShow();
    return false;
  }

  if ( ( KeyEvent.keyCode == 0 || KeyEvent.keyCode == KeyEvent.which ) &&    /* "t" */
       KeyEvent.which   == 116
     ) {
    toggleTitle();
    return false;
  }

  if ( ( KeyEvent.keyCode == 0 || KeyEvent.keyCode == KeyEvent.which ) &&    /* "i" */
       KeyEvent.which   == 105
     ) {
    toggleInfo();
    return false;
  }

  if ( ( KeyEvent.keyCode == 0 || KeyEvent.keyCode == KeyEvent.which ) &&    /* "+" */
       KeyEvent.which   == 43
     ) {
    speedUpSlideShow();
    return false;
  }

  if ( ( KeyEvent.keyCode == 0 || KeyEvent.keyCode == KeyEvent.which ) &&    /* "-" */
       KeyEvent.which   == 45
     ) {
    slowDownSlideShow();
    return false;
  }

  if ( ( KeyEvent.keyCode == 0 || KeyEvent.keyCode == KeyEvent.which ) &&    /* "0" */
       KeyEvent.which   == 48
     ) {
    initSpeedSlideShow();
    return false;
  }

  return true;
}

function nextPage() {
  if ( nextp != '' ) {
    changePicPage(nextp,nextimg);
  } else {
    parentPage();
  }
}

function prevPage() {
  if ( prevp != '' ) {
    changePicPage(prevp,previmg);
  } else {
    parentPage();
  }
}

function parentPage() {
  if ( parentp != '' ) {
    changePage(parentp);
  }
}

function thisPage() {
  changePage(thisp);
}

function child1Page() {
  changePage(childp);
}

function childPage(ref) {
  changePage(ref);
}

function initPicture () {
  isPicture = true;
  initState();
  initSlideShow();
}

function initAlbum () {
  isPicture = false;
  initState();
}

document.onkeypress = keyPressed;
