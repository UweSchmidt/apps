/* Navigation in picture pages */

function trc (t, Text) {
  if ( t > 0 ) {
      console.log(Text);
      //    alert(Text);
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

// ----------------------------------------

/* global state, these variables will be threaded through URL search string */

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

// ----------------------------------------

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

// ----------------------------------------

// variable take one of "pic-org", "pic-scaled", "pic-pano"
// pic-scaled indicates the image scaled down to display size is shown

var picstate = "pic-scaled";

function toggleOriginalPicture() {
    // no ref to full size picture
    if (orgimg === "") {
        return;
    }

    // turn off full size view
    if (picstate === "pic-org") {
        switchToScaledPicture();
        return;
    }

    // turn off other views (pano)
    if (picstate !== "pic-scaled") {
        switchToScaledPicture();
    }

    // load full size and switch to full size view
    var dv  = $('#pic-org');
    var img = $('#pic-org img');
    if (! img.attr('src') ) {
        // load image on demand
        img.attr('src', orgimg);
        console.log(img.attr('src'));
        img.load(function(){
            // image ready: make it visible
            toggleOriginalPicture();
        });
    } else {
        // make image visible
        console.log( "make org picture visible");
        hideDiv('#' + picstate);
        showDiv('#pic-org');
        picstate="pic-org";
    }
}

function cssKeyFrames(leftPos) {
    $("<style>")
        .prop("type", "text/css")
        .html("@keyframes moveToLeft {\
               from {left: 0px);}\
               to   {left: " + leftPos + "px;}\
               }"
             )
        .appendTo("head");
}

function togglePanoAnimation() {
    // no ref to panorama picture
    if (panoimg === ""
        ||
        picstate !== "pic-pano"
       ) {
        return;
    }
    var img = $('#pic-pano img');
    var as  = img.css('animation-play-state');
    console.log("animation-play-state=" + as);
    if (as === 'running') {
        as = 'paused';
    } else {
        as = 'running';
    }
    img.css('animation-play-state', as);
}

function togglePanoramaPicture() {
    // no ref to panorama picture
    if (panoimg === "") {
        return;
    }

    // turn off panorama view
    if (picstate === "pic-pano") {
        switchToScaledPicture();
        return;
    }

    // turn off other views
    if (picstate !== "pic-scaled") {
        switchToScaledPicture();
    }

    // load pano and switch to pano view
    var dv  = $('#pic-pano');
    var img = $('#pic-pano img');
    if (! img.attr('src') ) {
        // load image on demand
        img.attr('src', panoimg);
        console.log(img.attr('src'));
        img.load(function(){
            // add @keyframes for animation
            var pw = img[0].naturalWidth;
            var ph = img[0].naturalHeight;
            var du = 8.0 * (pw / ph);
            var sw = dv.width();
            console.log("PanoGeo=" + pw + "x" + ph);
            console.log("DisplayWidth=" + sw);
            console.log("AnimDuration=" + du);
            cssKeyFrames(sw - pw);
            // set anim duration relative to aspect ratio
            img.css('animation-duration', "" + du + "s");
            // image ready: make it visible
            togglePanoramaPicture();
        });
    } else {
        // make image visible
        console.log("make pano picture visible");
        hideDiv('#' + picstate);
        showDiv('#pic-pano');
        picstate="pic-pano";
        console.log("start animation");
        img.css('animation-play-state', 'running');
    }
}
function hideDiv(s) {
    $(s).css('display', 'none');
}

function showDiv(s) {
    $(s).css('display', 'block');
}

function switchToScaledPicture() {
    if (picstate === "pic-scaled") {return;}
    hideDiv('#' + picstate);
    showDiv('#pic-scaled');
    picstate="pic-scaled";
}

// ----------------------------------------

var slideShowTimer;

function advanceSlideShow() {
  trc(1, "advance SlideShow");
  nextPage();
}

function stopSlideShow () {
  if (typeof slideShowTimer != "undefined") {
    window.clearTimeout(slideShowTimer);
    trc(1, "timer cleared");
  }
  slideShow = 0;
  trc(1, "slideShow stopped");
}

function startSlideShow () {
    var d = 5000;
    if ( typeof duration == "number" ) {
        d = duration;
    }
    d = (d * speed) / 1000;
    slideShowTimer = window.setTimeout(advanceSlideShow, d);
    slideShow = 1;
    trc(1, "SlideShow started with msec: " + d);
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
        $('#title-area-line')
            .css('visibility',      "visible")
            .css('opacity',         opacity)
            .css('backgroundColor', backgroundColor);

        titleVisible = true;
    }
}

function hideTitleElement() {
    if (titleVisible) {
        $('#title-area-line')
            .css('visibility',      '')
            .css('opacity',         '')
            .css('backgroundColor', '');

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
        $('#info-area-content')
            .css('visibility',      "visible")
            .css('opacity',         opacity)
            .css('backgroundColor', backgroundColor);

        infoVisible = true;
    }
}

function hideInfoElement() {
    if (infoVisible) {
        $('#info-area-content')
            .css('visibility',      '')
            .css('opacity',         '')
            .css('backgroundColor', '');

        infoVisible = false;
    }
}

function isKey(e, c, s) {
    if ((e.keyCode == 0
         ||
         e.keyCode == e.which
        )
        &&
        e.which == c
       ) { return true;}
    return false;
}

function keyPressed (e) {
    if (! e)
        e = window.event;

    console.log("KeyCode=" + e.keyCode + " which=" + e.which);

    if ( e.keyCode == 39    /* right arrow */
         ||
         isKey(e, 32, " ")
         ||
         isKey(e, 62, ">")
         ||
         isKey(e, 110, "n")
       ) {
        stopSlideShow();
        nextPage();
        return false;
    }

    if ( e.keyCode   == 37    /* left arrow */
         ||
         e.keyCode   == 8
         ||    /* backspace */
         isKey(e, 60, "<")
         ||
         isKey(e, 112, "p")
       ) {
        stopSlideShow();
        prevPage();
        return false;
    }

    if ( e.keyCode   == 38    /* up arrow */
         ||
         isKey(e, 94, "^")
         ||
         isKey(e, 117, "u")
       ) {
        stopSlideShow();
        parentPage();
        return false;
    }

    if ( e.keyCode   == 40     /* down arrow */
         ||
         isKey(e, 118, "v")
         ||
         isKey(e, 100, "d")
       ) {
        child1Page();
        return false;
    }

    if ( isKey(e, 115, "s") ) {
        startStopSlideShow();
        return false;
    }

    if ( isKey(e, 116, "t") ) {
        toggleTitle();
        return false;
    }

    if ( isKey(e, 105, "i") ) {
        toggleInfo();
        return false;
    }

    if ( isKey(e, 43, "+") ) {
        speedUpSlideShow();
        return false;
    }

    if ( isKey(e, 45, "-") ) {
        slowDownSlideShow();
        return false;
    }

    if ( isKey(e, 48, "0") ) {
        initSpeedSlideShow();
        return false;
    }

    if ( isKey(e, 102, "f") ) {
        toggleOriginalPicture();
        return false;
    }

    if ( isKey(e, 97, "a") ) {
        togglePanoramaPicture();
        return false;
    }

    if ( isKey(e, 113, "q") ) {
        togglePanoAnimation();
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
