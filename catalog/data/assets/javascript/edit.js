function navClicked(e) {
    e.preventDefault();
    console.log("nav button was clicked");
    console.log(e);
    console.log(e.target);
    /*
    alert("nav button was clicked: "
          + e.target + ", " + e.currentTarget
          + ", " + e.pageX + "x" + e.pageY
          + ", " + e.which + ", " + e.type
          + ", " + e.target.id + ", " + e.currentTarget.id
         );
     */
}

$("#opn-button").on('click', navClicked);
$("#new-button").on('click', navClicked);
$("#rem-button").on('click', navClicked);
$("#mfc-button").on('click', navClicked);
$("#mtc-button").on('click', navClicked);
$("#ctc-button").on('click', navClicked);
$("#mtt-button").on('click', navClicked);
$("#emp-button").on('click', navClicked);
$("#srt-button").on('click', navClicked);

$('#collectionTab a').click(function (e) {
    e.preventDefault();
    // alert("switch tab to " + e.currentTarget);
    $(this).tab('show');
});

function toggleSlideMark(e) {
    e.preventDefault();
    console.log("mark image was clicked");
    console.log(e);
    console.log(e.target);
    var dia = $(e.target).closest("div.dia");
    toggleMark(dia);
}

function toggleMark(dia) {
    console.log("toggleDiaMark: dia");
    console.log(dia);

    if ( dia.hasClass("unmarked") ) {
        // dia was unmarked, mark it, remove old curmark, and set curmark here
        dia.removeClass("unmarked").addClass("marked");
        clearCurMark(dia);
        markThisAsCur(dia);
        var mx = getMarkedNo(getMarked()).length;
        console.log(mx);
        setMarkCount(dia, '' + mx);
    } else {
        // was marked, remove mark, and move curmark to last marked
        dia.removeClass("marked").addClass("unmarked");
        clearCurMark(dia);
        markLastAsCur(dia);
        // TODO renumber mark counts
        setMarkCount(dia, '-2');
    }
    return dia;
}

function clearCurMark(dia) {
    var tabContent = $(dia).parent();
    console.log('clearCurMark');
    console.log(tabContent);
    var img = tabContent.find("img.curmarked");
    console.log(img);
    $(img).removeClass("curmarked");
    return dia;
}

function removeMarkCount(dia) {
    dia.find("div.dia-mark").empty();
    return dia;
}

var imgcnt = 0;

function setMarkCount(dia, m) {
    console.log('setMarkCount');
    console.log(dia);
    console.log(m);
    dia.find("div.dia-mark").empty().append(m);
}

function markThisAsCur(dia) {
    dia.find("img.dia-src").addClass("curmarked");
}

function markLastAsCur() {
    var col = getMarked();
    console.log(col);
    var nos = getMarkedNo(col);
    console.log(nos);
    var i = maxVal(nos);
    console.log("marklastascur: " + i);
    if ( i >= 0 ) {
        var sel = "div.dia-mark:contains(" + i + ")";
        console.log(sel);
        col.find(sel).closest("div.dia").find("img.dia-src").addClass("curmarked");
    }
}

function setColMark(dia) {
    if ( dia.hasClass('imgmark') )
        dia.removeClass('imgmark').addClass('colmark');
}

function setImgMark(dia) {
    if ( dia.hasClass('colmark') )
        dia.removeClass('colmark').addClass('imgmark');
}

function getMarked() {
    return activeCollection().children("div.marked");
}

function getMarkedNo(col) {
    return col
        .find("div.dia-mark")
        .contents()
        .toArray()
        .map(function (x) {return parseInt(x.textContent);});
}

function maxVal(a) {
    if ( a.length == 0)
        return -1;
    return Math.max(...a);
}

function newDia(dia) {
    console.log("newDia");
    console.log(dia);
    var p = $("#prototype-dia").children("div").clone();

    // set the head line
    p.find("div.dia-name")
        .empty()
        .append(dia.name);

    setMarkCount(p, '-1');

    // set the icon url
    p.find("img.dia-src")
        .attr('src', dia.src);

    // add event handler for marking
    p.children("div.dia-img")
        .on('click', toggleSlideMark)
        .css('cursor','pointer');

    return p;
}

function activeCollection() {
    return $("#theCollections").children("div.active");
}

function addDiaToActiveCollection(dia) {
    var actCol = activeCollection();
    var newSlide = newDia(dia);
    console.log("addDiatoactivecollection");
    console.log("actColId");
    actCol.append(newSlide);
}

var td = {"name" : "_uwe1234",
          "path" : "/archive/photos/_uwe1234",
          "src" : "/pad-160x160/assets/icons/generated/photos.jpg"
         };

var td2 = {"name" : "trash",
          "path" : "/archive/collections/photos/trash",
          "src" : "/pad-160x160/assets/icons/generated/brokenImage.jpg"
         };
