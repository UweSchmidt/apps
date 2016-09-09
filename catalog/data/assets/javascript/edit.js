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
        // dia was unmarked, mark it, and existing curmark
        dia.removeClass("unmarked");
        clearCurMark(dia);

        var mx = getMarkedNo(getMarked()).length + 1;
        // console.log(mx);
        setMarkCount(dia, '' + mx);

        // set mark and curmark
        markThisAsCur(dia);
        dia.addClass("marked");
    } else {
        // dia was marked, so unmark it
        // save the mark no
        var markNo = dia.find("div.dia-mark")
                .contents()
                .get(0).textContent;
        // console.log("unmark dia " + markNo);

        // was marked, remove mark, and move curmark to last marked
        dia.removeClass("marked").addClass("unmarked");
        clearCurMark(dia);
        markLastAsCur(dia);

        // renumber mark counts
        removeMarkCount(dia);
        renumberMarkCount(parseInt(markNo));
    }
    return dia;
}

function clearCurMark(dia) {
    var tabContent = $(dia).parent();
    // console.log('clearCurMark');
    // console.log(tabContent);
    var img = tabContent.find("img.curmarked");
    // console.log(img);
    $(img).removeClass("curmarked");
    return dia;
}

function removeMarkCount(dia) {
    dia.find("div.dia-mark").empty();
    return dia;
}

function renumberMarkCount(pos) {
    // console.log('renumberMarkCount ' + pos);
    var col = getMarked();
    var nos = getMarkedNo(col);
    col.each(function (i, e) {
        // console.log("renumber");
        // console.log(i);
        // console.log(e);
        var ecol = $(e);
        var mno = ecol.find("div.dia-mark").contents().get(0).textContent;
        var no  = parseInt(mno);
        // console.log(no);
        if (no > pos) {
            setMarkCount(ecol, '' + (no - 1));
        }
    });
}

var imgcnt = 0;

function setMarkCount(dia, m) {
    // console.log('setMarkCount');
    // console.log(dia);
    // console.log(m);
    dia.find("div.dia-mark").empty().append(m);
}

function markThisAsCur(dia) {
    dia.find("img.dia-src").addClass("curmarked");
}

function markLastAsCur() {
    var col = getMarked();
    // console.log(col);
    var nos = getMarkedNo(col);
    // console.log(nos);
    var i = maxVal(nos);
    // console.log("marklastascur: " + i);
    if ( i >= 0 ) {
        var sel = "div.dia-mark:contains(" + i + ")";
        // console.log(sel);
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

    removeMarkCount(p);

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

function showNewCollection(colId, colVal) {

    // compute colId and colName from colId
    var o = splitPath(colId);
    console.log(o);
    console.log(colVal);
    // collect the tab-pane ids
    var colIds = [];
    $("div.tab-pane").each(function (i, e) {
        var id1 = $(e).attr('id');
        colIds.push(id1);
    });
    console.log(colIds);

    var cid   = o.name;
    var cname = o.name;
    var cnt = 1;
    while (colIds.indexOf(cid) >= 0) {
        cid = o.name + '-' + ++cnt;
        cname = o.name + ' (' + cnt + ')';
    }
    // for html ids replace all none alphanum chars by "-"
    o.colId   = "id-" + cid.replace(/[^a-zA-Z0-9]/g,"-");
    o.colname = cname;
    console.log(o);

    // readonly collection ?
    var ro = colVal.metadata[0]["descr:Access"] === "readonly";

    // add the tab panel
    var t = $('#prototype-tabpanel').children("div").clone();
    t.find('div.tab-panel').empty();
    t.attr('id', o.colId)
        .attr('data-path', o.path);
    if (ro) {
        t.addClass("readonly");
    }
    $('#theCollections').append(t);

    // add the tab
    var tb = $('#prototype-tab').find("li").clone();
    var lk = tb.find('a');
    var tt = o.path;
    var tn = o.colname;
    if (ro) {
        tt = tt + " (readonly)";
        tn = tn + " *";
    };
    lk.attr('href', '#' + o.colId)
        .attr('aria-controls', o.colId)
        .attr('title', tt)
        .empty()
        .append(tn);
    $('#collectionTab').append(tb);

    // fill the colection
    fillCollection(o.colId, colVal.entries);
}

function fillCollection(colId, entries) {
    console.log('fillCollection: TODO');
}

// ----------------------------------------
//
// string helper functions
// ----------------------------------------
//
// computes an object with path, name and cpath, and bname and ext

function splitPath(p) {
    if (typeof p === 'string') {
        return splitPath({'path' : p});
    }

    var a = p.path.split("/");
    p.name  = a.pop();
    p.cpath = a.join("/");

    var e = p.name.split(".");
    if (e.length < 2) {
        p.ext   = "";
        p.bname = p.name;
    } else {
        p.ext   = e.pop();
        p.bname = e.join(".");
    }
    return p;
}

// ----------------------------------------

// ajax calls

function getObj(fct, path, ok) {
    $.getJSON("/" + fct + path + ".json",
              function (res) {
                  lres = res;
                  if (res.err) {
                      alert(res.err);
                  } else {
                      ok(res);
                  }
              })
        .fail(function (err){
            alert(err.resposeText);
            return {};
        });
}

function getCollection(path, showCol) {
    getObj("get-obj", path,
           function (col) {
               if (col.ImgNode !== "COL") {
                   alert("got something, but not a collection");
                   return;
               }
               showCol(path, col);
    });
}

// ----------------------------------------

// commands with ajax calls

function openCollection(path) {
    getCollection(path, showNewCollection);
}

// ----------------------------------------

var td = {"name" : "_uwe1234",
          "path" : "/archive/photos/_uwe1234",
          "src" : "/pad-160x160/assets/icons/generated/photos.jpg"
         };

var td2 = {"name" : "trash",
          "path" : "/archive/collections/photos/trash",
          "src" : "/pad-160x160/assets/icons/generated/brokenImage.jpg"
         };
