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
// $("#rem-button").on('click', navClicked);
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

// ----------------------------------------

function activeCollectionId() {
    return activeCollection().attr('id');
}

function isSystemCollectionId(cid) {
    return cid == "col-photos" || cid === "col-clipboard" || cid === "col-trash";
}

function allCollectionIds() {
    // collect all collection ids
    var colIds = [];
    $("#theCollections div.tab-pane").each(function (i, e) {
        var id1 = $(e).attr('id');
        colIds.push(id1);
    });
    console.log(colIds);
    return colIds;
}

// ----------------------------------------

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

    var io = isAlreadyOpen(colId);

    if ( io[0] ) {
        // nothing to do, collections is already there
        // but we need the id attr value
        o.colId = io[1];
        console.log("collection already open");
        console.log(o.colId);

        // switch to the collection
        // it's already loaded
        setActiveTab(o.colId);
    } else {
        // create the tab

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
        o.colId   = "col-" + cid.replace(/[^a-zA-Z0-9]/g,"-");
        o.colname = cname;
        console.log(o);

        // readonly collection ?
        var ro = colVal.metadata[0]["descr:Access"] === "readonly";
        var ct = colVal.metadata[0]["descr:Title"];

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
        var tt = "path: " + o.path;
        var tn = o.colname;
        if (ct) {
            tt = "title: " + ct + "\n" + tt;
        }
        if (ro) {
            tt = tt + "\naccess: readonly";
            tn = tn + " *";
        };
        lk.attr('href', '#' + o.colId)
            .attr('aria-controls', o.colId)
            .attr('title', tt)
            .empty()
            .append(tn);
        $('#collectionTab').append(tb);

        // make the collection visible
        setActiveTab(o.colId);

        // fill the colection
        insertEntries(o.colId, colVal.entries);
    }
}

// search collection for path
// return true/false and the id attr value

function isAlreadyOpen(path) {
    var b   = false;
    var pid = undefined;

    $('#theCollections > div[role=tabpanel]')
        .each(function (i, e) {
            var dp = $(e).attr('data-path');
            if (dp === path) {
                b = true;
                pid = $(e).attr('id');
            };
        });

    return [b, pid];
}

function setActiveTab(colId) {
    $('#collectionTab').find('a[aria-controls=' + colId + ']').trigger('click');
}

function insertEntries(colId, entries) {
    console.log('insertEntries');
    console.log(colId);
    console.log(entries);

    $('#' + colId).empty();
    entries.forEach(function (e, i) {
        insertEntry(colId, e, i);
    });
}

function insertEntry(colId, entry, i) {
    console.log('insertEntry');
    console.log(colId);
    console.log(entry);

    var e = newEntry(entry, i);
    $('#' + colId).append(e);
}

function newEntry(entry, i) {
    console.log("newEntry");
    console.log(entry);
    var p = $("#prototype-dia").children("div").clone();

    var en = (i + 1) + ': ';
    var sc = iconSize('');
    var mk = '';
    var tt = '';
    var ref = splitPath(entry.ref);

    if (entry.ColEntry === "IMG") {
        en = en + entry.part;
        sc = sc + ref.cpath + "/" + entry.part;
        mk = "imgmark";
        tt = "image: " + entry.ref;
    }
    if (entry.ColEntry === "COL") {
        en = en + '<a href="">' + ref.name + '</a>';
        // TODO: extend server to deliver an icon for this ref
        // sc = sc + ref.path + ".jpg";
        sc = sc + "/assets/icons/generated/brokenImage.jpg";
        mk = "colmark";
        tt = "collection: " + entry.ref;
    }

    // check whether src ref has extension .jpg
    var scref = splitPath(sc);
    if (scref.ext !== "jpg") {
        sc = sc + ".jpg";
    }
    // set img/col mark
    p.addClass(mk);

    // set the head line
    p.find("div.dia-name")
        .empty()
        .append(en);

    if ( entry.ColEntry === "COL") {
        // if entry is a collection add open collection event handler
        p.find("div.dia-name a")
            .on('click', function (e) {
                e.preventDefault();
                openCollection(ref.path);
            });
        // set the icon url
        // url is computed on server and added in callback
        getIconRef(ref.path,
                   function (src) {
                       p.find("img.dia-src")
                           .attr('src', iconSize(src));
                   });
    }

    if ( entry.ColEntry === "IMG" ) {
        // set the icon url
        p.find("img.dia-src")
            .attr('src', sc);
    }

    removeMarkCount(p);

    // set the icon title
    p.find("img.dia-src")
        .attr("title", tt);

    // add event handler for marking
    p.children("div.dia-img")
        .on('click', toggleSlideMark)
        .css('cursor','pointer');

    return p;
}

// ----------------------------------------
// string helper functions

// compute an object with path, name and cpath, and bname and ext
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

// add the icon size to an image href
function iconSize(p) {
    return "/pad-160x160" + p;
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

function getIconRef(path, insertSrcRef) {
    getObj("get-iconref", path, insertSrcRef);
}

// ----------------------------------------

// top level commands, most with ajax calls

function openCollection(path) {
    getCollection(path, showNewCollection);
}

function closeCollection(cid) {
    if ( isSystemCollectionId(cid) ) {
        alert("system collection can't be closed: " + cid);
    } else {
        var cids = allCollectionIds();
        var ix   = cids.indexOf(cid);
        var next = '';

        if ( ix < 0) {
            return;
        }
        if ( ix === 0 ) {
            next = cids[1];
        } else {
            next = cids[ix - 1];
        }
        setActiveTab(next);

        // remove the tab content
        $('#' + cid).remove();
        // remove the tab
        $('li > a[aria-controls=' + cid + ']').remove();
    }
}

// ----------------------------------------

// navbar button handlers

function closeActiveCollection() {
    var cid = activeCollectionId();
    closeCollection(cid);
}

// ----------------------------------------

$(document).ready(function () {
    openCollection("/archive/collections/photos");

    // event handler for navbar buttons
    $("#rem-button")
        .on('click', function (e) { closeActiveCollection(); });

});

// ----------------------------------------

var td = {"name" : "_uwe1234",
          "path" : "/archive/photos/_uwe1234",
          "src" : iconSize("/assets/icons/generated/photos.jpg")
         };

var td2 = {"name" : "trash",
           "path" : "/archive/collections/photos/trash",
           "src" : iconSize("/assets/icons/generated/brokenImage.jpg")
         };
