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

// $("#opn-button").on('click', navClicked);
$("#new-button").on('click', navClicked);
// $("#mark-button").on('click', navClicked);
// $("#unmark-button").on('click', navClicked);
// $("#rem-button").on('click', navClicked);
$("#mfc-button").on('click', navClicked);
$("#mtc-button").on('click', navClicked);
// $("#ctc-button").on('click', navClicked);
$("#mtt-button").on('click', navClicked);
$("#emp-button").on('click', navClicked);
// $("#srt-button").on('click', navClicked);

$('#collectionTab a').click(function (e) {
    e.preventDefault();
    $(this).tab('show');
});

// ----------------------------------------
//
// mark/unmark entries
// toggleSlideMark is the event handler

function toggleSlideMark(e) {
    e.preventDefault();
    statusClear();
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
        var mno = ecol.find("div.dia-mark")
                .contents()
                .get(0).textContent;
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

function setMark(dia, mark) {
    var mark2 = 'colmark';
    if ( mark === mark2 ) {
        mark2 = 'imgmark';
    }
    dia.removeClass(mark2)
        .addClass(mark);
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

// end marking functions
//
// ----------------------------------------

function maxVal(a) {
    if ( a.length == 0)
        return -1;
    return Math.max(...a);
}

// ----------------------------------------
//
// collection ids and paths

function activeCollection() {
    return $("#theCollections").children("div.active");
}

function activeCollectionId() {
    return activeCollection().attr('id');
}

function isSystemCollectionId(cid) {
    return cid === idCollections()
        || cid === idClipboard()
        || cid === idTrash();
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

function collectionPath(cid) {
    return $('#' + cid).attr('data-path');
}

function allCollectionPaths() {
    // collect all collection paths (ObjId's on server)
    var colPaths = [];
    $("#theCollections div.tab-pane").each(function (i, e) {
        var path1 = $(e).attr('data-path');
        colPaths.push(path1);
    });
    console.log(colPaths);
    return colPaths;
}

function isReadOnlyCollection(colVal) {
    var acc = colVal.metadata[0]["descr:Access"];
    return acc && acc.search('no-write') >= 0;
}

function isSystemCollection(colVal) {
    var acc = colVal.metadata[0]["descr:Access"];
    return acc && acc.search('no-delete') >= 0;
}

function isNotSortableCollection(colVal) {
    var acc = colVal.metadata[0]["descr:Access"];
    return acc && acc.search('no-sort') >= 0;
}

// ----------------------------------------
//
// fill a collection tab

function addDiaToActiveCollection(dia) {
    var actCol = activeCollection();
    var newSlide = newDia(dia);
    console.log("addDiatoactivecollection");
    console.log("actColId");
    actCol.append(newSlide);
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

function showNewCollection(path, colVal) {

    // compute colId and colName from path
    var o = splitPath(path);
    console.log(o);
    console.log(colVal);

    var io = isAlreadyOpen(path);

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
        $("div.tab-pane")
            .each(function (i, e) {
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
        var ro = isReadOnlyCollection(colVal);
        var sr = isNotSortableCollection(colVal);
        var sy = isSystemCollection(colVal);
        var ct = colVal.metadata[0]["descr:Title"];

        // add the tab panel
        var t = $('#prototype-tabpanel').children("div").clone();
        t.find('div.tab-panel')
            .empty();
        t.attr('id', o.colId)
            .attr('data-path', o.path);
        if ( ro ) {
            t.addClass("readonly");
        }
        if ( sr ) {
            t.addClass("nosort");
        }
        if ( sy ) {
            t.addClass("nodelete");
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

    var en = '<span class="img-no">' + (i + 1) + '</span>: ';
    var sc = iconSize('');
    var mk = '';
    var tt = '';
    var ref = splitPath(entry.ref);

    if (entry.ColEntry === "IMG") {
        en = en + '<span class="img-part">' + entry.part + '</span>';
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
    setMark(p, mk);

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
        getIconRefFromServer(ref.path,
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

function refreshCollection(path, colVal) {
    var o = splitPath(path);
    console.log('refreshCollection');
    console.log(o);
    console.log(colVal);

    var io = isAlreadyOpen(path);
    // check whether collection is already there
    if ( io[0]) {
        o.colId = io[1];
        insertEntries(o.colId, colVal.entries);
    }
}

function refreshCollectionAndClipboard(path, colVal) {
    refreshCollection(path, colVal);
    getColFromServer(pathClipboard(), refreshCollection);
}

// ----------------------------------------

function statusMsg(msg) {
    $('#status-message')
        .removeClass('error-msg')
        .addClass('status-msg')
        .empty()
        .append(msg);
}

function statusError(msg) {
    $('#status-message')
        .removeClass('status-msg')
        .addClass('error-msg')
        .empty()
        .append(msg);
}

function statusClear() {
    statusMsg('');
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

// top level commands, most with ajax calls

function openCollection(path) {
    statusClear();
    getColFromServer(path, showNewCollection);
}

function updateCollection(path) {
    getColFromServer(path, refreshCollection);
}

function closeCollection(cid) {
    if ( isSystemCollectionId(cid) ) {
        statusError("system collection can't be closed: " + cid);
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

function markAll(cid) {
    statusClear();
    console.log('mark all images in ' + cid);

    $('#' + cid + ' > div.unmarked')
        .each(function (i, e) {
            toggleMark($(e));
    });
}

function unmarkAll(cid) {
    statusClear();
    console.log('unmark all images in ' + cid);

    var col = $('#' + cid);

    // remove all marked classes
    var ens = col.find('div.marked')
            .removeClass('marked')
            .addClass('unmarked');

    // remove all mark counts
    ens.find('div.dia-mark')
        .empty();

    // remove mark as current
    ens.find('img.curmarked')
        .removeClass('curmarked');
}

function getMarkedEntries(cid) {
    console.log('getMarkedEntries');
    var ixs = [];
    $('#' + cid + ' > div.dia > div.dia-top > div.dia-mark')
        .each(function(i, e) {
            // get the mark cnt
            var v = $(e).contents().get(0);
            var c = -1;
            if ( v ) {
                c = parseInt(v.textContent);
            }
            // console.log(v);
            // console.log(c);
            ixs.push(c);

        });
    console.log(ixs);
    return ixs;
}

function copyMarkedToClipboard(cid) {
    statusClear();
    var ixs = getMarkedEntries(cid);
    var cpath = collectionPath(cid);
    var dpath = pathClipboard();
    console.log('copyMarkedToClipboard');
    console.log(ixs);
    console.log(cpath);
    console.log(dpath);

    // do the work on server and refresh both collections
    copyToColOnServer(cpath, [ixs, dpath], refreshCollectionAndClipboard);
}

function sortCollection(cid) {
    statusClear();
    console.log('sort collection: ' + cid);
    var sr = $('#' + cid).hasClass('nosort');
    console.log(sr);
    if ( sr ) {
        statusError('collection not sortable: ' + cid);
        return;
    }
    var ixs  = getMarkedEntries(cid);
    var path = collectionPath(cid);
    console.log(path);

    sortColOnServer(path, ixs);
}

function setCollectionImg(cid) {
    statusClear();
    console.log("setCollectionImg: " + cid);
    var col  = $('#' + cid);
    var path = col.attr('data-path');
    var o    = splitPath(path);
    var img  = col.find(" img.curmarked")
            .closest('div.dia')
            .get(0);

    if ( col.hasClass('readonly')) {
        statusError('warning: collection is readonly: ' + cid);
        // return; // TODO: testing: remove this to make it an error
    }

    if ( img ) {
        console.log(img);
        if ( $(img).hasClass('imgmark') ) {
            var part = $(img)
                    .find('span.img-part')
                    .contents()
                    .get(0).textContent;
            if ( part.search(/[.]jpg$/) >= 0 ) {
                // it's a jpg image
                // so take this image as collection image
                var pos = $(img)
                        .find('span.img-no')
                        .contents()
                        .get(0)
                        .textContent;
                pos = parseInt(pos) -1;
                console.log('setCollectionImg:');
                console.log(pos);
                console.log(path);
                console.log(o);
                modifyServer('colimg', path, pos,
                             function () {
                                 var ppath = o.cpath;
                                 var pcol = isAlreadyOpen(ppath);
                                 if ( pcol[0] ) {
                                     // parent collection open
                                     // refresh the parent collection
                                     // to show the new collection image
                                     getColFromServer(ppath, refreshCollection);
                                 }
                             });
            } else {
                statusError('not a .jpg image');
            }
        } else {
            statusError('marked entry is a collection, not an image');
        }
        // unmark last marked entry
        toggleMark($(img));
    } else {
        statusError('no marked entry found');
    }
}

function createCollection() {
    statusClear();
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);
    var name  = $('#newCollectionName').val();
    name = name.replace(/[^-_.a-zA-Z0-9]/g,"");
    var cnames = $('#' + cid + ' div.dia.colmark div.dia-name a')
            .contents()
            .get()
            .map(function (x) {return x.textContent;});
    var ix = cnames.indexOf(name);

    console.log('createCollection');
    console.log(cpath);
    console.log(name);
    console.log(cnames);

    if ( name  && name.length > 0) {
        if ( ix >= 0 ) {
            statusError("collection name already exists: " + name);
            return;
        }
        createColOnServer(cpath, name, refreshCollection);
    }
}

// ----------------------------------------

// ajax calls

function copyToColOnServer(path, args, showCol) {
    modifyServer("copyToCollection", path, args,
                 function () {
                     getColFromServer(path, refreshCollection);
                 });
}

function sortColOnServer(path, args) {
    modifyServer("sort", path, args,
                 function () {
                     getColFromServer(path, refreshCollection);
                 });
}

function getColFromServer(path, showCol) {
    readServer("collection", path,
           function (col) {
               showCol(path, col);
           });
}

function getIconRefFromServer(path, insertSrcRef) {
    readServer('iconref', path, insertSrcRef);
}

function createColOnServer(path, name, showCol) {
    modifyServer("newcol", path, name,
                 function () {
                     getColFromServer(path, refreshCollection);
                 });
}

// ----------------------------------------

// http communication

function callServer(getOrModify, fct, args, processRes, processNext) {
    var rpc = [fct, args];
    console.log('callServer: ' + getOrModify);
    console.log(rpc);

    $.ajax({
        type: "POST",
        url: "/" + getOrModify + '.json',
        data: JSON.stringify(rpc),
        dataType: 'json'
    }).done(function (res) {
        if (res.err) {
            statusError(res.err);
        } else {
            processRes(res);
        }
    }).fail(function (err){
        statusError(err.resposeText);
    }).always(processNext);
}

// make a query call to server

function readServer(fct, path, processRes) {
    callServer("get", fct, [path, []], processRes, noop);
}

// make a modifying call to server
// modifying calls never get an interesting result back
// in success case (), else the error message
// all modifying ops are procedures, not functions

function modifyServer(fct, path, args, processNext) {
    callServer("modify", fct, [path, args], ignoreRes, processNext);
}

function ignoreRes(res) {}

function noop() {}


// ----------------------------------------
//
// the "main" program
// set the event handlers

$(document).ready(function () {
    openCollection("/archive/collections");

    // event handler for navbar buttons
    $("#rem-button")
        .on('click', function (e) {
            closeCollection(activeCollectionId());
        });

    $("#mark-button")
        .on('click', function (e) {
            markAll(activeCollectionId());
        });

    $("#unmark-button")
        .on('click', function (e) {
            unmarkAll(activeCollectionId());
        });

    $("#srt-button")
        .on('click', function (e) {
            sortCollection(activeCollectionId());
    });

    $("#ctc-button")
        .on('click', function (e) {
            copyMarkedToClipboard(activeCollectionId());
        });

    $("#colimg-button")
        .on('click', function (e) {
            setCollectionImg(activeCollectionId());
        });

    $('#newCollectionOK')
        .on('click', function (e) {
            console.log("newCollectionOK clicked");
            $('#newCollectionModal').modal('hide');
            createCollection();
        });
});

// ----------------------------------------
//
// "constants"

function pathCollections() { return "/archive/collections"; }
function pathClipboard()   { return "/archive/collections/clipboard"; }
function pathTrash()       { return "/archive/collections/trash"; }

function idCollections() { return 'col-collections'; }
function idClipboard()   { return 'col-clipboard'; }
function idTrash()       { return 'col-trash'; }


// ----------------------------------------

// test test test

var td = {"name" : "_uwe1234",
          "path" : "/archive/photos/_uwe1234",
          "src" : iconSize("/assets/icons/generated/photos.jpg")
         };

var td2 = {"name" : "trash",
           "path" : "/archive/collections/photos/trash",
           "src" : iconSize("/assets/icons/generated/brokenImage.jpg")
         };
