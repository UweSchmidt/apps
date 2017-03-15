// ----------------------------------------
//
// global state

var openCollections = {};

// remove a collection
function remCol(path) {
    console.log('remCol: ' + path);
    openCollections[path] = undefined;
}

// insert a  collection
function insCol(path, col) {
    console.log('insCol: ' + path);
    console.log(col);
    openCollections[path] = col;
}

// update a  collection and
// return whether col has changed
function updCol(path, col) {
    console.log('updCol: ' + path);
    var unchanged = eqCol(openCollections[path], col);
    if ( unchanged ) {
        console.log('nothing has changed');
    } else {
        console.log('something has changed');
        console.log(openCollections[path]);
        console.log(col);
        openCollections[path] = col;
    }
    return ! unchanged;
}

// compare collections
function eqCol(col, col1) {
    if (! col || ! col1) {
        return false;
    }
    var es  =  col.entries;
    var es1 = col1.entries;
    var ln  = es.length;
    if ( ln != es1.length ) {
        return false;
    }
    var i = 0;
    for (i = 0; i < ln; ++i) {
        var e  =  es[i];
        var e1 = es1[i];
        if (e.ColEntry != e1.ColEntry) {
            return false;
        }
        if (e.ref != e1.ref) {
            return false;
        }
        if (e.ColEntry === 'IMG'
            && e.part != e1.part) {
            return false;
        }
    }
    return true;
}

// ----------------------------------------
//
// dia button group handler

function diaButton(e) {
    e.preventDefault();
    statusClear();

    var res    = {};
    res.dia    = $(e.target).closest('div.dia');
    res.pos    = getEntryPos(res.dia);
    res.cid    = activeCollectionId();
    res.path   = collectionPath(res.cid);
    res.rating = getRatingVal(res.dia);
    // res.name = getDiaName(res.dia);
    console.log(res);
    return res;
}

function setDiaNo(dia, i) {
    $(dia).find('span.img-no').empty().append("" + (i + 1));
}

function setDiaImgName(dia, name) {
    $(dia)
        .find('span.img-part')
        .empty()
        .append(name);
}

function setDiaColName(dia, name) {
    $(dia)
        .find('span.col-part a')
        .empty()
        .append(name);
}

function setDiaColRef(dia, path) {
    $(dia)
        .find('span.col-part a')
        .attr('href', path);
}

function setDiaColAccess(dia, wr) {
    console.log('setDiacolaccess', wr);
    if (wr) {
        $(dia)
            .find('span.img-col-writeprotected')
            .addClass('hidden');
    } else {
        $(dia)
            .find('span.img-col-writeprotected')
            .removeClass('hidden');
    }
}

function getDiaColWriteProtected(dia) {
    var res = $(dia)
            .find('span.img-col-writeprotected')
            .hasClass('hidden');
    console.log('getdiaColWriteAccess', res);
    return res;
}

// --------------------

function setDiaRating(dia, rating) {
    console.log("setDiaRating: " + rating);
    console.log(dia);
    var stars = $(dia).find('div.dia-stars');
    stars.attr('data-rating', rating);
    var j = 0;
    for (j = 0; j <= 5; ++j) {
        var cls = j <= rating ? "dia-marked" : "dia-unmarked";
        stars.find('[data-star="' + j + '"]')
            .removeClass("dia-marked")
            .removeClass("dia-unmarked")
            .addClass(cls);
    }
}

function setRatingInCollection(cid, i, rating) {
    console.log("setRating: " + cid + ", " + i + ", " + rating);
    var dia = getDia(cid, i);
    setDiaRating(dia, rating);
}

// used as callback for getRatingsFromServer
function setAllRatingsInCollection(cid, ratings) {
    var i = 0;
    for (i = 0; i < ratings.length; ++i) {
        setRatingInCollection(cid, i, ratings[i]);
    }
}

function setRatingsInCollection(cid, ixs, rating) {
    var i = 0;
    for (i = 0; i < ixs.length; ++i) {
        if ( ixs[i] >= 0 ) {
            setRatingInCollection(cid, i, rating);
        }
    }
    unmarkAll(cid);
}

// --------------------

function getDia(cid, i) {
    var sel = '#' + cid + " > div.dia:nth-child(" + (i + 1) + ")";
    console.log("getDia: sel: " + sel);
    return $(sel);
}

function isMarkedDia(dia) {
    if (dia) {
        return dia.hasClass('marked');
    }
    return null;
}

function isUnmarkedDia(dia) {
    if (dia) {
        return ! dia.hasClass('marked');
    }
    return null;
}

function toggleDiaMark(cid, i) {
    var dia = getDia(cid, i);
    if (dia) {
        dia = toggleOrSetMark(dia, 'toggle');
    }
    return dia;
}

// --------------------

function getDiaNo(dia) {
    var pos = $(dia)
            .find('span.img-no')
            .contents()
            .get(0)
            .textContent;
    pos = parseInt(pos) - 1;
    return pos;
}

function getDiaColRef(dia) {
    var res = $(dia)
            .find('span.col-part a')
            .attr('href');
    console.log('getDiaColRef: ', res);
    return res;
}

function getDiaName(dia) {
    var res;

    // img
    res = $(dia)
        .find('span.img-part')
        .contents()
        .get(0);
    if (res && res.length > 0) {
        return res.textContent;
    }

    // col
    res = $(dia)
        .find('div.dia-name a')
        .contents()
        .get(0);
    if (res && res.length > 0) {
        return res.textContent;
    }

    return '???';
}

function diaBtnRemove(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#RemoveButton').click();
}

function diaBtnMoveToClipboard(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#MoveToClipboardButton').click();
}

function diaBtnCopyToClipboard(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#CopyToClipboardButton').click();
}

function diaBtnRename(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#RenameCollectionButton0').click();
}

function diaBtnWriteProtected(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#WriteProtectedButton').click();
}

function diaBtnSort(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#SortButton').click();
}

function diaBtnView(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#PreviewButton').click();
}

function diaBtnMeta(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#ShowMetaDataButton').click();
}

function diaBtnTitle(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#MetaDataButton').click();
}

function diaBtnColimg(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#CollectionImgButton').click();
}

function diaBtnBlog(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#BlogEditButton0').click();
}

// ----------------------------------------
//
// mark/unmark entries
// toggleSlideMark is the event handler

function toggleSlideMark(e) {
    e.preventDefault();
    statusClear();
    // console.log("mark image was clicked");
    // console.log(e);
    // console.log(e.shiftKey);
    // console.log(e.target);

    var dia      = $(e.target).closest("div.dia");
    var unmarked = dia.hasClass('unmarked');
    if (e. shiftKey) {
        toggleLeft(dia, unmarked);
    } else {
        toggleMark(dia);
    }
}

function toggleLeft(dia, unmarked) {
    var dno = getDiaNo(dia);
    var uml  = dia.hasClass('unmarked');
    // console.log('toggleLeft', dno, uml);

    if (unmarked === uml) {
        if (dno > 0) {
            var prev = dia.prev('div.dia');
            // console.log(prev);
            toggleLeft(prev, unmarked);
        }
        toggleMark(dia);
    }
}

function toggleMark(dia) {
    toggleOrSetMark(dia, 'toggle');
}

function setEntryMark(dia) {
    // the clear guaranties, this entry will be
    // the last marked entry
    // this becomes important when sorting is added to button groups
    toggleOrSetMark(dia, 'clear');
    toggleOrSetMark(dia, 'set');
}

function clearEntryMark(dia) {
    toggleOrSetMark(dia, 'clear');
}

function toggleOrSetMark(dia, fct) {
    console.log("toggleDiaMark: dia");
    console.log(dia);

    if ( dia.hasClass("unmarked") ) {
        if (fct === 'toggle' || fct === 'set') {
            // dia was unmarked, mark it, and existing curmark
            dia.removeClass("unmarked");
            clearCurMark(dia);

            var mx = getMarkedNo(getMarked()).length + 1;
            // console.log(mx);
            setMarkCount(dia, '' + mx);

            // set mark and curmark
            markThisAsCur(dia);
            dia.addClass("marked");
        }
    } else {
        if (fct === 'toggle' || fct === 'clear') {
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
        col.find(sel)
            .closest("div.dia")
            .find("img.dia-src")
            .addClass("curmarked");
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

function alwaysOpenCollectionId(cid) {
    return cid === idCollections()
        || cid === idClipboard();
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

function collectionIsWriteProtected(cid) {
    var res = $('#' + cid).hasClass('no-write');
    console.log('collectionIsWriteProtected');
    console.log(res);
    return res;
}

function collectionIsSortProtected(cid) {
    var res = $('#' + cid).hasClass('no-sort');
    console.log('collectionIsSortProtected');
    console.log(res);
    return res;
}

function collectionIsGenerated(path) {
    console.log('collectionIsGenerated');
    console.log(path);
    return path === pathClipboard()
        || path === pathCollections()
        || isPathPrefix(pathPhotos(), path)
        || isPathPrefix(pathTimeline(), path);
}

function collectionId(path) {
    var res = undefined;
    $("#theCollections div.tab-pane").each(function (i, e) {
        var id1 = $(e).attr('id');
        var dpath = $(e).attr('data-path');
        if ( path === dpath ) {
            res = id1;
        }
    });
    return res;
}

function allCollectionPaths() {
    // collect all collection paths (ObjId's on server)
    var colPaths = [];
    $("#theCollections div.tab-pane")
        .each(function (i, e) {
            var path1 = $(e).attr('data-path');
            colPaths.push(path1);
        });
    console.log(colPaths);
    return colPaths;
}

function isWriteProtectedCollection(colVal) {
    var acc = colVal.metadata[0]["descr:Access"];
    return acc && acc.search('no-write') >= 0;
}

function isNoDeleteCollection(colVal) {
    var acc = colVal.metadata[0]["descr:Access"];
    return acc && acc.search('no-delete') >= 0;
}

function isNotSortableCollection(colVal) {
    var acc = colVal.metadata[0]["descr:Access"];
    return acc && acc.search('no-sort') >= 0;
}

// ----------------------------------------

function showNewCollection(path, colVal) {
    // insert into global state
    insCol(path, colVal);

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

        o.colId   = path2id(o.path);
        console.log(o);

        // no-write collection ?
        var ro = isWriteProtectedCollection(colVal);
        var sr = isNotSortableCollection(colVal);
        var nd = isNoDeleteCollection(colVal);
        var gn = collectionIsGenerated(o.path);
        var ct = colVal.metadata[0]["descr:Title"];

        // add the tab panel
        var t = $('#prototype-tabpanel').children("div").clone();

        t.find('div.tab-panel').empty();
        t.attr('id', o.colId).attr('data-path', o.path);

        if ( ro ) { t.addClass("no-write");  }
        if ( sr ) { t.addClass("no-sort");   }
        if ( nd ) { t.addClass("no-delete"); }
        if ( gn ) { t.addClass("generated"); }
        $('#theCollections').append(t);

        // create a new tab from prototype
        var tb = $('#prototype-tab').find("li").clone();

        var tt = "path: " + o.path;
        if (ct) { tt = "title: " + ct + "\n" + tt; }
        if (ro) { tt = tt + "\naccess: no-write"; };

        tb.find('a')
            .attr('href', '#' + o.colId)
            .attr('aria-controls', o.colId)
            .attr('title', tt);
        tb.find('.coltab-name')
            .empty()
            .append(o.name);

        $('#collectionTab').append(tb);
        markAccess(o.colId, ro);

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
            if ( dp === path ) {
                b = true;
                pid = $(e).attr('id');
            };
        });
    return [b, pid];
}

function setActiveTab(colId) {
    $('#collectionTab')
        .find('a[aria-controls=' + colId + ']')
        .trigger('click');
}

function insertEntries(colId, entries) {
    console.log('insertEntries');
    console.log(colId);
    console.log(entries);
    var path = collectionPath(colId);

    var col = $('#' + colId);
    col.empty();
    entries.forEach(function (e, i) {
        insertEntry(colId, path, e, i);
    });

    // insert the ratings
    getRatingsFromServer(path,
                         function(ratings) {
                             setAllRatingsInCollection(colId, ratings);
                         });

    // set handler for showing edit buttons
    col.find('div.dia')
        .hover(function () {
            $(this)
                .find('.dia-btn-group')
                .removeClass('hidden');
        }, function () {
            $(this)
                .find('.dia-btn-group')
                .addClass('hidden');
        });

    // install handler at rating stars
    col.find('div.dia div.dia-stars a.star')
        .on('click', function (e) {
            console.log('rating button');
            console.log(e.target); // target is glyphicon-star span elem
            var o = diaButton(e);
            var s = $(e.target)
                    .closest('a.star')
                    .attr('data-star');
            var newRating = parseInt(s) || 0;
            var curRating = o.rating;
            if ( curRating === 1 && newRating === 1) {
                // 1 star set and 1 star pressed
                // -> remove star
                newRating = 0;
            }
            setEntryMark(o.dia);
            setRating(newRating);
        });

    // set handler for button groups
    col.find('div.dia button.dia-btn-remove')
        .on('click', diaBtnRemove);
    /*
    col.find('div.dia button.dia-btn-movefromclipboard')
        .on('click', diaBtnMoveFromClipboard);
    col.find('div.dia button.dia-btn-copyfromclipboard')
        .on('click', diaBtnCopyFromClipboard);
     */
    col.find('div.dia button.dia-btn-movetoclipboard')
        .on('click', diaBtnMoveToClipboard);
    col.find('div.dia button.dia-btn-copytoclipboard')
        .on('click', diaBtnCopyToClipboard);
    col.find('div.dia button.dia-btn-rename')
        .on('click', diaBtnRename);
    col.find('div.dia button.dia-btn-writeprotected')
        .on('click', diaBtnWriteProtected);
    col.find('div.dia button.dia-btn-sort')
        .on('click', diaBtnSort);
    col.find('div.dia button.dia-btn-view')
        .on('click', diaBtnView);
    col.find('div.dia button.dia-btn-meta')
        .on('click', diaBtnMeta);
    col.find('div.dia button.dia-btn-title')
        .on('click', diaBtnTitle);
    col.find('div.dia button.dia-btn-colimg')
        .on('click', diaBtnColimg);
    col.find('div.dia.data-md button.dia-btn-colblog')
        .on('click', diaBtnColimg);
    // all col dias and .md img dias get this handler
    col.find('div.dia.data-md button.dia-btn-blog')
        .on('click', diaBtnBlog);

    //collections don't have all buttons
    col.find('div.dia.colmark button.dia-btn-colimg')
        .addClass('hidden');
    col.find('div.dia.colmark button.dia-btn-blog')
        .removeClass('hidden')
        .attr('title', 'Edit blog text for this collection');
    col.find('div.dia.colmark button.dia-btn-view')
        .attr('title', 'Preview image and/or blog text for this collection');


    // hide write protect button for generated collections
    col.find('div.dia.colmark')
        .each(function (i, e) {
            var cname = getDiaName(e);
            var cpath = path + "/" + cname;
            console.log('hide lock button ', cpath);
            if (collectionIsGenerated(cpath)) {
                $(e).find('button.dia-btn-writeprotected')
                .addClass('hidden');
            }
        });

    // images don't have all buttons
    col.find('div.dia.imgmark button.dia-btn-rename')
        .addClass('hidden');
    col.find('div.dia.imgmark button.dia-btn-writeprotected')
        .addClass('hidden');

    // .md text files don't have meta data
    col.find('div.dia.imgmark.data-md button.dia-btn-meta')
        .addClass('hidden');
    col.find('div.dia.imgmark.data-md button.dia-btn-title')
        .addClass('hidden');
    // redefine colimg button to set colblog entry
    col.find('div.dia.imgmark.data-md button.dia-btn-colimg')
        .attr('title', "Take this text as blog text for the current collection");
    col.find('div.dia.imgmark.data-md button.dia-btn-view')
        .attr('title', "Preview this blog text");
    col.find('div.dia.imgmark.data-md button.dia-btn-blog')
        .removeClass('hidden');

    // clipboard has a special set of image buttons
    if (colId === idClipboard()) {
        col.find('div.dia button.dia-btn-movefromclipboard')
            .addClass('hidden');
        col.find('div.dia button.dia-btn-copyfromclipboard')
            .addClass('hidden');
        col.find('div.dia button.dia-btn-movetoclipboard')
            .addClass('hidden');
        col.find('div.dia button.dia-btn-copytoclipboard')
            .addClass('hidden');
    }

    // in clipboard and subcollections remove is possible
    // not in the rest of the collections
    // clipboard collections are never write protected
    if ( ! isPathPrefix(pathClipboard(), path) ) {
        col.find('div.dia button.dia-btn-remove')
            .addClass('hidden');
    } else {
        col.find('div.dia button.dia-btn-writeprotected')
            .addClass('hidden');
    }
}

function insertEntry(colId, colPath, entry, i) {
    var e = newEntry(colId, colPath, entry, i);
    $('#' + colId).append(e);
}

function newEntry(colId, colPath, entry, i) {
    console.log("newEntry: colId=" + colId);
    console.log(entry);
    var p = $("#prototype-dia").children("div").clone();

    setDiaNo(p, i);

    var sc = iconSize('');
    var mk = '';
    var tt = '';
    var ref = splitPath(entry.ref);

    if (entry.ColEntry === "IMG") {
        setDiaImgName(p, entry.part);
        // add the part extension as class to the entry
        // "data-jpg" for images,
        // "data-md" for blog entries (markdown text .md or .txt)
        // in preview modal box this info becomes important
        var ep = splitPath(entry.part);
        var ex = ep.ext;
        if (ep.ext == "txt" || ep.ext == "md") {
            ex = "data-md";
        } else {
            ex = "data-jpg";
        }
        p.addClass(ex);

        sc = sc + ref.cpath + "/" + entry.part;
        mk = "imgmark";
        tt = "image: " + entry.ref;
    }
    if (entry.ColEntry === "COL") {
        setDiaColName(p, ref.name);
        // collections always have a .jpg as preview
        p.addClass('data-jpg')
         .addClass('data-md');


        // this ref is a dummy
        // the real ref of a collection is inserted later
        // by a server call
        // but we need a legal src ref

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

    if ( entry.ColEntry === "COL") {
        // if entry is a collection add open collection event handler
        p.find("div.dia-name a")
            .on('click', function (e) {
                e.preventDefault();
                openCollection(ref.path);
            });

        getIsWriteableFromServer(ref.path,
                                 function (ro) {
                                     setDiaColAccess(p, ro);
                                 });
        // set the icon url
        // url is computed on server and added in callback
        getIconRefFromServer(ref.path,
                             iconSize(),
                             function (ref) {
                                 p.find("img.dia-src")
                                     .attr('src', ref);
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
    p.find("div.dia-img")
        .on('click', toggleSlideMark)
        .css('cursor','pointer');

    return p;
}

// ----------------------------------------

function checkAndRefreshCol(path, refresh, force) {
    console.log('colIsThere: ' + path);
    getIsColFromServer(path,
                       function (isThere) {
                           if (! isThere) {
                               var cid = collectionId(path);
                               removeCollectionFromDom(cid);
                               statusMsg('collection closed: ' + path);
                           } else {
                               if ( refresh ) {
                                   if ( force ) {
                                       remCol(path);
                                   }
                                   updateCollection(path);
                               }
                           }
                       });
}

function checkAllColAreThere(refresh, force) {
    var colPaths = allCollectionPaths();
    colPaths.forEach(function (path, i) {
        checkAndRefreshCol(path,refresh, force);
    });

}

// ----------------------------------------

function syncActiveCollection(sync) {
    var cid  = activeCollectionId();
    var path = collectionPath(cid);
    syncCollectionWithFilesystem(sync, path);
}

function syncCollectionWithFilesystem(sync, path) {
    console.log("syncCollectionWithFilesystem: " + path);

    if ( ! isPathPrefix(pathPhotos(), path) ) {
        statusError('collection to be synchronized must be a subcollection of '
                    + pathPhotos());
        return;
    }

    // start syncing on server side
    statusMsg('synchronizing collection with filesystem: ' + path);
    modifyServer1(sync, path, [],
                  function(log) {
                      statusMsg('synchronizing on server side done');
                      // TODO: show log file in modal box
                      console.log(log);
                      checkAllColAreThere(true, false);
                  });
}

function exifActiveCollection() {
    var cid  = activeCollectionId();
    var path = collectionPath(cid);
    exifCollectionWithFilesystem(path);
}

function exifCollectionWithFilesystem(path) {
    console.log("exifCollectionWithFilesystem: " + path);

    if ( ! isPathPrefix(pathPhotos(), path) ) {
        statusError('collection to be updated with exif must be a subcollection of '
                    + pathPhotos());
        return;
    }

    // start syncing on server side
    statusMsg('recomputing exif data for: ' + path);
    readServer('syncExif', path,
                function(log) {
                    statusMsg('exif update done');
                    console.log(log);
                });
}

function checkArchiveConsistency() {
    console.log("checkArchiveConsistency");
    statusMsg('checking/repairing archive consistency');
    modifyServer1("checkArchive", pathArchive(), [],
                  function(log) {
                      statusMsg('checking archive on server side done');
                      // TODO
                      console.log(log);
                  });
}

// ----------------------------------------

// for remove, move and rename commands
// check whether subcollections are still there
// maybe sometimes there are subcollections,
// which were also moved removed

function refreshCollection1(path, colVal) {
    refreshCollection(path, colVal);
    checkAllColAreThere(false, false);
}

function refreshCollection(path, colVal) {
    refreshCollection2(path,colVal, false);
}

function refreshCollectionF(path, colVal) {
    refreshCollection2(path,colVal, true);
}

function refreshCollection2(path, colVal, force) {
    var o = splitPath(path);
    console.log('refreshCollection: ' + force);
    console.log(o);
    console.log(colVal);

    // only if the collection content has changed
    // update the entries
    var changed = updCol(path, colVal);
    if ( changed || force) {
        var io = isAlreadyOpen(path);
        // check whether collection is already there
        if ( io[0]) {
            o.colId = io[1];
            insertEntries(o.colId, colVal.entries);
        }
    } else {
        unmarkAll(collectionId(path));
    }
}

// ----------------------------------------

// messages don't overwrite errors
function statusMsg(msg) {
    if (! $('#status-container')
         .hasClass('status-err-bg') ) {

        $('#status-message')
            .removeClass('error-msg')
            .addClass('status-msg')
            .empty()
            .append(msg);
    }
}

function statusError(msg) {
    $('#status-container')
        .addClass('status-err-bg');
    $('#status-message')
        .removeClass('status-msg')
        .addClass('error-msg')
        .empty()
        .append(msg);
}

// clear errors and messages
function statusClear() {
    $('#status-container')
        .removeClass('status-err-bg');
    statusMsg('');
}

// ----------------------------------------
// string helper functions

function path2id(path) {
    // for html ids replace all none alphanum chars by "_"
    // else it isn't a valid id value

    var res = 'col-' + path.replace(/[^-_a-zA-Z0-9]/g,"_");
    console.log('path2id');
    console.log(path);
    console.log(res);

    return res;
}

// compute an object with path, name and cpath, and bname and ext
function splitPath(p) {
    if (typeof p === 'string') {
        return splitPath({'path' : p});
    }

    var a = p.path.split("/");
    p.name    = a.pop();
    // console.log(a);
    p.cpath   = a.join("/");
    // a.shift();
    console.log(a);
    p.topname = a.shift(); // get the name of the top dir
    // console.log(a);
    a.unshift("");
    console.log(a);
    p.cpath1  = a.join("/"); // remove the topdir

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

// check whether p1 is a path prefix of p2

function isPathPrefix(p1, p2) {
    var l1  = p1.length;
    var l2  = p2.length;
    var px2 = p2.substr(0, l1);
    if (p1 === p2) {
        return true;
    }
    if ( (l2 > l1) && (p1 === px2) && (p2.charAt(l1) === "/") ) {
        return true;
    }
    return false;
}

// ----------------------------------------

// top level commands, most with ajax calls

// initialize the collections in a fixed sequence,
// first the clipboard, so it's always the leftmost collection in the tab
// the root collection is not shown until the clipboard is there

function openSystemCollections() {
    statusClear();
    getColFromServer(pathClipboard(),
                     function (path, colVal) {
                         showNewCollection(path, colVal);
                         getColFromServer(pathCollections(),
                                          showNewCollection);
                     });
}

function openCollection(path) {
    statusClear();
    getColFromServer(path, showNewCollection);
}

function updateCollection(path) {
    getColFromServer(path, refreshCollection);
}

function closeCollection(cid) {
    statusClear();
    var cp   = collectionPath(cid);
    if ( alwaysOpenCollectionId(cid) ) {
        statusError("this collection must stay open: " + cp);
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
        removeCollectionFromDom(cid);
        statusMsg('collection closed: ' + cp);
    }
}

function removeCollectionFromDom(cid) {
    // remove from state
    remCol(collectionPath(cid));

    // remove the tab content
    $('#' + cid).remove();

    // remove the tab
    $('li > a[aria-controls=' + cid + ']').remove();
}

// ----------------------------------------
// mark functions

// process each dia in active collection
function eachADia(pred, fct) {
    eachDia(activeCollectionId(), pred, fct);
}

function eachDia(cid, pred, fct) {
    console.log('eachDia: ' + cid);
    $('#' + cid + ' > div.dia')
        .each(function (i, e) {
            if ( pred($(e)) ) {
                fct($(e));
            }
        });
}

function constTrue(dia) { return true; }

function hasRating(pred) {
    var f = function (dia) {
        var r = getRatingVal(dia);
        return pred(r);
    };
    return f;
}

function eqInt(i) {
    var p = function (j) {return j === i;}
    return p;
}

function geInt(i) {
    var p = function (j) {return j >= i;}
    return p;
}

function testDia(dia) {
    console.log(dia.find('img.dia-src').attr('title'));
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
    // statusClear();
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

function anyMarked(ixs) {
    var res = false;
    ixs.forEach(function (e, i) {
        if ( e >= 0 ) {
            res = true;
        }
    });
    return res;
}

function getLastMarkedEntry(cid) {
    var res = $('#' + cid)
            .find("img.curmarked")
            .closest('div.dia')
            .get(0);
    console.log('getLastmarkedEntry');
    console.log(res);
    return res;
}

function getColNames(cid) {
    var cnames = $('#' + cid + ' div.dia.colmark div.dia-name a')
            .contents()
            .get()
            .map(function (x) {return x.textContent;});
    console.log('getColNames');
    console.log(cnames);
    return cnames;
}

function getMarkedCollections(cid) {
    console.log('getMarkedCollections: ' + cid);
    var paths =[];
    $('#' + cid + ' div.dia.colmark.marked div.dia-img img')
        .each(function(i,e) {
            var t = $(e).attr('title');
            t = t.replace(/collection: /, '');
            console.log(t);
            paths.push(t);
        });
    console.log(paths);
    return paths;
}

function getOpenMarkedCollections(cid) {
    console.log("getOpenMarkedCollections: " + cid);
    var mcs = getMarkedCollections(cid);
    var ocs = allCollectionPaths();
    var res = [];
    mcs.forEach(function(e, i) {
        ocs.forEach(function(e2, i2) {
            if ( e === e2) {
                res.push(e2);
            }
        });
    });
    console.log('getOpenMarkedCollections');
    console.log(res);
    return res;
}

function getOpenMarkedSubCollections(cid) {
    console.log("getOpenMarkedSubCollections: " + cid);
    var mcs = getMarkedCollections(cid);
    var ocs = allCollectionPaths();
    var res = [];
    mcs.forEach(function(e, i) {
        ocs.forEach(function(e2, i2) {
            if ( isPathPrefix(e, e2) ) {
                res.push(e2);
            }
        });
    });
    console.log('getOpenMarkedSubCollections');
    console.log(res);
    return res;
}

function closeSubCollections(cid) {
    getOpenMarkedSubCollections(cid)
        .forEach(function(e, i) {
            var id = collectionId(e);
            var cp = collectionPath(id);
            removeCollectionFromDom(id);
            statusMsg('collection closed: ' + cp);
        });
    setActiveTab(cid);
}

// ----------------------------------------

function removeMarkedFromClipboard() {
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);
    statusClear();
    if ( ! isPathPrefix(pathClipboard(), cpath)) {
        statusError('removing images/collections only possible in clipboard collections');
        return;
    }
    removeMarkedFromCollection(cid);
}

function removeMarkedFromCollection(cid) {
    // if open collections are to be removed (or moved)
    // they must be closed first
    // get marked collections (not images)
    // get all paths of open collections
    // the intersection is the set of collections to be closed
    closeSubCollections(cid);

    var cpath = collectionPath(cid);
    if (! cpath ) {
        statusError('collection not found: ' + cid);
        return;
    }

    var ixs   = getMarkedEntries(cid);
    console.log('removeMarkedFromCollection');
    console.log(ixs);
    console.log(cpath);

    // remove on server and refresh collection
    removeFromColOnServer(cpath, ixs);
}

// ----------------------------------------

function moveMarkedFromClipboard(cid) {
    statusClear();
    var ixs = getMarkedEntries(idClipboard());
    var dpath = collectionPath(cid);
    var cpath = pathClipboard();
    console.log('moveMarkedFromClipboard');
    console.log(ixs);
    console.log(cpath);
    console.log(dpath);

    // do the work on server and refresh both collections
    moveToColOnServer(cpath, dpath, ixs);
}

function copyMarkedFromClipboard(cid) {
    statusClear();
    var ixs = getMarkedEntries(idClipboard());
    var dpath = collectionPath(cid);
    var cpath = pathClipboard();
    console.log('copyMarkedFromClipboard');
    console.log(ixs);
    console.log(cpath);
    console.log(dpath);

    // do the work on server and refresh both collections
    copyToColOnServer(cpath, dpath, ixs);
}

// ----------------------------------------

function moveMarkedToClipboard(cid) {
    statusClear();
    var ixs   = getMarkedEntries(cid);
    var cpath = collectionPath(cid);
    var dpath = pathClipboard();
    console.log('moveMarkedToClipboard');
    console.log(ixs);
    console.log(cpath);
    console.log(dpath);

    // do the work on server and refresh both collections
    moveToColOnServer(cpath, dpath, ixs);
}

function copyMarkedToClipboard(cid) {
    statusClear();
    var ixs   = getMarkedEntries(cid);
    var cpath = collectionPath(cid);
    var dpath = pathClipboard();
    console.log('copyMarkedToClipboard');
    console.log(ixs);
    console.log(cpath);
    console.log(dpath);

    // do the work on server and refresh both collections
    copyToColOnServer(cpath, dpath, ixs);
}

// ----------------------------------------

function sortCollection(cid) {
    statusClear();
    console.log('sort collection: ' + cid);

    // var sr = $('#' + cid).hasClass('no-sort');
    var sr = collectionIsSortProtected(cid);
    console.log(sr);
    if ( sr ) {
        var path = collectionPath(cid);
        statusError('not sortable, the collection is sort protected: ' + path);
        return;
    }

    var ixs  = getMarkedEntries(cid);
    if (! anyMarked(ixs)) {
        statusMsg('no marked images/collections found for sorting');
        return;
    }

    var path = collectionPath(cid);
    console.log(path);

    sortColOnServer(path, ixs);
}

// ----------------------------------------

function getEntryPos(img) {
    var pos = $(img)
            .find('span.img-no')
            .contents()
            .get(0)
            .textContent;
    pos = parseInt(pos) -1;
    console.log('getEntryPos');
    console.log(pos);
    return pos;
}

function getRatingVal(img) {
    var r = $(img)
            .find('div.dia-stars')
            .attr('data-rating');
    r = parseInt(r) || 0;
    console.log('getRatingVal: ' + r);
    return r;
}

function setCollectionImg(cid) {
    statusClear();
    console.log("setCollectionImg: " + cid);
    var path  = collectionPath(cid);
    var o     = splitPath(path);

    // first try current collection for a marked image
    // then lookup clipboard for a marked entry
    var scid  = cid;
    var spath = path;
    var simg  = getLastMarkedEntry(scid);
    if (! simg) {
        scid  = idClipboard();
        spath = pathClipboard();
        simg  = getLastMarkedEntry(scid);
    }
    if (! simg) {
        statusError('no marked image found');
        return;
    }
    if ( $(simg).hasClass('imgmark') ) {
        var pos = getDiaNo(simg);
        console.log('setCollectionImg:');
        console.log(pos);
        console.log(path);
        console.log(spath);
        console.log(o);

        if ( $(simg).hasClass('data-jpg') ) {
            // it's a .jpg image
            // so take this image as collection image
            modifyServer('colimg', path, [spath, pos],
                         function () {
                             var ppath = o.cpath;
                             var pcol = isAlreadyOpen(ppath);
                             statusMsg('collection image set in: ' + path);
                             if ( pcol[0] ) {
                                 // parent collection open
                                 // refresh the parent collection
                                 // to show the new collection image
                                 remCol(ppath); // force refresh
                                 getColFromServer(ppath, refreshCollection);
                             }
                         });
        }
        else if ( $(simg).hasClass('data-md') ) {
            // it's a .md text file
            // so take this as the collection blog text
            modifyServer('colblog', path, [spath, pos], noop);
        } else {
            statusError('not a .jpg image or a .md text file');
        }
    } else {
        statusError('marked entry is a collection, not an image');
    }
    // unmark last marked entry
    toggleMark($(simg));
}

// ----------------------------------------

function createCollection() {
    statusClear();
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);
    var name  = $('#newCollectionName').val();
    name = name.replace(/[^-_.a-zA-Z0-9]/g,"");
    var cnames = getColNames(cid);
    var ix = cnames.indexOf(name);

    console.log('createCollection');
    console.log(cpath);
    console.log(name);
    console.log(cnames);

    if (name.length === 0) {
        statusError("no collection name given");
        return;
    }
    if ( ix >= 0 ) {
        statusError("collection name already exists: " + name);
        return;
    }
    createColOnServer(cpath, name, refreshCollection);
}

// ----------------------------------------

function writeProtectedCollection() {
    statusClear();
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);
    var ixs   = getMarkedEntries(cid);
    var opcs  = getOpenMarkedCollections(cid);
    var dia   = getLastMarkedEntry(cid);
    var ro    = getDiaColWriteProtected(dia);
    console.log('writeProtectedCollection');
    console.log(cid);
    console.log(ixs);
    console.log(opcs);
    console.log(ro);

    changeWriteProtectedOnServer(cpath, ixs, ro, opcs);
}

function markWriteProtected(opcs, ro) {
    console.log('markWriteProtected', ro);
    console.log(opcs);

    opcs.forEach(function (e, i) {
        var cid = collectionId(e);
        markAccess(cid, ro);
    });
}

// the lock icon in tab list of open collections is set/removed
// the marker class no-write is added/removed in tab panel
function markAccess(cid, ro) {
    var c = $('#collectionTab')
            .find('[href="#' + cid + '"]')
            .find('.coltab-writeprotected');
    if (ro) {
        c.removeClass('hidden');
        $('#' + cid).addClass('no-write');
    } else {
        c.addClass('hidden');
        $('#' + cid).removeClass('no-write');
    }
}

// ----------------------------------------

// renameCollectionCheck does all the error checks
// if this is o.k. the #RenameCollectionButton is invoked
// and this one triggers the modal mox
// so error check and reporting can be done before the modal is shown

function renameCollectionCheck() {
    statusClear();
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);

    if (collectionIsWriteProtected(cid)) {
        statusError('rename not allowed, collection is write protected: ' + cpath);
        return;
    }
    var img   = getLastMarkedEntry(cid);
    if (! img) {
        statusError("no collection marked in: " + cpath);
        return;
    }
    if ($(img).hasClass('imgmark')) {
        statusError("last marked isn't a collection in: " + cpath);
        // clearEntryMark($(img));
        return;
    }
    var iname = getDiaName(img);
    $('#RenameCollectionModalLabel')
        .empty()
        .append('Rename collection: ' + iname);
    $('#RenameCollectionButton').click();
}

function renameCollection() {
    statusClear();
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);
    var img   = getLastMarkedEntry(cid);
    var iname = getDiaName(img);
    var path  = cpath + "/" + iname;
    console.log('renameCollection');
    console.log(path);

    var newname  = $('#RenameCollectionName').val();
    newname = newname.replace(/[^-_.a-zA-Z0-9]/g,"");

    console.log(newname);

    if (newname.length === 0) {
        statusError("no collection name given");
        return;
    }

    var cnames = getColNames(cid);
    var ix = cnames.indexOf(newname);

    console.log(cnames);
    console.log(ix);

    if ( ix >= 0 ) {
        statusError("collection name already exists: " + newname);
        return;
    }

    renameColOnServer(cpath, path, newname, refreshCollection1);
}

// ----------------------------------------

function setMetaData() {
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);

    /*
     // write protection is limited to collection entries,
     // collection image and meta data may be modified

    if (collectionIsWriteProtected(cid)) {
        statusError('can\'t set meta data, active collection is write protected: '
                    + cpath
                   );
        return;
    }
     */

    var metadata = {};
    var keys = ["Title", "Subtitle", "Comment", "Rating",
                "Keywords", "Web", "Wikipedia",
                "TitleEnglish", "TitleLatin" // <-- not yet implemented in edit.html
               ];
    keys.forEach(function (e, i) {
        var k =    'descr:' + e;
        var v = $('#descr-' + e).val();
        if (v && v.length > 0) {
            metadata[k] = v;
        }
    });
    if (jQuery.isEmptyObject(metadata)) {
        statusMsg('no meta data given');
        return;
    }

    var ixs = getMarkedEntries(cid);
    if (! anyMarked(ixs)) {
        statusMsg('no marked images/collections found');
        return;
    }
    console.log('setMetaData');
    console.log(metadata);
    console.log(ixs);

    setMetaOnServer(cpath,ixs,metadata);
}

function setRating(rating) {
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);
    var ixs   = getMarkedEntries(cid);

    if (! anyMarked(ixs)) {
        statusMsg('no marked images/collections found');
        return;
    }
    console.log('setRating: ' + rating);
    console.log(ixs);

    setRatingsOnServer(cid, cpath, ixs, rating);
}

// ----------------------------------------

// runs concurrently with metadata modal show event
function fillMetaData() {
    var o  = {};
    o.cid  = activeCollectionId();
    o.path = collectionPath(o.cid);
    o.dia  = getLastMarkedEntry(o.cid);

    if (! o.dia) {
        statusError('no marked image/collection found');
        return ;
    }
    o.pos  = getEntryPos(o.dia);
    o.name = getDiaName(o.dia);
    fillMetaFromServer(o);

}

function fillMetaData1(md0, args) {
    // meta data is wrapped into a single element list (why?)
    var md = md0[0];
    console.log('fillMetaData1');
    console.log(md);
    console.log(args);

    // insert fields into metadata edit form
    var dt = md["descr:Title"] || "";
    console.log(dt);
    $('#descr-Title').val(dt);
    var ds = md["descr:Subtitle"] || "";
    $('#descr-Subtitle').val(ds);
    var dr = md["descr:Rating"] || "";
    $('#descr-Rating').val(dr);
    var dc = md["descr:Comment"] || "";
    $('#descr-Comment').val(dc);
    var dk = md["descr:Keywords"] || "";
    $('#descr-Keywords').val(dk);
    var dw = md["descr:Web"] || "";
    $('#descr-Web').val(dw);
    var di = md["descr:Wikipedia"] || "";
    $('#descr-Wikipedia').val(di);
}

// ----------------------------------------

// check whether there is a marked entry
// if not, it's a noop
// else the real getMeta is performed

function getMetaData0() {
    var cid  = activeCollectionId();
    var dia  = getLastMarkedEntry(cid);

    if (! dia) {
        statusError('no marked image/collection found');
        return ;
    }
    $('#ShowMetaDataButton').click();
}

function getMetaData() {
    var o  = {};
    o.cid  = activeCollectionId();
    o.path = collectionPath(o.cid);
    o.dia  = getLastMarkedEntry(o.cid);

    if (! o.dia) {
        $('#ShowMetaDataModal').modal('hide');
        statusError('no marked image/collection found');
        return ;
    }
    o.pos  = getEntryPos(o.dia);
    o.name = getDiaName(o.dia);
    getMetaFromServer(o);
}

function showMetaData(md0, args) {
    // meta data is wrapped into a single element list (why?)
    var md = md0[0];
    console.log('showMetaData');
    console.log(md);
    console.log(args);

    $('#ShowMetaDataModalLabel')
        .empty()
        .append('Metadata: ' + args.path + "/" + args.name);

    var kvs = [];
    $.each(md, function (k, v) {
        kvs.push([k, v]);
    });
    kvs.sort(function (p1, p2) {
        var x = p1[0];
        var y = p2[0];
        if ( x > y ) { return +1; }
        if ( x < y ) { return -1; }
        return 0;
    });
    console.log(kvs);

    var mdt = $('#ShowMetaDataTable').empty();

    kvs.forEach(function (e, i) {
        mdt.append('<tr><th>' + e[0] + '</th><td>' + e[1] + '</td></tr>');
    });

    // clear mark for entry invoked
    clearEntryMark($(args.dia));
}

function imageCarousel() {
    var args = {};
    args.cid  = activeCollectionId();
    args.path = collectionPath(args.cid);

    console.log("imageCarousel: get collection from server:" + args.path);
    getColFromServer(args.path,
                     function (path, colVal) {
                         buildImgCarousel(args, colVal)
                     });
}

function buildImgCarousel(args, colVal) {
    console.log("buildImgCarousel: " + args.path);
    console.log(JSON.stringify(colVal));

    if (colVal.entries.length == 0) {
        statusError("empty collection: " + args.path);
        return;
    }

    var g = previewGeo();

    // build the carousel DOM by copying the prototype
    // and rename id an did refs
    var c   = $("#prototype-carousel")
            .children("div")
            .clone();
    c.attr('id', 'image-carousel');
    c.find('[data-target="#proto-carousel"]')
        .attr('data-target', "#image-carousel");
    c.find('[href="#proto-carousel"]')
        .attr('href', "#image-carousel");

    var cli = c.find('ol.carousel-indicators li')
            .clone()
            .removeClass('active');
    var cit = c.find('div.carousel-inner div.item')
            .clone()
            .removeClass('active');
    // add the appropriate geo class for carousel images to prototype
    cit.find('div.img-box').addClass("img-box-" + g.geo);

    // remove prototype list of indicators and list of items
    c.find('ol.carousel-indicators').empty();
    c.find('div.carousel-inner').empty();

    // insert images
    colVal.entries.forEach(function (e, i) {
        console.log("entry: " + i);
        console.log(JSON.stringify(e));

        // append to the list of carousel indicators
        var item = $(cli).clone().attr("data-slide-to", i);
        c.find('ol.carousel-indicators').append(item);

        // append carousel item to item list
        var iscol = e.ColEntry == "COL";
        var cimg = $(cit).clone();
        var eref = splitPath(e.ref);
        var capt = "."
                + (i + 1)
                + " "
                + (iscol ? "Collection: " : "")
                + eref.name;

        cimg.find('.carousel-image-capt')
            .empty()
            .append(capt);

        var state = isMarkedDia(getDia(args.cid, i));
        var cls   = state ? "carousel-image-marked" : "carousel-image-unmarked";

        cimg.find('div.carousel-caption a.carousel-image-mark')
            .addClass(cls)
            .on('click', function (e) {
                var state = isMarkedDia(toggleDiaMark(args.cid, i));
                var cls   = state ? "carousel-image-marked" : "carousel-image-unmarked";
                console.log(e.target);
                $(e.target)
                    .removeClass("carousel-image-marked")
                    .removeClass("carousel-image-unmarked")
                    .addClass(cls);
                console.log(e.target);
                console.log("toggle image mark: " + args.cid + " ." + i);
            });

        cimg.find('div.carousel-caption a.star')
            .addClass("carousel-image-unmarked")
            .on('click', function (e) {
                // var state = isMarkedDia(toggleDiaMark(args.cid, i));
                // var cls   = state ? "carousel-image-marked" : "carousel-image-unmarked";
                console.log(e.target);
                var bno = parseInt($(e.target).closest("a.star").attr("data-star"));
                var bgp = $(e.target).closest("span.carousel-stars");
                var b1m = bgp.find('[data-star="1"]').hasClass("carousel-image-marked");
                var b2u = bgp.find('[data-star="2"]').hasClass("carousel-image-unmarked");
                if (bno === 1 && b1m && b2u) {
                    bno = 0;
                }
                var j = 0;
                for (j = 0; j <= 5; ++j) {
                    var cls = j <= bno ? "carousel-image-marked" : "carousel-image-unmarked";
                    bgp.find('[data-star="' + j + '"]')
                        .removeClass("carousel-image-marked")
                        .removeClass("carousel-image-unmarked")
                        .addClass(cls);
                }
                // console.log(e.target);
                console.log("set rating: " + args.cid + " ." + i + "= " + bno);
                console.log(args);
                setRatingOnServer(args.cid, args.path, i, bno);
            });


        // insert the icon ref into cimg
        if ( iscol ) {
            // for a collection this is done asynchronously by the callback fct
            getIconRefFromServer(eref.path,
                                 previewGeo().img,
                                 function (ref) {
                                     console.log("iconref: " + ref);
                                     cimg.find("div.img-box img")
                                         .attr('src', ref)
                                         .attr('alt', eref.name);
                                 });
        } else {
            var iref = "/" + g.img + eref.cpath1 + "/" + e.part;
            var notJpg = splitPath(e.part).ext !== "jpg";
            if ( notJpg ) {
                iref = iref + ".jpg";
            }
            console.log("iconref: " + iref);
            cimg.find("div.img-box img")
                .attr('src', iref)
                .attr('alt', eref.name);

        }
        c.find('div.carousel-inner').append(cimg);

    }); // end forEach loop

    // set the initially active slide
    c.find("li[data-slide-to='0']")
        .addClass('active');
    c.find("div.carousel-inner div:first-child")
        .addClass('active');

    $('#CarouselModal')
        .off('keypress')
        .on('keypress', function(e){
            console.log('CarouselModal:');
            console.log(e);
            var charCode = e.which || e.keyCode;
            if ( charCode == 109 ) {  // 'm': mark
                console.log('keypress: toggle image mark');
                $('#CarouselModalBody div.carousel-inner div.item.active a.carousel-image-mark').click();
            } else if ( charCode == 110 ) { // 'n': next image
                console.log('keypres: next image');
                $("#CarouselModalBody .carousel-control.right").click();
            } else if ( charCode == 112 ) { // 'p': prev image
                console.log('keypres: prev image');
                $("#CarouselModalBody .carousel-control.left").click();
            } else if ( charCode >= 48 && charCode <= 53) { // '0'..'5' set rating
                var rating = charCode - 48;
                var sel = '[data-star="' + rating + '"]';
                console.log('keypress: set rating to ' + rating);
                $('#CarouselModalBody div.carousel-inner div.item.active ' + sel).click();
            }
            return false;
        });

    // configure the modal box and its size
    $('#CarouselModal > div.modal-dialog')
        .attr('class', 'modal-dialog modal-carousel-' + g.geo);
    $('#CarouselModalBody')
        .empty()
        .append(c);

    var clab = "";
    var ttt  = colVal.metadata[0]["descr:Title"];
    if ( ttt ) {
        clab += "<h3>" + ttt + "<h3>"
    }
    ttt  = colVal.metadata[0]["descr:Subtitle"];
    if ( ttt ) {
        clab += "<h4>" + ttt + "<h4>"
    }
    clab += "<h5>" + args.path + "</h5>";

    $('#CarouselModalLabel')
        .empty()
        .append(clab);

    $('#CarouselModal').modal('show');
}

function previewImage() {
    var args = {};
    args.cid  = activeCollectionId();
    args.path = collectionPath(args.cid);
    args.dia  = getLastMarkedEntry(args.cid);
    if (! args.dia) {
        statusError('no marked image/collection found');
        return ;
    }
    args.iscol  = $(args.dia).hasClass('colmark');
    args.pos    = getEntryPos(args.dia);
    args.name   = getDiaName(args.dia);
    args.fmt    = previewGeo().img;
    clearEntryMark($(args.dia));

    $('#PreviewModal > div.modal-dialog')
        .attr('class', 'modal-dialog modal-preview-' + previewGeo().geo);

    $('#PreviewModalBody > div')
        .addClass('hidden');

    $('#PreviewModalLabel')
        .empty()
        .append('Preview: ' + args.path + "/" + args.name);

    // if dia has a .jpg then load the preview ref and show the image
    // via call back insertPreviewRef

    if ( $(args.dia).hasClass('data-jpg') ) {
        getPreviewRef(args);
    }
    if ( $(args.dia).hasClass('data-md') ) {
        getBlogText(args);
    }
}

function insertPreviewRef(ref, args) {
    // come from: previewImage and getPreviewRef
    console.log('insertPreviewRef');
    console.log(ref);
    console.log(args);

    // make the div for images visible
    $('#PreviewModalBody div.data-jpg').removeClass('hidden');

    // insert the image ref, browser will load and show the image
    $('#PreviewModalImgRef')
        .attr('src', ref)
        .attr('alt', args.path);

    $('#PreviewModal').modal('show');
}

function insertBlogText(txt, args) {
    // come from: previewImage and getPreviewRef
    console.log('insertBlogText');
    console.log(txt);
    console.log(args);

    if (txt === "") {
        console.log('empty text');
        return;
    }
    // make the div for images visible
    $('#PreviewModalBody div.data-md').removeClass('hidden');
    $('#PreviewModalBlog')
        .empty()
        .append(txt);

    $('#PreviewModal').modal('show');
}

// ----------------------------------------

function blogEdit() {
    statusClear();
    var args  = {};
    args.cid  = activeCollectionId();
    args.path = collectionPath(args.cid);
    args.img  = getLastMarkedEntry(args.cid);
    if (! args.img) {
        statusError("no blog entry marked in: " + args.path);
        return;
    }
    args.isimg  = $(args.img).hasClass('imgmark');
    args.iscol  = $(args.img).hasClass('colmark');
    args.isblog = $(args.img).hasClass('data-md');

    if (args.isimg && ! args.isblog) {
        statusError("last marked isn't a blog entry in: " + args.path);
        return;
    }
    args.iname = getDiaName(args.img);
    args.pos   = getEntryPos(args.img);

    var hdl = "???";
    if (args.iscol) {
        hdl = 'Edit blog text for collection: ';
    }
    if (args.isimg) {
        hdl = 'Edit blog entry: ';
    }

    // prepare the edit modal title
    $('#EditBlogModalLabel')
        .empty()
        .append(hdl + args.path + "/" + args.iname);

    console.log('blogEdit');
    console.log(args);

    clearEntryMark($(args.img));

    // on the highway to call back hell
    getBlogTextForEdit(args);
}

function insertBlogTextForEdit(res, args) {
    // come from: blogEdit

    console.log('insertBlogTextForEdit');
    console.log(res);

    $('#EditBlogContents')
        .empty()
        .val(res);

    // install event handler for OK button
    // and pass the args data to the handler
    $('#EditBlogOK')
        .off('click')
        .on('click', function () {
            console.log("EditBlogOK clicked");
            $('#EditBlogModal').modal('hide');
            saveBlogText(args);
        });

    // modal box is configured,
    // show it and wait for the save button beeing clicked
    $('#BlogEditButton').click();
}

function saveBlogText(args) {
    // come from: insertBlogTextForEdit
    var res = $('#EditBlogContents').val();
    console.log('saveBlogText');
    console.log(args);
    console.log(res);

    saveBlogTextFromEdit(args, res);
}

// ----------------------------------------

// ajax calls

function removeFromColOnServer(path, args) {
    modifyServer("removeFromCollection", path, args,
                 function () {
                     getColFromServer(path, refreshCollection1);
                 });
}

function copyToColOnServer(spath, dpath, args) {
    copyMoveToColOnServer("copyToCollection", spath, dpath, args);
}

function moveToColOnServer(spath, dpath, args) {
    copyMoveToColOnServer("moveToCollection", spath, dpath, args);
}

function copyMoveToColOnServer(cpmv, spath, dpath, args) {
    modifyServer(cpmv, spath, [args, dpath],
                 function () {
                     getColFromServer(spath, refreshCollection );
                     getColFromServer(dpath, refreshCollection1);
                 });
}

function sortColOnServer(path, ixs) {
    modifyServer("sort", path, ixs,
                 function () {
                     getColFromServer(path, refreshCollection);
                 });
}

function changeWriteProtectedOnServer(path, ixs, ro, opcs) {
    modifyServer("changeWriteProtected", path, [ixs, ro],
                 function () {
                     getColFromServer(path, refreshCollectionF);
                     markWriteProtected(opcs, ro);
                 });
}

function setMetaOnServer(path, ixs, metadata) {
    modifyServer("setMetaData", path, [ixs, [metadata]],
                 function () {
                     getColFromServer(path, refreshCollection);
                 });
}

function setRatingOnServer(cid, path, ix, rating) {
    modifyServer("setRating1", path, [ix, rating],
                 function () {
                     setRatingInCollection(cid, ix, rating);
                 });
}

function setRatingsOnServer(cid, path, ixs, rating) {
    console.log('setRatingsOnserver:' + path + ", " + ixs + ", " + rating);
    modifyServer1("setRating", path, [ixs, rating],
                  function () {
                      setRatingsInCollection(cid, ixs, rating);
                  });
}

function getColFromServer(path, showCol) {
    readServer("collection", path,
           function (col) {
               showCol(path, col);
           });
}

function getIsWriteableFromServer(path, markWriteable) {
    readServer('isWriteable', path, markWriteable);
}

function getIsColFromServer(path, cleanupCol) {
    readServer('isCollection', path, cleanupCol);
}

function getIconRefFromServer(path, fmt, insertSrcRef) {
    readServer1('iconref', path, fmt, insertSrcRef);
}

function createColOnServer(path, name, showCol) {
    modifyServer("newcol", path, name,
                 function () {
                     getColFromServer(path, refreshCollection);
                 });
}

function renameColOnServer(cpath, path, newname, showCol) {
    modifyServer("renamecol", path, newname,
                 function () {
                     getColFromServer(cpath, refreshCollection1);
                 });
}

function fillMetaFromServer(args) {
    // thread the args object into the callback function
    readServer1('metadata',
                args.path,
                args.pos,
                function (res) { fillMetaData1(res, args); }
               );
}

function getMetaFromServer(args) {
    // thread the args object into the callback function
    readServer1('metadata',
                args.path,
                args.pos,
                function (res) { showMetaData(res, args); }
               );
}

function getRatingFromServer(path, pos, setRating) {
    readServer1('rating', path, pos, setRating);
}

// TODO: substitute getRatingFromServer for every picture
// with getRatingsFromServer for the whole collection
function getRatingsFromServer(path, setRating) {
    readServer('ratings', path, setRating);
}

function getPreviewRef(args) {
    readServer1('previewref',
                args.path,
                [args.pos, args.fmt],
                function (res) { insertPreviewRef(res, args); }
               );
}

function getBlogText(args) {
    readServer1('blogcontents',
                args.path,
                args.pos,
                function (res) { insertBlogText(res, args); }
               );
}

function getBlogTextForEdit(args) {
    readServer1('blogsource',
                args.path,
                args.pos,
                function (res) { insertBlogTextForEdit(res, args); }
               );
}

function saveBlogTextFromEdit(args, text) {
    modifyServer("saveblogsource",
                 args.path,
                 [args.pos, text],
                 function () {
                     statusMsg('blog contents saved for: ' + args.iname);
                 });
}

function saveImgStoreStart() {
    // statusClear();
    statusMsg('taking snapshot of image archive, one moment please');
    var name = $('#saveImgStoreName').val();
    $('#saveImgStoreName').val('');
    saveImgStore(name);
}

function saveImgStore(text) {
    console.log('saveImgStore: ' + text);
    modifyServer("snapshot",
                 pathArchive(),
                 text,
                 function () {
                     statusMsg('snapshot of image archive taken');
                 });
}

// ----------------------------------------

// http communication

function callServer(getOrModify, fct, args, processRes, processNext) {
    var rpc = [fct, args];
    console.log('callServer: ' + getOrModify);
    console.log(rpc);
    console.log(JSON.stringify(rpc));

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
    readServer1(fct, path, [], processRes);
    // callServer("get", fct, [path, []], processRes, noop);
}

function readServer1(fct, path, args, processRes) {
    callServer("get", fct, [path, args], processRes, noop);
}

// make a modifying call to server
// modifying calls usualls don't get an interesting result back
// in success case (), else the error message
// so most modifying ops are procedures, not functions

function modifyServer(fct, path, args, processNext) {
    callServer("modify", fct, [path, args], ignoreRes, processNext);
}

// a modify with a result, e.g. a log file of the operation
function modifyServer1(fct, path, args, processRes) {
    callServer("modify", fct, [path, args], processRes, noop);
}

function ignoreRes(res) {}

function noop() {}


// ----------------------------------------
//
// the "main" program
// set the event handlers

$(document).ready(function () {
    $('#collectionTab a').click(function (e) {
        e.preventDefault();
        $(this).tab('show');
    });

    openSystemCollections();

    // event handler for navbar buttons
    $("#CloseButton")
        .on('click', function (e) {
            closeCollection(activeCollectionId());
        });
    /* now part of mark menue
    $("#MarkButton")
        .on('click', function (e) {
            markAll(activeCollectionId());
        });

    $("#UnmarkButton")
        .on('click', function (e) {
            unmarkAll(activeCollectionId());
        });
     */

    $("#MoveFromClipboardButton")
        .on('click', function (e) {
            moveMarkedFromClipboard(activeCollectionId());
        });

    $("#CopyFromClipboardButton")
        .on('click', function (e) {
            copyMarkedFromClipboard(activeCollectionId());
        });

    $("#MoveToClipboardButton")
        .on('click', function (e) {
            moveMarkedToClipboard(activeCollectionId());
        });

    $("#CopyToClipboardButton")
        .on('click', function (e) {
            copyMarkedToClipboard(activeCollectionId());
        });

    $("#RemoveButton")
        .on('click', function (e) {
            removeMarkedFromClipboard();
        });

    $("#CollectionImgButton")
        .on('click', function (e) {
            setCollectionImg(activeCollectionId());
        });

    $('#newCollectionButton')
        .on('click', function () {
            statusClear();
        });

    $('#newCollectionModal')
        .on('show.bs.modal', function () {
            statusClear();
        });

    $('#newCollectionModal')
        .on('hidden.bs.modal', function () {
            $('#newCollectionButton').blur();
        });

    $('#newCollectionOK')
        .on('click', function (e) {
            console.log("newCollectionOK clicked");
            $('#newCollectionModal').modal('hide');
            createCollection();
        });

    $('#WriteProtectedButton')
        .on('click', function () {
            statusClear();
            writeProtectedCollection();
        });

    $('#RenameCollectionButton0')
        .on('click', function () {
            statusClear();
            renameCollectionCheck();
        });

    $('#RenameCollectionOK')
        .on('click', function (e) {
            console.log("renameCollectionOK clicked");
            $('#RenameCollectionModal').modal('hide');
            renameCollection();
        });

    $("#SortButton")
        .on('click', function (e) {
            statusClear();
            sortCollection(activeCollectionId());
        });

    $('#MetaDataButton')
        .on('click', function () {
            statusClear();
            fillMetaData();
        });

    $('#MetaDataModal')
        .on('show.bs.modal', function () {
            statusClear();
        });

    $('#MetaDataOK')
        .on('click', function (e) {
            console.log("MetaDataOK clicked");
            $('#MetaDataModal').modal('hide');
            setMetaData();
        });

    $('#PreviewButton')
        .on('click', function () {
            statusClear();
            previewImage();
        });

    $('#CarouselButton')
        .on('click', function () {
            statusMsg("open image carousel");
            imageCarousel();
        });

    $('#ShowMetaDataButton0')
        .on('click', function () {
            statusClear();
            getMetaData0();
        });

    $('#ShowMetaDataModal')
        .on('show.bs.modal', function () {
            statusClear();
            getMetaData();
        });

    $('#BlogEditButton0')
        .on('click', function () {
            statusClear();
            blogEdit();
        });

    // #BlogEditButton triggers the modal box
    // it is invoked by blogEdit handler

    // mark menue event handler
    $('#MarkAll')   .on('click', function (e) { eachADia(isUnmarkedDia,       setEntryMark); });
    $('#Mark1')     .on('click', function (e) { eachADia(hasRating(geInt(1)), setEntryMark); });
    $('#Mark2')     .on('click', function (e) { eachADia(hasRating(geInt(2)), setEntryMark); });
    $('#Mark3')     .on('click', function (e) { eachADia(hasRating(geInt(3)), setEntryMark); });
    $('#Mark4')     .on('click', function (e) { eachADia(hasRating(geInt(4)), setEntryMark); });
    $('#Mark5')     .on('click', function (e) { eachADia(hasRating(geInt(5)), setEntryMark); });
    $('#ToggleMark').on('click', function (e) { eachADia(constTrue,           toggleMark); });
    $('#UnmarkAll') .on('click', function (e) { eachADia(isMarkedDia,         clearEntryMark); });
    $('#Unmark1')   .on('click', function (e) { eachADia(hasRating(eqInt(1)), clearEntryMark); });
    $('#Unmark2')   .on('click', function (e) { eachADia(hasRating(eqInt(2)), clearEntryMark); });
    $('#Unmark3')   .on('click', function (e) { eachADia(hasRating(eqInt(3)), clearEntryMark); });
    $('#Unmark4')   .on('click', function (e) { eachADia(hasRating(eqInt(4)), clearEntryMark); });
    $('#Unmark5')   .on('click', function (e) { eachADia(hasRating(eqInt(5)), clearEntryMark); });

    // rating menue event handler
    [0,1,2,3,4,5].forEach(function (e, i) {
        $('#Rating' + i)
            .on('click', function () {
                console.log("set ratings to " + i);
                setRating(i);
            });
    });


    $('#saveImgStoreOK')
        .on('click', function (e) {
            console.log("saveImgStoreOK clicked");
            $('#saveImgStoreModal').modal('hide');
            saveImgStoreStart();
        });

    $('#SyncCollection')
        .on('click', function () {
            // statusClear();
            statusMsg('sync collection with images on the filesystem, one moment please');
            syncActiveCollection('syncCol');
        });

    $('#SyncExif')
        .on('click', function () {
            // statusClear();
            statusMsg('recomputing exif data');
            exifActiveCollection();
        });

    $('#NewCollection')
        .on('click', function () {
            // statusClear();
            statusMsg('import new subcollections collection from filesystem, one moment please');
            syncActiveCollection('newSubCols');
        });

    // refresh all collections, just a debug op
    $('#RefreshCollection')
        .on('click', function () {
            statusClear();
            // statusMsg('refreshing all open collections');
            checkAllColAreThere(true, true);
        });

    $('#ConsistencyCheck')
        .on('click', function () {
            statusClear();
            // statusMsg('refreshing all open collections');
            checkArchiveConsistency();
        });

});

// ----------------------------------------
//
// "constants"

function pathArchive()     { return "/archive"; }
function pathCollections() { return "/archive/collections"; }
function pathClipboard()   { return "/archive/collections/clipboard"; }
function pathPhotos()      { return "/archive/collections/photos"; }
function pathTimeline()    { return "/archive/collections/timeline"; }
function pathTrash()       { return "/archive/collections/trash"; }

function idCollections() { return path2id(pathCollections()); }
function idClipboard()   { return path2id(pathClipboard()); }
function idTrash()       { return path2id(pathTrash()); }

function iconSize()    { return "pad-160x160"; }

function previewGeo() {
    var g = window.screen;
    if (g.width === 2560 && g.height === 1440) {
        return previewGeoXY(1400, 1050);
    }
    return previewGeoXY(900,600);
}

function previewGeoXY(x, y) {
    var o = {w : x, h: y};
    o.geo = "" + o.w + "x" + o.h;
    o.img = "pad-" + o.geo;
    return o;
}

// ----------------------------------------
