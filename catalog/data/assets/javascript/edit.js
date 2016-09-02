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

function toggleMark(e) {
    e.preventDefault();
    console.log("mark image was clicked");
    console.log(e);
    console.log(e.target);
}

function newDia(dia) {
    console.log("newDia");
    console.log(dia);
    var p = $("#prototype-dia").children("div").clone();

    // set the slide id
    $(p).attr('id',dia.path + "/" + dia.name);
    console.log(p);

    // set the head line
    var pt = $(p).children("div.dia-top");
    $(pt).empty();
    $(pt).append(dia.title);

    // set the icon url
    var pi = $(p).find("img.dia-src");
    $(pi).attr('src', dia.src);

    // add event handler for marking
    $(p).children("div.dia-img").on('click', toggleMark).css('cursor','pointer');

    return p;
}

function activeCollection() {
    return $("#theCollections").children("div.active").attr("id");
}

function addDiaToActiveCollection(dia) {
    var actColId = activeCollection();
    var newSlide = newDia(dia);
    console.log("addDiatoactivecollection");
    console.log("actColId");
    console.log("TODO");
}

var td = {"name" : "clipboard",
          "path" : "/archive/collections/photos",
          "title" : "clipboard",
          "src" : "/pad-160x160/assets/icons/generated/photos.jpg"
         };
