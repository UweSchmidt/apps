// catalog server with scotty
//
// rpc communication

var serverVersion = { "server"  : "scotty",
                      "version" : "0.1.2.0",
                      "date"    : "2018-02-15"
                    };

// --------------------

function callServer(getOrModify, fct, args, processRes, processNext) {
    var rpc = [fct, args];
    console.log('callScottyServer: ' + getOrModify);
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

// --------------------
