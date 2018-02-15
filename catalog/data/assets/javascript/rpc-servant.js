// catalog server with scotty
//
// rpc communication

var serverVersion = { "server"  : "servant",
                      "version" : "0.1.2.0",
                      "date"    : "2018-02-15"
                    };

// --------------------

function callServer(getOrModify, fct, args, processRes, processNext) {
    var rpc = [fct, args];
    var arg0 = args[0];
    var arg1 = args[1];

    console.log('callServantServer: ' + getOrModify);
    console.log(rpc);
    console.log(JSON.stringify(rpc));
    console.log(arg0);
    console.log(arg1);

    $.ajax({
        type: "POST",
        contentType: "application/json",
        url: "/" + getOrModify + '/' + fct + arg0,
        data: JSON.stringify(arg1),
        dataType: 'json'
    }).done(function (res) {
        processRes(res);
    }).fail(function (err){
        statusError(err.resposeText);
    }).always(processNext);
}

// --------------------
