// catalog server with servant
//
// rpc communication

var serverVersion = { "server"  : "servant",
                      "version" : "0.2.1.0",
                      "date"    : "2018-11-23"
                    };

// --------------------

function callServer(getOrModify, fct, args, processRes, processErr, processNext) {
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
        console.log("got server error");
        console.log(err);
        console.log(err.status);
        if (err.status == 500) {
            // internal server error
            // remove name of the function raising the error
            var msg = err.responseJSON || err.responseText;
            console.log(msg);
            processErr(msg.replace(/[^:]*: /, ""));
        } else
            if (err.status == 404) {
                processErr("unimplemented server operation: " + fct);
            } else {
                processErr("server error: " + err.status +
                           " when processing operation " + fct);
            }
    }).always(processNext);
}

// --------------------
