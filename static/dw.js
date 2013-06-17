// Do not edit, generated from parenscript.
window.dw = (function () {
    var x;
    var dw = this;
    var lg = function (msg) {
        sendWsMessage({ 'type' : 'page-log', 'message' : msg });
        return console.log(msg);
    };
    var makeElementInserter = function (how, location) {
        return { 'how' : how, 'location' : location };
    };
    var insertElement = function (newElement, continuation, elementInserter) {
        if (elementInserter === undefined) {
            elementInserter = INSERTER;
        };
        var loc = elementInserter.location;
        if (1 !== loc.length) {
            throw new Error(['Selector "', String(place), '" found ', String(loc.length), ' elements, must result in exactly one.'].join(''));
        };
        switch (elementInserter.how) {
        case 'before':
            newElement.hide();
            loc.before(newElement);
            return newElement.slideDown(1000, continuation);
        case 'after':
            newElement.hide();
            loc.after(newElement);
            return newElement.slideDown(1000, continuation);
        case 'append':
            newElement.hide().fadeIn(1000, continuation);
            return loc.append(newElement);
        case 'prepend':
            newElement.hide();
            loc.prepend(newElement);
            return newElement.slideDown(1000, continuation);
        case 'replace-content':
            var _c20 = function () {
                $(loc.children()).wrapAll('<div/>');
                loc.children().fadeOut(400, _c21);
                return undefined;
            };
            var _c21 = function () {
                loc.empty();
                newElement.hide().fadeIn(400, continuation);
                loc.prepend(newElement);
                return undefined;
            };
            return _c20();
        };
    };
    var initPhase2 = function (e) {
        ws.onmessage = onMessage;
        ws.onerror = function () {
            return lg('ws error');
        };
        return lg('Hello');
    };
    var sendWsMessage = function (data) {
        return ws.send($.toJSON(data));
    };
    var onMessage = function (e) {
        var msg = $.parseJSON(e.data);
        var target3 = msg.target;
        var selection = target3 ? jQuery(msg.target) : jQuery;
        var event4 = msg.event;
        var argument5 = msg.argument ? msg.argument : msg;
        lg('Got: ' + JSON.stringify(msg));
        return selection[event4](argument5);
    };
    this.lg = lg;
    this.makeElementInserter = makeElementInserter;
    this.insertElement = insertElement;
    this.initPhase2 = initPhase2;
    this.sendWsMessage = sendWsMessage;
    this.onMessage = onMessage;
    INSERTER = makeElementInserter('prepend', 'body');
    ws = (x = $.gracefulWebSocket('ws://127.0.0.1:8766/jm'), (x.onopen = initPhase2, x));
    return this;
}).call({ INSERTER : null, ws : null });
// Do not edit, generated from parenscript.
