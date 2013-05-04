goog.provide('todo.ws');

goog.require('goog.net.WebSocket');

goog.require('todo.conf');

goog.scope(function() {

var WebSocket = goog.net.WebSocket;

todo.ws.setup = function() {
  var ws = new WebSocket();
  ws.addEventListener(WebSocket.EventType.MESSAGE,
    function(e) {
      console.log('WS:', e);
    });

  ws.open(todo.conf.WS_BIND_ADDR);

  return ws;
};


});  // !goog.scope

// vim: set ts=2 sts=2 sw=2:
