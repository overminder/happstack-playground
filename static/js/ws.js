goog.provide('todo.ws');

goog.require('goog.net.WebSocket');
goog.require('goog.json');
goog.require('goog.events');
goog.require('goog.events.EventType');
goog.require('goog.object');

goog.require('todo.conf');

goog.scope(function() {

var WebSocket = goog.net.WebSocket;

todo.ws.setup = function(ws, opt_debug) {
  goog.events.listen(ws,
    WebSocket.EventType.MESSAGE,
    function(e) {
      var msg = goog.json.unsafeParse(e.message);
      this.dispatchEvent({
        type: msg['type'],
        target: msg['todo']
      });
    }, undefined, ws);

  if (opt_debug) {
    goog.events.listen(ws,
      goog.object.getValues(todo.ws.EventType),
      function(e) {
        console.log(e.type, e.target);
      });
  }

  var onClose = function() {
    console.log('ws close');
    ws.close();
  };

  goog.events.listen(window, goog.events.EventType.UNLOAD, onClose);
  goog.events.listen(document, goog.events.EventType.UNLOAD, onClose);

  ws.open(todo.conf.WS_BIND_ADDR);

  return ws;
};

todo.ws.EventType = {
  TODO_CREATED: 'todo-created',
  TODO_UPDATED: 'todo-updated',
  TODO_DELETED: 'todo-deleted'
};

});  // !goog.scope

// vim: set ts=2 sts=2 sw=2:
