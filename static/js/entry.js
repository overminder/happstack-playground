goog.provide('todo.entry');

goog.require('goog.dom');

goog.require('todo.ui');

todo.entry.main = function() {
  var rootEl = goog.dom.getElement('todo-home');
  var homeView = new todo.ui.Home();
  homeView.decorate(rootEl);
};

goog.exportSymbol('todo.entry.main', todo.entry.main);

// vim: set ts=2 sts=2 sw=2:
