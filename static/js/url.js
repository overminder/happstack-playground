goog.provide('todo.url');

goog.require('todo.conf');

todo.url.createTodo = function() {
  return todo.conf.DOMAIN_NAME + '/a/todo';
};

todo.url.deleteTodo = function(_id) {
  return todo.conf.DOMAIN_NAME + '/a/todo/' + _id;
};

// vim: set ts=2 sts=2 sw=2:
