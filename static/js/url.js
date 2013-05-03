goog.provide('todo.url');

goog.require('todo.conf');

todo.url.todoResource = function(opt_id) {
  return todo.conf.DOMAIN_NAME + '/a/todo' + (opt_id ? '/' + opt_id : '');
};

// vim: set ts=2 sts=2 sw=2:
