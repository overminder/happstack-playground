goog.provide('todo.ui');

goog.require('goog.array');
goog.require('goog.dom');
goog.require('goog.events');
goog.require('goog.events.EventType');
goog.require('goog.json');
goog.require('goog.ui.Component');
goog.require('goog.ui.Container');
goog.require('goog.ui.Button');
goog.require('goog.net.XhrIo');

goog.require('todo.url');

goog.scope(function() {

var XhrIo = goog.net.XhrIo;
var Component = goog.ui.Component;
var Container = goog.ui.Container;

/**
 * @constructor
 */
todo.ui.Home = function() {
  goog.base(this);

  this.listView_ = new todo.ui.List();
  this.createForm_ = new todo.ui.CreateForm();

  this.addChild(this.listView_);
  this.addChild(this.createForm_);
};
goog.inherits(todo.ui.Home, Component);

todo.ui.Home.prototype.decorateInternal = function(el) {
  goog.base(this, 'decorateInternal', el);

  var todoListEl = goog.dom.getElementByClass('todo-list', el);
  this.listView_.decorate(todoListEl);

  var createFormEl = goog.dom.getElementByClass('create-todo', el);
  this.createForm_.decorate(createFormEl);
};

/**
 * @constructor
 */
todo.ui.List = function() {
  goog.base(this);
};
goog.inherits(todo.ui.List, Component);

todo.ui.List.prototype.decorateInternal = function(el) {
  goog.base(this, 'decorateInternal', el);
  var todoEls = goog.dom.getElementsByClass('todo', el);
  goog.array.forEach(todoEls, function(todoEl) {
    var todoView = new todo.ui.Todo();
    this.addChild(todoView);
    todoView.decorate(todoEl);
  }, this);
};

todo.ui.Home.EventType = {
  TODO_ADDED: goog.events.getUniqueId('todo-added')
};

/**
 * @constructor
 */
todo.ui.Todo = function() {
  goog.base(this);
};
goog.inherits(todo.ui.Todo, Container);

todo.ui.Todo.prototype.enterDocument = function() {
  goog.base(this, 'enterDocument');
  var el = this.getElement();
  var formEl = goog.dom.getElementByClass('submit-form', el);
  var idValue = goog.dom.getElementByClass('todo-id', el).value;
  goog.events.listen(formEl,
    goog.events.EventType.SUBMIT,
    function(e) {
      e.preventDefault();
      XhrIo.send(todo.url.deleteTodo(idValue), goog.bind(function() {
        // Deletion ok: remove this component
        console.log('Ajax delete ok:', idValue);
        this.getParent().removeChild(this, true);
      }, this),
      /* method */ 'DELETE',
      /* data */ undefined, {
        'content-type': 'application/json'
      });
      console.log('Using ajax to do delete', idValue);
      this.setEnabled(false);
    }, undefined, this);
};

/**
 * @constructor
 */
todo.ui.CreateForm = function() {
  goog.base(this);

  this.contentInput_ = new LabelInput('New todo...');
  this.addChild(this.contentInput_);
};
goog.inherits(todo.ui.CreateForm, Container);

todo.ui.CreateForm.prototype.decorateInternal = function(el) {
  goog.base(this, 'decorateInternal', el);

  var inputEl = goog.dom.getElementByClass('todo-content', el);
  this.contentInput_.decorate(inputEl);
};

todo.ui.CreateForm.prototype.enterDocument = function() {
  goog.base(this, 'enterDocument');

  this.contentInput_.setAllowTextSelection(true);

  var el = this.getElement();
  var inputEl = goog.dom.getElementByClass('todo-content', el);
  goog.events.listen(el,
    goog.events.EventType.SUBMIT,
    function(e) {
      e.preventDefault();
      XhrIo.send(todo.url.createTodo(), goog.bind(function(e) {
        var responseJson = e.target.getResponseJson();
        console.log('ajax create ok', responseJson);
      }, this),
      'POST',
      goog.json.serialize({
        '_id': null,
        'content': inputEl.value
      }), {
        'content-type': 'application/json'
      });
      console.log('using ajax to create');
      this.setEnabled(false);
    }, undefined, this);
};

});  // !goog.scope

// vim: set ts=2 sts=2 sw=2:


