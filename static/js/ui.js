goog.provide('todo.ui');

goog.require('goog.array');
goog.require('goog.dom');
goog.require('goog.events');
goog.require('goog.events.EventType');
goog.require('goog.json');
goog.require('goog.ui.Component');
goog.require('goog.ui.Control');
goog.require('goog.ui.Container');
goog.require('goog.ui.ContainerRenderer');
goog.require('goog.ui.Button');
goog.require('goog.ui.Textarea');
goog.require('goog.net.XhrIo');
goog.require('goog.net.WebSocket');

goog.require('todo.templates');
goog.require('todo.url');
goog.require('todo.model');
goog.require('todo.ws');

goog.scope(function() {

var XhrIo             = goog.net.XhrIo;
var Component         = goog.ui.Component;
var Control           = goog.ui.Control;
var Button            = goog.ui.Button;
var Textarea          = goog.ui.Textarea;
var Container         = goog.ui.Container;
var ContainerRenderer = goog.ui.ContainerRenderer;

/**
 * @constructor
 * @extends {Component}
 */
todo.ui.Home = function() {
  goog.base(this);

  this.listView_ = new todo.ui.List();
  this.createForm_ = new todo.ui.CreateForm();
  this.ws_ = new goog.net.WebSocket();

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

todo.ui.Home.prototype.enterDocument = function() {
  goog.base(this, 'enterDocument');

  goog.events.listen(this.ws_,
    todo.ws.EventType.TODO_CREATED,
    function(e) {
      this.dispatchEvent({
        type: todo.ui.Home.EventType.TODO_CREATED,
        target: e.target
      });
    }, undefined, this);

  goog.events.listen(this.ws_,
    todo.ws.EventType.TODO_UPDATED,
    function(e) {
      this.dispatchEvent({
        type: todo.ui.Home.EventType.TODO_UPDATED,
        target: e.target
      });
    }, undefined, this);

  goog.events.listen(this.ws_,
    todo.ws.EventType.TODO_DELETED,
    function(e) {
      this.dispatchEvent({
        type: todo.ui.Home.EventType.TODO_DELETED,
        target: e.target
      });
    }, undefined, this);

  todo.ws.setup(this.ws_);
};

/**
 * @constructor
 * @extends {Component}
 */
todo.ui.List = function() {
  goog.base(this);

  this.idToTodo_ = {};
};
goog.inherits(todo.ui.List, Component);

todo.ui.List.prototype.decorateInternal = function(el) {
  goog.base(this, 'decorateInternal', el);

  var todoEls = goog.dom.getElementsByClass('todo', el);
  goog.array.forEach(todoEls, function(todoEl) {
    var todoView = new todo.ui.Todo();
    this.addChild(todoView);
    todoView.decorate(todoEl);
    this.recordIdMapping(todoView);
  }, this);
};

todo.ui.List.prototype.enterDocument = function() {
  goog.base(this, 'enterDocument');

  goog.events.listen(this.getParent(),
    todo.ui.Home.EventType.TODO_CREATED,
    function(e) {
      var model = /** @type {todo.model.Todo} */ (e.target);
      var todoView = new todo.ui.Todo();
      todoView.setModel(model);
      this.addChildAt(todoView, 0, /* opt_render */ true);
      this.recordIdMapping(todoView);
    }, undefined, this);

  goog.events.listen(this.getParent(),
    todo.ui.Home.EventType.TODO_UPDATED,
    function(e) {
      var model = e.target;
      var _id = model['_id'];
      var todoView = this.idToTodo_[_id];
      if (todoView) {
        todoView.syncNewModel(model);
      }
    }, undefined, this);

  goog.events.listen(this.getParent(),
    todo.ui.Home.EventType.TODO_DELETED,
    function(e) {
      var _id = e.target['_id'];
      var todoView = this.idToTodo_[_id];
      if (todoView) {
        delete this.idToTodo_[_id];
        this.removeChild(todoView, true);
      }
    }, undefined, this);
};

todo.ui.List.prototype.recordIdMapping = function(todoView) {
  this.idToTodo_[todoView.getModel()['_id']] = todoView;
};

todo.ui.Home.EventType = {
  TODO_CREATED: goog.events.getUniqueId('todo-created'),
  TODO_UPDATED: goog.events.getUniqueId('todo-updated'),
  TODO_DELETED: goog.events.getUniqueId('todo-deleted'),

  LOCAL_START_EDIT:     goog.events.getUniqueId('local-start-edit'),
  LOCAL_SUBMIT_EDIT_OK: goog.events.getUniqueId('local-submit-ok'),
  LOCAL_CANCEL_EDIT:    goog.events.getUniqueId('local-cancel-edit')
};

/**
 * @constructor
 * @extends {Container}
 */
todo.ui.Todo = function() {
  goog.base(this, undefined, todo.ui.Todo.Renderer);

  this.edit_ = new Control(null);
  this.addChild(this.edit_);
  this.delete_ = new Control(null);
  this.addChild(this.delete_);
  this.content_ = new Control(null);
  this.addChild(this.content_);

  this.editField_ = new Textarea('');
  this.addChild(this.editField_);
  this.submitEdit_ = new Button('Submit');
  this.addChild(this.submitEdit_);
  this.cancelEdit_ = new Button('Cancel');
  this.addChild(this.cancelEdit_);

  this.setIsEditing(false);
};
goog.inherits(todo.ui.Todo, Container);

todo.ui.Todo.Renderer = ContainerRenderer.getCustomRenderer(
  ContainerRenderer, "todo");

todo.ui.Todo.prototype.createDom = function() {
  goog.base(this, 'createDom');

  var el = /** @type {Element!} */ (this.getElement());
  var model = this.getModel();
  // XXX: consider use goog closure compiler's rename map instead?
  var rekeyedModel = {
    _id: model['_id'],
    content: model['content']
  };
  var frag = soy.renderAsFragment(todo.templates.showOne, rekeyedModel);
  goog.dom.append(el, frag);

  this.decorateChildren(el);
};

todo.ui.Todo.prototype.decorateInternal = function(el) {
  goog.base(this, 'decorateInternal', el);

  // Extract model data from html
  // XXX: better ways to do it?
  var content = goog.dom.getElementByClass('content', el).innerText;
  var _id = goog.dom.getElementByClass('todo-id', el).value;
  this.setModel({
    '_id': _id,
    'content': content
  });

  this.decorateChildren(el);
};

todo.ui.Todo.prototype.decorateChildren = function(el) {
  this.edit_.decorate(goog.dom.getElementByClass('edit', el));
  this.delete_.decorate(goog.dom.getElementByClass('submit-btn', el));
  this.content_.decorate(goog.dom.getElementByClass('content', el));
  this.editField_.render(el);
  this.submitEdit_.render(el);
  this.cancelEdit_.render(el);
};

todo.ui.Todo.prototype.enterDocument = function() {
  goog.base(this, 'enterDocument');

  // Buggy closure :(
  this.editField_.setAllowTextSelection(true);

  var el = this.getElement();
  var formEl = goog.dom.getElementByClass('submit-form', el);
  var _id = goog.dom.getElementByClass('todo-id', el).value;
  goog.events.listen(formEl,
    goog.events.EventType.SUBMIT,
    function(e) {
      e.preventDefault();
      // Actual deletion is handled by the websocket
      XhrIo.send(todo.url.todoResource(_id), undefined,
        /* method */ 'DELETE');
      this.setEnabled(false);
    }, undefined, this);

  goog.events.listen(this.edit_.getElement(),
    goog.events.EventType.CLICK,
    function(e) { e.preventDefault(); });

  goog.events.listen(this.edit_,
    Component.EventType.ACTION,
    function(e) {
      this.getParent().dispatchEvent({
        type: todo.ui.Home.EventType.LOCAL_START_EDIT,
        target: this
      });
      this.setIsEditing(true);
    }, undefined, this);

  goog.events.listen(this.submitEdit_,
    Component.EventType.ACTION,
    function() {
      var _id = this.getModel()['_id'];
      XhrIo.send(todo.url.todoResource(_id), goog.bind(function() {
        // Actual update handled in websocket
        this.setIsEditing(false);
        this.dispatchEvent(todo.ui.Home.EventType.LOCAL_SUBMIT_EDIT_OK);
      }, this),
      /* method */ 'PUT',
      /* data */ goog.json.serialize({
        'content': this.editField_.getValue()
      }), {
        'content-type': 'application/json'
      });
      this.setEnabled(false);
    }, undefined, this);

  goog.events.listen(this.cancelEdit_,
    Component.EventType.ACTION,
    function() {
      this.setIsEditing(false);
      this.dispatchEvent(todo.ui.Home.EventType.LOCAL_CANCEL_EDIT);
    }, undefined, this);

  goog.events.listen(this.getParent(),
    todo.ui.Home.EventType.LOCAL_START_EDIT,
    function(e) {
      if (/** @type {todo.ui.Todo} */ (e.target) !== this) {
        this.setEnabled(false);
      }
    }, undefined, this);

  goog.events.listen(this.getParent(),
    [ todo.ui.Home.EventType.LOCAL_SUBMIT_EDIT_OK
    , todo.ui.Home.EventType.LOCAL_CANCEL_EDIT
    ], function(e) {
      this.setEnabled(true);
    }, undefined, this);
};

todo.ui.Todo.prototype.syncNewModel = function(newModel) {
  this.setModel(newModel);
  this.content_.getElement().innerHTML =
    todo.templates.showContent({
      content: this.getModel()['content']
    });
};

todo.ui.Todo.prototype.setIsEditing = function(editing) {
  this.edit_.setVisible(!editing);
  this.delete_.setVisible(!editing);
  this.content_.setVisible(!editing);
  this.editField_.setVisible(editing);
  this.submitEdit_.setVisible(editing);
  this.cancelEdit_.setVisible(editing);

  if (editing) {
    this.editField_.setValue(this.getModel()['content']);
  }
};

/**
 * @constructor
 * @extends {Container}
 */
todo.ui.CreateForm = function() {
  goog.base(this);

  this.contentInput_ = new Textarea('');
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
  var formEl = goog.dom.getElementByClass('submit-form', el);
  goog.events.listen(formEl,
    goog.events.EventType.SUBMIT,
    function(e) {
      e.preventDefault();
      XhrIo.send(todo.url.todoResource(), goog.bind(function(e) {
        // Dom insertion is handled by the websocket part.
        this.contentInput_.setValue('');
        this.setEnabled(true);
      }, this),
      /* method */ 'POST',
      /* data */ goog.json.serialize({
        '_id': null,
        'content': this.contentInput_.getValue()
      }), {
        'content-type': 'application/json'
      });
      this.setEnabled(false);
    }, undefined, this);
};

});  // !goog.scope

// vim: set ts=2 sts=2 sw=2:


