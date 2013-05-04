goog.provide('todo');
goog.provide('todo.conf');

/** @define {string} */
todo.conf.DOMAIN_NAME = '//localhost:9000';

/** @type {string} */
todo.conf.WS_BIND_ADDR = 'ws:' + todo.conf.DOMAIN_NAME;

/** @define {string} */
todo.conf.JSON_XSS_PREFIX = 'for(;;){}';

