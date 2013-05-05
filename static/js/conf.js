goog.provide('todo');
goog.provide('todo.conf');

/** @type {string} */
todo.conf.DOMAIN_NAME = '//' + location.hostname + ':' + location.port;

todo.conf.IS_RHCLOUD = /rhcloud/.test(location.hostname);

/**
 * On development server we are duplexing the ws on port 80, while on the rhcloud
 * we need to connect to 8000 (8443 for wss).
 * @type {string}
 */
todo.conf.WS_BIND_ADDR = todo.conf.IS_RHCLOUD ?
                         'ws://' + location.hostname + ':8000' :
                         'ws:' + todo.conf.DOMAIN_NAME;

/** @define {string} */
todo.conf.JSON_XSS_PREFIX = 'for(;;){}';

