if ('undefined' === typeof VISITORCOUNT) {
    var VISITORCOUNT = 0;
};
function sendHtml(html) {
    __PS_MV_REG = [];
    return new self['Response'](html, { 'status' : 200, 'headers' : { 'content-type' : 'text/html' } });
};
function sendJson(jsonString) {
    __PS_MV_REG = [];
    return new self['Response'](jsonString, { 'status' : 200, 'headers' : { 'content-type' : 'application/json' } });
};
function substituteTemplate(template, placeholder, value) {
    var parts = template.split(placeholder);
    return parts.join(value);
};
function sendError(status, message) {
    __PS_MV_REG = [];
    return new self['Response'](message, { 'status' : status, 'headers' : { 'content-type' : 'text/plain' } });
};
function loadTemplate(templateName) {
    __PS_MV_REG = [];
    return self['ASSETS'].fetch(new self['Request']('https://example.com/' + templateName));
};
function handleRoot() {
    VISITORCOUNT += 1;
    __PS_MV_REG = [];
    return loadTemplate('home.html').then(function (templateResponse) {
        if (templateResponse.ok) {
            __PS_MV_REG = [];
            return templateResponse.text().then(function (template) {
                var html = substituteTemplate(template, '{{visitor_count}}', '' + VISITORCOUNT);
                __PS_MV_REG = [];
                return sendHtml(html);
            });
        } else {
            __PS_MV_REG = [];
            return sendError(500, 'Template not found');
        };
    });
};
function handleUser(username) {
    __PS_MV_REG = [];
    return loadTemplate('user.html').then(function (templateResponse) {
        if (templateResponse.ok) {
            __PS_MV_REG = [];
            return templateResponse.text().then(function (template) {
                var html = substituteTemplate(template, '{{username}}', username);
                var htmlWithAvatar = substituteTemplate(html, '{{username_initial}}', username.charAt(0));
                __PS_MV_REG = [];
                return sendHtml(htmlWithAvatar);
            });
        } else {
            __PS_MV_REG = [];
            return sendError(500, 'Template not found');
        };
    });
};
function handlePost(postId) {
    __PS_MV_REG = [];
    return loadTemplate('post.html').then(function (templateResponse) {
        if (templateResponse.ok) {
            __PS_MV_REG = [];
            return templateResponse.text().then(function (template) {
                var html = substituteTemplate(template, '{{post_id}}', postId);
                __PS_MV_REG = [];
                return sendHtml(html);
            });
        } else {
            __PS_MV_REG = [];
            return sendError(500, 'Template not found');
        };
    });
};
function handleApiStats() {
    var jsonResponse = '{\"visitor-count\":' + VISITORCOUNT + ',\"server\":\"Parenscript Cloudflare Worker\"' + ',\"timestamp\":' + self['Date'].now() + '}';
    __PS_MV_REG = [];
    return sendJson(jsonResponse);
};
if ('undefined' === typeof APIROUTES) {
    var APIROUTES = {  };
};
function initApiRoutes() {
    APIROUTES['GET /users'] = 'list-users';
    APIROUTES['POST /users'] = 'create-user';
    APIROUTES['GET /users/:id'] = 'get-user';
    APIROUTES['PUT /users/:id'] = 'update-user';
    return APIROUTES['DELETE /users/:id'] = 'delete-user';
};
function listUsers(request, params, body) {
    var users = [{ 'id' : 1,
                   'name' : 'Alice',
                   'email' : 'alice@example.com'
                 }, { 'id' : 2,
                      'name' : 'Bob',
                      'email' : 'bob@example.com'
                    }, { 'id' : 3,
                         'name' : 'Charlie',
                         'email' : 'charlie@example.com'
                       }];
    return { 'users' : users,
             'count' : users.length,
             'status' : 'success'
           };
};
function createUser(request, params, body) {
    if (body) {
        var newUser = { 'id' : 1 + self['Math'].floor(self['Math'].random() * 1000),
                        'name' : body['name'],
                        'email' : body['email'],
                        'created-at' : self['Date'].now()
                      };
        return { 'user' : newUser, 'status' : 'created' };
    } else {
        return { 'error' : 'Missing user data', 'status' : 'error' };
    };
};
function getUser(request, params, body) {
    var userId = params['id'];
    if (userId) {
        var user = { 'id' : self['parseInt'](userId),
                     'name' : 'Sample User',
                     'email' : 'user@example.com',
                     'created-at' : self['Date'].now()
                   };
        return { 'user' : user, 'status' : 'success' };
    } else {
        return { 'error' : 'User ID required', 'status' : 'error' };
    };
};
function updateUser(request, params, body) {
    var userId = params['id'];
    if (userId && body) {
        var updatedUser = { 'id' : self['parseInt'](userId),
                            'name' : body['name'],
                            'email' : body['email'],
                            'updated-at' : self['Date'].now()
                          };
        return { 'user' : updatedUser, 'status' : 'updated' };
    } else {
        return { 'error' : 'User ID and data required', 'status' : 'error' };
    };
};
function deleteUser(request, params, body) {
    var userId = params['id'];
    return userId ? { 'message' : 'User ' + userId + ' deleted', 'status' : 'deleted' } : { 'error' : 'User ID required', 'status' : 'error' };
};
function listUsers(request, params, body) {
    var users = [{ 'id' : 1,
                   'name' : 'Alice',
                   'email' : 'alice@example.com'
                 }, { 'id' : 2,
                      'name' : 'Bob',
                      'email' : 'bob@example.com'
                    }, { 'id' : 3,
                         'name' : 'Charlie',
                         'email' : 'charlie@example.com'
                       }];
    return { 'users' : users,
             'count' : users.length,
             'status' : 'success'
           };
};
function createUser(request, params, body) {
    if (body) {
        var newUser = { 'id' : 1 + self['Math'].floor(self['Math'].random() * 1000),
                        'name' : body['name'],
                        'email' : body['email'],
                        'created-at' : self['Date'].now()
                      };
        return { 'user' : newUser, 'status' : 'created' };
    } else {
        return { 'error' : 'Missing user data', 'status' : 'error' };
    };
};
function getUser(request, params, body) {
    var userId = params['id'];
    if (userId) {
        var user = { 'id' : self['parseInt'](userId),
                     'name' : 'Sample User',
                     'email' : 'user@example.com',
                     'created-at' : self['Date'].now()
                   };
        return { 'user' : user, 'status' : 'success' };
    } else {
        return { 'error' : 'User ID required', 'status' : 'error' };
    };
};
function updateUser(request, params, body) {
    var userId = params['id'];
    if (userId && body) {
        var updatedUser = { 'id' : self['parseInt'](userId),
                            'name' : body['name'],
                            'email' : body['email'],
                            'updated-at' : self['Date'].now()
                          };
        return { 'user' : updatedUser, 'status' : 'updated' };
    } else {
        return { 'error' : 'User ID and data required', 'status' : 'error' };
    };
};
function deleteUser(request, params, body) {
    var userId = params['id'];
    return userId ? { 'message' : 'User ' + userId + ' deleted', 'status' : 'deleted' } : { 'error' : 'User ID required', 'status' : 'error' };
};
function handleApiRequest(request) {
    var url1 = new self['URL'](request.url);
    var pathname2 = url1.pathname;
    var method3 = request.method;
    var routeKey = method3 + ' ' + pathname2;
    var handler = APIROUTES.routeKey;
    if (handler) {
        var result = self.handler(request, {  }, member(method3, ['POST', 'PUT', 'PATCH']) ? request.json().then(function (json) {
            return json;
        }) : null);
        __PS_MV_REG = [];
        return typeof result === 'string' ? sendJson(result) : sendJson(self['JSON'].stringify(result));
    } else {
        __PS_MV_REG = [];
        return sendError(404, 'API endpoint not found');
    };
};
function handleRequest(request) {
    var pathname4 = (new self['URL'](request.url)).pathname;
    if (pathname4 === '/') {
        __PS_MV_REG = [];
        return handleRoot();
    } else if (pathname4.length > 4 && pathname4.substring(0, 4) === '/api') {
        __PS_MV_REG = [];
        return handleApiRequest(request);
    } else if (pathname4.length > 6 && pathname4.substring(0, 6) === '/user/') {
        var username = pathname4.substring(6);
        __PS_MV_REG = [];
        return handleUser(username);
    } else if (pathname4.length > 6 && pathname4.substring(0, 6) === '/post/') {
        var postId = pathname4.substring(6);
        __PS_MV_REG = [];
        return handlePost(postId);
    } else if (pathname4 === '/api/stats') {
        __PS_MV_REG = [];
        return handleApiStats();
    } else {
        __PS_MV_REG = [];
        return sendError(404, 'Not Found');
    };
};
initApiRoutes();
self['addEventListener']('fetch', function (event) {
    __PS_MV_REG = [];
    return event['respondWith'](handleRequest(event.request));
});