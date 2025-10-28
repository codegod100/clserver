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
    APIROUTES['GET /api/users'] = 'list-users';
    APIROUTES['POST /api/users'] = 'create-user';
    APIROUTES['GET /api/users/:id'] = 'get-user';
    APIROUTES['PUT /api/users/:id'] = 'update-user';
    return APIROUTES['DELETE /api/users/:id'] = 'delete-user';
};
function pathMatchesPattern(pattern, path) {
    var patternParts = pattern.split('/');
    var pathParts = path.split('/');
    var matches = true;
    if (patternParts.length === pathParts.length) {
        var _js2 = patternParts.length;
        for (var i = 0; i < _js2; i += 1) {
            var patternPart = patternParts[i];
            var pathPart = pathParts[i];
            if (!(patternPart.length > 1 && patternPart.charAt(0) === ':' || patternPart === pathPart)) {
                matches = false;
            };
        };
        return matches;
    };
};
function extractUrlParams(pattern, path) {
    var patternParts = pattern.split('/');
    var pathParts = path.split('/');
    var params = {  };
    if (patternParts.length === pathParts.length) {
        var _js4 = patternParts.length;
        for (var i = 0; i < _js4; i += 1) {
            var patternPart = patternParts[i];
            var pathPart = pathParts[i];
            if (patternPart.length > 1 && patternPart.charAt(0) === ':') {
                params[patternPart.substring(1)] = pathPart;
            };
        };
        return params;
    };
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
    var url5 = new self['URL'](request.url);
    var pathname6 = url5.pathname;
    var method7 = request.method;
    var routeKey = method7 + ' ' + pathname6;
    var handler = APIROUTES[routeKey];
    if (handler) {
        if (handler === 'list-users') {
            var result = listUsers(request, {  }, null);
            __PS_MV_REG = [];
            return typeof result === 'string' ? sendJson(result) : sendJson(self['JSON'].stringify(result));
        } else if (handler === 'create-user') {
            var result8 = createUser(request, {  }, null);
            __PS_MV_REG = [];
            return typeof result8 === 'string' ? sendJson(result8) : sendJson(self['JSON'].stringify(result8));
        } else if (handler === 'get-user') {
            var result9 = getUser(request, {  }, null);
            __PS_MV_REG = [];
            return typeof result9 === 'string' ? sendJson(result9) : sendJson(self['JSON'].stringify(result9));
        } else if (handler === 'update-user') {
            var result10 = updateUser(request, {  }, null);
            __PS_MV_REG = [];
            return typeof result10 === 'string' ? sendJson(result10) : sendJson(self['JSON'].stringify(result10));
        } else if (handler === 'delete-user') {
            var result11 = deleteUser(request, {  }, null);
            __PS_MV_REG = [];
            return typeof result11 === 'string' ? sendJson(result11) : sendJson(self['JSON'].stringify(result11));
        } else {
            __PS_MV_REG = [];
            return sendError(404, 'API endpoint not found');
        };
    } else {
        var foundHandler = null;
        var foundParams = {  };
        nilBlock: {
            var _js12 = self['Object'].keys(APIROUTES);
            var _js14 = _js12.length;
            for (var _js13 = 0; _js13 < _js14; _js13 += 1) {
                var routeKey15 = _js12[_js13];
                var routeParts = routeKey15.split(' ');
                if (routeParts.length === 2 && routeParts[0] === method7) {
                    var pattern = routeParts[1];
                    if (pathMatchesPattern(pattern, pathname6)) {
                        foundHandler = APIROUTES[routeKey15];
                        foundParams = extractUrlParams(pattern, pathname6);
                        __PS_MV_REG = [];
                        break nilBlock;
                    };
                };
            };
        };
        if (foundHandler) {
            if (foundHandler === 'get-user') {
                var result15 = getUser(request, foundParams, null);
                __PS_MV_REG = [];
                return typeof result15 === 'string' ? sendJson(result15) : sendJson(self['JSON'].stringify(result15));
            } else if (foundHandler === 'update-user') {
                var body = (method7 === 'POST')(method7, ['POST', 'PUT', 'PATCH']) ? request.json().then(function (json) {
                    return json;
                }) : null;
                var result16 = updateUser(request, foundParams, body);
                __PS_MV_REG = [];
                return typeof result16 === 'string' ? sendJson(result16) : sendJson(self['JSON'].stringify(result16));
            } else if (foundHandler === 'delete-user') {
                var result17 = deleteUser(request, foundParams, null);
                __PS_MV_REG = [];
                return typeof result17 === 'string' ? sendJson(result17) : sendJson(self['JSON'].stringify(result17));
            } else {
                __PS_MV_REG = [];
                return sendError(404, 'API endpoint not found');
            };
        } else {
            __PS_MV_REG = [];
            return sendError(404, 'API endpoint not found');
        };
    };
};
function handleRequest(request) {
    var pathname18 = (new self['URL'](request.url)).pathname;
    if (pathname18 === '/') {
        __PS_MV_REG = [];
        return handleRoot();
    } else if (pathname18.length > 4 && pathname18.substring(0, 4) === '/api') {
        __PS_MV_REG = [];
        return handleApiRequest(request);
    } else if (pathname18.length > 6 && pathname18.substring(0, 6) === '/user/') {
        var username = pathname18.substring(6);
        __PS_MV_REG = [];
        return handleUser(username);
    } else if (pathname18.length > 6 && pathname18.substring(0, 6) === '/post/') {
        var postId = pathname18.substring(6);
        __PS_MV_REG = [];
        return handlePost(postId);
    } else if (pathname18 === '/api/stats') {
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