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
    var jsonResponse = '{\"visitor-count\":' + VISITORCOUNT + ',\"server\":\"Parenscript Cloudflare Worker\"' + ',\"timestamp\":' + date.now() + '}';
    __PS_MV_REG = [];
    return sendJson(jsonResponse);
};
function handleRequest(request) {
    var pathname1 = (new self['URL'](request.url)).pathname;
    if (pathname1 === '/') {
        __PS_MV_REG = [];
        return handleRoot();
    } else if (pathname1.length > 6 && pathname1.substring(0, 6) === '/user/') {
        var username = pathname1.substring(6);
        __PS_MV_REG = [];
        return handleUser(username);
    } else if (pathname1.length > 6 && pathname1.substring(0, 6) === '/post/') {
        var postId = pathname1.substring(6);
        __PS_MV_REG = [];
        return handlePost(postId);
    } else if (pathname1 === '/api/stats') {
        __PS_MV_REG = [];
        return handleApiStats();
    } else {
        __PS_MV_REG = [];
        return sendError(404, 'Not Found');
    };
};
self['addEventListener']('fetch', function (event) {
    __PS_MV_REG = [];
    return event['respondWith'](handleRequest(event.request));
});