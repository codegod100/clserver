# Cursor Rules for Parenscript Cloudflare Worker Project

## HTML Generation Guidelines

### Use Templating, Not Hardcoded HTML
- **NEVER** use hardcoded HTML strings in Parenscript code
- **ALWAYS** use proper templating systems for HTML generation
- **PREFER** template functions that separate HTML structure from data
- **USE** template variables and interpolation instead of string concatenation

### Template Structure
```lisp
(defun render-template (template-name data)
  "Render a template with the given data"
  (let ((template (load-template template-name)))
    (substitute-template-variables template data)))
```

### HTML Best Practices
- Include proper DOCTYPE and meta tags
- Use semantic HTML structure
- Separate CSS into external stylesheets when possible
- Use template inheritance for common layouts
- Validate HTML structure and accessibility

### Example Template Usage
```lisp
;; Good: Using template function
(let ((html (render-template "api-demo.html" 
                            (create :visitor-count *visitor-count*
                                    :origin origin))))
  (send-html html))

;; Bad: Hardcoded HTML strings
(let ((html (+ "<!DOCTYPE html><html>...")))
  (send-html html))
```

## Lisp Coding Standards

### Parenscript Integration
- Always use `(in-package #:clserver)` at the top of Lisp files
- Use `(ps ...)` macro for JavaScript generation
- Export functions with `(:export #:function-name)` in package definition

### Code Organization
- Define helper functions before main generation functions
- Use descriptive function names like `generate-basic-server` and `generate-dynamic-server`
- Include proper documentation strings with `"Description of function"`
- Use `format t` for build output messages

### Syntax Validation
- **Always use SBCL for Lisp file manipulation** to avoid syntax errors
- Use `sbcl --script` to validate Lisp syntax before making changes
- Test syntax with `sbcl --eval "(load \"servers.lisp\")"` to catch unbalanced parentheses
- **Never manually edit Lisp files** without syntax validation
- Use Parenscript's built-in validation when possible

### Parenscript Patterns
- Use `(ps ...)` for JavaScript code generation
- Access JavaScript object properties with `(@ object property)`
- Use `(str ...)` for string concatenation in generated HTML
- Define JavaScript variables with `(defvar *var-name* value)`
- Use `(lambda () ...)` for JavaScript callbacks

### File Output
- Use `(with-open-file)` for writing generated JavaScript files
- Set `:if-exists :supersede` to overwrite existing files
- Always close file streams properly

### Editing Workflow
- **Prefer SBCL-driven edits**: Plan code changes as forms that SBCL could evaluate (e.g. use `with-open-file`, `format`, `replace-all`) instead of purely manual text edits
- **Leverage Lisp abstractions**: Express transformations via macros/functions; reason about changes as if running SBCL helper utilities
- **Maintain structural integrity**: Ensure every edit preserves balanced parentheses and valid forms by thinking in terms of evaluated expressions
- **Validate after conceptual changes**: Assume SBCL syntax validation (`sbcl --eval`, scripted helpers) runs after each change
