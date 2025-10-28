# Parenscript Web Server

A Node.js web server generated entirely using Parenscript from Common Lisp code. This demonstrates how Parenscript can be used to generate complete JavaScript applications, including server-side code.

## Features

- **Complete Node.js Server**: Generated entirely from Parenscript
- **HTTP Server**: Handles HTTP requests and responses
- **HTML Content**: Serves web pages with proper headers
- **Build System**: Automated generation from Lisp code

## Dependencies

- **Parenscript**: For JavaScript generation
- **Node.js**: To run the generated server
- **Common Lisp**: SBCL or similar implementation

## Quick Start

1. **Install Dependencies**:
   ```bash
   # Install Node.js (if not already installed)
   # Install Quicklisp and Parenscript in your Lisp environment
   ```

2. **Generate the Server**:
   ```bash
   sbcl --script build.lisp
   ```

3. **Run the Server**:
   ```bash
   node server.js
   ```

4. **Visit the Application**:
   Open your browser and go to `http://localhost:3000`

## Project Structure

- `servers.lisp` - Parenscript code that generates both servers
- `build.lisp` - Single build script to generate all JavaScript
- `package.lisp` - Package definition
- `server.js` - Generated basic Node.js server (created by build)
- `dynamic-server.js` - Generated dynamic Node.js server (created by build)
- `README.md` - This documentation

## How it Works

1. **Parenscript Generation**: Common Lisp code using Parenscript macros generates JavaScript
2. **Node.js Server**: The generated JavaScript creates a complete HTTP server
3. **HTTP Handling**: Server responds to requests with HTML content
4. **Build Process**: Automated generation from Lisp source

## Example Usage

```lisp
;; Generate all servers
(build-all)

;; Or generate individually
(generate-basic-server)
(generate-dynamic-server)
```

## Building

Simply run the build script:
```bash
sbcl --script build.lisp
```

This will generate `server.js` which you can then run with Node.js.

## Architecture

The project demonstrates:
- **Code Generation**: Using Parenscript to generate JavaScript from Lisp
- **Server-side JavaScript**: Creating Node.js applications with Parenscript
- **HTTP Server**: Complete web server functionality
- **Build Automation**: Simple build process for code generation
