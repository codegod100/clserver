# Parenscript Cloudflare Worker

A dynamic Cloudflare Worker with routing generated using Parenscript from Common Lisp.

## Features

- **Lisp-Powered API Generation**: `defapi` macro generates complete CRUD endpoints
- **Dynamic Routing**: Handles `/user/{username}`, `/post/{id}`, and API endpoints
- **Parameter Extraction**: Automatic URL parameter parsing (e.g., `/users/:id`)
- **JSON API**: Full REST API with automatic JSON serialization
- **Static Assets**: Serves HTML templates as static assets for better performance
- **Template Processing**: Simple string substitution for dynamic content
- **Code Generation**: Generates JavaScript from Common Lisp using Parenscript
- **Cloudflare Workers**: Runs on Cloudflare's edge network for global performance

## Project Structure

- `worker.lisp` - Main Parenscript code that generates the Cloudflare Worker
- `build.lisp` - Build script that loads dependencies and generates the worker
- `package.lisp` - Package definition for the clserver package
- `wrangler.toml` - Cloudflare Worker configuration
- `package.json` - Node.js package configuration
- `worker.js` - Generated Cloudflare Worker (created by build)
- `assets/` - Static HTML templates served as assets

## Development

### Prerequisites

- Common Lisp (SBCL recommended)
- Quicklisp
- Node.js and npm/pnpm
- Cloudflare account (for deployment)

### Setup

1. Install dependencies:
```bash
pnpm install
```

2. Build the worker:
```bash
sbcl --script build.lisp
```

3. Run locally:
```bash
pnpm run dev
```

### Deployment

1. Install Wrangler CLI:
```bash
npm install -g wrangler
```

2. Login to Cloudflare:
```bash
wrangler login
```

3. Deploy:
```bash
pnpm run deploy
```

## Routes

### Web Pages
- `/` - Home page with visitor counter and API testing interface
- `/user/{username}` - User profile page
- `/post/{id}` - Blog post page

### API Endpoints
- `/api/users` - List all users (GET)
- `/api/users` - Create new user (POST)
- `/api/users/{id}` - Get user by ID (GET)
- `/api/users/{id}` - Update user (PUT)
- `/api/users/{id}` - Delete user (DELETE)
- `/api/stats` - JSON API with statistics

## Architecture

- **Code Generation**: Parenscript macros generate JavaScript from Common Lisp
- **Static Assets**: HTML templates served as static assets for optimal performance
- **Template Processing**: Simple string substitution for dynamic content
- **Edge Computing**: Runs on Cloudflare's global network
- **Serverless**: No server management required

## Build Process

1. Run `sbcl --script build.lisp` to generate JavaScript from Lisp
2. Generated file: `worker.js`
3. Deploy with `wrangler deploy` or test locally with `wrangler dev`

## License

MIT