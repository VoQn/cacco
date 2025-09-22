# Haskell LSP Setup for Serena

## Language Server Configuration
This project uses a **patched version of Serena** that supports Haskell Language Server (HLS) for semantic code analysis. The standard Serena distribution does not include HLS support, so a local patched repository is required.

HLS provides:
- Type checking and inference
- Code navigation (find definitions, references)
- Refactoring capabilities
- Error diagnostics

## Serena Integration
Serena leverages HLS through its LSP integration to provide:
- Symbolic code retrieval (`find_symbol`, `find_referencing_symbols`)
- Precise code editing (`replace_symbol_body`, `insert_after_symbol`)
- Type-aware refactoring

## Local Repository Setup
The local patched Serena repository path is configured in `.serena/.serenarc` (gitignored for security). The configuration uses environment variable format:

```bash
# .serena/.serenarc example:
SERENA_ROOT=/path/to/local/serena-repo
```

## Recommended Workflow
1. Ensure the local patched Serena repository is available at the path specified in `.serena/.serenarc`
2. Start Serena MCP server using the local repository:
   ```bash
   # Load Serena configuration safely
   set -a
   source .serena/.serenarc 2>/dev/null || echo "Warning: .serenarc not found. Please set SERENA_ROOT."
   set +a

   # Get current project directory before changing directory
   PROJECT_DIR=$(pwd)

   # Navigate to Serena repository and start MCP server
   if [ -n "${SERENA_ROOT}" ] && [ -d "${SERENA_ROOT}" ]; then
       cd "${SERENA_ROOT}" && uv run serena start-mcp-server --context ide-assistant --project "$PROJECT_DIR"
   else
       echo "Error: SERENA_ROOT not set or directory does not exist. Please configure .serenarc."
       exit 1
   fi
   ```
3. Use Serena's tools for efficient code navigation and editing
4. Run `stack build` or `stack test` to verify changes

## Project-Specific Notes
- This is a Stack-based project using `stack.yaml`
- Module structure follows standard Haskell conventions
- Type safety is enforced through explicit annotations
- Test-driven development is encouraged

## Performance Tips
- Index the project with `serena project index` for faster tool performance
- Use `planning` mode for complex refactoring tasks
- Leverage Serena's symbolic tools instead of text-based searches

## Security Note
- Local repository paths are stored in `.serena/.serenarc` (gitignored) to prevent path leakage
- Environment variable format allows for easy extension and shell integration
- Commands load configuration using `eval $(cat .serena/.serenarc ...)` for secure path resolution
- Fallback paths are provided for cases where the configuration is not available
