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
2. Start Serena MCP server using the secure shell scripts:
   ```bash
   # Use the provided secure script
   .serena/scripts/serena-start-mcp.sh
   ```
3. Use Serena's tools for efficient code navigation and editing
4. Run `stack build` or `stack test` to verify changes

## Available Scripts
All Serena operations are provided through secure shell scripts:
- `.serena/scripts/serena-start-mcp.sh` - Start MCP server
- `.serena/scripts/serena-with-haskell.sh` - Start with planning mode
- `.serena/scripts/serena-activate-project.sh` - Activate project
- `.serena/scripts/serena-index-project.sh` - Index project for performance

Each script supports `--help` for detailed usage information and `--verbose` for detailed output.

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
- Scripts use secure `source` command with proper error handling instead of `eval`
- No hardcoded fallback paths - proper error messages guide configuration setup
- All scripts follow bash security best practices with `set -euo pipefail`
