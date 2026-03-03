# tree-sitter-hurl

Tree-sitter grammar for the [hurl](https://github.com/your-org/hurl) HTTP scripting language.
Provides syntax highlighting for `.hurl` files in Neovim (0.9+) and any other
editor that supports Tree-sitter.

## Prerequisites

- [Neovim](https://neovim.io/) ≥ 0.9
- [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter) plugin
- [tree-sitter CLI](https://github.com/tree-sitter/tree-sitter/blob/master/cli/README.md) (for generating the parser from source)
- Node.js and a C compiler (for building the parser)

Install the tree-sitter CLI:

```bash
npm install -g tree-sitter-cli
```

## Generating the Parser

From the `vim/` directory:

```bash
cd vim
tree-sitter generate
```

This reads `grammar.js` and produces `src/parser.c` (and supporting files).
Run this whenever `grammar.js` changes.

## Installation

### Option A: nvim-treesitter (recommended)

Add the following to your Neovim config (Lua):

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.hurl = {
  install_info = {
    -- path to this vim/ directory (absolute or relative to your config)
    url = "/path/to/hurl/vim",
    files = { "src/parser.c" },
    branch = "main",
    generate_requires_npm = false,
    requires_generate_from_grammar = true,
  },
  filetype = "hurl",
}
```

Then install the parser from Neovim:

```vim
:TSInstall hurl
```

Or via Lua:

```lua
require("nvim-treesitter.install").install("hurl")
```

Copy the query files so nvim-treesitter can find them:

```bash
mkdir -p ~/.config/nvim/queries/hurl
cp vim/queries/*.scm ~/.config/nvim/queries/hurl/
```

### Option B: Manual installation

1. Generate and compile the parser:

```bash
cd vim
tree-sitter generate
gcc -shared -fPIC -o hurl.so src/parser.c -I./src
```

2. Register the parser and copy queries in your Neovim config:

```lua
vim.treesitter.language.add("hurl", { path = "/absolute/path/to/hurl.so" })
```

```bash
mkdir -p ~/.config/nvim/queries/hurl
cp vim/queries/*.scm ~/.config/nvim/queries/hurl/
```

## Filetype Detection

Neovim should detect `.hurl` files automatically. If it does not, add this to
your config:

```lua
vim.filetype.add({ extension = { hurl = "hurl" } })
```

## Verification

```bash
# Parse an example file — output should show no ERROR nodes
cd vim
tree-sitter parse ../examples/post.hurl
```

In Neovim, open a `.hurl` file and run `:TSPlaygroundToggle` (requires
`nvim-treesitter/playground`) to inspect the parse tree and confirm
highlighting is applied.

## JSON Injection

Content inside `json { { … } }` blocks is highlighted using the built-in JSON
grammar via `queries/injections.scm`. This requires `tree-sitter-json` to be
available, which nvim-treesitter installs by default when you run
`:TSInstall json`.
