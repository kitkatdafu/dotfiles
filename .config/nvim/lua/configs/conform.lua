local options = {
  formatters_by_ft = {
    lua = { "stylua" },
    python = { "ruff_check", "ruff_organize_imports", "ruff_format" },
  },
  formatters = {
    ruff_check = {
      command = "ruff",
      args = { "check", "--fix", "--stdin-filename", "$FILENAME", "-" },
    },
  },
  format_on_save = {
    -- These options will be passed to conform.format()
    timeout_ms = 500,
    lsp_fallback = true,
  },
}

return options
