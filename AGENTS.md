# AGENTS

## 概览
- 本仓库是名为 UNICORN 的 Emacs 配置。
- 主要语言为 Emacs Lisp。
- 入口文件是 `early-init.el` 和 `init.el`。
- 模块位于 `lisp/`，命名为 `init-*.el`。
- `init.el` 逐一 `require` 各模块；新增模块需在此加入。
- 默认 LSP 是 Eglot；`lsp-mode` 有配置但未启用。
- 诊断默认使用 Flymake，而非 Flycheck。

## 仓库结构
- `lisp/` 存放功能模块（如 `init-ui.el`, `init-tool.el` 等）。
- `config_snippets/` 保存 Python 项目引导模板。
- `elpa/` 是 Emacs 包缓存目录，禁止手动修改。
- `custom.el` 是可选的用户状态文件，由 `init.el` 加载。
- `early-init.el` 负责 load-path 与原生编译设置。
- `history` 记录 Emacs 命令历史；代码变更应忽略。
- `imgs/` 存放 README 相关资源。

## 构建、检查与测试
- 仓库根目录没有 Makefile/package.json/测试运行器。
- 未发现 `.github/workflows` 下的 CI 配置。
- GUI 启动配置：`emacs`。
- TUI 启动配置：`emacs -nw`。
- 批量加载校验：`emacs -Q --batch -L . -l init.el`。
- 编译全部模块：`emacs -Q --batch -L lisp -f batch-byte-compile lisp/*.el`。
- 编译单个文件：`emacs -Q --batch -L lisp -f batch-byte-compile lisp/init-foo.el`。
- 运行全部 ERT 测试（若存在）：`emacs -Q --batch -L lisp -l path/to/tests.el -f ert-run-tests-batch-and-exit`。
- 运行单个 ERT 测试：`emacs -Q --batch -L lisp -l path/to/tests.el --eval "(ert-run-tests-batch-and-exit 'test-name)"`。
- 编辑器内诊断：`M-x flymake-show-buffer-diagnostics`（默认）。
- 可选 Flycheck 诊断 UI：`M-x flycheck-list-errors`（启用时）。
- Python lint（可选 CLI）：`ruff check path/to/file.py`。
- Python 格式化（可选 CLI）：`yapf -i path/to/file.py` 后接 `ruff format path/to/file.py`。
- JS/TS 格式化（可选 CLI）：`prettier --write path/to/file.ts`。

## 格式化与静态检查
- Apheleia 启动时全局启用；`C-c f` 格式化当前缓冲区。
- `M-f` 运行 `format-all-buffer` 格式化当前缓冲区。
- 行宽目标为 `fill-column` 120，并显示指示线。
- EditorConfig 模式已启用；若新增 `.editorconfig` 请遵守。
- Emacs Lisp 在 `emacs-lisp-mode` 中启用 `aggressive-indent`。
- Python `python-ts-mode` 先用 `yapf` 再用 `ruff`。
- Python `python-mode` 先用 `isort` 再用 `ruff`。
- Python 诊断在 Eglot 启用时使用 `flymake-ruff`。
- C/C++ 的 `c++-ts-mode` 使用 `clang-format`。
- C 的 `format-all` 格式化器为 `astyle --mode=c`。
- Shell 的 `format-all` 格式化器为 `shfmt -i 4 -ci`。
- JS/TS/Vue/JSON/Web 缓冲区使用 `prettier`。
- Prettier 参数包含 `--tab-width 4 --bracket-same-line true`。
- Go 在保存时运行 `gofmt`，格式化器为 `goimports`。

## Emacs Lisp 文件结构
- 保留 `;;;` 文件头与 `-*- lexical-binding: t; -*-`。
- 保留 `;;; Commentary:` 与 `;;; Code:` 分区。
- 配置优先使用 `use-package`。
- 内置包用 `:ensure nil`。
- 外部包用 `:ensure t` 或 `:quelpa`。
- 约定顺序为 `:init` -> `:hook` -> `:bind` -> `:config`。
- 避免在 `use-package` 之外加入大量顶层副作用。
- 每个模块末尾添加 `(provide 'init-<topic>)`。
- 保留 `;;; init-foo.el ends here` 结尾。
- 仅在需要计时输出时使用 `(mark-time-here)`/`get-time-diff`。

## 命名与组织
- 项目代码函数使用 `unicorn/` 前缀。
- 变量使用 `unicorn-` 前缀。
- OpenCode 集成使用 `pl/opencode-` 前缀。
- 文件名使用 kebab-case，模块命名为 `init-<topic>.el`。
- 交互命令必须包含 docstring 并调用 `(interactive)`。
- 面向用户的配置项使用 `defcustom`。

## 错误处理
- 交互式失败用 `user-error` 并给出清晰提示。
- 需要硬失败时使用 `error`。
- 可选依赖用 `executable-find` 或 `fboundp` 防护。
- 不要假设外部工具一定已安装。

## 导入与依赖
- 内部模块使用 `require` 从 `lisp/` 加载。
- `setq`/`setq-default` 放在相关 `use-package` 附近。
- 已启用 `use-package-always-defer`；依赖 hooks/commands 延迟加载。
- `quelpa` 可用于非 ELPA/MELPA 包。

## 类型与数据
- Emacs Lisp 为动态类型；在 docstring 中注明预期类型。
- 可变全局使用 `defvar`，常量使用 `defconst`。
- 避免全局状态，除非是用户选项或缓存。

## 语言相关说明
- Python 项目初始化使用 `pyrightconfig.json` 与 `.projectile`。
- `pyrightconfig.json` 模板在 `config_snippets/python/`。
- Python docstring 使用 `docstr` 的 NumPy 风格。
- Web/TS：`web-mode` 与 `typescript-ts-mode` 缩进为 2 空格。
- JSON 可使用 `prettier-js` 相关命令。
- Go 工具链支持 `golangci-lint`（安装时启用）。
- C/C++ LSP 期望 `build/compile_commands.json` 存在。

## LSP 与补全
- Eglot 服务映射包含 basedpyright、clangd、gopls、TypeScript 服务。
- Shell、YAML、Docker、CMake 的 LSP 也已配置。
- Pyright 诊断被过滤，Ruff 是主要 Python 诊断来源。
- `eglot-booster` 为可选项，需 `/usr/bin/` 下的二进制。
- `lsp-mode` 配置位于 `lisp/init-lsp.el`，但未加载。

## 外部工具
- `rg`/`ripgrep` 是首选项目搜索工具。
- `node`/`npm` 是 Prettier 与 TypeScript 服务器依赖。
- Python 工具期望 `basedpyright-langserver`、`ruff`、`yapf`、`isort`。
- Go 工具期望 `go`、`gopls`、`goimports`。
- C/C++ 工具期望 `clangd` 与 `clang-format`。
- Shell 工具期望 `bash-language-server` 与 `shfmt`。
- YAML 工具期望 `yaml-language-server`。

## 新增模块清单
- 新建 `lisp/init-foo.el` 并包含 header 与 lexical-binding。
- 仅在需要计时输出时添加 `(mark-time-here)`。
- 配置用 `use-package` 包裹。
- 结尾添加 `(provide 'init-foo)` 和 footer。
- 在 `init.el` 中按启动顺序 `require` 新模块。

## 常用按键
- `C-c f` 格式化当前缓冲区（apheleia）。
- `M-f` 格式化当前缓冲区（format-all）。
- Python 中 `C-c C-b` 运行当前文件。
- `C-c p` 打开 Projectile 命令。
- `<f2>` 打开 `init.el`。

## 编辑流程提示
- 新模块放在 `lisp/` 并在 `init.el` 中加载。
- 通用快捷键放在 `lisp/leader-key-binding.el`。
- 项目根与文件发现优先使用 `projectile`。
- 项目级搜索首选 `rg`/`ripgrep`。
- 未要求时不要改动 `elpa/` 包缓存。
- 避免提交用户机器状态文件（`custom.el`/`history`）。

## Cursor/Copilot 规则
- 未发现 `.cursor/rules`、`.cursorrules` 或 `.github/copilot-instructions.md`。

## 关注文件
- `init.el` 负责模块加载与启动顺序。
- `early-init.el` 管理 load-path 与原生编译。
- `lisp/init-tool.el` 配置格式化与编辑工具。
- `lisp/init-eglot.el` 配置 Eglot LSP。
- `lisp/init-flymake.el` 配置诊断。
- `lisp/init-lsp-python.el` 包含 Python 辅助与 Pyright 设置。
- `lisp/init-project.el` 设置 Projectile 默认行为。
- `lisp/init-ui.el` 管理 UI 与主题设置。

## 代理备注
- 修改应尽量小，并贴合现有模块边界。
- 新增依赖时在对应模块说明。
- 优先安全默认值，并对平台差异做防护。
- 除非已有内容需要，不要新增非 ASCII 字符。
