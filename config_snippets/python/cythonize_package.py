# -*- coding: utf-8 -*-

import os, sys, shutil
from setuptools import setup, Extension
from Cython.Build import cythonize
from sysconfig import get_config_var

# 源与目标
resource_directory = r"../src/deeptrainer/"
build_directory    = r"./deeptrainer"

# 规则
EXCLUDE_COMPILE = {"__init__.py"}
EXCLUDE_COPY    = (".git", ".svn", ".idea", ".vscode", "__pycache__", "setup.py")
RESOURCE_SUFFIX = (".py", ".pyx")

# 中间产物目录：绑定到 build_directory 下
TEMP_DIR        = os.path.join(build_directory, "build")

# 整个目录名跳过（当作不存在）
SKIP_DIRS = {"test"}
_env = os.getenv("CY_SKIP_DIRS")
if _env:
    SKIP_DIRS = {s.strip() for s in _env.split(",") if s.strip()}

all_files = []
EXT_SUFFIX = get_config_var("EXT_SUFFIX") or ".so"

def _normalize_answer(s: str) -> str:
    s = (s or "").strip().lower()
    for ch in ":：。.!！?？,，；; ":
        s = s.rstrip(ch)
    return s

def _remove_egg_info(base_dir: str, proj_name: str):
    # 删除同级的 proj_name.egg-info；若存在其他 *.egg-info 也一并清理
    target = os.path.join(base_dir, f"{proj_name}.egg-info")
    if os.path.isdir(target):
        shutil.rmtree(target)
        print(f"已删除 {target}")
    # 顺带清理同级其它 egg-info（可按需保留）
    for name in os.listdir(base_dir):
        if name.endswith(".egg-info"):
            path = os.path.join(base_dir, name)
            try:
                shutil.rmtree(path)
                print(f"已删除 {path}")
            except Exception:
                pass

def confirm_delete_build_dir():
    if not os.path.exists(build_directory):
        return
    if os.getenv("CY_FORCE_YES", "").strip() in ("1", "true", "True"):
        shutil.rmtree(build_directory)
        _remove_egg_info(os.path.dirname(build_directory), os.path.basename(build_directory))
        print("已删除旧的编译文件夹（CY_FORCE_YES=1）。")
        return
    ans = input(f"检测到已存在编译文件夹：{build_directory}\n是否需要删除原编译文件夹？[y/N]: ")
    ans = _normalize_answer(ans)
    if ans in ("y", "yes", "是", "shi"):
        shutil.rmtree(build_directory)
        _remove_egg_info(os.path.dirname(build_directory), os.path.basename(build_directory))
        if os.path.exists(build_directory):
            raise RuntimeError(f"删除失败：仍存在 {build_directory}")
        print("已删除旧的编译文件夹。")
    else:
        print("已取消。"); sys.exit(0)

def _ignore_names(src_dir, names):
    ignore = set(EXCLUDE_COPY)
    ignore.update({n for n in names if n in SKIP_DIRS})
    return ignore

def copy_project():
    shutil.copytree(resource_directory, build_directory, ignore=_ignore_names)

def walk_sources(root, out_list):
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d not in SKIP_DIRS]
        for fn in filenames:
            if fn.endswith(RESOURCE_SUFFIX) and fn not in EXCLUDE_COMPILE:
                out_list.append(os.path.join(dirpath, fn))

def rel_from_build(path):
    return os.path.relpath(path, build_directory)

def to_module_name(rel_path):
    return os.path.splitext(rel_path)[0].replace(os.sep, ".")

def compiled_exists(py_path):
    stem = os.path.splitext(py_path)[0]
    d = os.path.dirname(py_path)
    base = os.path.basename(stem)
    try:
        for name in os.listdir(d):
            if name.startswith(base) and name.endswith(EXT_SUFFIX):
                return True
    except FileNotFoundError:
        return False
    return False

def cleanup_after_build(root):
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d not in SKIP_DIRS]
        for fn in filenames:
            if fn == "__init__.py":
                continue
            p = os.path.join(dirpath, fn)
            if fn.endswith(".py"):
                if compiled_exists(p):
                    try: os.remove(p)
                    except FileNotFoundError: pass
                continue
            if fn.endswith((".c", ".html")):
                try: os.remove(p)
                except FileNotFoundError: pass

def ensure_inits(src_root, dst_root):
    for dirpath, dirnames, filenames in os.walk(dst_root):
        dirnames[:] = [d for d in dirnames if d not in SKIP_DIRS]
        has_pkg = any(
            f.endswith(EXT_SUFFIX) or f.endswith((".py", ".pyx")) for f in filenames
        ) or bool(dirnames)
        if not has_pkg:
            continue
        init_dst = os.path.join(dirpath, "__init__.py")
        if os.path.exists(init_dst):
            continue
        rel = os.path.relpath(dirpath, dst_root)
        if any(part in SKIP_DIRS for part in rel.split(os.sep)):
            continue
        init_src = os.path.join(src_root, rel, "__init__.py")
        os.makedirs(dirpath, exist_ok=True)
        if os.path.exists(init_src):
            shutil.copy2(init_src, init_dst)
        else:
            open(init_dst, "a").close()

def write_list(paths, fp):
    with open(fp, "w", encoding="utf-8") as f:
        for p in paths: f.write(p + "\n")

if __name__ == "__main__":
    assert os.path.abspath(resource_directory) != os.path.abspath(build_directory)

    confirm_delete_build_dir()
    copy_project()
    walk_sources(build_directory, all_files)

    rel_files = [rel_from_build(p) for p in all_files if os.path.isfile(p)]
    rel_files = [p for p in rel_files if not p.startswith("..")]

    exts = [Extension(name=to_module_name(p), sources=[p]) for p in rel_files]

    cwd = os.getcwd()
    os.chdir(build_directory)
    try:
        setup(
            ext_modules=cythonize(
                exts,
                annotate=True,
                language_level=3,
                build_dir=os.path.abspath(TEMP_DIR),
            ),
            script_args=[
                "build_ext", "--inplace",
                "--build-temp", os.path.abspath(TEMP_DIR),
            ],
            options={"build_ext": {"inplace": True}},
        )
    finally:
        os.chdir(cwd)

    write_list(all_files, "./module_extensions.txt")

    if os.path.isdir(TEMP_DIR):
        shutil.rmtree(TEMP_DIR)

    cleanup_after_build(build_directory)
    ensure_inits(resource_directory, build_directory)
