# -*- coding: utf-8 -*-
# ref: http://90codes.com/course/python-course/41227.html

import sys

if "build_ext" not in sys.argv:
    sys.argv.append("build_ext")
import os
from distutils.core import setup
from Cython.Build import cythonize
import shutil

# 需要编译的项目地址
resource_directory = r"./AINLP_test"
# 编译后存放so/pyd文件的项目地址
build_directory = r"./deeptrainer"
# 不编译的文件，例如程序入口
exclude_build = ["__init__.py", "main.py"]
# 不需要复制的文件，例如缓存
exclude_copy = (".git", ".svn", ".idea", ".vscode", "__pycache__", "setup.py")
# 需要编译的资源文件，一般为py和pyx
resource_suffix = (".py", ".pyx")
# 存放编译文件路径
temp_directory = "./build"
# 存放加载的路径
extensions = []


def copy_project():
    """
    将当前项目复制到目标编译文件夹
    """
    if os.path.exists(build_directory):
        # 如果目标路径存在就先删除
        if input("目标文件夹已存在，是否删除?:（y删除/orther退出）\n").lower() == "y":
            shutil.rmtree(build_directory)
        else:
            sys.exit(0)
    shutil.copytree(
        resource_directory,
        build_directory,
        ignore=shutil.ignore_patterns(*exclude_copy),
    )


def load_project_resource(dir_path, extensions):
    """
    加载项目或文件夹下所有py和pyx文件
    """
    dir_path = os.path.abspath(dir_path)
    for file_name in os.listdir(dir_path):
        new_path = os.path.join(dir_path, file_name)
        if os.path.isdir(new_path):
            load_project_resource(new_path, extensions)
        elif file_name.endswith(resource_suffix) and file_name not in exclude_build:
            # 加载所有需要编译的资源文件
            extensions.append(new_path)


def remove_resource(dir_path):
    """
    编译完毕后，删除py和c文件
    """
    for file_name in os.listdir(dir_path):
        new_path = os.path.join(dir_path, file_name)
        if os.path.isdir(new_path):
            remove_resource(new_path)
        elif (
            file_name.endswith((".py", ".c", ".html"))
            and os.path.abspath(resource_directory) != os.path.abspath(build_directory)
            and file_name not in exclude_build
        ):
            os.remove(new_path)


def build(extensions):
    """
    开始编译文件，为了保证输出文件夹需要单个文件循环编译
    """
    for file_path in extensions:
        setup(
            ext_modules=cythonize(
                file_path,  # 直接是list对象，可以用多核加速 #
                annotate=True,  # 生成html，用来观察代码是否加速
                language_level=3,  # Python 3
                # nthreads=nthreads,  # 多核编译，因为每次只有一个文件，实际上无法加速
                build_dir=temp_directory,  # 编译的文件夹
            ),
            # 将编译好的pyd文件放在python文件目录下
            options={
                "build": {
                    "build_lib": "./",
                }
            },
        )


if __name__ == "__main__":
    assert os.path.abspath(resource_directory) != os.path.abspath(
        build_directory
    ), "为了防止出错，项目文件夹不能和编译文件夹一致"  # 复制项目到编译文件夹
    copy_project()  # 加载所有可以编译的资源文件
    load_project_resource(build_directory, extensions)
    build(extensions)  # 开始编译
    shutil.rmtree(temp_directory)  # 删除编译的文件夹
    remove_resource(build_directory)  # 删除py文件和pyx文件
