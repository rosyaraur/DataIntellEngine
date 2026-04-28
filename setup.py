from setuptools import setup, find_packages

setup(
    name='dataintellengine_python',
    version='0.1.0',
    # This explicitly targets the python_core directory and its subdirectories
    packages=find_packages(include=['python_core', 'python_core.*']),
)