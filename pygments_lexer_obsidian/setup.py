import os.path
from distutils.core import setup
import setuptools

def read(fname):
    return open(os.path.join(os.path.dirname(__file__), fname)).read()

setup(
    name='pygments-lexer-obsidian',
    version='0.1.2',
    description='Obsidian lexer for Pygments',
    long_description=read('README.rst'),
    license="BSD",
    author='Michael Coblenz',
    author_email='mcoblenz@cs.cmu.edu',
    url='https://www.obsidian-lang.com',
    packages=['pygments_lexer_obsidian'],
    classifiers=[
        'Environment :: Plugins',
        'Intended Audience :: Developers',
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 3'
    ],
    keywords='pygments lexer syntax highlight obsidian',
    install_requires=[
        'pygments>=2.1'
    ],
    entry_points="""
    [pygments.lexers]
    obsidian = pygments_lexer_obsidian:ObsidianLexer
    """
)
