GIT_BRANCH_NAME=`git branch | grep '*' | cut -d ' ' -f 2`
GIT_COMMIT_HEX=`git show|grep commit`
SVN_REPOS_DIR=$(shell if [ -n "$(PSYNTAX_SVN_REPOS_DIR)" ]; then echo "$(PSYNTAX_SVN_REPOS_DIR)"; else echo "~/repos-mosh/branches"; fi)

# 1. Expand psyntax.scm using Mosh.
# 2. svn commit to branches/psyntax.[branchname]
all:
	MOSH_GENSYM_PREFIX=`../../mosh print-prefix.ss` ../../mosh psyntax-buildscript-mosh.ss
	cp psyntax.scm $(SVN_REPOS_DIR)/psyntax.$(GIT_BRANCH_NAME)/
	svn ci -m "See http://github.com/higepon/mosh $(GIT_COMMIT_HEX)" $(SVN_REPOS_DIR)/psyntax.$(GIT_BRANCH_NAME)/

