GIT_BRANCH_NAME=`git branch | grep '*' | cut -d ' ' -f 2`
SVN_REPOS="http://mosh-scheme.googlecode.com/svn/branches"

bootstrap:
	svn checkout $(SVN_REPOS)/psyntax.$(GIT_BRANCH_NAME)/ boot/runtimes/psyntax-mosh/psyntax.$(GIT_BRANCH_NAME) || \
    svn checkout $(SVN_REPOS)/psyntax.master/ boot/runtimes/psyntax-mosh/psyntax.$(GIT_BRANCH_NAME)
	cp -p boot/runtimes/psyntax-mosh/psyntax.$(GIT_BRANCH_NAME)/psyntax.scm boot/runtimes/psyntax-mosh/ 
	$(MAKE) -C boot
