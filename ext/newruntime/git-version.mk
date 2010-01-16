all: nmosh_version_git.h

# from mosh
GIT_IS_DIRTY=$(shell git diff --quiet HEAD || echo " *dirty*")

# do not use newline here...(use c-style \n insted)
GIT_COMMIT_DATA=$(shell git rev-list --all -n 1 --pretty="[commit]%h %ci" -- . | tail -1)$(GIT_IS_DIRTY)

nmosh_version_git.h: _timestamp
	rm -f $@
	git log -1 > /dev/null #make sure git installed
	echo "#define NMOSH_COMMIT_DATA \"$(GIT_COMMIT_DATA)\"" >> $@




