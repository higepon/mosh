; file.ss
;
;   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;   1. Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;
;   2. Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;  $Id: concurrent.ss 621 2008-11-09 06:22:47Z higepon $

#|
    Title: File

    Filesystem library

    library: (mosh file)

    Filesystem Library
|#
(library (mosh file)
  (export
   create-directory delete-directory rename-file create-symbolic-link
   file-directory? file-symbolic-link? file-regular? file-readable?
   file-executable? file-writable? file-size-in-bytes file-stat-mtime
   file-stat-atime  file-stat-ctime
   file->string file->list file->sexp-list write-to-file directory-list)
  (import (only (system)
                create-directory delete-directory rename-file create-symbolic-link
                file-directory? file-symbolic-link? file-regular? file-readable?
                file-executable? file-writable? file-size-in-bytes file-stat-mtime
                file-stat-atime  file-stat-ctime file->sexp-list
                directory-list file->string file->list write-to-file))

#|
    Function: create-directory

    Create a directory.

    Prototype:
    > (create-directory path)

    Parameters:

      path - directory path to create.
|#

#|
    Function: delete-directory

    Delete a directory.

    Prototype:
    > (delete-directory path)

    Parameters:

      path - directory path to delete.
|#

#|
    Function: rename-file

    Rename a file.

    Prototype:
    > (rename-file old-name new-name)

    Parameters:

      old-name - old name.
      new-name - new name to rename.
|#

#|
    Function: create-symbolic-link

    Create a symbolic link.

    Prototype:
    > (create-symbolic-link old-name new-name)

    Parameters:

      old-name - old name.
      new-name - new name to create.
|#

#|
    Function: file-directory?

    Returns #t if file is directory, otherwise #f.

    Prototype:
    > (file-directory? file)

    Parameters:

      file - file.

    Returns:

      #t #t if file is directory, otherwise #f.
|#

#|
    Function: file-symbolic-link?

    Returns #t if file is symbolic link, otherwise #f.

    Prototype:
    > (file-directory? file)

    Parameters:

      file - file.

    Returns:

      #t #t if path is symbolic link, otherwise #f.
|#

#|
    Function: file-symbolic-link?

    Returns #t if file is symbolic link, otherwise #f.

    Prototype:
    > (file-directory? file)

    Parameters:

      file - file.

    Returns:

      #t #t if file is symbolic link, otherwise #f.
|#

#|
    Function: file-regular?

    Returns #t if file is regular file, otherwise #f.

    Prototype:
    > (file-regular? file)

    Parameters:

      file - file.

    Returns:

      #t #t if file is regular file, otherwise #f.
|#

#|
    Function: file-readable?

    Returns #t if file is readable, otherwise #f.

    Prototype:
    > (file-readable? file)

    Parameters:

      file - file.

    Returns:

      #t #t if file is readable, otherwise #f.
|#

#|
    Function: file-writable?

    Returns #t if file is writable, otherwise #f.

    Prototype:
    > (file-writable? file)

    Parameters:

      file - file.

    Returns:

      #t #t if file is writable, otherwise #f.
|#

#|
    Function: file-executable?

    Returns #t if file is executable, otherwise #f.
    On Windows this procedure checks the file extention.
    .COM .EXE .BAT .VBS .VBE .JS .JSE .WSF .WSH .MSC are executable.

    Prototype:
    > (file-executable? file)

    Parameters:

      file - file.

    Returns:

      #t #t if file is executable, otherwise #f.
|#

#|
    Function: file-size-in-bytes

    Returns file size in bytes.

    Prototype:
    > (file-size-in-bytes file)

    Parameters:

      file - file.

    Returns:

      file size in bytes.
|#

#|
    Function: file-stat-mtime

    Returns last modify time of file.

    Prototype:
    > (file-stat-mtime file)

    Parameters:

      file - file.

    Returns:

      last modify time of file.
|#

#|
    Function: file-stat-atime

    Returns last access time of file.

    Prototype:
    > (file-stat-atime file)

    Parameters:

      file - file.

    Returns:

      last access time of file.
|#

#|
    Function: file-stat-ctime

    Returns last change time (or creation time on Windows) of file.

    Prototype:
    > (file-stat-ctime file)

    Parameters:

      file - file.

    Returns:

       last change time (or creation time on Windows) of file.
|#

#|
    Function: directory-list

    Read directory and returns result as list of string.

    Prototype:
    > (directory-list path)

    Parameters:
      path - Path to directory.

    Returns:

      directory entries list of string.

    See Also:

      <current-directory> and <set-current-directory!>
|#

#|
    Function: file->list

    Convenience procedures to read from a file filename.
    (read-line file) and returns list of lines.

    Prototype:
    > (file->list file)

    Parameters:

      file - file.

    Returns:

       list of lines.
|#

#|
    Function: file->sexp-list

    Convenience procedures to read from a file filename.
    (read file) and returns list of S-expressions.

    Prototype:
    > (file->sexp-list file)

    Parameters:

      file - file.

    Returns:

       list of S-expressions.
|#

#|
    Function: file->string

    Convenience procedures to read from a file filename.
    Read whole file and returns contents as string.

    Prototype:
    > (file->string file)

    Parameters:

      file - file.

    Returns:

       contents as string.
|#

#|
    Function: write-to-file

    Write string to file

    Prototype:
    > (write-to-file file contents)

    Parameters:

      file - file.
      contents - string contents.
|#
)
