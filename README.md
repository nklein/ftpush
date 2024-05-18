# FTPUSH -- a library to push directories to an FTP site

The `FTPUSH` library lets you specify files and directories to be upload to an FTP site and upload them.

The top-level entry point to `FTPUSH` is:

    (ftpush (:hostname ftp-hostname
             :username ftp-username
             :password ftp-password
             :state-file path-to-remember-what-was-pushed-before
             :local-dir local-directory-as-starting-point
             :remote-dir remote-directory-as-starting-point
             :excludes list-of-matchers
             :dry-run-p whether-to-do-a-dry-run)
      ...body...)

The `HOSTNAME` gives the FTP server's hostname.
The `USERNAME` specifies the login name for the FTP server.
The `PASSWORD` specifies the login password for the FTP server.
**Note**: there is a `READ-PASSWORD-FROM-FILE` helper function to let you read the password from a given file.

The `STATE-FILE` gives the name of the file in which to record what it uploaded so that the next time you run you do not need to push files which have not changed since the previous run.

The `LOCAL-DIR` specifies the starting point of transfer on the local host.
The `REMOTE-DIR` specifies the initial point of transfer on the remote host.

The `EXCLUDES` is a list of matchers.
These are described in the **Matches** section below.
If any matcher in the list returns non-`NIL` for a local pathname, then that file will not be uploaded.


The `DRY-RUN-P` if non-`NIL` makes the run a simulation. As a simulation, it will not
push any files to the FTP server nor will it save its state on completion.

The body of the `FTPUSH` form can be any executable code. There are certain forms, however,
that are useful for specifying transfers that should take place. These are described
in the **FTPush Forms** section below.

## Matchers

A matcher is a function of a single argument.
`FTPUSH` uses matchers to determine whether to exclude a file.
If the matcher returns non-`NIL` when given a local pathname, then that file is not pushed to the FTP site.

There are a number of functions that assist in generating matchers using `CL-PPCRE` regular expressions.
To explain these, assume we are running the generated matchers against the pathname
`#P"/tmp/path/to/an/image.png"`:

    (file-matcher regex)       => return true if "image.png" matches the regex
    (path-matcher regex)       => return true if "/tmp/path/to/an/image.png" matches the regex
    (extension-matcher regex)  => return true if "png" matches the regex
    (directory-matcher regex)  => return true if "/tmp/path/to/an/" matches the regex

If you wanted to only upload files that were less than 1024 bytes, you could make a matcher like:

    (lambda (path)
      (with-open-file (in path)
        (< (file-length in) 1024))

## FTPush Forms

You can push an individual file with the function:

    (ftpush-file :local-path path-relative-to-local-dir
                 :remote-path path-relative-to-remote-dir)

You can push a whole directory of files with the function:

    (ftpush-tree :local-path path-relative-to-local-dir
                 :remote-path path-relative-to-remote-dir
                 :excludes matchers-in-addition-to-the-globa-one)
