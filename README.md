# FTPUSH -- a library to push directories to an FTP site

The `FTPUSH` library lets you specify files and directories to be upload to an FTP site and upload them.

## Top-level Function

The top-level entry point to `FTPUSH` is the macro:

    (ftpush (:remote-provider remote-provider
             :state-file path-to-remember-what-was-pushed-before
             :local-dir local-directory-as-starting-point
             :remote-dir remote-directory-as-starting-point
             :excludes list-of-matchers
             :dry-run-p whether-to-do-a-dry-run)
      ...body...)

The `REMOTE-PROVIDER` is an instance used to communicate with a remote server.
The available providers are described in the **Remote Providers** section below.
If the `REMOTE-PROVIDER` is `NIL`, then `FTPUSH` operates in *dry run* mode and no files are uploaded anywhere.
**Note**: there is a `READ-PASSWORD-FROM-FILE` helper function to let you read the password from a given file which will be useful for any remote providers that require passwords.
The `READ-PASSWORD-FROM-FILE` function reads the first line of the given file to use as a password.

The `STATE-FILE` gives the name of the file in which to record what it uploaded so that the next time you run you do not need to push files which have not changed since the previous run.

The `LOCAL-DIR` specifies the starting point of transfer on the local host.
The `REMOTE-DIR` specifies the initial point of transfer on the remote host.

The `EXCLUDES` is a list of matchers.
These are described in the **Matchers** section below.
If any matcher in the list returns non-`NIL` for a local pathname, then that file will not be uploaded.


The body of the `FTPUSH` form can be any executable code. There are certain functions, however,
that are useful for specifying transfers that should take place. These are described
in the **FTPush Forms** section below.

### Caveats

**Do not** use the same `STATE-FILE` with multiple remote sites.
The `STATE-FILE` does not know anything about what site it sent data to the last time.
`FTPUSH` reads from a `STATE-FILE` at the start of a run and writes to the `STATE-FILE` when it finishes a run.
If you push to one site and then push to a second site with the same `STATE-FILE` path, you will only send files to the second site if they have changed since they were sent to the first site.

If `FTPUSH` uploaded files in the past and you modify its `EXCLUDES` criteria so that a file
that was sent before is no longer going to be sent, then `FTPUSH` will remove that file from the remote server.
This **may not be what you want** if you wanted to upload that file one time and never upload changes to it again.
To accomplish that, you either need to delete it from the `STATE-FILE` after upload or you need to manually put the file onto the remote server by some other means after `FTPUSH` deletes it.

If any errors occur during processing, the `STATE-FILE` is left as-is.

## Remote Providers

The `FTPUSH-REMOTE-FTP` is a provider which operates remotely over FTP.
The `FTPUSH-REMOTE-FTP` class is in the `FTPUSH/FTP` package.
To create an instance of it, load the `FTPUSH/FTP` package and then do:

    (make-instance 'ftpush/ftp:ftpush-remote-ftp
                   :hostname ftp-hostname
                   :port ftp-port
                   :username ftp-username
                   :password ftp-password
                   :passive-ftp-p non-nil-to-use-passive-mode)

The `HOSTNAME` is required and specifies the FTP server's hostname.

The `PORT` is optional and gives the FTP server's port.
This defaults to `21`.

The `USERNAME` is required and specifies the login name for the FTP server.

The `PASSWORD` is required and specifies the login password for the FTP server.

The `PASSIVE-FTP-P` is optional.
When this is non-`NIL`, the provider uses FTP in passive mode.
This defaults to `T`.

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

## Example Program

The `example.sh` file included here shows an example of uploading a simple web-application to an FTP server.

You can check out the options for this script using:

    ./example.sh --help

The example program expects some files to be there which are not in the repository. To get them,
you need to run `npm install` in the `./example-webapp` directory.

    cd ./example-webapp
    npm install

This script must be run from the directory in which this `README.md` file and the `example.sh` script are found.
