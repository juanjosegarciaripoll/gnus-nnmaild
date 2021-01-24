# New Maildir backend for Gnus

Version: 0.4

## Motivation

I store my email in Maildir folders that are  synchronized to external IMAP servers using isync or offlineimap. Normally, I rely on mu4e for reading and exploring those folders, but I would like to have a pure Emacs solution. However, the `nnmaildir` backend is *veeery* slow on Windows, relying on one additional file per message to store information, using links, extra flags, etc. This contrasts with other backends, such as `nnml`, that are snappier and use a single cache file.

## How it works

The backend started as a rewrite of `nnml` to handle the Maildir format. After a lot of reconsideration and tests, it has become a completely different backend, with a new caching system based on hash tables, where the flags are provided by the backend, not by Gnus. This means it only supports the `read`, `replied-to`, `forwarded` and `tick` flags, but it does it very efficiently, using Maildir's conventions.

When `nnmaild` faces a directory, is searches a file with the name `.nnmaild-data.el`. If it exists, it used to restore some basic information about all messages in the folde, the association between file names and Gnus messages numbers, or cached headers (Gnus NOV format). When the file is missing or it has become obsolete, the backend recreates the cache, comparing a list of existing message files with the previous data, loading the headers from messages that were not scanned before, etc.

## Configuration

Typically, you would add this backend to `gnus-secondary-select-methods`, as in the following example:
```
(setq gnus-secondary-select-methods
	  '((nnmaild "maildir"
	     (nnmaild-directory "~/Maildir")
		 (nnmaild-flag-separators ";!")
		 (nnmaild-cache-strategy full)
		 (nnmaild-cache-expiration-strategy 'directory-mtime))))
```
Here, we assume that `~/Maildir` is a directory that contains other folders, each of them storing messages in the Maildir format. We also select the Maildir flag separators that are commonly used in Windows and tell `nnmaild` to watch the folders' modification time to detect when the cache has become obsolete.

The following variables apply to the backend:

- `nnmaild-directory` is the path to the Maildir directory. Individual groups are direct subfolders that contain the `cur`, `new` and `tmp` directories of a typical Maildir folder. By default, all groups are subscribed.

- `nnmaild-flag-separators` is a string with all characters that could be used to separate the message identifier from the flags that specify it has been read, replied to, forwarded, deleted, etc. In Unix systems this string is ":" by default. On Windows, this is not a valid character name; hence, `isync` and `offlineimap` typically use ";" or "!".

- `nnmaild-cache-strategy` is a symbol that selects the way we cache information about the messages in a Maildir folder
  - `nil` means we do not cache anything. This is a bad strategy, because there is no precise association between file names and Gnus article numbers.
  - `memory` means we preserve the information in memory. The cache is built the first time we subscribe to or visit a group,  and it is update as we read content or the group is modified by external programs. Once Gnus is closed, caches are deleted from memory.
  - `file` means we load the caches from an `.nnmaild-data.el` file at the root of the Maildir folder. Every time we visit a group, the cache is loaded, refreshed and saved. Caches are removed from memory when we stop working with a group.
  - `memory+file` is the default strategy. If a cache file exists for a folder, it is loaded and refreshed the first time we visit or subscribe it. After that first time, the cache is preserved in memory, until we close the server. At this point the cache is saved to file.

- `nnmaild-cache-expiration-strategy` determines how we decide to refresh or not the information about a group. This refresh consists on looking at the new list of messages, determining which ones have been deleted, which ones have new flags and which ones stay the same. For new messages we may reload their header information. This variable can take the following values
  - `T` means this refresh is performed every time the cache is used.
  - `directory-mtime` means that we only refresh the cache when the list of files in a directory has been changed, due to new files, renaming, etc. It is a rather safe and fast way.
  - `file-nmtime` means that instead of looking at the directory, we look at a specific file, with a name given by `nnmaild-cache-control-file` that is updated by the Maildir synchronization software. For instance, `isync` uses `.mbsyncstate`.


## Status

This **has not** been implemented:
- nnmaild-request-expire-articles

These are **unlikely to be implemented**. For efficiency and consistency, the backend asssumes that the content of messages is immutable, and that groups exist for as long a server is open:
- Creating, deleting or renaming folders (AKA groups)
- Article edition

## Caveats

This software is really experimental and still in beta stage. I am using it locally in my own computer, but there is no warranty that it will not wipe out your entire Maildir spool.

The backend should not access the mail spool while it is being updated by other programs. I suggest you configure Gnus to invoke  `mbsync` or `offlineimap` synchronously (i.e. blocking emacs) prior to checking for new messages.
