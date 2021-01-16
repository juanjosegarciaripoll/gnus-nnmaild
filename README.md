# New Maildir backend for Gnus

## Motivation

I use Maildir spools synchronized to external mail servers using isync or offlineimap. Normally, I rely on mu4e for reading and exploring those folders, but I would like to have a pure Emacs solution. However, the `nnmaildir` backend is *veeery* slow on Windows, relying on multiple files to store information, flags, etc (at least one or two files per message). This contrasts with other backends, such as `nnml`, that are snappier and use a single cache file.

## How it works

The backend is a heavy rewrite of `nnml` to handle the Maildir format and use the flags from the Maildir specification. This means it only supports the `read`, `replied-to`, `forwarded` and `tick` flags. It is not much, but it saves us from having to store information on separate caches.

When it faces a directory, the backend tries to find a `.nnmaild-data.el` file. If it exists, it loads the files as the default source of information about the messages, mapping between file names and messages numbers, and cached headers (Gnus NOV format). If that information does not exist, or Gnus request a more thorough check, the backend will recreate the cache, comparing a list of existing message files with the previous data, loading the headers from messages that were not scanned before, etc.

## Status

This **has not** been implemented:
- Saving or editing of messages
- Moving messages betwen groups
- Creating new folders (AKA groups)
- Scanning of the `new` folder for incoming messages

## Caveats

This software is really experimental and still in beta stage. I am using it locally in my own computer, but there is no warranty that it will not wipe out your entire Maildir spool.

The backend should not access the mail spool while it is being updated by other programs. I suggest you configure Gnus to call `mbsync` or `offlineimap` prior to checking for new messages.
