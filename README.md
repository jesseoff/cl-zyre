# Zyre for Common Lisp

[Zyre](https://github.com/zeromq/zyre#overview) is very simple
clustering/service discovery protocol and C library built on top of
[ZeroMQ](http://zeromq.org), which is a very simple abstraction for passing
messages between threads or processes on a local machine or across the network
to other machine/machines. This system exposes a mostly complete interface to
the Zyre library, and also a partial interface to the CZMQ functions to which
the base ZeroMQ library Zyre depends on.

## Why Zyre?

This is some overlap in functionality of Zyre with other systems such as Apple's
Bonjour, multicast-dns, and Avahi. Zyre is not only for service discovery, but
also for reliable and fast network message passing and broadcasting. Nodes join
the network very quickly (<1 second) and the protocol behind the scenes uses
heartbeats to detect crashed or rebooted nodes within about 30 seconds. When
this happens, the entire network is made aware and not just the nodes involved
in active messaging. For more information, please see
[Zyre](https://github.com/zeromq/zyre).

## Trivial Example of Using Zyre

The original sources to the Zyre C library included a sample C program "zpinger"
which:

  * Starts up a node and uses UDP broadcasting to discover and join the local
    Zyre network cluster.
  * Sends a string message "Hello" to everyone it sees on the network as a Zyre
    *whisper* message
  * Joins a Zyre group named "GLOBAL"
  * Responds to any "Hello" *whispers* it receives with a "Hello" *shout* to the
    "GLOBAL" group.
  * Prints a log message for every new peer entrance, exit, and message.
  
A version of the zpinger is included in zyre.lisp as a sample application; its
implementation serves as a good introduction to using the Lisp version of this
library:

``` common-lisp
(defun simple-zpinger ()
  (labels
      ((zyre-handler (x)
         (trivia:match x
           ((start-event (uuid u) (name n))
            (log:info "Create Zyre node, uuid=~a, name=~a" u n))
           ((exit-event (name n))
            (log:info "[~a] peer exited" n))
           ((enter-event (name n) (peer-addr a))
            (whisper x "Hello")
            (log:info "[~a] peer entered, ip=~a" n (ppcre:scan-to-strings "[0-9\.]+" a)))
           ((shout-event (event-msg "Hello") (name n) (group g))
            (log:info "[~a](~a) received ping (SHOUT)" n g))
           ((whisper-event (event-msg "Hello") (name n))
            (shout x "Hello")
            (log:info "[~a] received ping (WHISPER)" n)))))
    (pipe-mapc #'zyre-handler (zyre-pipe :group "GLOBAL"))))
```

## ZeroMQ Support Via CZMQ

Since Zyre is built on top of ZeroMQ, it could not be helped but to bring in
some of these functions as well. ZeroMQ sockets can be created with the
`make-zsock` function and there are some method functions `send` and `recv` that
can send, currently, either zframes (via `make-zframe`) or strings.
`make-zframe` can create blank frames which can then be memcpy'ed into via CFFI,
or a Lisp native simple byte vector can be used instead. (which is then copied
into a CZMQ private buffer). If you've never before used ZeroMQ, I highly
recommend looking into it.

## Pipe functions

The main public function `zyre-pipe` utilizes a pattern unique to Common Lisp to
return something best described in chapter 25, "Streams and Delayed Evaluation",
in the book "Lisp, 3rd Edition" by Winston/Horn. The chapter introduces a stream
data structure pattern (that I have renamed to "pipe" to avoid name clash
confusion), that utilizes Lisp's lexical closures to create something akin to an
infinite list. Utilities to manipulate, map, filter, and transform these data
structures are included in pipe.lisp and are made public in the zyre package,
though not specifically relating to the zyre primary interfaces. These routines
are superficially documented in pipe.lisp, though depending on familiarity with
the above book or Lisp's lexical enclosures and functional programming
paradigms, might either seem extremely trivial or extremely obtuse.

One possibility of the pipe functions with Zyre is to give the `zyre-pipe` to
the `pipe-signaler` function. This will then _spray_ the Zyre conditions up
through layers of code as Lisp signals to allow more flexibility on which layers
of code do what with the various types of Zyre events being generated.

## Caveats/Limitations

Currently, I am assuming all whisper and shout payloads are single zframe
strings. The full library allows the message payloads to be multipart zmsgs
comprised of any number of binary frames, strings, etc. There is no reason for
this limitation in Lisp, just that I haven't yet pulled over the full set of
CZMQ helper functions that facilitate this. There is a lot of functions in CZMQ
and I'd like to build a beautiful and complete Lispy layer on top of them, but I
have not yet done so in this version.

Zyre does have some support for using what it calls _zgossip_ node discovery
rather than the default UDP broadcasting. This involves the setup of some
centralized zgossip servers and thereby somewhat defeating the point as a
decentralized node discovery protocol. I have not included support for this.

## Future work

A potential goal is utilize the Zyre layer underneath the
[lfarm-server/lfarm-client](https://lparallel.org/2013/06/15/introducing-lfarm/)
systems to realize an auto-discovering compute cluster of Lisp machines. The
above quicklisp systems, combined with [lparallel](http://lparallel.org), would
make for an extremely attractive high level interface to parallel processing.

Another included sample application, `zyredir` simply joins the local Zyre
network and maintains a directory of .json files representing all currently
online nodes. This was envisioned as a simple means to enable other system
applications to use node discovery without having to link and join zyre
themselves. The directory can be even be published across NFS, SMB, or HTTP.

