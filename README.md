socksmaster
===========

A Simple VPN of fortune, using socks and routing table.

config file
-----------

The config file need to be at ~/.sockmaster/config

first of, the providers: it's a list of different way to tunnel your data.
the name is the reference to this provider that can be used later in the routing table:

    [providers]
    name=provider
    ...

the provider need to be one of those 3 options:

* none: drop the connection request by returning a connection refused.
* direct: the connection is established directly by socksmaster.
* external:<hostname>:<port> : connect to a sub SOCKS server that will tunnel the data.

Then the rules:

    [rules=<name>]
    <domain name>=<provider name>
    ...


