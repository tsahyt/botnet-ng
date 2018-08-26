# botnet-ng Help

## GitHub

Currently only queries for repositories are supported. The syntax is `:github
<user>/<repo>`.

## WolframAlpha

The WolframAlpha API can be accessed in one of two ways. The simple way is to
use `:wa <query>`, which will employ the Short Answers API to answer your query.
The second way is to use `:hal <query>`, which uses the conversations API, and
is thus able to answer follow up queries. Conversations are tracked per user
using the token returned by the API.

## Market Quotes

Stock quotes can be received via `:stock <ticker symbol>`.

Cryptocurrency quotes can be received via `:crypto <currency symbol>[/<fiat
symbol>]`. `:cc` is a synonym for `:crypto`.

## Search

There is a binding to the DuckDuckGo instant answers API available via `:ddg
<query>`.

## Citations

Citation commands have the following form: `:<cmd> [ <num> | <search query> ]`.
The list of available values for `<cmd>` can be retrieved with `:cites`.
Numbering of citations starts at 0. Text search is fuzzy.

## Markov Chains

Markov chains can be invoked with `:<cmd>`. The command takes no arguments. A
list of all available chains can be retrieved via `:chains`.

## Personal Data

Personal data can be erased with `:delete-data` or retrieved with `:dump-data`.
However, at the moment no such data is collected, making these commands
meaningless.

## Misc

Interjections can be created using `:interject`. Up to three parameters are
supported, e.g.

```
:interject GNU/Linux POSIX
:interject systemd Linux
```

However, when given only *one* argument, the command will fail.

The following miscellaneous commands are supported:

* `:cia`
* `:fbi`
* `:fiveeyes`
* `:kgb`
* `:eu`
* `:gnu`
