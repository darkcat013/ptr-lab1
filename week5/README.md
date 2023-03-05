Week 5
====

Install rebar3 on windows with chocolatey
-----

    $ choco install rebar3

Build and run
-----

    $ rebar3 shell

Example
-----
    $> minimal:quotes_to_json().

env.json schema
-----
```json
{
    "clientId" : string,
    "clientSecret": string,
    "userId": string
}
```