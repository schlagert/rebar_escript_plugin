rebar_escript_plugin
====================

[![Build Status](https://travis-ci.org/schlagert/rebar_escript_plugin.png?branch=master)](https://travis-ci.org/schlagert/rebar_escript_plugin)

A `rebar` plugin to package applications or releases as executable escript
similar to `rebar escriptize` trying to overcome its limitiations.

Did you ever write a nifty Erlang application you wanted to easily share with
somebody else? Or be able to painlessly get it onto another machine? If yes,
it is pretty sure you already discovered the `rebar escriptize` command.

Unfortunately, it is almost sure you also discovered one of the following
problems:
* the escript doesn't start because you forgot to provide a `main/1` function
* the escript doesn't contain the dependencies because you forgot to duplicate
  the `deps` information into `escript_incl_apps`
* you can't access your `priv` data at runtime
* you want to create an escript based on a release description, e.g. to contain
  two independent nifty services

This is where the `rebar_escript_plugin` tries to help.

TODO
