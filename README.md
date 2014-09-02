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
* the escript doesn't start because you forgot to provide a `main/1` function in
  the correct module
* the escript doesn't contain the dependencies because you forgot to duplicate
  the `deps` information into `escript_incl_apps`
* you can't access your `priv` data at runtime

This is where the `rebar_escript_plugin` tries to help.

Now tell me what's so _terribly_ different from `rebar escriptize`? In fact not
much, the plugin
* runs automatically as `post_compile` or `post_generate` hook.
* avoids the necessity of duplicate configuration.
* provides a default main/1 function, starting all needed applications as a
  normal Erlang release would.
* makes your application's `priv` directory contents accessible at runtime.
* additionally allows escript creation *based on release specifications*.

Usage
-----

The `rebar_escript_plugin` currently needs [rebar](https://github.com/rebar/rebar)
version `2.5.1` and [Erlang/OTP](http://erlang.org) `17.0` or newer.

First of all the plugin must reside somewhere in your project's code path. This
can be achieved by either placing the application somewhere into your `lib_dir`
(or `ERL_LIBS`) or by specifying it as a project dependency in the `deps`
section of your project's `rebar.config`. Ultimatively, you also need to include
the plugin in your project's `rebar.config`. The most common configuration would
look something like this:

```erlang
{deps, [{rebar_escript_plugin, ".*", {git, "https://github.com/schlagert/rebar_escript_plugin.git"}}]}.
{plugins, [rebar_escript_plugin]}.
%% Only needed if you depend on special emulator args, e.g. Erlang distribution
{rebar_escript_plugin, [{emu_args, string()}]}.
```

That's it. The escript will be regenerated every time you compile your
application or generate your release.

Please note that if you choose to include the plugin as a project dependency you
will need to compile the project twice initially. This must be done because the
plugin needs to be compiled itself first and is therefore not yet available
during the first compiler run.

Another note on escripts created from release specifications: Currently, there
is no `overlay` support (yet). If your release depends on external files
provided by an overlay, it will most probably not be able to start/run properly.

Examples
--------

The [serve_it](https://github.com/schlagert/serve_it) application is an example
for an application that uses the plugin to create a standalone escript. For a
release-based example refer to the `examples` directory.
