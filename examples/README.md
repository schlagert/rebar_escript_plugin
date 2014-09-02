This is a simple example how to use the `rebar_escript_plugin` to create an
escript from a release specification. The project structure is based on the
one recommended in the [rebar wiki](https://github.com/rebar/rebar/wiki/Release-handling)
leaving out the `apps` directory.

To compile, generate and run execute the following:

```
$ rebar get-deps
$ rebar compile
$ rebar generate
$ rel/serve_it
```
