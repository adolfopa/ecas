# eCAS

eCAS is a reimplementation of Jasig CAS in Erlang. The objective is to
implement all the features described in the [CAS
Protocol](http://www.jasig.org/cas/protocol).

Right now, everything but proxy tickets is implemented.  This is still
at a very early stage of development, and is definitely not prepared
for deployment.

## Usage

To compile:
```sh
$ make
```
Once compiled, you can start the server at port 8000 with:
```sh
$ ./start
```

To use eCAS as a SSO provider to other applications, you should point
them to the root context of the server. In other words, eCAS has the
following HTTP resources available:
```
http://host:8000/login
http://host:8000/logout
http://host:8000/validate
http://host:8000/serviceValidate
http://host:8000/proxyValidate
```

## Authentication

You should provide the authentication strategy to use.  That is
configured in the OTP application descriptor file, in the environment
property `auth_strategy`.

The value of this property should be a tuple of the form `{M, F}`,
where M and F are the names of a module and a function (both atoms).
The function should take two arguments (username and password, both
strings) and return a boolean value determining if the credentials are
correct.