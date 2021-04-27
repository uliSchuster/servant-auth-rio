# Authentication with Servant and RIO

This is a little exercise and experiment to understand how to use the [Servant framework](https://docs.servant.dev/en/stable/) for typed web APIs and cookie authentication by means of the [Servant.Auth Package](https://github.com/haskell-servant/servant-auth), in combination with the [ReaderT/IO design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) as implemented in the [RIO Monad](https://www.fpcomplete.com/blog/2017/07/the-rio-monad/).

By default, Servant uses its own `Handler` monad, which is a wrapper around `ExceptT ServerError IO`.
This is the [ExceptT IO anti-pattern](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/), which I want to avoid.
The [Servant documentation](https://docs.servant.dev/en/stable/tutorial/Server.html#using-another-monad-for-your-handlers) provides functions to _hoist_ a custom monad into Servant's `Handler`; and example using ReaderT is available [here](https://harporoeder.com/posts/servant-13-reader-io/).

As of version 0.18.2, Servant supports HTTP basic authentication and a generic extension mechanism for other authentication schemes.
The [Servant.Auth Package](https://github.com/haskell-servant/servant-auth) provides token and cookie-based authentication.
When serving a protected API, an authentication function must be provided as server _context_.
When using a custom handler monad, the context must be hoisted together with the custom monad: Instead of `hoistServer`, Servant provides the function `hoistServerWithContext` to this end.

In the present minimal example, I came across two problems, with solutions by helpful comunnity members:

- For a complex API with multiple endpoints combined via Servant's `:<|>` type combinator, the return type is a combination of the individual endpoint return types. If instead of returning a value the authentication check throws an error, this must also be a combination of errors, one for each endpoint. Servant provides the [throwAll](http://hackage.haskell.org/package/servant-auth-server-0.4.6.0/docs/Servant-Auth-Server.html#v:throwAll) helper function (from the [ThrowAll](https://hackage.haskell.org/package/servant-auth-server-0.4.6.0/docs/Servant-Auth-Server.html#t:ThrowAll) type class). With RIO as handler monad, the helper function does not work because RIO does not have an instance of the `MonadError` typeclass. To alleviate this problem, [danidiaz](https://stackoverflow.com/users/1364288/danidiaz) provided the `RIOThworAll` type class with the `rioThrowAll` helper, as discussed in [this](https://stackoverflow.com/questions/67262209/deny-authentication-in-servant-auth-with-rio) post on StackOverflow.
- Because of some limitation in the Servant base package (see issue [#1267](https://github.com/haskell-servant/servant/issues/1267)), Servant-Auth currently cannot return headers for a HTTP 204 (No Content) response (see servant-auth issue [#177](https://github.com/haskell-servant/servant-auth/issues/177)). Authentication requires Cookie and X-CSRF headers, though. A workaround suggested in the above issue is to provide an instance for the particular NoContent verbs needed, as discussed in [this](https://stackoverflow.com/questions/67270239/serving-a-servant-nocontent-response-with-rio) StackOverflow post.

## Resources

In putting together the example, I examined the following blog posts, code snippets, and documentation:

- Servant documentation on [using a custom handler monad](https://docs.servant.dev/en/stable/tutorial/Server.html#using-another-monad-for-your-handlers).
- An [example](https://harporoeder.com/posts/servant-13-reader-io/) on how to hoist a ReaderT/IO monad stack as custom handler.
- A [blog post](https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html) on how to avoid the ExceptT anti-pattern in Servant; note that the blog targets older Servant versions, where the `hoistServer` function was not yet available.
- Servant's [generalized authentication](https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication) interface.
- The authentication examples from [Servant.Auth](https://github.com/haskell-servant/servant-auth#readme).
- An example on how to [configure authentication settings](https://jappieklooster.nl/authentication-in-reflex-servant.html) - XSRF cookie settings in particular.
- Another [cookie authentication example](https://github.com/jappeace/awesome-project-name/blob/auth/backend/src/Lib.hs).
