The Answer: Yes, for clients and documentation, but not for servers:
  https://www.reddit.com/r/haskellquestions/comments/73hh05/what_purpose_does_the_symbol_serve_in_servants/do0lrgd/


The Question:

For reference:

    *Server Server> :i Capture
    type role Capture phantom phantom
    data Capture (sym :: GHC.Types.Symbol) (a :: k)
            -- Defined in ‘Servant.API.Capture’
    ...

The [Servant tutorial](http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html) introduces the Capture type with this API:

    type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> ...

Passing the values 1 and 2 to that endpoint using *curl*, I am rewarded with:

    jeff@jbb-lenovo:~/code$ curl http://localhost:8081/position/1/2
    {"yCoord":2,"xCoord":1}jeff@jbb-lenovo:~/code$ 

If I swap the "x" and "y" in the definition of API, that curl command still gives the same output. In fact even if I change it to "x" and "z", it still does the same thing.

What, then, is the purpose of the *Symbols* "x" and "y"?
