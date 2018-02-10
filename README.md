# servant-starter

This template allows for a quick start based on the `servant` and `protolude` library. It features a custom app monad instead of the servant Handler monad.

It comes with a set of helper functions to make working with postgresql easier based on the `postgresql-simple` library.

It also integrates with the `ekg` library to allow for monitoring of your process and help in debugging slow SQL queries.


# Quick start

Edit the follwing datatypes in `src/Lib/Types/App.hs`:

+ `AppError` defines the types of errors you want to throw. Edits to this have to be mapped back into http errors in the `runAppT` function which can be found at `src/Lib/Util/App.hs`.

+ `AppEnv` is a readonly value that you can `ask` for inside your app monad. It currently contains the sessions, database pool and ekg store and distributions.

+ `Session` is a value that can be associated with a `Text` serving as the session key.

## Util functions

The following utility functions can make writing your route handlers easier. Import `Lib.Prelude` to bring them into scope.

+ `appMaybeWith :: AppError -> App (Maybe a) -> App a`. Throws an AppError if the given maybe value is Nothing.
+ `appMaybe :: App (Maybe a) -> App a`. Wrapper around `appMaybeWith` which throws the `NotFound` error


+ `queryPG :: (PG.ToRow q, PG.FromRow r) => Text -> PG.Query -> q -> App [r] `. Query the database given a query and arguments expecting result rows
+ `queryPG_ :: (PG.FromRow r) => Text -> PG.Query -> App [r]`. Query the database given a query with no arguments expecting result rows
+ `executePG :: (PG.ToRow q) => Text -> PG.Query -> q -> App ()`. Execute a query on the database given a query and arguments with no expected results
+ `executePG_ :: Text -> PG.Query -> App ()`. Execute a query on the database given a query with no arguments expecting no results
+ `executeManyPG :: (PG.ToRow q) => Text -> PG.Query -> [q] -> App ()`. Execute a query on the database with many times with different arguments expecting no results

+ `asSingleRow :: App [a] -> App a`. Helper function for when you expect a DB query to only return a single row. Will throw `NotFound` if no rows are returned

+ `getSession :: Text -> App (Maybe Session)`. Fetch a session given its key
+ `putSession :: Text -> Session -> App ()`. Insert/modify a session given a key and session
+ `deleteSession :: Text -> App ()`. Delete a session given its key. Does nothing if the sessionKey does not exist.


