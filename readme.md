What is it?
-----------

Glutton is an RSS/ATOM feed reader for a single user that uses the browser for its user interface.

How do I get it?
----------------

Install GHC 7.8+ and cabal-install 1.22+ and run:

```
git clone https://github.com/alancocanour/glutton.git glutton
cd glutton
cabal sandbox init
cabal update
cabal install
```

This will place the `glutton` executable in `$HOME/.cabal/bin`

How do I use it?
----------------

- Create a configuration file at `$HOME/.glutton/glutton.conf`. You can see the definition of the configuration file format in the `Glutton.Config` module or just modify this example:

  ```
  Config { refreshTime = 1200
         , port = 9999
         , feeds = [ "http://example.com/something.xml"
                   , "https://example.com/blah/blah/blah" ]
         }
  ```
- Run the `glutton` executable

- Open `http://localhost:9999/` in your browser. (Use the port you specified in the config file)
